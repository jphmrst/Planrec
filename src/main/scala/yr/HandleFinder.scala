// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.yr.table
import scala.collection.mutable.Queue
import org.maraist.graphviz.{Graphable,GraphvizOptions}
import org.maraist.fa.annotated.
  {EdgeAnnotatedNDFA, NDFAEdgeAnnotationsBuilder,
    HashEdgeAnnotatedNDFABuilder, EdgeAnnotatedDFA, setCombiner}
import org.maraist.planrec.rules.{All,One,Act,TriggerHint,TriggerMatchIndex}
import org.maraist.planrec.rules.HTN.*
import org.maraist.planrec.terms.TermImpl
import org.maraist.planrec.terms.Term.termHead
import TriggerHint.*
import org.maraist.planrec.terms.{>?<, >><<}
import scala.compiletime.ops.any

case class Ind[T, H, S](val rule: HTNrule[T, H, S])

case class NfaAnnotation[T, H, S](indirects: List[H])

type Node[T, H, S] = Item[T, H, S] | H | Ind[T, H, S]

given yrNdaGraphvizOptions[T, H, S]: GraphvizOptions[Node[T, H, S], H] =
  new GraphvizOptions[Node[T, H, S], H](
    nodeShape = "rectangle",
    getNodeLabel = (node: Node[T, H, S], graph: Graphable[Node[T, H, S], H])
      => node match {
        case AllItem(All(goal, subgoals, _), ready) => {
          val sb = new StringBuilder
          sb ++= goal.toString()
          sb ++= " -"
          for (i <- 0 until subgoals.size) do {
            sb ++= " "
            if (ready.contains(i)) { sb ++= "*" }
            sb ++= subgoals(i).toString()
          }
          sb.toString()
        }
        case OneItem(One(goal, subgoals, _), isFinal) => {
          val sb = new StringBuilder
          sb ++= goal.toString()
          sb ++= " - "
          if (!isFinal) { sb ++= "*" }
          sb ++= "("
          var sep = ""
          for (subgoal <- subgoals) do {
            sb ++= sep
            sb ++= subgoal.toString()
            sep = " | "
          }
          sb ++= ")"
          if (isFinal) { sb ++= "*" }
          sb.toString()
        }
        case ActItem(Act(goal, action), isFinal) => {
          val sb = new StringBuilder
          sb ++= goal.toString()
          sb ++= " - "
          if (!isFinal) { sb ++= "*" }
          sb ++= action.toString()
          if (isFinal) { sb ++= "*" }
          sb.toString()
        }
        case ind: Ind[T, H, S] => "NN"
        case station: H => station.toString
      }
  )

type HandleFinder[T, H, S] =
  EdgeAnnotatedDFA[
    Set[Node[T, H, S]],
    H,
    Set[NfaAnnotation[T, H, S]]
  ]

type NondetHandleFinder[T, H, S] =
  EdgeAnnotatedNDFA[
    Node[T, H, S],
    H,
    NfaAnnotation[T, H, S],
    Set[NfaAnnotation[T, H, S]],
    ? <: HandleFinder[T, H, S]
  ]

type NondetHandleFinderBuilder[T, H, S] =
  NDFAEdgeAnnotationsBuilder[
    Node[T, H, S], H,
    NfaAnnotation[T, H, S],
    Set[NfaAnnotation[T, H, S]],
    ?,
    NondetHandleFinder[T, H, S],
    ?
  ]

type NondetHandleFinderBuilderConcrete[T, H, S] =
  HashEdgeAnnotatedNDFABuilder[
    Node[T, H, S],
    H,
    Set[NfaAnnotation[T, H, S]],
    NfaAnnotation[T, H, S]
  ]

type ItemsQueue[T, H, S] =
  Queue[(Option[(AllItem[T, H, S], H)], AllItem[T, H, S], Boolean)]

object HandleFinder {
  import org.maraist.planrec.rules.HTNLib

  def libToNFA[T, H, S](library: HTNLib[T, H, S])(using TermImpl[T, H, S]):
      NondetHandleFinder[T, H, S] = {
    val nfaBuilder = new NondetHandleFinderBuilderConcrete[T, H, S]()

    // First add all of the rule heads as stations.
    for (head <- library.goals.map(_.termHead))
      do {
        if library.top.contains(head)
        then nfaBuilder.addInitialState(head)
        else nfaBuilder.addState(head)
      }

    // Queue for all of the items which need to be processed.
    val itemsQueue =
      new ItemsQueue[T, H, S]

    // Process each rule, or stage it for processing.
    for (rule <- library.rules) do {
      val ruleGoalHead = rule.goal.termHead
      val initial = initialItem(rule)

      // Add the rule station and initial item to the NFA, with an
      // epsilon transition from the station to that item.
      nfaBuilder.addState(initial)
      nfaBuilder.addETransition(ruleGoalHead, initial)

      // For One- and Act-rules, we can just add the related items
      // right away.  Otherwise for All-rules, we queue the initial
      // item.  Note that we maintain the invariant that all items in
      // the processing queue are already nodes in the NFA.
      rule match {
        case r @ One(_, subgoals, probs) => { // TODO Do something with probs
          val finalItem = OneItem(r, true)
          nfaBuilder.addState(finalItem)
          // OK to use triggers instead of actionHints here because
          // they all lead to the finalItem.
          for (t <- initial.triggers)
            do nfaBuilder.addTransition(initial, t, finalItem)
          for (s <- subgoals)
            do nfaBuilder.addETransition(initial, s.termHead)
        }
        case r: Act[T, H, S] => {
          val finalItem = ActItem(r, true)
          nfaBuilder.addState(finalItem)
          nfaBuilder.addTransition(initial, r.action.termHead, finalItem)
        }
        case r: All[T, H, S] => {
          initial match {
            case i: AllItem[T, H, S] => {
              val isMulti = (i.actionHints.size > 1)

              // If multi, then add annotations to the epsilon
              // transition for the subgoals.
              if isMulti then
                nfaBuilder.setEAnnotation(
                  ruleGoalHead, initial, NfaAnnotation(List.from(i.triggers))
                )

              // Set up this initial item for mapping out its
              // successors.
              itemsQueue.enqueue((None, i, isMulti))
            }

            case _ => { } // Other cases not possible, even though
                          // Scala cannot detect.
          }
        }
      }
    }

    // Process the items in the queue
    while (!(itemsQueue.isEmpty))
      itemsQueue.dequeue match
        case (prev, item, multi) =>
          encodeItemTransition(prev, item, nfaBuilder, itemsQueue, multi)

    nfaBuilder.toNDFA
  }

  /** Add the necessary components to an [[NDFABuilder]] for the
    * transition from a `prev` item to the `next` item, and queue up
    * any succeeding transitions.
    *
    * @param prev Item from which the `next` item was derived, if any.
    * @param next The item being added to the NFA.
    * @param queue Queue of items to subsequently be added to the NFA.
    * Additional items pairs may be pushed to the `queue`, but no
    * pairs should be read from it.
    */
  def encodeItemTransition[T, H, S](
    prev: Option[(AllItem[T, H, S], H)],
    nextItem: AllItem[T, H, S],
    nfa: NondetHandleFinderBuilder[T, H, S],
    queue: ItemsQueue[T, H, S],
    succMulti: Boolean):
      Unit = {

    // If there is a previous element, add a transition to it.
    prev.map(_ match {
      case (prevItem, trans) => nfa.addTransition(prevItem, trans, nextItem)
    })

    val prevMulti = prev match {
      case None => succMulti
      case Some(item, _) => (item.actionHints.size > 1)
    }

    // If the resulting state has more than one ready element:
    if (succMulti)
    then {
      // If the prior state does not have multiple ready elements,
      // then we must annotate the jump to multiple elements.
      if !prevMulti then
        prev.map(_ match {
          case (prevItem, trans) =>
            nfa.setAnnotation(
              prevItem, trans, nextItem, NfaAnnotation(List.from(nextItem.triggers))
            )
        })

    } else
      // Otherwise add an epsilon transition to the station of the
      // trigger above.
      prev.map(_ match {
        case (prevItem, trans) =>
          nfa.addETransition(prevItem, trans)
      })

    // Now look at each of the action hints for the succeeding
    // element, and enqueue elements for them.
    for ((trigger, hint) <- nextItem.actionHints)
      do
        nextItem(trigger) match {
          case None => { } // Weird, but whatevs
          case Some(afterNext) =>
            queue.enqueue((
              Some((nextItem, trigger)),
              afterNext,
              afterNext.actionHints.size > 1
            ))
        }
  }
}
