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
import org.maraist.graphviz.Graphable
import org.maraist.fa.EdgeAnnotatedNFABuilder.Completer
import org.maraist.fa.{EdgeAnnotatedNFA, EdgeAnnotatedDFA}
import org.maraist.fa.full.EdgeAnnotatedNFABuilder
import org.maraist.fa.styles.{EdgeAnnotatedAutomatonStyle}
import org.maraist.fa.elements.EdgeAnnotatedNFAelements
import org.maraist.fa.util
import org.maraist.planrec.rules.{All,One,Act,TriggerHint,TriggerMatchIndex}
import org.maraist.planrec.rules.HTNLib
import org.maraist.planrec.rules.HTN.*
import org.maraist.planrec.terms.TermImpl
import org.maraist.planrec.terms.Term.termHead
import TriggerHint.*
import org.maraist.planrec.terms.{>?<, >><<}
import scala.compiletime.ops.any

class HandleFinder[T, H, S] extends EdgeAnnotatedNFABuilder[
  HState[T, H, S], H,
  NfaAnnotation[T, H, S], Set[NfaAnnotation[T, H, S]],
  Set,
  EdgeAnnotatedDFA, EdgeAnnotatedNFA,
  EdgeAnnotatedNFAelements[HState[T, H, S], H, NfaAnnotation[T, H, S]],
  EdgeAnnotatedAutomatonStyle,
  EdgeAnnotatedAutomatonStyle
]

with Completer[
  HState[T, H, S], H,
  NfaAnnotation[T, H, S], Set[NfaAnnotation[T, H, S]]
] {


  def libToNFA(library: HTNLib[T, H, S])(using TermImpl[T, H, S]):
      Unit = {

    // First add all of the rule heads as stations.
    for (rule <- library.rules) do {
      val head = rule.goal.termHead

      // Add the rule goal head as a station to the NFA if it is not
      // already present.
      if !isState(head) then {
        if library.top.contains(head)
        then addInitialState(head)
        else addState(head)
      }
    }

    // Queue for all of the items which need to be processed.
    val itemsQueue = new ItemsQueue[T, H, S]

    // Process each rule, or stage it for processing.
    for (rule <- library.rules) do {
      val ruleGoalHead = rule.goal.termHead

      // Add the rule station and initial item to the NFA, with an
      // epsilon transition from the station to that item.
      val initial = initialItem(rule)
      if !isState(initial) then addState(initial)
      addETransition(ruleGoalHead, initial)

      // For One- and Act-rules, we can just add the related items
      // right away.  Otherwise for All-rules, we queue the initial
      // item.  Note that we maintain the invariant that all items in
      // the processing queue are already nodes in the NFA.
      rule match {
        case r @ One(_, subgoals, probs) => { // TODO Do something with probs
          val finalItem = OneItem(r, true)
          addFinalState(finalItem)
          // OK to use triggers instead of actionHints here because
          // they all lead to the finalItem.
          for (t <- initial.triggers)
            do addTransition(initial, t, finalItem)
          for (s <- subgoals)
            do addETransition(initial, s.termHead)
        }
        case r: Act[T, H, S] => {
          val finalItem = ActItem(r, true)
          addFinalState(finalItem)
          addTransition(initial, r.action.termHead, finalItem)
        }
        case r: All[T, H, S] => {
          initial match {
            case i: AllItem[T, H, S] => {
              val isMulti = (i.actionHints.size > 1)

              // If not multi, then add annotations to the epsilon
              // transition for the subgoals.
              if isMulti then
                setEAnnotation(
                  ruleGoalHead, initial, NfaAnnotation(List.from(i.triggers))
                )

              // Set up this initial item for mapping out its
              // successors.
              itemsQueue.enqueue((ruleGoalHead, Set.empty[Int], None, i))
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
        case (prev, par, trans, item) =>
          encodeItemTransition(prev, par, trans, item, itemsQueue)
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
  def encodeItemTransition(
    prev: HState[T, H, S],
    par: Set[Int],
    transIdx: Option[Int],
    nextItem: AllItem[T, H, S],
    queue: ItemsQueue[T, H, S])(
    using TermImpl[T, H, S]):
      Unit = {
    val rule = nextItem.rule

    // Make sure the successor item is in the NFA already
    ensureItemAdded(nextItem)

    // Work out the current spawned tasks _minus_ any in the
    // transition.
    val postTransPar = transIdx match {
      case None => par
      case Some(i) => par - i
    }

    // If there are any spawned tasks left, then nextItem is catching
    // indirected subgoals.
    val wasMulti = !postTransPar.isEmpty

    // We will also need to examine any triggers in the nextItem's
    // ready set which are not already spawned out.
    val newInNextItem = nextItem.ready -- postTransPar

    // There is already a transition between prev and nextItem, but we
    // may need to annotate it.
    if ((wasMulti && newInNextItem.size > 0) || newInNextItem.size > 1)
      then transIdx match {
        case None => setEAnnotation(
          prev, nextItem,
          NfaAnnotation(List.from(newInNextItem.map(rule.subgoals(_).termHead)))
        )
        case Some(idx) => setAnnotation(
          prev, rule.subgoals(idx).termHead, nextItem,
          NfaAnnotation(
            List.from(newInNextItem.map((i) => rule.subgoals(i).termHead)))
        )
      }

    // Calculate the set of spawned terms active with nextItem
    val parAfterNext = if (wasMulti || newInNextItem.size > 1) then {
      postTransPar ++ newInNextItem
    } else {
      Set.empty[Int]
    }


    // TODO Check if nextItem has already been expanded --- skip the
    // next bits if so.

    // If we are not spawning the newly enabled subgoals, then we
    // epsilon-transition to its station.
    if (!wasMulti && newInNextItem.size <= 1) then {
      newInNextItem.map(
        (idx) => addETransition(nextItem, rule.subgoals(idx).termHead))
    }

    // We have (at least) a new queue entry for each subgoal which is
    // ready in the new item.
    for (newTransIdx <- nextItem.ready) do {
      val newTransTerm = rule.subgoals(newTransIdx)
      val newTrans = newTransTerm.termHead

      // Build the resulting item, and add it to the NFA if it is not
      // already there.
      nextItem.applyIdx(newTransIdx) match {
        case None => { }
        case Some(afterNextItem) => {

          // Make sure the item is a state in the NFA
          ensureItemAdded(afterNextItem)

          // Add the transition
          addTransition(nextItem, newTrans, afterNextItem)

          // Add a queue entry
          queue.enqueue((
            nextItem, parAfterNext, Some(newTransIdx), afterNextItem))
        }
      }
    }
  }

  def ensureItemAdded(item: AllItem[T, H, S]): Unit =
    if !isState(item) then {
      if (item.isFinal)
        then addFinalState(item)
      else addState(item)
    }

  // =================================================================
  // Draw the extra edges implied by annotations
  // =================================================================

  override protected def plotPresentEdge(
    sb: StringBuilder,
    style: EdgeAnnotatedAutomatonStyle[
      HState[T, H, S], H, NfaAnnotation[T, H, S]],
    stateList: IndexedSeq[HState[T, H, S]],
    stateMap: Map[HState[T, H, S], Int],
    si0: Int, s0: HState[T, H, S],
    ti0: Int, t: H,
    si1: Int, s1: HState[T, H, S]):
      Unit = {
    super.plotPresentEdge(
      sb, style, stateList, stateMap, si0, s0, ti0, t, si1, s1)
    annotation(s0, t, s1) match {
      case None => { }
      case Some(NfaAnnotation(indirects)) => {
        for (h <- indirects) do plotAnnotationEdge(sb, stateList, si1, h)
      }
    }
  }

  def plotAnnotationEdge(
    sb: StringBuilder, stateList: IndexedSeq[HState[T, H, S]],
    fromIndex: Int, toStation: H
  ): Unit = {
    sb ++= "\tV"
    sb ++= fromIndex.toString
    sb ++= " -> V"
    sb ++= stateList.indexOf(toStation).toString
    sb ++= " [arrowhead = none, style = dotted]\n"
  }
}
