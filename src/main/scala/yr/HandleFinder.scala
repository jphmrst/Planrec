// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.yr
import scala.collection.mutable.{HashMap, Queue}
import org.maraist.graphviz.Graphable
import org.maraist.fa
import org.maraist.fa.util.{EdgeAnnotationCombiner,IndexSetsTracker}
import org.maraist.fa.full.{
  UnindexedEdgeAnnotatedFA,
  EdgeAnnotatedNFA, EdgeAnnotatedDFA, EdgeAnnotatedNFABuilder}
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

trait Combinable[T, H, S] {
  def stationBases: HashMap[H, Int]
}

class Combiner[T, H, S](base: Combinable[T, H, S])
extends EdgeAnnotationCombiner[
  NfaAnnotation[T, H, S], Set[DfaAnnotation[T, H, S]]
] {

  def single(a: NfaAnnotation[T, H, S]): Set[DfaAnnotation[T, H, S]] =
    Set(DfaAnnotation(for (h <- a.indirects) yield (h, base.stationBases(h))))

  def include(k: Set[DfaAnnotation[T, H, S]], a: NfaAnnotation[T, H, S]):
      Set[DfaAnnotation[T, H, S]] =
    k + DfaAnnotation(for (h <- a.indirects) yield (h, base.stationBases(h)))

  def combine(
    k1: Set[DfaAnnotation[T, H, S]],
    k2: Set[DfaAnnotation[T, H, S]]):
      Set[DfaAnnotation[T, H, S]] = k1 ++ k2
}

// =================================================================

class HandleFinder[T, H, S]

extends EdgeAnnotatedNFABuilder[
  HState[T, H, S], H,
  NfaAnnotation[T, H, S], Set[DfaAnnotation[T, H, S]],
  Set,
  [DS, DT, DDA] =>> EdgeAnnotatedDFA[DS, DT, DDA, EdgeAnnotatedAutomatonStyle],
  [DS, DT, DNA, DDA] =>> EdgeAnnotatedNFA[
    DS, DT, DNA, DDA, Set,
    [NDS, NDT, NDDA] =>> EdgeAnnotatedDFA[
      NDS, NDT, NDDA, EdgeAnnotatedAutomatonStyle],
    EdgeAnnotatedAutomatonStyle,
    EdgeAnnotatedAutomatonStyle
  ],
  EdgeAnnotatedNFAelements[HState[T, H, S], H, NfaAnnotation[T, H, S]],
  EdgeAnnotatedAutomatonStyle,
  EdgeAnnotatedAutomatonStyle
]

with Combinable[T, H, S] {

  override val stationBases = new HashMap[H, Int]

  override val combiner: EdgeAnnotationCombiner[
    NfaAnnotation[T, H, S], Set[DfaAnnotation[T, H, S]]] = new Combiner(this)

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

  /** Add the necessary components to an
    * [[fa.full.NFABuilder][NFABuilder]] for the transition from a
    * `prev` item to the `next` item, and queue up any succeeding
    * transitions.
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

  override protected def assembleNFA(
    statesSeq: IndexedSeq[HState[T, H, S]],
    initials: Set[Int],
    finals: Set[Int],
    transitionsSeq: IndexedSeq[H],
    labelsArray: Array[Array[Set[Int]]],
    epsilonsArray: Array[Set[Int]],
    labelledEdgeAnnotations:
        Array[Array[Array[Option[NfaAnnotation[T, H, S]]]]],
    unlabelledEdgeAnnotations: Array[Array[Option[NfaAnnotation[T, H, S]]]]
  ): HandleNFA[T, H, S] =
    new HandleNFA[T, H, S](
      statesSeq, initials, finals, transitionsSeq, labelsArray, epsilonsArray,
      labelledEdgeAnnotations, unlabelledEdgeAnnotations
    )

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
        for (h <- indirects)
          do plotAnnotationEdge(sb, si1, stateList.indexOf(h))
      }
    }
  }

  override protected def plotPresentEdge(
    sb: StringBuilder,
    style: EdgeAnnotatedAutomatonStyle[
      HState[T, H, S], H, NfaAnnotation[T, H, S]],
    stateList: IndexedSeq[HState[T, H, S]],
    stateMap: Map[HState[T, H, S], Int],
    si0: Int, s0: HState[T, H, S],
    si1: Int, s1: HState[T, H, S]):
      Unit = {
    super.plotPresentEdge(
      sb, style, stateList, stateMap, si0, s0, si1, s1)
    annotation(s0, s1) match {
      case None => { }
      case Some(NfaAnnotation(indirects)) => {
        for (h <- indirects)
          do plotAnnotationEdge(sb, si1, stateList.indexOf(h))
      }
    }
  }

  def plotAnnotationEdge(sb: StringBuilder, fromIndex: Int, toIndex: Int):
      Unit = {
    sb ++= "\tV"
    sb ++= fromIndex.toString
    sb ++= " -> V"
    sb ++= toIndex.toString
    sb ++= " [arrowhead = none, style = dotted];\n"
  }
}

// =================================================================

class HandleNFA[T, H, S](
  override val stateSeq: IndexedSeq[HState[T, H, S]],
  override val initialStateIndices: Set[Int],
  override val finalStateIndices: Set[Int],
  override val transitionsSeq: IndexedSeq[H],
  override val transitionsArray: Array[Array[Set[Int]]],
  override val epsilonsArray: Array[Set[Int]],
  override val labelledEdgeAnnotations:
      Array[Array[Array[Option[NfaAnnotation[T, H, S]]]]],
  override val unlabelledEdgeAnnotations:
      Array[Array[Option[NfaAnnotation[T, H, S]]]])

extends EdgeAnnotatedNFA[
  HState[T, H, S], H, NfaAnnotation[T, H, S], Set[DfaAnnotation[T, H, S]], Set,
  [DS, DT, DDA] =>>
    EdgeAnnotatedDFA[DS, DT, DDA, EdgeAnnotatedAutomatonStyle],
  EdgeAnnotatedAutomatonStyle, EdgeAnnotatedAutomatonStyle
]

with Combinable[T, H, S] {

  val stationBases = new HashMap[H, Int]

  override protected def assembleDFA(
    dfaStates: IndexedSeq[Set[HState[T, H, S]]],
    initialStateIdx: Int,
    dfaFinals: Set[Int],
    transitionsSeq: IndexedSeq[H],
    dfaTransitions: Array[Array[Int]],
    tracker: IndexSetsTracker,
    appearsIn: Array[Set[Int]],
    edgeAnnotations: Array[Array[Option[Set[DfaAnnotation[T, H, S]]]]]):
      HandleDFA[T, H, S] = {
    stationBases.clear
    new HandleDFA[T, H, S](
      dfaStates, initialStateIdx, dfaFinals, transitionsSeq,
      dfaTransitions, edgeAnnotations)
  }

  // =================================================================
  // Exploit the hook into Rabin-Scott to add additional states into
  // the DFA
  // =================================================================

  val combiner: EdgeAnnotationCombiner[
    NfaAnnotation[T, H, S],
    Set[DfaAnnotation[T, H, S]]
  ] = new EdgeAnnotationCombiner[
    NfaAnnotation[T, H, S],
    Set[DfaAnnotation[T, H, S]]
  ] {
    def single(a: NfaAnnotation[T, H, S]): Set[DfaAnnotation[T, H, S]] =
      Set(DfaAnnotation(for (h <- a.indirects) yield (h, stationBases(h))))
    def include(k: Set[DfaAnnotation[T, H, S]], a: NfaAnnotation[T, H, S]):
        Set[DfaAnnotation[T, H, S]] =
      k + DfaAnnotation(for (h <- a.indirects) yield (h, stationBases(h)))
    def combine(
      k1: Set[DfaAnnotation[T, H, S]],
      k2: Set[DfaAnnotation[T, H, S]]):
        Set[DfaAnnotation[T, H, S]] = k1 ++ k2
  }

  override def seedAdditionalDfaStates(tracker: IndexSetsTracker): Unit = {
    foreachEdgeAnnotation((
      src: HState[T, H, S], dest: HState[T, H, S],
      ann: NfaAnnotation[T, H, S]) => ann match {
      case NfaAnnotation(ids) => {
        ids.map((id) => queueStation(id, tracker))
      }
    })
    foreachEdgeAnnotation((
      src: HState[T, H, S], trans: H, dest: HState[T, H, S],
      ann: NfaAnnotation[T, H, S]) => ann match {
      case NfaAnnotation(ids) => {
        ids.map((id) => queueStation(id, tracker))
      }
    })
  }

  protected def queueStation(station: H, tracker: IndexSetsTracker): Unit = {
    stationBases.getOrElseUpdate(station, {
      val (stationClosed, _) = epsilonCloseIndices(Set(indexOf(station)))
      tracker.getIndex(stationClosed)
    })
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
        for (h <- indirects)
          do plotAnnotationEdge(sb, si1, stateList.indexOf(h))
      }
    }
  }

  override protected def plotPresentEdge(
    sb: StringBuilder,
    style: EdgeAnnotatedAutomatonStyle[
      HState[T, H, S], H, NfaAnnotation[T, H, S]],
    stateList: IndexedSeq[HState[T, H, S]],
    stateMap: Map[HState[T, H, S], Int],
    si0: Int, s0: HState[T, H, S],
    si1: Int, s1: HState[T, H, S]):
      Unit = {
    super.plotPresentEdge(
      sb, style, stateList, stateMap, si0, s0, si1, s1)
    annotation(s0, s1) match {
      case None => { }
      case Some(NfaAnnotation(indirects)) => {
        for (h <- indirects)
          do plotAnnotationEdge(sb, si1, stateList.indexOf(h))
      }
    }
  }

  def plotAnnotationEdge(sb: StringBuilder, fromIndex: Int, toIndex: Int):
      Unit = {
    sb ++= "\tV"
    sb ++= fromIndex.toString
    sb ++= " -> V"
    sb ++= toIndex.toString
    sb ++= " [arrowhead = none, style = dotted];\n"
  }
}

// =================================================================

class HandleDFA[T, H, S](
  protected val stateSeq: IndexedSeq[Set[HState[T, H, S]]],
  val initialStateIndex: Int,
  val finalStateIndices: Set[Int],
  protected val transitionsSeq: IndexedSeq[H],
  protected val transitionsMatrix: Array[Array[Int]],
  protected val edgeAnnotations:
      Array[Array[Option[Set[DfaAnnotation[T, H, S]]]]]
)

extends EdgeAnnotatedDFA[
  Set[HState[T, H, S]], H,
  Set[DfaAnnotation[T, H, S]],
  EdgeAnnotatedAutomatonStyle
] {

  override protected def plotPresentEdge(
    sb: StringBuilder,
    style: EdgeAnnotatedAutomatonStyle[
      Set[HState[T, H, S]], H, Set[DfaAnnotation[T, H, S]]],
    stateList: IndexedSeq[Set[HState[T, H, S]]],
    stateMap: Map[Set[HState[T, H, S]], Int],
    si0: Int,
    s0: Set[HState[T, H, S]],
    ti0: Int,
    t: H,
    si1: Int,
    s1: Set[HState[T, H, S]]
  ): Unit = {
    super.plotPresentEdge(
      sb, style, stateList, stateMap, si0, s0, ti0, t, si1, s1)
    annotation(s0, t, s1) match {
      case None => { }
      case Some(sets) => {
        for (DfaAnnotation(indirects) <- sets; (_, h) <- indirects)
          do plotAnnotationEdge(sb, si1, h)
      }
    }
  }

  def plotAnnotationEdge(sb: StringBuilder, fromIndex: Int, toIndex: Int):
      Unit = {
    sb ++= "\tV"
    sb ++= fromIndex.toString
    sb ++= " -> V"
    sb ++= toIndex.toString
    sb ++= " [arrowhead = none, style = dotted];\n"
  }
}

// =================================================================
