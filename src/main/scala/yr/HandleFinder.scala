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
import org.maraist.fa.util.IndexSetsTracker
import org.maraist.fa.full.{UnindexedFA, NFA, DFA, NFABuilder}
import org.maraist.fa.styles.{AutomatonStyle}
import org.maraist.fa.elements.NFAelements
import org.maraist.fa.util
import org.maraist.planrec.rules.{All,One,Act,TriggerHint,TriggerMatchIndex}
import org.maraist.planrec.rules.HTNLib
import org.maraist.planrec.rules.HTN.*
import org.maraist.planrec.terms.TermImpl
import org.maraist.planrec.terms.Term.termHead
import TriggerHint.*
import org.maraist.planrec.terms.{>?<, >><<}
import scala.compiletime.ops.any

// =================================================================

class HandleFinder[T, H, S]

extends NFABuilder[
  HState[T, H, S], H,
  Set,
  [X, Y] =>> DFA[X, Y, AutomatonStyle],
  [X, Y] =>> NFA[
    X, Y, Set, [X, Y] =>> DFA[X, Y, AutomatonStyle],
    AutomatonStyle, AutomatonStyle],
  NFAelements[HState[T, H, S], H],
  AutomatonStyle,
  AutomatonStyle
] {

  val stationBases = new HashMap[H, Int]

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
      val bareInitial: Item[T, H, S] = initialItem(rule)

      // For One- and Act-rules, we can just add the related items
      // right away.  Otherwise for All-rules, we queue the initial
      // item.  Note that we maintain the invariant that all items in
      // the processing queue are already nodes in the NFA.
      bareInitial match {
        case OneItem(r @ One(_, subgoals, probs), _) => {
          addState(bareInitial)
          addETransition(ruleGoalHead, bareInitial)
          // TODO Do something with probs
          val finalItem = OneItem(r, true)
          addFinalState(finalItem)
          // OK to use triggers instead of actionHints here because
          // they all lead to the finalItem.
          for (t <- bareInitial.triggers)
            do addTransition(bareInitial, t, finalItem)
          for (s <- subgoals)
            do addETransition(bareInitial, s.termHead)
        }

        case ActItem(r, _) => {
          addState(bareInitial)
          addETransition(ruleGoalHead, bareInitial)
          val finalItem = ActItem(r, true)
          addFinalState(finalItem)
          addTransition(bareInitial, r.action.termHead, finalItem)
        }

        case i @ AllItem(r, ready, past) => {
          val isMulti = (i.actionHints.size > 1)

          // Prepare the initial item for adding to the
          // automaton: if it has more that one ready subgoal, then
          // these should be sparked.
          val initial: Sparking[T, H, S] | AllItem[T, H, S] = if isMulti
          then Sparking(List.from(i.triggers), i)
          else i

          // Add the rule station and initial item to the NFA, with an
          // epsilon transition from the station to that item.
          addState(initial)
          addETransition(ruleGoalHead, initial)

          // Set up this initial item for mapping out its
          // successors.
          itemsQueue.enqueue((ruleGoalHead, Set.empty[Int], None, initial))
        }
      }
    }

    // Process the items in the queue
    while (!(itemsQueue.isEmpty)) itemsQueue.dequeue match {
      case (prev, par, trans, item) =>
        encodeItemTransition(prev, par, trans, item, itemsQueue)
    }
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
    nextItem: Sparking[T, H, S] | AllItem[T, H, S],
    queue: ItemsQueue[T, H, S])(
    using TermImpl[T, H, S]):
      Unit = {
    val rule = nextItem match {
      case Sparking(_, AllItem(r, _, _)) => r
      case AllItem(r, _, _) => r
    }

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
    val nextItemBare = nextItem match {
      case i @ AllItem(_, _, _) => i
      case Sparking(_, i) => i
    }
    val nextItemReady = nextItemBare.ready
    val newInNextItem = nextItemReady -- postTransPar

    // This next block is taken care of by the Sparking?

//    // There is already a transition between prev and nextItem, but we
//    // may need to annotate it.
//    if ((wasMulti && newInNextItem.size > 0) || newInNextItem.size > 1)
//      then transIdx match {
//        case None => setEAnnotation(
//          prev, nextItem,
//          NfaAnnotation(List.from(newInNextItem.map(rule.subgoals(_).termHead)))
//        )
//        case Some(idx) => setAnnotation(
//          prev, rule.subgoals(idx).termHead, nextItem,
//          NfaAnnotation(
//            List.from(newInNextItem.map((i) => rule.subgoals(i).termHead)))
//        )
//      }

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
    for (newTransIdx <- nextItemReady) do {
      val newTransTerm = rule.subgoals(newTransIdx)
      val newTrans = newTransTerm.termHead

      // Build the resulting item, and add it to the NFA if it is not
      // already there.
      nextItemBare.applyIdx(newTransIdx) match {
        case None => { }
        case Some(afterNextItem) => afterNextItem match {
          case AllItem(allRule : All[T, H, S], _, _) => {

            // Does the follow-on state introduce additional
            // concurrent states?
            val diffReady = afterNextItem.ready -- nextItemBare.ready
            val afterNextActual: Sparking[T, H, S] | AllItem[T, H, S] =
              if diffReady.size > 1 || (diffReady.size > 0 && wasMulti) then
                Sparking(
                  List.from(diffReady.map(allRule.subgoals(_).termHead)),
                  afterNextItem)
              else afterNextItem

            // Make sure the item is a state in the NFA
            ensureItemAdded(afterNextActual)

            // Add the transition
            addTransition(nextItem, newTrans, afterNextActual)

            // Add a queue entry
            queue.enqueue((
              nextItem, parAfterNext, Some(newTransIdx), afterNextActual))
          }
        }
      }
    }
  }

  def ensureItemAdded(item: Sparking[T, H, S] | AllItem[T, H, S]): Unit =
    if !isState(item) then {
      if (item match {
        case Sparking(_, _) => false
        case i @ AllItem(_, _, _) => i.isFinal
      })
        then addFinalState(item)
      else addState(item)
    }

  override protected def assembleNFA(
    statesSeq: IndexedSeq[HState[T, H, S]],
    initials: Set[Int],
    finals: Set[Int],
    transitionsSeq: IndexedSeq[H],
    labelsArray: Array[Array[Set[Int]]],
    epsilonsArray: Array[Set[Int]]):
      HandleNFA[HState[T, H, S], H] =
    new HandleNFA[HState[T, H, S], H](
      statesSeq, initials, finals, transitionsSeq, labelsArray, epsilonsArray
    ) {

      override protected def afterStatePlot(
        sb: StringBuilder,
        style: AutomatonStyle[HState[T, H, S], H],
        stateList: IndexedSeq[HState[T, H, S]],
        stateMap: Map[HState[T, H, S], Int]):
          Unit = {
        super.afterStatePlot(sb, style, stateList, stateMap)
        foreachState((s) => s match {
          case Sparking(indirects, _): Sparking[T, H, S] =>
            for (ind <- indirects)
              do this.plotAnnotationEdge(sb, stateMap(s), stateMap(ind))

          case _ => { }
        })
      }

      override def seedAdditionalDfaStates(tracker: IndexSetsTracker): Unit = {
        foreachState(_ match {
          case Sparking(ids, _): Sparking[T, H, S] => {
            ids.map((station) => this.stationBases.getOrElseUpdate(station, {
              val (stationClosed, _) =
                epsilonCloseIndices(Set(indexOf(station)))
              tracker.getIndex(stationClosed)
            }))
          }
          case _ => { }
        })
      }
    }

  override protected def derivedNFA[SS, TT](
    statesSeq: IndexedSeq[SS],
    transitionsSeq: IndexedSeq[TT],
    initials: Set[Int],
    finals: Set[Int],
    labelsArray: Array[Array[Set[Int]]],
    epsilonsArray: Array[Set[Int]]):
      HandleNFA[SS, TT] =
    new HandleNFA[SS, TT](
      statesSeq, initials, finals, transitionsSeq, labelsArray, epsilonsArray
    )

  // =================================================================
  // Draw the extra edges implied by annotations
  // =================================================================

//  override protected def plotPresentEdge(
//    sb: StringBuilder,
//    style: AutomatonStyle[
//      HState[T, H, S], H, NfaAnnotation[T, H, S]],
//    stateList: IndexedSeq[HState[T, H, S]],
//    stateMap: Map[HState[T, H, S], Int],
//    si0: Int, s0: HState[T, H, S],
//    ti0: Int, t: H,
//    si1: Int, s1: HState[T, H, S]):
//      Unit = {
//    super.plotPresentEdge(
//      sb, style, stateList, stateMap, si0, s0, ti0, t, si1, s1)
//    annotation(s0, t, s1) match {
//      case None => { }
//      case Some(NfaAnnotation(indirects)) => {
//        for (h <- indirects)
//          do plotAnnotationEdge(sb, si1, stateList.indexOf(h))
//      }
//    }
//  }
//
//  override protected def plotPresentEdge(
//    sb: StringBuilder,
//    style: AutomatonStyle[
//      HState[T, H, S], H, NfaAnnotation[T, H, S]],
//    stateList: IndexedSeq[HState[T, H, S]],
//    stateMap: Map[HState[T, H, S], Int],
//    si0: Int, s0: HState[T, H, S],
//    si1: Int, s1: HState[T, H, S]):
//      Unit = {
//    super.plotPresentEdge(
//      sb, style, stateList, stateMap, si0, s0, si1, s1)
//    annotation(s0, s1) match {
//      case None => { }
//      case Some(NfaAnnotation(indirects)) => {
//        for (h <- indirects)
//          do plotAnnotationEdge(sb, si1, stateList.indexOf(h))
//      }
//    }
//  }

  override protected def afterStatePlot(
    sb: StringBuilder,
    style: AutomatonStyle[HState[T, H, S], H],
    stateList: IndexedSeq[HState[T, H, S]],
    stateMap: Map[HState[T, H, S], Int]):
      Unit = {
    super.afterStatePlot(sb, style, stateList, stateMap)
    foreachState((s) => {
      println(s)
      s match {
      case Sparking(indirects, _): Sparking[T, H, S] => {
        val sIdx = stateMap(s)
        for (ind <- indirects) {
          plotAnnotationEdge(sb, sIdx, stateMap(ind))
        }
      }

      case _ => { }
      }
    })
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

class HandleNFA[SS, TT](
  override val stateSeq: IndexedSeq[SS],
  override val initialStateIndices: Set[Int],
  override val finalStateIndices: Set[Int],
  override val transitionsSeq: IndexedSeq[TT],
  override val transitionsArray: Array[Array[Set[Int]]],
  override val epsilonsArray: Array[Set[Int]])

extends NFA[
  SS, TT, Set, HandleDFA, AutomatonStyle, AutomatonStyle
] {

  val stationBases = new HashMap[TT, Int]

  override protected def assembleDFA(
    dfaStates: IndexedSeq[Set[SS]],
    initialStateIdx: Int,
    dfaFinals: Set[Int],
    transitionsSeq: IndexedSeq[TT],
    dfaTransitions: Array[Array[Int]],
    tracker: IndexSetsTracker,
    appearsIn: Array[Set[Int]]):
      HandleDFA[Set[SS], TT] = {
    stationBases.clear
    new HandleDFA[Set[SS], TT](
      dfaStates, initialStateIdx, dfaFinals, transitionsSeq, dfaTransitions)
  }

  override def derivedNFA[SS, TT](
    statesSeq: IndexedSeq[SS],
    transitionsSeq: IndexedSeq[TT],
    labelsArray: Array[Array[Set[Int]]],
    epsilonsArray: Array[Set[Int]],
    finals: Set[Int],
    initials: Set[Int]):
      HandleNFA[SS, TT] =
    new HandleNFA[SS, TT](
      statesSeq, initials, finals, transitionsSeq, labelsArray, epsilonsArray
    )

  // =================================================================
  // Exploit the hook into Rabin-Scott to add additional states into
  // the DFA
  // =================================================================

//  val combiner: EdgeAnnotationCombiner[
//    NfaAnnotation[T, TT, S],
//    Set[DfaAnnotation[T, TT, S]]
//  ] = new EdgeAnnotationCombiner[
//    NfaAnnotation[T, TT, S],
//    Set[DfaAnnotation[T, TT, S]]
//  ] {
//    def single(a: NfaAnnotation[T, TT, S]): Set[DfaAnnotation[T, TT, S]] =
//      Set(DfaAnnotation(for (h <- a.indirects) yield (h, stationBases(h))))
//    def include(k: Set[DfaAnnotation[T, TT, S]], a: NfaAnnotation[T, TT, S]):
//        Set[DfaAnnotation[T, TT, S]] =
//      k + DfaAnnotation(for (h <- a.indirects) yield (h, stationBases(h)))
//    def combine(
//      k1: Set[DfaAnnotation[T, TT, S]],
//      k2: Set[DfaAnnotation[T, TT, S]]):
//        Set[DfaAnnotation[T, TT, S]] = k1 ++ k2
//  }

//  // =================================================================
//  // Draw the extra edges implied by annotations
//  // =================================================================
//
//  override protected def plotPresentEdge(
//    sb: StringBuilder,
//    style: AutomatonStyle[
//      SS, TT, NfaAnnotation[T, TT, S]],
//    stateList: IndexedSeq[SS],
//    stateMap: Map[SS, Int],
//    si0: Int, s0: SS,
//    ti0: Int, t: TT,
//    si1: Int, s1: SS):
//      Unit = {
//    super.plotPresentEdge(
//      sb, style, stateList, stateMap, si0, s0, ti0, t, si1, s1)
//    annotation(s0, t, s1) match {
//      case None => { }
//      case Some(NfaAnnotation(indirects)) => {
//        for (h <- indirects)
//          do plotAnnotationEdge(sb, si1, stateList.indexOf(h))
//      }
//    }
//  }
//
//  override protected def plotPresentEdge(
//    sb: StringBuilder,
//    style: AutomatonStyle[
//      SS, TT, NfaAnnotation[T, TT, S]],
//    stateList: IndexedSeq[SS],
//    stateMap: Map[SS, Int],
//    si0: Int, s0: SS,
//    si1: Int, s1: SS):
//      Unit = {
//    super.plotPresentEdge(
//      sb, style, stateList, stateMap, si0, s0, si1, s1)
//    annotation(s0, s1) match {
//      case None => { }
//      case Some(NfaAnnotation(indirects)) => {
//        for (h <- indirects)
//          do plotAnnotationEdge(sb, si1, stateList.indexOf(h))
//      }
//    }
//  }

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

class HandleDFA[SS, TT](
  override protected val stateSeq: IndexedSeq[SS],
  override val initialStateIndex: Int,
  override val finalStateIndices: Set[Int],
  override protected val transitionsSeq: IndexedSeq[TT],
  override protected val transitionsMatrix: Array[Array[Int]]
)

extends DFA[SS, TT, AutomatonStyle] {

//  override protected def plotPresentEdge(
//    sb: StringBuilder,
//    style: AutomatonStyle[
//      SS, TT, Set[DfaAnnotation[T, TT, S]]],
//    stateList: IndexedSeq[SS],
//    stateMap: Map[SS, Int],
//    si0: Int,
//    s0: SS,
//    ti0: Int,
//    t: TT,
//    si1: Int,
//    s1: SS
//  ): Unit = {
//    super.plotPresentEdge(
//      sb, style, stateList, stateMap, si0, s0, ti0, t, si1, s1)
//    annotation(s0, t, s1) match {
//      case None => { }
//      case Some(sets) => plotAnnotationEdges(sb, si1, sets)
//    }
//  }
//
//  def plotAnnotationEdges(
//    sb: StringBuilder, fromIndex: Int, sets: Set[DfaAnnotation[T, TT, S]]):
//      Unit =
//    for (DfaAnnotation(indirects) <- sets; (_, h) <- indirects)
//      do plotAnnotationEdge(sb, fromIndex, h)
//
//  def plotAnnotationEdge(sb: StringBuilder, fromIndex: Int, toIndex: Int):
//      Unit = {
//    sb ++= "\tV"
//    sb ++= fromIndex.toString
//    sb ++= " -> V"
//    sb ++= toIndex.toString
//    sb ++= " [arrowhead = none, style = dotted];\n"
//  }
//
//  /** {@inheritDoc} Overridden in
//    * [[org.maraist.planrec.yr.HandleDFA][HandleDFA]] to add dotted
//    * lines to concurrent states.
//    */
//  override protected
//  def plotInitialStateMarker(
//    sb: StringBuilder,
//    style: AutomatonStyle[
//      SS,
//      TT,
//      Set[DfaAnnotation[T, TT, S]]],
//    s: SS,
//    idx: Int):
//      Unit = {
//    super.plotInitialStateMarker(sb, style, s, idx)
//
//    initialAnnotation match {
//      case None => { }
//      case Some(ann) => plotAnnotationEdges(sb, idx, ann)
//    }
//  }

  def assembleDFA[SS, TT](
    stateSeq: IndexedSeq[SS],
    transitionsSeq: IndexedSeq[TT],
    initialStateIndex: Int,
    finalStateIndices: Set[Int],
    transitionsMatrix: Array[Array[Int]]):
      HandleDFA[SS, TT] =
    new HandleDFA[SS, TT](
      stateSeq, initialStateIndex, finalStateIndices,
      transitionsSeq, transitionsMatrix)
}

// =================================================================
