// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa
import scala.collection.mutable.{ArrayBuffer,ListBuffer,HashSet,Queue}
import org.maraist.util.IndexSetsTracker

/** Immutable [[org.maraist.fa.NDFA NDFA]] implementation using
  * [[scala.collection.immutable.IndexedSeq `IndexedSeq`s]] and
  * `Array`s.
  *
  * @param stateSeq IndexedSeq of the actual state objects
  * @param initialStateSet Set of indices into `stateSeq` of the initial states
  * @param finalStateSet Set of indices into `stateSeq` of the final states
  * @param transitionsSeq IndexedSeq of the actual objects used as transition
  * labels
  * @param labels Matrix (2x2 array) mapping the set of states at the end of
  * transitions from the state and with the given label, the latter two given
  * by their index in `stateSeq` and `transitionsSeq` respectively
  * @param epsilons Array mapping each state to the states mapped from it by an
  * &epsilon;-transition
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  * @tparam ThisDFA Specific class of the assembled DFA
  * {@link org.maraist.fa.ArrayDFA ArrayDFA}.
  */
abstract class AbstractArrayNDFA[S, T, +ThisDFA <: AbstractArrayDFA[Set[S],T]](
  private val stateSeq: IndexedSeq[S],
  private val initialStateSet: Set[Int],
  private val finalStateSet: Set[Int],
  private val transitionsSeq: IndexedSeq[T],
  private val transitionsArray: Array[? <: Array[? <: Set[Int]]],
  private val epsilons: Array[? <: Set[Int]])
    extends IndexedNDFA[S,T,ThisDFA] {

  def size: Int = stateSeq.length
  def states: IndexedSeq[S] = stateSeq
  def labels: IndexedSeq[T] = transitionsSeq
  /** Retrieve the state with index `i` */
  def state(i:Int):S = stateSeq(i)
  def label(i: Int):T = transitionsSeq(i)
  def labelIndex(t:T):Int = transitionsSeq.indexOf(t)
  def transitionIndex(si: Int,ti: Int): Option[Int] = ???
  /** Retrieve the index of state `s` */
  def indexOf(s:S):Int = stateSeq.indexOf(s)
  /** Retrieve the indices of initial states */
  def initialStateIndices: Set[Int] = initialStateSet.toSet
  /** Retrieve the indices of finals states */
  def finalStateIndices: Set[Int] = finalStateSet.toSet
  def initialStates: Set[S] = initialStateSet.map(stateSeq)
  def finalStates: Set[S] = finalStateSet.map(stateSeq)
  def isState(s:S):Boolean = stateSeq.contains(s)
  def isInitialState(s:S):Boolean = initialStateSet.contains(indexOf(s))
  def isFinalState(s:S):Boolean = finalStateSet.contains(indexOf(s))

  def transitions(s:S, t:T):Set[S] = transitionIndices(s,t).map(stateSeq)
  /** Retrieve the indices of states found at the end of transitions labeled
    * `t` and starting from `s` */
  def transitionIndices(s:S, t:T):Set[Int] =
    transitionsArray(indexOf(s))(transitionsSeq.indexOf(t))
  def eTransitions(s:S):Set[S] = {
    val eti = eTransitionIndices(s)
    // println(eti)
    eti.map(stateSeq)
  }
  /** Retrieve the indices of states found at the end of &epsilon;-transitions
    * starting from `s` */
  def eTransitionIndices(s:S):Set[Int] = {
    val index = indexOf(s)
    if (index<0) {
      throw new IllegalArgumentException(s.toString() + " not a state in NDFA")
    }
    epsilons(index)
  }

  protected def epsilonCloseIndex(si:Int): (Set[Int],Boolean) = {
    val queue = Set(si)
    epsilonCloseIndices(queue)
  }
  protected def epsilonCloseIndices(source:Set[Int]): (Set[Int],Boolean) = {
    // println("    * Closing " + source)
    // println("      Epsilons " + epsilons)
    // for (ep <- epsilons) { println("        " + ep) }
    val result = Set.newBuilder[Int]
    var isFinal = false

    for(id <- source) {
      if (finalStateSet.contains(id)) isFinal=true
      result += id
    }

    val queue:Queue[Int] = Queue.from[Int](source)
    while (!queue.isEmpty) {
      val thisIdx:Int = queue.dequeue()
      // println("    ** "+thisIdx)
      for(newIdx <- epsilons(thisIdx)) {
        // println("    ** ** "+newIdx)
        if (!result.result().contains(newIdx)) {
          result += newIdx
          queue += newIdx
          if (finalStateSet.contains(newIdx)) isFinal=true
        }
      }
    }
    (result.result(), isFinal)
  }

  def seedAdditionalInitialStates(tracker:IndexSetsTracker):Unit = { }

  def toDFA: ThisDFA = {// scalastyle:ignore cyclomatic.complexity method.length
    // Set up components of ArrayDFA.  We can't convert to a
    // transitions array until the end, so for now we build a map over
    // state/transition label indices.  Note that we just use the same
    // transitions as for this ArrayNDFA.
    val dfaIndexSets:ListBuffer[Set[Int]] = new ListBuffer[Set[Int]]
    val dfaFinals:HashSet[Int] = new HashSet[Int]

    // It will be useful to know the DFA states in which a particular
    // NDFA state appears
    val appearsIn =
      Array.fill[HashSet[Int]](stateSeq.length)(new HashSet[Int]())

    // Set up tracker for state sets
    val tracker = new IndexSetsTracker(stateSeq.length, dfaIndexSets)

    // Build initial state
    val (initialClosure,initialIsFinal) =
      epsilonCloseIndices(initialStateIndices)
    val initialState = tracker.getIndex(initialClosure)
    if (initialState != 0)
      throw new Exception("Unexpected non-zero from first addition to tracker")
    if (initialIsFinal) dfaFinals += initialState
    for(initialNda <- initialClosure) {
      appearsIn(initialNda) += initialState
    }

    // Later versions of the NDFA will have hyperedge annotations;
    // we have a thunk here to add more states to the queue
    seedAdditionalInitialStates(tracker)

    // Iterate through the DFA state list from beginning, adding new
    // states to the end
    val dfaTransitions = new ArrayBuffer[Array[Int]]
    var nextState = initialState
    val labelCount = transitionsSeq.size
    while(nextState < tracker.size) {
      val s = tracker(nextState)
      val thisRow : Array[Int] = Array.ofDim[Int](labelCount)
      dfaTransitions.insert(nextState, thisRow)

      // The states are the rows of the transitions table; now iterate
      // through the transitions label indices for the columns
      for(ti:Int <- 0 until labelCount) {
        val t:T = transitionsSeq(ti)

        // We build the set of states to which this state
        // set/transition pair could lead
        val destinationsBuilder = Set.newBuilder[Int]
        for(origIndex <- s) {
          val destIdxs = transitionsArray(origIndex)(ti)
          for(destIdx:Int <- destIdxs) {
            destinationsBuilder += destIdx
          }
        }

        val destinations = destinationsBuilder.result()
        // If the destinations set is empty, then it's an error state
        if (destinations.isEmpty) {
          // Add an error transition
          thisRow(ti) = -1

        } else {

          // Close the set under epsilon transitions, cache it, and
          // possibly set it as final
          epsilonCloseIndices(destinations) match {
            case (closedSet, hasFinal) => {
              val newStateIdx = tracker.getIndex(closedSet)
              for(state <- closedSet) appearsIn(state) += newStateIdx
              if (hasFinal) dfaFinals += newStateIdx

              // Add the new destination index to the action tables
              thisRow(ti) = newStateIdx
            }
          }
        }
      }
      nextState += 1
    }

    val dfaStates = ArrayBuffer.from(dfaIndexSets.map(_.map(stateSeq(_))))
    val dfaTransArray = Array.from(dfaTransitions)

    // Assemble components
    assembleDFA(
      IndexedSeq.from(dfaStates), 0, dfaFinals.toSet, transitionsSeq,
      dfaTransArray, tracker, appearsIn.map(_.toSet))
  }

  /**
   * This method is implemented by subclasses where
   * {@org.maraist.fa.NDFA#ThisDFA ThisDFA} is concretized, to provide a DFA
   * implementation of the specific type.
   */
  protected def assembleDFA(dfaStates:IndexedSeq[Set[S]],
                            initialStateIdx:Int,
                            dfaFinals:Set[Int],
                            transitionsSeq:IndexedSeq[T],
                            dfaTransitions:Array[Array[Int]],
                            tracker:IndexSetsTracker,
                            appearsIn:Array[Set[Int]]):ThisDFA
}
