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
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

/** Implementation of [[org.maraist.fa.NDFABuilder NDFABuilder]] using
  * [[scala.collection.mutable.HashMap `HashMap`s]] and
  * [[scala.collection.mutable.HashSet `HashSet`s]]
  * @constructor Returns a builder holding an initially empty automaton
  * @tparam S The type of all states of the automaton
  * @tparam T The type of labels on (non-epsilon) transitions of the automaton
  */
abstract class AbstractHashNDFABuilder[S, T, +ThisDFA <: IndexedDFA[Set[S],T],
                                       +ThisNDFA <: NDFA[S,T,ThisDFA]]
extends NDFABuilder[S,T,ThisDFA,ThisNDFA] {
  /** Set of the states */
  val allStates:HashSet[S] = new HashSet[S]
  /** Set of the initial states */
  val initialStatesSet:HashSet[S] = new HashSet[S]
  /** Set of the final states */
  val finalStatesSet:HashSet[S] = new HashSet[S]
  /** Maps from a state `s` and label `t` to the set of states at the
    * end of transitions starting from `s` and labeled `t` */
  val transitionsMap = new HashMap[S,HashMap[T,HashSet[S]]]
  /** Maps from a state `s` to the set of states at the end of
    * &epsilon;-transitions starting from `s` */
  val epsilons:HashMap[S,HashSet[S]] = new HashMap[S,HashSet[S]]

  def addState(s:S):Unit = allStates += s
  def removeState(s:S):Unit = {
    allStates -= s
    initialStatesSet -= s
    finalStatesSet -= s
    transitionsMap -= s
    epsilons -= s
    for(lmap <- transitionsMap.valuesIterator)
      for(v <- lmap.valuesIterator) v -= s
    for(v <- epsilons.valuesIterator) v -= s
  }
  def addInitialState(s:S):Unit = {
    allStates += s
    initialStatesSet += s
  }
  def removeInitialState(s:S):Unit = initialStatesSet -= s
  def addFinalState(s:S):Unit = {
    allStates += s
    finalStatesSet += s
  }
  def removeFinalState(s:S):Unit = finalStatesSet -= s
  def addTransition(s1:S, t:T, s2:S):Unit = {
    allStates += s1
    allStates += s2
    val submap:HashMap[T,HashSet[S]] =
      transitionsMap.getOrElseUpdate(s1,new HashMap[T,HashSet[S]])
    val subsubmap:HashSet[S] = submap.getOrElseUpdate(t, new HashSet[S])
    subsubmap += s2
  }
  def addETransition(s1:S, s2:S):Unit = {
    // println("** " + s1 + " --> " + s2)
    allStates += s1
    allStates += s2
    val s1Set:HashSet[S] = epsilons.getOrElseUpdate(s1, new HashSet[S])
    s1Set += s2
  }
  def removeTransition(s1:S, t:T, s2:S):Unit = {
    if (transitionsMap.contains(s1)) {
      val submap:HashMap[T,HashSet[S]] = transitionsMap(s1)
      if (submap.contains(t)) submap(t) -= s2
    }
  }
  def removeETransition(s1:S, s2:S):Unit = {
    if (epsilons.contains(s1)) epsilons(s1) -= s2
  }

  def size: Int = allStates.size
  def states: Set[S] = allStates.toSet
  /** @inheritdoc
    *
    * This method is always calculated by traversing the `transitionsMap` map;
    * it is not cached.
    */
  def labels: Set[T] = {
    val result = new HashSet[T]()
    for(map <- transitionsMap.valuesIterator)
      for(t <- map.keysIterator)
        result += t
    result.toSet
  }
  def initialStates: Set[S] = initialStatesSet.toSet
  def finalStates: Set[S] = finalStatesSet.toSet
  def isState(s:S):Boolean = allStates(s)
  def isInitialState(s:S):Boolean = initialStatesSet(s)
  def isFinalState(s:S):Boolean = finalStatesSet(s)

  def transitions(s:S, t:T): Set[S] = {
    if (transitionsMap.contains(s)) {
      val submap:HashMap[T,HashSet[S]] = transitionsMap(s)
      if (submap.contains(t))
        submap(t).toSet
      else
        Set.empty[S]
    } else
      Set.empty[S]
  }
  def eTransitions(s:S): Set[S] = {
    if (epsilons.contains(s))
      epsilons(s).toSet
    else
      Set.empty[S]
  }

  /** @deprecated */
  def toNDFA: ThisNDFA = result()

  /** Creates an immutable [[org.maraist.fa.ArrayNDFA ArrayNDFA]]
    * corresponding to the automaton described to this builder.
    */
  def result(): ThisNDFA = {
    val statesSeq: IndexedSeq[S] = IndexedSeq.from(allStates)
    val transitionsSeq: IndexedSeq[T] = IndexedSeq.from(labels)
    val initials: HashSet[Int] = new HashSet[Int]
    for(s <- initialStatesSet) initials += statesSeq.indexOf(s)
    val finals: HashSet[Int] = new HashSet[Int]
    for(s <- finalStatesSet) finals += statesSeq.indexOf(s)
    val empty = new HashSet[Int]

    val labelsArray = Array.ofDim[HashSet[Int]](statesSeq.size,
                                                transitionsSeq.size)
    val epsilonsArray = Array.ofDim[HashSet[Int]](statesSeq.size)
    for(si:Int <- 0 until statesSeq.length) {
      val s:S = statesSeq(si)
      for(ti:Int <- 0 until transitionsSeq.length) {
        val t:T = transitionsSeq(ti)
        labelsArray(si)(ti) =
          transitionsMap.get(s) match {
            case Some(curry) =>
              curry.get(t).fold(empty)(_.map(statesSeq.indexOf(_)))
            case None => empty
          }
        }

      epsilonsArray(si) =
        epsilons.get(s).fold(empty)(_.map(statesSeq.indexOf(_)))
    }

    // println(epsilonsArray)
    assembleNDFA(
      statesSeq, initials.toSet, finals.toSet, transitionsSeq, labelsArray,
      epsilonsArray)
  }

  protected def assembleNDFA(statesSeq: IndexedSeq[S], initials:Set[Int],
                             finals:Set[Int], transitionsSeq: IndexedSeq[T],
                             labelsArray:Array[Array[HashSet[Int]]],
                             epsilonsArray:Array[HashSet[Int]]):ThisNDFA
}
