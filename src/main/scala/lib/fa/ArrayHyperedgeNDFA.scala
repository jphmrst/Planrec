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
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import org.maraist.util.IndexSetsTracker
import org.maraist.graphviz.NodeLabeling

class ArrayHyperedgeNDFA[S,T](
  stateSeq: IndexedSeq[S],
  initialStateSet: Set[Int],
  finalStateSet: Set[Int],
  transitionsSeq: IndexedSeq[T],
  labels: Array[Array[HashSet[Int]]],
  epsilons: Array[? <: Set[Int]],
  val eHyperedgeMap: Map[Int,Set[Set[Int]]])
extends AbstractArrayNDFA[S,T,ArrayHyperedgeDFA[Set[S],T]](
  stateSeq, initialStateSet, finalStateSet,
  transitionsSeq, labels.map(_.map(_.toSet)), epsilons)
with HyperedgeNDFA[S,T,ArrayHyperedgeDFA[Set[S],T]] {

  def eHyperedgeTargets(s:S): Set[Set[S]] =
    (eHyperedgeMap.get(stateSeq.indexOf(s)) match {
      case Some(set) => set
      case None => Set.empty[Set[Int]]
    }).map(_.map(stateSeq(_)))
  def eHyperedgeTargetIndices(si:Int): Set[Set[Int]] =
    eHyperedgeMap.get(si) match {
      case Some(set) => set
      case None => Set.empty[Set[Int]]
    }

  override def seedAdditionalInitialStates(tracker:IndexSetsTracker):Unit = {
    for((fromNdaState,toNfaStateSets) <- eHyperedgeMap)
      for(toNfaStateSet <- toNfaStateSets)
        for(toNfaState <- toNfaStateSet) {
          epsilonCloseIndex(toNfaState) match {
            case (closedSet, isFinal) => tracker.getIndex(closedSet)
          }
        }
  }

  override protected def assembleDFA(
    dfaStates: IndexedSeq[Set[S]], // scalastyle:ignore method.length
    initialStateIdx: Int,
    dfaFinals: Set[Int],
    transitionsSeq: IndexedSeq[T],
    dfaTransitions: Array[Array[Int]],
    tracker: IndexSetsTracker,
    appearsIn: Array[Set[Int]]
  ):ArrayHyperedgeDFA[Set[S],T] = {
    // Convert the NDFA hyperedges to DFA hyperedges
    val dfaEHyperedges = new HashMap[Int,HashSet[Set[Int]]]

    // Look at each node of the NDFA, and the hyperedges anchored at
    // it.  Pull the set of DFA states in which this NDFA state
    // appears as an element.
    for((fromNdaState,toNfaStateSets) <- eHyperedgeMap) {
      // Use for debugging
      // println("*" + fromNdaState)
      val fromDfaStates = appearsIn(fromNdaState)

      // For each NDFA hyperedge (viz. the set of states to which this
      // one leads), we'll have one DFA hyperedge (and again, it's the
      // set of target states that we need)
      for(toNfaStateSet <- toNfaStateSets) {
        val toDfaStates:HashSet[Int] = new HashSet[Int]

        // For each targeted NDFA state, the DFA hyperedge will lead
        // to the DFA state formed from the epsilon-closure of the
        // targeted NDFA state
        for(toNfaState <- toNfaStateSet) {
          epsilonCloseIndex(toNfaState) match {
            case (closedNfaIndexSet, isFinal) => {
              val dfaState = tracker.getIndex(closedNfaIndexSet)
              toDfaStates += dfaState
            }
          }
        }

        // For each DFA state with which the origin NDFA state is
        // associated, add a hyperedge from that DFA state
        for(fromDfaState <- fromDfaStates) {
          dfaEHyperedges.get(fromDfaState) match {
            case Some(set) => {
              set += toDfaStates.toSet
            }
            case None => {
              val set = new HashSet[Set[Int]]
              set += toDfaStates.toSet
              dfaEHyperedges(fromDfaState) = set
            }
          }
        }
      }
    }

    // The dfaEHyperedges are what we need to create an
    // ArrayHyperedgeDFA vs. just an ArrayDFA.
    val result:ArrayHyperedgeDFA[Set[S],T] =
      new ArrayHyperedgeDFA[Set[S],T](
        dfaStates, initialStateIdx, dfaFinals.toSet, transitionsSeq,
        dfaTransitions,
        dfaEHyperedges.map({case (k, v) => (k, v.toSet)}).toMap)
    result.nodeLabeling =
      NodeLabeling.labelingSetOf("", "<br/>", "", nodeLabeling)
    result.transitionLabeling = transitionLabeling
    result.graphvizOptions = graphvizOptions
    result
  }
}
