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
import scala.collection.mutable.HashSet
import org.maraist.util.IndexSetsTracker
import org.maraist.graphviz.NodeLabeling

/**
 * Concrete implementation of an {@link org.maraist.fa.NDFA NDFA} based on
 * arrays.
 *
 * NDFAs of this type are translated to
 * {@link org.maraist.fa.ArrayDFA ArrayDFAs}.
 *
 *  @tparam S The type of all states of the automaton
 *  @tparam T The type of labels on transitions of the automaton
 */
class ArrayNDFA[S,T](
  stateSeq: IndexedSeq[S],
  initialStateSet: Set[Int],
  finalStateSet: Set[Int],
  transitionsSeq: IndexedSeq[T],
  labels: Array[? <: Array[? <: Set[Int]]],
  epsilons: Array[? <: Set[Int]]
) extends AbstractArrayNDFA[S,T,ArrayDFA[Set[S],T]](stateSeq, initialStateSet,
                                         finalStateSet, transitionsSeq,
                                         labels, epsilons) {
  // for(i <- 0 until epsilons.length)  println("*** ["+i+"] "+epsilons(i))
  def dump():Unit = {
    println("stateSeq: " + stateSeq)
    println("transitionsSeq: " + transitionsSeq)
    println("initialStateSet: " + initialStateSet)
    println("finalStateSet: " + finalStateSet)
    println("Raw labels array: [")
    for(i <- 0 until labels.length) {
      val row = labels(i)
      var sep = "  ["
      for(j <- 0 until row.length) {
        print(sep + row(j))
        sep = ", "
      }
      println("], ")
    }
    println("]")
    println("Transitions by method: [")
    for(from <- stateSeq) {
      var sep = "  [ "
      for(trans <- transitionsSeq) {
        print(sep + transitions(from, trans))
        sep = ", "
      }
      println(" ], ")
    }
    println("]")
  }

  /** {@inheritdoc} */
  protected def assembleDFA(dfaStates:IndexedSeq[Set[S]],
                            initialStateIdx:Int,
                            dfaFinals:Set[Int],
                            transitionsSeq:IndexedSeq[T],
                            dfaTransitions:Array[Array[Int]],
                            tracker:IndexSetsTracker,
                            appearsIn:Array[Set[Int]]): ArrayDFA[Set[S],T] = {
    //println(" ** assembling DFA: initial " + initialStateIdx + ", final states " + dfaFinals)
    val result:ArrayDFA[Set[S],T] =
      new ArrayDFA[Set[S],T](dfaStates, initialStateIdx, dfaFinals,
                             transitionsSeq, dfaTransitions)
    result.nodeLabeling = NodeLabeling.labelingSetOf(nodeLabeling)
    result.transitionLabeling = transitionLabeling
    result.graphvizOptions = graphvizOptions
    result
  }
}
