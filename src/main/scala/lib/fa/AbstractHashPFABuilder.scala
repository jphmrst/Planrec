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

/** Implementation of [[org.maraist.fa.PFABuilder PFABuilder]] using
 *  [[scala.collection.mutable.HashSet `HashSet`s]] and
 *  [[scala.collection.mutable.HashMap `HashMap`s]].
 * @param initialState The initial state of the automaton.  This builder must
 * be instantiated with one state, initially taken to be the initial state.
 * @tparam S The type of all states of the automaton
 * @tparam T The type of labels on (non-epsilon) transitions of the automaton
 */
abstract class AbstractHashPFABuilder[S,T]
extends StateHashBuilderTrait[S,T] with PFABuilder[S,T] {
  private val transitionsMap = new HashMap[S,HashMap[T,HashMap[S,Double]]]
  private val eTransitionsMap = new HashMap[S,HashMap[S,Double]]
  private val initialProb = new HashMap[S,Double]
  private val finalProb = new HashMap[S,Double]

  def initialStateProb(s:S): Double = initialProb.get(s) match {
    case Some(p) => p
    case None => 0.0
  }

  def finalStateProb(s:S): Double = finalProb.get(s) match {
    case Some(p) => p
    case None => 0.0
  }

  def addInitialState(s:S, prob:Double):Unit = {
    addState(s)
    initialProb(s) = prob
  }

  def removeInitialState(s:S):Unit = { initialProb.remove(s) }

  def addFinalState(s:S, prob:Double):Unit = {
    addState(s)
    finalProb(s) = prob
  }

  def removeFinalState(s:S):Unit = { finalProb.remove(s) }

  private[fa] def deleteInitialState(s:S):Unit = { initialProb.remove(s) }
  private[fa] def deleteFinalState(s:S):Unit = { finalProb.remove(s) }
  private[fa] def deleteTransitionsFrom(s:S) = {
    transitionsMap -= s
    for(lmap:HashMap[T,HashMap[S,Double]] <- transitionsMap.valuesIterator)
      for(v <- lmap.keysIterator)
        if (lmap(v).contains(s)) {
          lmap(v) -= s
          if (lmap.isEmpty) { lmap -= v }
        }
  }

  def addTransition(s1:S, t:T, s2:S, prob:Double):Unit = {
    allStates += s1
    allStates += s2
    if (!transitionsMap.contains(s1)) {
      transitionsMap += (s1 -> new HashMap[T,HashMap[S,Double]])
    }

    val sub1:HashMap[T,HashMap[S,Double]] = transitionsMap(s1)
    if (!sub1.contains(t)) {
      sub1 += (t -> new HashMap[S,Double])
    }

    val sub2:HashMap[S,Double] = sub1(t)
    sub2 += (s2 -> prob)
  }
  def removeTransition(s1:S, t:T, s2:S):Unit =
    if (transitionsMap.contains(s1))
      transitionsMap(s1) -= t

  def transition(s0:S, t:T, s1:S):Double = {
    if (transitionsMap.contains(s0)) {
      val sub:HashMap[T,HashMap[S,Double]] = transitionsMap(s0)
      if (sub.contains(t)) {
        val sub2 = sub(t)
        if (sub2.contains(s1)) sub2(s1) else 0.0
      } else 0.0
    } else 0.0
  }

  def eTransition(s0:S, s1:S): Double = {
    eTransitionsMap.get(s0) match {
      case Some(submap) => {
        submap.get(s1) match {
          case Some(p) => p
          case None => 0.0
        }
      }
      case None => 0.0
    }
  }

  def addETransition(s1:S, s2:S, prob:Double): Unit = {
    eTransitionsMap.get(s1) match {
      case Some(submap) => { submap(s2) = prob }
      case None => {
        val submap = new HashMap[S,Double]
        submap(s2) = prob
        eTransitionsMap(s1) = submap
      }
    }
  }

  def removeETransition(s1:S, s2:S): Unit = {
    eTransitionsMap.get(s1) match {
      case Some(submap) => { submap.remove(s2) }
      case None => { }
    }
  }

  def transition(s0:S, t:T):Map[S,Double] = {
    val result = new HashMap[S,Double]()
    if (transitionsMap.contains(s0)) {
      val sub:HashMap[T,HashMap[S,Double]] = transitionsMap(s0)
      if (sub.contains(t)) {
        for ((s1,prob) <- sub(t)) {
          if (prob>0.0)
            result(s1) = prob
        }
      }
    }
    result.toMap
  }

  def labels: Set[T] = {
    val result = new HashSet[T]()
    for(map <- transitionsMap.valuesIterator)
      for(t <- map.keysIterator)
        result += t
    result.toSet
  }

  import scala.util.control.NonLocalReturns.*
  override def accepts(ts:Seq[T]): Double = returning {
    var current = List.from(for(s<-initialProb.keys) yield (initialProb(s),s))
    for(t <- ts) {
      val oldCurrent = current
      current = List[(Double,S)]()
      for ((p0,s0) <- oldCurrent) {
        for ((s1,p1) <- transition(s0,t)) {
          if (p1>0.0) {
            current = (p0*p1,s1) :: current
          }
        }
      }
      if (current.size == 0) throwReturn(0.0)
    }

    var result:Double = 0.0
    for ((p,s) <- current) { result += p }
    result
  }

  def normalize:Unit = { // scalastyle:ignore cyclomatic.complexity
    val totalOut = new HashMap[S,Double]
    var totalInit:Double = 0.0
    for(s1 <- allStates) {
      totalInit += initialStateProb(s1)
      totalOut(s1) = finalStateProb(s1)
      for(s2 <- allStates) {
        totalOut(s1) += eTransition(s1,s2)
        for(t <- labels)
          totalOut(s1) += transition(s1,t,s2)
      }
    }
    for(s1 <- allStates) {
      if (totalInit>0.0) initialProb.get(s1) match {
        case Some(p) => { initialProb(s1) = p/totalInit }
        case None => { }
      }
      if (totalOut(s1) > 0.0) {
        finalProb.get(s1) match {
          case Some(p) => { finalProb(s1) = p/totalOut(s1) }
          case None => { }
        }
        transitionsMap.get(s1) match {
          case Some(s1submap) => {
            for((t,tSubmap) <- s1submap)
              for((s2,p) <- tSubmap)
                transitionsMap(s1)(t)(s2) = p / totalOut(s1)
          }
          case None => { }
        }
        eTransitionsMap.get(s1) match {
          case Some(s1submap) => {
            for((s2,p) <- s1submap)
              s1submap(s2) = p / totalOut(s1)
          }
          case None => { }
        }
      }
    }
  }

  /** @deprecated */
  def toPFA: ThisPFA = result()

  def result(): ThisPFA = { // scalastyle:ignore cyclomatic.complexity method.length
    val statesSeq: IndexedSeq[S] = IndexedSeq.from(allStates)
    val transitionsSeq: IndexedSeq[T] = IndexedSeq.from(labels)
    val initialProbs = Array.ofDim[Double](statesSeq.length)
    val finalProbs = Array.ofDim[Double](statesSeq.length)
    val totalOut = Array.ofDim[Double](statesSeq.length)
    val idxLabels:Array[Array[Array[Double]]] =
      Array.ofDim[Double](statesSeq.length, transitionsSeq.length,
                          statesSeq.length)
    val eLabels:Array[Array[Double]] = Array.ofDim[Double](statesSeq.length,
                                                           statesSeq.length)
    var totalInit = 0.0
    for(si0 <- 0 until statesSeq.length) {
      val s0:S = statesSeq(si0)

      val thisInit  = initialStateProb(s0)
      val thisFinal = finalStateProb(s0)
      initialProbs(si0) = thisInit
      totalInit += thisInit
      finalProbs(si0) = thisFinal
      totalOut(si0) = thisFinal

      if (transitionsMap.contains(s0)) {
        val s0map = transitionsMap(s0)

        for(ti <- 0 until transitionsSeq.length) {
          val t:T = transitionsSeq(ti)
          if (s0map.contains(t)) {
            val tMap = s0map(t)

            for(si1 <- 0 until statesSeq.length) {
              val s1:S = statesSeq(si1)
              if (tMap.contains(s1)) {
                idxLabels(si0)(ti)(si1) = tMap(s1)
                totalOut(si0) += tMap(s1)
              } else {
                idxLabels(si0)(ti)(si1) = 0.0
              }
            }
          } else {
            for(si1 <- 0 until statesSeq.length)
              idxLabels(si0)(ti)(si1) = 0.0
          }
        }
      } else {
        for(ti <- 0 until transitionsSeq.length)
          for(si1 <- 0 until statesSeq.length)
            idxLabels(si0)(ti)(si1) = 0.0
      }
    }

    // Now normalize the weights for the final array
    for(si0 <- 0 until statesSeq.length) {
      if (totalInit>0.0) { initialProbs(si0) = initialProbs(si0)/totalInit }
      val out = totalOut(si0)
      if (out > 0.0) {
        finalProbs(si0) = finalProbs(si0) / out
        for(ti <- 0 until transitionsSeq.length)
          for(si1 <- 0 until statesSeq.length)
            idxLabels(si0)(ti)(si1) = idxLabels(si0)(ti)(si1) / out
      }
    }

    assemblePFA(statesSeq, initialProbs, finalProbs, transitionsSeq,
                idxLabels, eLabels)
  }

  protected def assemblePFA(statesSeq: IndexedSeq[S],
                            initialProbs: Array[Double],
                            finalProbs: Array[Double],
                            transitionsSeq: IndexedSeq[T],
                            idxLabels: Array[Array[Array[Double]]],
                            eTransLabels: Array[Array[Double]]): ThisPFA
}
