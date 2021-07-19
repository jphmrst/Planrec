// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.rules
import org.maraist.planrec.terms.Term.*
import org.maraist.util.Collections.normalizeSeq

/** Common trait of all rules. */
trait RuleForm[T, H](using TermImpl[T,H,?]) {
  def goal: T
}

/** An "and-rule", indicating that a goal can be fulfilled by meeting
  * all of the given subgoals.
  */
case class All[T, H](
  val goal: T, val subgoals: IndexedSeq[T], val order: Array[(Int,Int)])
  (using termImpl: TermImpl[T,H,?])
    extends RuleForm[T, H] {
  // Check for well-formedness of order constraints when creating a rule
  for ((before, after) <- order) {
    if before == after
    then throw IllegalArgumentException
               ("One index may not be ordered against itself")
    else if before > after
    then throw IllegalArgumentException
               ("Index order must respect subgoal array order")
    else if after >= subgoals.length || before < 0
    then throw IllegalArgumentException
               ("Ordered indices must refer to subgoal index")
  }
}

/** An "or-rule", indicating that a goal can be fulfilled by meeting
  * any one of the given subgoals.
  *
  * @tparam T Type of terms mentioned in this rule.
  * @param goal The goal term of this rule.
  * @param subgoals The subgoals required to satisfy the goal term.
  * @param rawProb The probabilities associated with the subterms.
  * This sequence must be of the same length as the sequence of
  * subgoals.
  */
case class One[T, H](val goal: T, val subgoals: Seq[T], subgoalProbs: Seq[Double])
  (using TermImpl[T,H,?])
    extends RuleForm[T,H] {

  if subgoals.length != subgoalProbs.length
  then throw new IllegalArgumentException(
    "Must have the same number of subgoals and subgoal probabilities")

  /**
    * Normalized probabilities of each subgoal.
    */
  val probs: Seq[Double] = normalizeSeq(subgoalProbs)
}

/** A "terminal rule," indicating that a goal corresponds to a simple
  * action.
  */
case class Act[T, H](val goal: T, val action: T)(using TermImpl[T, H, ?])
    extends RuleForm[T, H]

object HTN {
  export org.maraist.planrec.rules.All
  export org.maraist.planrec.rules.One
  export org.maraist.planrec.rules.Act
  type HTNrule[T,H] = All[T,H] | One[T,H] | Act[T,H]
}

// -----------------------------------------------------------------

import HTN.*
import org.maraist.planrec.PlanLibrary

class HTNLib[T, H](
  val rules: Set[HTNrule[T, H]], val top: Seq[H], topProbs: Seq[Double]
)(using TermImpl[T, H, ?]) extends PlanLibrary[HTNrule, T, H] {

  if top.length != topProbs.length
  then throw new IllegalArgumentException(
    "Must have the same number of top-level goals and top-level goal "
      + "probabilities")

  /**
    * Normalized probabilities of each subgoal.
    */
  val probs: Seq[Double] = normalizeSeq(topProbs)

  def heads: Set[H] = {
    val builder = Set.newBuilder[H]
    for (rule <- rules) do rule match {
      case All(goal, subgoals, _) => {
        builder += goal.termHead
        for (subgoal <- subgoals) do builder += subgoal.termHead
      }
      case One(goal, subgoals, _) => {
        for (subgoal <- subgoals) do builder += subgoal.termHead
      }
      case Act(goal, action) => {
        builder += goal.termHead
        builder += action.termHead
      }
    }
    builder.result()
  }
}