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
import TermImplementation.*

/** Common trait of all HTN rules. */
trait RuleForm[T <: Term[T]]

/** An "and-rule", indicating that a goal can be fulfilled by meeting
  * all of the given subgoals.
  */
case class All[T <: Term[T]](
  val goal: T, val subgoals: IndexedSeq[T], val order: Array[(Int,Int)]
)(
  using termImpl: TermImplementation[T]
) extends RuleForm[T] {
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
  */
case class One[T <: Term[T]](
  val goal: T, val subgoals: Iterable[T]
) extends RuleForm[T]

/** A "terminal rule," indicating that a goal corresponds to a simple
  * action.
  */
case class Act[T <: Term[T]](val goal: T, val action: T)
    extends RuleForm[T]

object HTN {
  type HTNrule[T <: Term[T]] = All[T] | One[T] | Act[T]
}
