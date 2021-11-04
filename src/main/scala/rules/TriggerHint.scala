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

/** Provides a hint to the
  * [[org.maraist.planrec.yr.Item#apply Item.apply]] method.
  *
  * There may be ambiguity as to how a trigger should be applied to
  * the goals and actions of a rule.  For example it may be that a
  * term can unify with more than one element of the subgoals of an
  * [[org.maraist.planrec.rules.All All]] rule.  In these cases it is
  * possible to pass a hint.
  *
  * Note that the companion object declares
  * [[org.maraist.planrec.rules.FirstTriggerMatch FirstTriggerMatch]]
  * as a default hint.
  */
trait TriggerHint
/** Hint that the [[org.maraist.planrec.yr.Item#apply Item.apply]]
  * method should take the first match it finds.
  */
case class FirstTriggerMatch() extends TriggerHint
/** Hint that the [[org.maraist.planrec.yr.Item#apply Item.apply]]
  * method should use the match corresponding to a particular index in
  * the rule/item.
  */
case class TriggerMatchIndex(i: Int) extends TriggerHint
/** Empty hint.
  */
case class NoHint() extends TriggerHint

/** This main purpose of the companion object is to hold the default
  * [[org.maraist.planrec.rules.TriggerHint TriggerHint]].
  */
object TriggerHint {
  /** The default [[org.maraist.planrec.rules.TriggerHint TriggerHint]]
    * is to use the
    * [[org.maraist.planrec.rules.FirstTriggerMatch FirstTriggerMatch]].
    */
  given TriggerHint = FirstTriggerMatch()
}
