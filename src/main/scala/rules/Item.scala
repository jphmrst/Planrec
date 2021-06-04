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

trait PositionImpl[T <: Term[T], RuleType[X<:Term[X]], PosType[Y<:Term[Y]]] {}

/** An item is the combination of a rule and a particular state of
  * progress through that rule.  Instances are instantiated with the
  * [[org.maraist.planrec.rules.Item#Item Item]] apply method in the
  * companion object.
  * @tparam T Type of terms for goals and actions in rules and items
  */
sealed abstract class Item[T <: Term[T]] { }

/** This object contains the public pseudoconstructor for creating
  * [[Item]] instances, and the `given` implementations of
  * [[PositionImpl]] for various rules.
  */
object Item {
  /** Public pseudoconstructor for creating [[Item]] instances.
    * @param r The rule contained in this item.
    * @param p The position marker contained in this item.
    * @tparam T Type of terms used to name goals/subgoals and actions.
    * @tparam R Constructor for the type of plan rule in this item.
    * @tparam P Constructor for the type of position marker in this
    * item.
    */
  def apply[T <: Term[T], R[_], P[_]](r: R[T], p: P[T])
    (using PositionImpl[T, R, P]): Item[T] = new Item[T] { }
}
