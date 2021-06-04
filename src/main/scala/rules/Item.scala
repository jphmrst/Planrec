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
import TriggerHint.*

// =================================================================
// Representation of a position

/** This trait links one type of [[org.maraist.planrec.rules.Rule Rule]]
  * with the type of item positions within that rule, and the
  * implementation of item operations for the rule/position.  It may
  * be that not every rule form is used with an algorithm based on
  * items, so there is no mention of items in the rules themselves.
  *
  * @tparam T The type of terms used to describe goals, subgoals and
  * actions in these rules.
  * @tparam RuleType Two-argument type constructor for the plan rules
  * whose items are implemented here.  The arguments to this type
  * constructor must both implement [[Term]].
  * @tparam PosType Two-argument type constructor for the position
  * markers used in the items implemented here.  The arguments to this
  * type constructor must both implement [[Term]].
  */
trait PositionImpl[T <: Term[T], RuleType[X<:Term[X]], PosType[Y<:Term[Y]]] {
  def initPos(rule: RuleType[T]): PosType[T]

  def goalHints(rule: RuleType[T], pos: PosType[T]): Set[(T, TriggerHint)]
  def actionHints(rule: RuleType[T], pos: PosType[T]):
      Set[(T, TriggerHint)]
  def goalTriggers(rule: RuleType[T], pos: PosType[T]): Set[T] =
    for((g,h) <- goalHints(rule, pos)) yield g
  def actionTriggers(rule: RuleType[T], pos: PosType[T]): Set[T] =
    for((a,h) <- actionHints(rule, pos)) yield a
  /** FILL IN
    *
    * Note that Skolemizing, if needed, should be performed *before*
    * calling this method.
    */
  def afterTrigger(rule: RuleType[T], pos: PosType[T], trigger: T)
    (using hint: TriggerHint): Option[PosType[T]]
  def applications(rule: RuleType[T], pos: PosType[T], trigger: T):
      Seq[TriggerHint]
  def triggers(rule: RuleType[T], pos: PosType[T]): Set[T]
}

/** Position of an item in an [[org.maraist.planrec.rules.All All]]
  * rule.
  */
case class AllItem[T <: Term[T]](val ready: Set[Int])

/** Position of an item in a [[org.maraist.planrec.rules.One One]]
  * rule.
  */
case class OneItem[T <: Term[T]](val complete: Boolean)

/** Position of an item in an [[org.maraist.planrec.rules.Act Act]]
  * rule.
  */
case class ActItem[T <: Term[T]](val complete: Boolean)

/** An item is the combination of a rule and a particular state of
  * progress through that rule.  Instances are instantiated with the
  * [[org.maraist.planrec.rules.Item#Item Item]] apply method in the
  * companion object.
  * @tparam T Type of terms for goals and actions in rules and items
  */
sealed abstract class Item[T <: Term[T]] {
  /** The type constructor of the rule in this item. */
  type RuleType[_]
  /** The type constructor of the position marker in this item. */
  type PosType[_]
  /** The rule in this item. */
  def rule: RuleType[T]
  /** The position marker in this item. */
  def pos: PosType[T]

  /** The triggers corresponding to subgoals for this item.
    * @param impl Item implementation.
    */
  def goalHints(using impl: PositionImpl[T,RuleType,PosType]):
      Set[(T, TriggerHint)] =
    impl.goalHints(rule, pos)
  /** The triggers corresponding to actions for this item.
    * @param impl Item implementation.
    */
  def actionHints(using impl: PositionImpl[T,RuleType,PosType]): Set[(T, TriggerHint)] =
    impl.actionHints(rule, pos)
  /** All triggers for this item.
    * @param impl Item implementation.
    */
  def triggers(using impl: PositionImpl[T,RuleType,PosType]): Set[T] =
    impl.triggers(rule, pos)
  /** Result of advancing this item for the given trigger.
    *
    * Note that Skolemizing, if needed, should be performed *before*
    * calling this method.
    *
    * @param trigger The action or subgoal used to advance this item.
    * @param impl Item implementation.
    */
  def apply(trigger: T)(
    using impl: PositionImpl[T,RuleType,PosType], hint: TriggerHint):
      Option[Item[T]] =
    impl.afterTrigger(rule, pos, trigger).map(Item(rule, _))

  /** Hints which might apply to this trigger, if it is ambiguous within
    * the possible subgoals/actions.
    * @param trigger The action or subgoal under question.
    * @param impl Item implementation.
    */
  def applications(trigger: T)(
    using impl: PositionImpl[T,RuleType,PosType]): Seq[TriggerHint] =
    impl.applications(rule, pos, trigger)
}

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
    (using PositionImpl[T, R, P]): Item[T] = new Item[T] {
    type RuleType[X] = R[X]
    type PosType[Y] = P[Y]
    val rule: RuleType[T] = r
    val pos: PosType[T] = p
  }

  /** [[PositionImpl]] instance for [[All]] rules and [[AllItem]]
    * positions.
    */
  given all[T <: Term[T]]: PositionImpl[T, All, AllItem] with {
    override def initPos(rule: All[T]): AllItem[T] = {
      val getDelayed = Set.newBuilder[Int]
      for((_,after) <- rule.order) {
        getDelayed += after
      }
//
      val delayed = getDelayed.result()
      val getReady = Set.newBuilder[Int]
      for (i <- 0 until rule.subgoals.length) {
        if (!delayed.contains(i)) {
          getReady += i
        }
      }
//
      AllItem[T](getReady.result())
    }
//
    override def goalHints(rule: All[T], pos: AllItem[T]): Set[(T, TriggerHint)] = {
      val res = Set.newBuilder[(T, TriggerHint)]
      for(i <- pos.ready) res += ((rule.subgoals(i), TriggerMatchIndex(i)))
      res.result()
    }
    override def actionHints(rule: All[T], pos: AllItem[T]): Set[(T, TriggerHint)] =
      Set.empty[(T, TriggerHint)]
//
    override def afterTrigger(rule: All[T], pos: AllItem[T], trigger: T)(
      using hint: TriggerHint
    ): Option[AllItem[T]] = {
      import scala.util.control.NonLocalReturns.*
      val idx: Option[Int] = hint match {
        case TriggerMatchIndex(i) => Some(i)
        case _ => returning {
          for(i <- 0 until rule.subgoals.length) {
            if (rule.subgoals(i) >?< trigger) {
              throwReturn[Option[Int]](Some(i))
            }
          }
          None
        }
      }
//
      idx.flatMap(rule.subgoals(_) >><< trigger)
        .map((term) => {
          val i = idx.get
          val sb = Set.newBuilder[Int]
          for(j <- pos.ready; if i != j) { sb += j }
          for((before, after) <- rule.order; if before == i) { sb += after }
          AllItem(sb.result())
        })
    }
//
    override def applications(
      rule: All[T], pos: AllItem[T], trigger: T): Seq[TriggerHint] = {
      val builder = Seq.newBuilder[TriggerHint]
      for
        i <- 0 until rule.subgoals.length if rule.subgoals(i) >?< trigger
      do
        builder += TriggerMatchIndex(i)
      builder.result()
    }
//
    override def triggers(rule: All[T], pos: AllItem[T]): Set[T] =
      Set.from(goalTriggers(rule, pos))
  }
//
  /** [[PositionImpl]] instance for [[One]] rules and [[OneItem]]
    * positions.
    */
  given one[T <: Term[T]]: PositionImpl[T, One, OneItem] with {
    override def initPos(rule: One[T]): OneItem[T] = OneItem[T](false)
//
    override def goalHints(rule: One[T], pos: OneItem[T]): Set[(T, TriggerHint)] =
      ???
    override def actionHints(rule: One[T], pos: OneItem[T]): Set[(T, TriggerHint)] =
      Set.empty[(T, TriggerHint)]
    override def afterTrigger(
      rule: One[T], pos: OneItem[T], trigger: T
    )(using hint: TriggerHint): Option[OneItem[T]] =
      ???
    override def applications(
      rule: One[T], pos: OneItem[T], trigger: T): Seq[TriggerHint] =
      ???
    override def triggers(rule: One[T], pos: OneItem[T]): Set[T] =
      Set.from(goalTriggers(rule, pos))
  }
//
  /** [[PositionImpl]] instance for [[Act]] rules and [[ActItem]]
    * positions.
    * @tparam T This `given` will apply for any `Term` subtype.
    */
  given act[T <: Term[T]]: PositionImpl[T, Act, ActItem] with {
    override def initPos(rule: Act[T]): ActItem[T] = ActItem[T](false)
//
    override def goalHints(rule: Act[T], pos: ActItem[T]): Set[(T, TriggerHint)] =
      Set.empty[(T, TriggerHint)]
    override def actionHints(rule: Act[T], pos: ActItem[T]): Set[(T, TriggerHint)] =
      ???
    override def afterTrigger(
      rule: Act[T], pos: ActItem[T], trigger: T
    )(using hint: TriggerHint): Option[ActItem[T]] =
      ???
    override def applications(
      rule: Act[T], pos: ActItem[T], trigger: T): Seq[TriggerHint] =
      ???
    override def triggers(rule: Act[T], pos: ActItem[T]): Set[T] =
      Set.from(actionTriggers(rule, pos))
  }
}
