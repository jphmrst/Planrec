
package org.maraist.planrec.rules

trait PositionImpl[T, RuleType[X], PosType[Y]] {}

sealed abstract class Item[T] { }

object Item {
  /** Public pseudoconstructor for creating [[Item]] instances.
    * @param r The rule contained in this item.
    * @param p The position marker contained in this item.
    * @tparam T Type of terms used to name goals/subgoals and actions.
    * @tparam R Constructor for the type of plan rule in this item.
    * @tparam P Constructor for the type of position marker in this
    * item.
    */
  def apply[T, R[_], P[_]](r: R[T], p: P[T])
    (using PositionImpl[T, R, P]): Item[T] = new Item[T] { }
}
