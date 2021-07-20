// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.yappr
import scala.collection.mutable.{HashSet,Queue}
import org.maraist.planrec.
  {Recognizer, PreparedPlanLibrary, RecognitionSession, Explanation}
import org.maraist.planrec.rules.HTNLib
import org.maraist.planrec.rules.HTN.HTNrule
import org.maraist.planrec.terms.TermImpl

class PFFGlib[T, H](override val lib: HTNLib[T, H])
  (using impl: TermImpl[T, H, ?])
extends PreparedPlanLibrary[
  HTNrule, HTNLib, T, H, YapprSession, YapprExpl
] {
  def newSession: YapprSession[T] = ???
}

class YapprSession[T] extends RecognitionSession[T, YapprExpl] {
  def explanation: YapprExpl[T] = ???
  override def addOne(obs: T): this.type = ???
}

class YapprExpl[T] extends Explanation[T] {
}

class YapprErr[T, H](s: String) extends RuntimeException(s) {
}

object YapprErrs {
  class MultiYapprErrs[T,H](val errs: Seq[YapprErr[T,H]])
      extends YapprErr[T,H]("Multiple exceptions")
  class UnguardedLibraryRecursion[T,H](val onTermHead: H)
      extends YapprErr[T,H]("Unguarded recursion in plan library.")
}

object Yappr extends Recognizer
  [HTNrule, HTNLib, PFFGlib, YapprSession, YapprExpl, YapprErr] {

  override def validLibrary[T, H](lib: HTNLib[T, H])
    (using impl: TermImpl[T, H, ?]): List[YapprErr[T, H]] = {
    import org.maraist.planrec.yappr.YapprErrs.UnguardedLibraryRecursion
    val checked = new HashSet[H]
    List.from(
      for (head <- lib.ruleGoalHeads) yield checkForUnguardedLoopOnRules(
        lib, Set(head), checked, lib.rules(head))
    ).flatten.map(new UnguardedLibraryRecursion[T,H](_))
  }

  private def checkForUnguardedLoopOnRules[T, H](
    lib: HTNLib[T, H],
    seen: Set[H],
    checked: HashSet[H],
    rules: Set[HTNrule[T, H]]
  )(using impl: TermImpl[T, H, ?]): List[H] = {
    val res = List.newBuilder[H]
    for(rule <- rules) {
      res ++= checkForUnguardedLoopOnGoals(
        lib, seen, checked, rule.unblockedSubgoals)
    }
    res.result()
  }

  private def checkForUnguardedLoopOnGoals[T, H](
    lib: HTNLib[T, H],
    seen: Set[H],
    checked: HashSet[H],
    goals: Set[T]
  )(using impl: TermImpl[T, H, ?]): List[H] = {
    val res = List.newBuilder[H]
    for(goal <- goals) {
      val h = impl.head(goal)
      if !checked.contains(h) then
        if seen.contains(h) then
          res += h
        else
          val these =
            checkForUnguardedLoopOnRules(lib, seen + h, checked, lib.rules(h))
          res ++= these
        checked += h
    }
    res.result()
  }

  override def prepareLibrary[T,H](lib: HTNLib[T, H])
    (using impl: TermImpl[T, H, ?]):
      PFFGlib[T, H] = ???
}

