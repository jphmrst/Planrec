// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec
import scala.collection.mutable.Builder
import org.maraist.planrec.rules.RuleForm
import org.maraist.planrec.terms.TermImpl

/** Trait marking plan libraries collecting rules.
  * @tparam R Type constructor for the rules in this library
  * @tparam T The type of term being declared.
  * @tparam H The type of term head associated with these terms.
  */
trait PlanLibrary[R[X,Y] <: RuleForm[X,Y], T, H] {
  /** The rules contained in this plan library. */
  def rules: Set[R[T, H]]

  /** Retrieve the rules associated with a particular goal head. */
  def rules(h: H): Set[R[T, H]]

  /** Set of all rule goal terms. */
  def ruleGoals: Set[T] = {
    val res = Set.newBuilder[T]
    for (r <- rules) { res += r.goal }
    res.result()
  }

  /** Set of all rule goal heads. */
  def ruleGoalHeads(using impl: TermImpl[T, H, ?]): Set[H] = {
    val res = Set.newBuilder[H]
    for (g <- ruleGoals) { res += impl.head(g) }
    res.result()
  }

  /** The top-level goals which may be taken as an intention of an
    * actor.
    */
  def top: Seq[H]

  /** Probabilities of the respective top-level goals.  The `top` and
    * `probs` collections must have the same size.
    */
  def probs: Seq[Double]

  // Verify that the sequences of top-level goals and top-level goal
  // probabilities are the same
  if (top.size != probs.size) then throw new IllegalArgumentException(
    "Collections top and probs must be the same size")

  /** Set of head components of terms in this library. */
  def heads: Set[H]
}

/** Trait marking the precompiled/otherwise prepared version of a plan
  * library for a particular plan recognition algorithm.
  *
  * @tparam R Type constructor for the rules in this library
  * @tparam T The type of term being declared.
  * @tparam H The type of term head associated with these terms.
  * @tparam RS Evolving state of recognition of a single sequence of
  * observations.
  * @tparam EX Type constructor over terms for explanations for
  * observations.
  */
trait PreparedPlanLibrary[
  R[M, N] <: RuleForm[M,N],
  L[Y, Z] <: PlanLibrary[R, Y, Z],
  T, H,
  RS[Y] <: RecognitionSession[Y, EX],
  EX[X] <: Explanation[X]
](using impl: TermImpl[T, H, ?]) {

  /** Underlying plan library. */
  def lib: L[T, H]

  /** Create a new stateful object for assembling explanations from a
    * sequence of observations.
    */
  def newSession: RS[T]
}

/** Trait marking the stateful object accumulating observations to be
  * explained.
  *
  * @tparam T The type of term being declared.
  * @tparam EX Type constructor over terms for explanations for
  * observations.
  */
trait RecognitionSession[T, EX[X] <: Explanation[X]]
    extends Builder[T,EX[T]] {
  /** By default we do not expect the `clear` method to be supported. */
  override def clear(): Unit =
    throw new UnsupportedOperationException("clear() not supported")

  /** Synonym for the `explanation` method. */
  override def result(): EX[T] = explanation

  /** Returns an [[Explanation explanation]] for the observations to
    * this point.
    */
  def explanation: EX[T]
}

/** Trait marking explanations constructed by a plan recognizer.
  *
  * @tparam T The type of term being declared.
  */
trait Explanation[T] {
}

/** Trait bringing together the elements of a plan recognition
  * algorithm.
  *
  * @tparam PL Class of plan library accepted by this recognizer.
  * @tparam PPL Prepared library ready for a sequence of observations.
  * @tparam RS Evolving state of recognition of a single sequence of
  * observations.
  * @tparam EX Type constructor over terms for explanations for
  * observations.
  * @tparam ERR Representation of algorithm-specific error messages.
  */
trait Recognizer[
  R[T,H] <: RuleForm[T, H],
  PL[T,H] <: PlanLibrary[R, T, H],
  PPL[T,H] <: PreparedPlanLibrary[R, PL, T, H, RS, EX],
  RS[T] <: RecognitionSession[T, EX],
  EX[T] <: Explanation[T],
  ERR[T,H] <: RuntimeException]{

  /** Diagnose whether a plan library is valid for this algorithm.
    * @return An empty list when the library *is* valid for this
    * algorithm.
    */
  def validLibrary[T, H](lib: PL[T, H])(using impl: TermImpl[T, H, ?]):
      List[ERR[T, H]]

  /** Precompile a plan library to collect any additional artifacts
    * required for this recognition algorithm.
    */
  def prepareLibrary[T,H](lib: PL[T, H])(using impl: TermImpl[T, H, ?]):
      PPL[T, H]

  /** Attach the precompilation method to the plan library type.
    */
  extension [T, H](library: PL[T, H])(using impl: TermImpl[T, H, ?]) {
    def prepare: PPL[T, H] = prepareLibrary(library)
  }
}
