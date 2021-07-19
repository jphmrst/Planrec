// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.terms
import scala.annotation.targetName

/** Implementation of a type as terms which can be used in plan rules.
  *  @tparam T The type of term being declared.
  *  @tparam H The type of term head associated with these terms.
  *  @tparam S The type of substitution associated with unification of
  *  these terms.
  */
trait TermImpl[T,H,S] {

  /** If possible, return the unification of the given terms.  For some
    * types T, may cause muatations to the argument terms as unbound
    * structure is grounded.
    */
  def unifyTerms(t1: T, t2: T): Option[T]

  /** If possible, return a substitution which will unify the given
    * terms.  Implementations should not mutate either argument.
    */
  def getUnifier(t1: T, t2: T): Option[S]

  /** Return the head component of the given term.
    */
  def head(t: T): H

  /** Apply the substitution to the term, leaving the original term
    * unchanged.
    */
  def substitute(t: T, s: S): T

  /** Returns true if the two arguments are unifiable.  By default,
    * checks whether [[#getUnifier]] returns `Some` or `None`.
    * Implementations should not mutate either argument.
    */
  def unifiable(t1: T, t2: T): Boolean = getUnifier(t1, t2) match {
    case Some(_) => true
    case None => false
  }
}

/** Extension methods on the underlying term, all implemented with the
  * methods on this trait.
  */
extension [T, H, S](t1: T)(using impl: TermImpl[T, H, S]) {

  /** Check whether this term can be unified with the argument.
    * Implementations should not mutate either this term or the
    * argument.
    */
  @targetName("unifiableWith")
  def >?< (t2: T): Boolean = impl.unifiable(t1, t2)

  /** If possible, return a unifier for this term and the arugment.
    * Implementations should not mutate either this term or the
    * argument.
    */
  @targetName("unifier")
  def >+< (t2: T): Option[S] = impl.getUnifier(t1, t2)

  /** Unify this term with the arguments term.  There may be side
    * effects to this term, to the argument, or to both.
    */
  @targetName("unifiesTo")
  def >><< (t2: T): Option[T] = impl.unifyTerms(t1, t2)

  /** Return the head component of this term. */
  def termHead: H = impl.head(t1)

  /** Apply the given substitution to this term, and return a new term.
    * This term should not be mutated.
    */
  def subst(s: S): T = impl.substitute(t1, s)
}

/** Bundle of common term implementations, givens and extension
  * methods from the [[org.maraist.planrec.terms]] top level and
  * classes.
  */
object Term {
  export org.maraist.planrec.terms.TermImpl
  export org.maraist.planrec.terms.{>?<, >+<, >><<, termHead, subst}
  export org.maraist.planrec.terms.String.StringAsTerm
  export org.maraist.planrec.terms.Symbol.SymbolAsTerm
}
