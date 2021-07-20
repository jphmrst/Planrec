// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.yappr
import org.maraist.planrec.
  {Recognizer, PreparedPlanLibrary, RecognitionSession, Explanation}
import org.maraist.planrec.rules.HTNLib
import org.maraist.planrec.rules.HTN.HTNrule
import java.util.Queue

class PFFGlib[T, H](override val lib: HTNLib[T, H])
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
      extends YapprErr("Multiple exceptions")
  class UnguardedLibraryRecursion[T,H](val onTerm: T)
      extends YapprErr("Unguarded recursion inplan library.")
}

object Yappr extends Recognizer
  [HTNrule, HTNLib, PFFGlib, YapprSession, YapprExpl, YapprErr] {
  override def validLibrary[T, H](lib: HTNLib[T, H]): List[YapprErr[T, H]] = {
    val visited: Set[T] = Set()
    for (top <- ???) {
      val queue: Queue[T] = new Queue
      queue += ???
      ???
    }
  }
  override def prepareLibrary[T,H](lib: HTNLib[T, H]): PFFGlib[T, H] = ???
}

