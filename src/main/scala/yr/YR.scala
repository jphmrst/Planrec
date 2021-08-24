// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.yr
import org.maraist.planrec.
  {Recognizer, PreparedPlanLibrary, RecognitionSession, Explanation}
import org.maraist.planrec.terms.TermImpl
import org.maraist.planrec.rules.HTNLib
import org.maraist.planrec.rules.HTN.HTNrule

class TablesLib[T, H, S](using TermImpl[T, H, S])
extends PreparedPlanLibrary[
  HTNrule, HTNLib, T, H, YRSession, YRExpl, S
] {
  def lib: HTNLib[T, H, S] = ???
  def newSession: YRSession[T] = ???
}

class YRSession[T] extends RecognitionSession[T, YRExpl] {
  def explanation: YRExpl[T] = ???
  override def addOne(obs: T): this.type = ???
}

class YRExpl[T] extends Explanation[T] {
}

class YRErr[T, H] extends RuntimeException {
}

object YR
    extends Recognizer[HTNrule, HTNLib, TablesLib, YRSession, YRExpl, YRErr] {
  override def validLibrary[T, H, S](lib: HTNLib[T, H, S])
    (using impl: TermImpl[T, H, S]):
      List[YRErr[T, H]] = ???
  override def prepareLibrary[T, H, S](lib: HTNLib[T, H, S])
    (using impl: TermImpl[T, H, S]):
      TablesLib[T, H, S] = ???
}

