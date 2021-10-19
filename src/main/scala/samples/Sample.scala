// Copyright (C) 2021 John Maraist
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.samples
import org.maraist.planrec.rules.HTNLib

trait Sample {
  type Term
  type Head
  type Subst
  def name: String
  def library: HTNLib[Term, Head, Subst]
  def desc: String
  def sequences: Seq[Seq[Term]]
  def essay: String = ""
}

object Sample {
  private val samplesBank =
    scala.collection.mutable.ArrayBuffer.empty[Sample]

  def samples: Iterable[Sample] = samplesBank.toArray

  def apply[T, H, S](
    nam: String,
    lib: HTNLib[T, H, S],
    dsc: String,
    seq: Seq[Seq[T]],
    txt: String = ""
  ): Sample = {
    val result = new Sample() {
      type Term = T
      type Head = H
      type Subst = S
      override val name: String = nam
      override val library: HTNLib[T, H, S] = lib
      override val desc: String = dsc
      override val sequences: Seq[Seq[T]] = seq
      override val essay: String = txt
    }
    samplesBank += result
    result
  }
}
