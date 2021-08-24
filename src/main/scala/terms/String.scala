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
import org.maraist.latex.{LaTeXRenderer, LaTeXdoc}

object String {
  given StringAsTerm: TermImpl[String, String, Unit] with
    override def unifiable(t1: String, t2: String): Boolean = t1.equals(t2)
    def unifyTerms(t1: String, t2: String): Option[String] =
      if unifiable(t1, t2) then Some(t1) else None
    def head(t: String): String = t
    def getUnifier(t1: String, t2: String): Option[Unit] =
      if unifiable(t1, t2) then Some(()) else None
    def substitute(t: String, s: Unit): String = t

  given RenderStringAsTerm: LaTeXRenderer[String] with
    override def toLaTeX(doc: LaTeXdoc, s: String) = { doc ++= s }
}
