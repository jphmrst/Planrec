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
import org.maraist.util.FilesCleaner
import org.maraist.latex.{LaTeXdoc,Sampler}
import org.maraist.planrec.rules.HTNLib
import org.maraist.planrec.terms.Term.TermImpl
import org.maraist.planrec.yr.table.{HandleFinder,Table}

trait Sample {
  type Term
  type Head
  type Subst
  def name: String
  def library: HTNLib[Term, Head, Subst]
  def desc: String
  def sequences: Seq[Seq[Term]]
  def essay: String = ""
  def termImpl: TermImpl[Term, Head, Subst]
  def nfaWidth: String = "6in"
  def dfaWidth: String = "7in"
}

object Sample extends Sampler {
  private val samplesBank =
    scala.collection.mutable.ArrayBuffer.empty[Sample]

  def samples: Iterable[Sample] = samplesBank.toArray

  def apply[T, H, S](
    nam: String,
    lib: HTNLib[T, H, S],
    dsc: String,
    seq: Seq[Seq[T]],
    txt: String = ""
  )(using ti: TermImpl[T, H, S]): Sample = {
    val result = new Sample() {
      type Term = T
      type Head = H
      type Subst = S
      override val name: String = nam
      override val library: HTNLib[T, H, S] = lib
      override val desc: String = dsc
      override val sequences: Seq[Seq[T]] = seq
      override val essay: String = txt
      override val termImpl: TermImpl[Term, Head, Subst] = ti
    }
    samplesBank += result
    result
  }

  def addSamples(guide: LaTeXdoc): FilesCleaner = {
    val cleaner = newCleaner()
    for (sample <- samples) { addSample(guide, sample, cleaner) }
    cleaner
  }

  def addSample(guide: LaTeXdoc, sample: Sample, cleaner: FilesCleaner) = {
    type T = sample.Term
    type H = sample.Head
    type S = sample.Subst
    val library: HTNLib[T, H, S] = sample.library
    val tag = sample.name

    guide ++= "\\section{"
    if (!sample.desc.equals("")) guide ++= s"${sample.desc} --- "
    guide ++= s"${sample.name}}\n"
    if (!sample.essay.equals("")) guide ++=/ sample.essay
    guide ++= "\\begin{center}\n"
    library.toLaTeX(guide)
    guide ++= "\\end{center}\n"
    // TODO

    given TermImpl[T, H, S] = sample.termImpl
    val table = Table(library)
    guide ++= "\\subsection{YR}\n"
    guide ++= "\\subsection*{NFA}\n"
    val nfa = HandleFinder.libToNFA(library)
    graphable(guide, cleaner, nfa, tag+"NFA", tag+" NFA", sample.nfaWidth)
    guide ++= "\\subsection*{DFA}\n"
    graphable(
      guide, cleaner, table.dfa, tag+"DFA", tag+" DFA", sample.dfaWidth
    )
  }

  @main def writeSamples: Unit = {
    HTNs.load
    val guide = new LaTeXdoc("samples")
    guide.addPackage("geometry", "margin=1in")
    guide.addPackage("times")
    guide.addPackage("graphicx")
    // guide.addPackage("multicol")
    guide.open()
    // guide ++= "\\begin{multicols}{2}"
    val cleanup = addSamples(guide)
    // guide ++= "\\end{multicols}"
    guide.close()
    cleanup.clean
  }
}

