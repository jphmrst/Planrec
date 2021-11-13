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
import org.maraist.graphviz.GraphStyle
import org.maraist.util.FilesCleaner
import org.maraist.latex.{LaTeXdoc,Sampler}
import org.maraist.planrec.rules.HTNLib
import org.maraist.planrec.terms.Term.TermImpl
import org.maraist.planrec.yr.{HandleFinder, HandleNFA, HandleDFA, HState}

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
  def nfaWidth: String = "7.5in"
  def dfaWidth: String = "7.5in"

  def artifacts: (
    HandleFinder[this.Term, this.Head, this.Subst],
    HandleNFA[
      HState[this.Term, this.Head, this.Subst], this.Head
    ],
    HandleDFA[
      Set[HState[this.Term, this.Head, this.Subst]], this.Head
    ]
  ) = {
    given TermImpl[this.Term, this.Head, this.Subst] = this.termImpl
    val nfaBuilder = new HandleFinder[this.Term, this.Head, this.Subst]
    nfaBuilder.libToNFA(this.library)
    val nfa = nfaBuilder.result
    val dfa = nfa.toDFA
    (nfaBuilder, nfa, dfa)
  }
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
    txt: String = "",
    nw: String = "6in",
    dw: String = "7in"
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
      override val nfaWidth: String = nw
      override val dfaWidth: String = dw
    }
    samplesBank += result
    result
  }

  def addSamples(guide: LaTeXdoc): FilesCleaner = {
    val cleaner = newCleaner()
    for (sample <- samples) {
      println(s"*** ${sample.name}") ////////////////////////////////////////
      addSample(guide, sample, cleaner)
    }
    cleaner
  }

  def addSample(guide: LaTeXdoc, sample: Sample, cleaner: FilesCleaner) = {
    type T = sample.Term
    type H = sample.Head
    type S = sample.Subst
    val library: HTNLib[T, H, S] = sample.library
    val tag = sample.name

    guide ++= "\\clearpage\n"
    guide ++= s"\\section{${sample.name}"
    if (!sample.desc.equals("")) guide ++= s" --- ${sample.desc}"
    guide ++= "}\n"
    if (!sample.essay.equals("")) guide ++=/ sample.essay
    guide ++= "\\begin{center}\n"
    library.toLaTeX(guide)
    guide ++= "\\end{center}\n"
    // TODO

    given TermImpl[T, H, S] = sample.termImpl

    guide ++= "\\subsection{YR}\n"

    // guide ++= "\\subsection*{NFA builder}\n"
    import org.maraist.planrec.yr.yrNfaGraphStyle
    val nfaBuilder = new HandleFinder[T, H, S]
    nfaBuilder.libToNFA(library)
    // graphable(guide, cleaner, nfaBuilder, tag+"NFA builder", sample.nfaWidth)

    // guide ++= "\\begin{verbatim}\n"
    // val baos = new java.io.ByteArrayOutputStream
    // val utf8 =java.nio.charset.StandardCharsets.UTF_8.name()
    // val ps = new java.io.PrintStream(baos, true, utf8)
    // nfaBuilder.dump(ps)
    // ps.close
    // guide ++= baos.toString(utf8);
    // baos.close
    // guide ++= "\\end{verbatim}\n"

    guide ++= "\\subsection*{NFA}\n"
    val nfa = nfaBuilder.result
    // println("\nFrom Sample for NFA " + tag + ":")
    graphable(guide, cleaner, nfa, tag+"NFA", sample.nfaWidth)

    guide ++= "\\subsection*{DFA}\n"
    import org.maraist.planrec.yr.yrDfaGraphStyle
    val dfa = nfa.toDFA
    // println("\nFrom Sample for DFA " + tag + ":")
    graphable(guide, cleaner, dfa, tag+"DFA", sample.dfaWidth)

    // val table = Table(library)
  }

  trait Focus
  case class GuideDoc() extends Focus
  case class OnSample(sample: Sample) extends Focus
  // case class OnNFA[T, H, S](name: String, nfa: HandleNFA[T, H, S])
  //     extends Focus

  val focusSample: Focus = GuideDoc() // OnSample(HTNs.b11) //

  @main def printSamples: Unit = focusSample match {
    case OnSample(sample) => {
      type T = sample.Term
      type H = sample.Head
      type S = sample.Subst
      given TermImpl[T, H, S] = sample.termImpl
      println(sample.name)
      val nfaBuilder = new HandleFinder[T, H, S]
      nfaBuilder.libToNFA(sample.library)
      nfaBuilder.dump()
      val nfa = nfaBuilder.result
      nfa.dump()
      val dfa = nfa.toDFA
      dfa.dump()
    }

    // case OnNFA(name, nfa) => {
    //   nfa.dump()
    // }

    case GuideDoc() => {
      HTNs.load
      val guide = new LaTeXdoc("samples")
      guide.addPackage("geometry", "margin=1in")
      guide.addPackage("times")
      guide.addPackage("graphicx")
      // guide.addPackage("multicol")
      guide.open()

      val cleanup = focusSample match {
        // case OnNFA(name, nfa) => { FilesCleaner() }
        case GuideDoc() => addSamples(guide)
      }

      guide.close()
      cleanup.clean
    }
  }
}
