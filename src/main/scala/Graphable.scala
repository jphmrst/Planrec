// Copyright (C) 2017 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.graphviz
import scala.language.postfixOps
import scala.sys.process.*  // scalastyle:ignore
import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter

/** Trait of objects which can be rendered with Graphviz.
 *
 * The toDOT method is the only abstract method of the class.
 * Implementations should return lines of text to be included within
 * a digraph (or other Graphviz block).
 *
 * The graphviz methods actually invoke a Graphviz executable according
 * to (possibly implicit) options to render an object.  These methods
 * both have defaults provided in this trait.
 */
trait Graphable[S,T] {

  var graphvizOptions:GraphvizOptions = summon[GraphvizOptions]

  /** Use Graphviz to render this object (in the default format) to the
   *  given file.
   */
  def graphviz(fileRoot:String):Unit = {
    val options = graphvizOptions
    options.sourceFile = fileRoot + ".dot"
    options.outputFile = fileRoot + ".pdf"
    graphviz(options)
  }

  /** Use Graphviz to render this object as specified. */
  def graphviz(options:GraphvizOptions):Unit = {
    val file = new File(options.sourceFile)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write("digraph finite_state_machine {\n")
    bw.write("\tmargin=\"")
    bw.write(options.margin.toString())
    bw.write("\"; fontsize=\"")
    bw.write(options.fontSize.toString())
    bw.write("\"; rankdir=LR; \tsize=\"8,5\"\n")
    bw.write("}\n")
    bw.close()

    val cmd = Seq(options.executable,
                  "-T" + options.format,
                  "-o" + options.outputFile,
                  options.sourceFile)
    cmd !

    if (!options.keepDOT) file.delete()
  }
}

/** Trait of factories which render objects with Graphviz.
 *
 * The toDOT method is the only abstract method of the class.
 * Implementations should return lines of text to be included within
 * a digraph (or other Graphviz block).
 *
 * The graphviz methods actually invoke a Graphviz executable according
 * to (possibly implicit) options to render an object.  These methods
 * both have defaults provided in this trait.
 */
trait Grapher[X,S,T] {
  /**
   *  Return the inner lines of a digraph block (or other Graphviz style)
   *  to render an object.
   */
  def toDOT(x:X):String

  var graphvizOptions:GraphvizOptions = summon[GraphvizOptions]

  /** Use Graphviz to render this object (in the default format) to the
   *  given file.
   */
  def graphviz(fileRoot:String, x:X):Unit = {
    val options = graphvizOptions
    options.sourceFile = fileRoot + ".dot"
    options.outputFile = fileRoot + ".pdf"
    graphviz(options, x)
  }

  /** Use Graphviz to render this object as specified.
   */
  def graphviz(options:GraphvizOptions, x:X):Unit = {
    val file = new File(options.sourceFile)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write("digraph finite_state_machine {\n")
    bw.write("\tmargin=\"")
    bw.write(options.margin.toString())
    bw.write("\"; fontsize=\"")
    bw.write(options.fontSize.toString())
    bw.write("\"; rankdir=LR; \tsize=\"8,5\"\n")
    bw.write("}\n")
    bw.close()

    val cmd = Seq(options.executable,
                  "-T" + options.format,
                  "-o" + options.outputFile,
                  options.sourceFile)
    cmd !

    if (!options.keepDOT) file.delete()
  }
}
