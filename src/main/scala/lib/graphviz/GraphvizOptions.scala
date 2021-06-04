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

class GraphvizOptions(var sourceFile:String = "graph.dot",
                      var outputFile:String = "graph.pdf",
                      var format:String = "pdf",
                      var executable:String = "dot",
                      var keepDOT:Boolean = false,
                      var fontSize:Int = GraphvizOptions.defaultFontSize,
                      var margin:Double = GraphvizOptions.defaultMargin,
                      var nodeShape:String = "circle",
                      var finalNodeShape:String = "doublecircle") {
  def this(opts:GraphvizOptions) = {
    this(opts.sourceFile, opts.outputFile, opts.format, opts.executable,
         opts.keepDOT, opts.fontSize, opts.margin, opts.nodeShape,
         opts.finalNodeShape)
  }
}
object GraphvizOptions {
  given GraphvizOptions = new GraphvizOptions()
  val defaultFontSize:Int = 12
  val defaultMargin:Double = 0.5
}
