// Copyright (C) 2020 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.outlines
import java.io.PrintWriter

class OutlineItem[E](val heading: E, val items: Seq[OutlineItem[E]],
                     val summary: Option[E] = None)
 {

  def formatAsItem(
    dest: PrintWriter, lead: String, prefix: String = ""
  ): Unit = {
  }

  def fullFormatAsItem(dest:PrintWriter, lead:String, prefix:String): Unit = {
    dest.println(lead + prefix + heading)
    val sublead = "  " + lead
    for(item <- items) {
      item.formatAsItem(dest, sublead)
    }
  }
}

object OutlineItem {
  given basicToItem[E]: Conversion[E, OutlineItem[E]] =
    new OutlineItem(_, Seq())
  given givenSeqToItem[E]: Conversion[Seq[E], OutlineItem[E]] with
    def apply(s: Seq[E]): OutlineItem[E] = {
      new OutlineItem[E](s.head, s.tail.map(basicToItem(_)))
    }
}
