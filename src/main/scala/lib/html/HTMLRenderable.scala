// Copyright (C) 2017, 2018 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.html

/** Objects which can be displayed as HTML */
trait HTMLRenderable {
  /** Extract the HTML representation of this object. */
  def toHTML:String
}

/** Renderer for objects as HTML */
trait HTMLRenderer[A] {
  /** Extract the HTML representation of the given object. */
  def toHTML(a:A):String
}

