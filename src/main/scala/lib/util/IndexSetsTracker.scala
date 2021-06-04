// Copyright (C) 2017 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.util

import scala.collection.mutable.ListBuffer

/**
 * Index a set of index subranges
 */
class IndexSetsTracker(private val count:Int,
                       private val setsBuf:ListBuffer[Set[Int]]) {
  private var root: TrackTree = Absent()
  override def toString(): String = root.toString()
  /** Return the index of the given set in the registry, adding
    * it if necessary.  Every member of `elements` must be in the
    * range [0,`count`), where `count` is the initialized number
    * of indices.
    */
  def apply(idx: Int): Set[Int] = setsBuf(idx)
  def size: Int = setsBuf.size
  def getIndex(elements:Set[Int]): Int = {  // scalastyle:ignore
    var thisIdx = 0
    var toFile = elements.toSeq.sorted
    root match {
      case Absent() => {
        root = newTree(thisIdx,elements,toFile)
        setsBuf.length-1
      }
      case _ => {
        var node: TrackTree = root
        while (thisIdx<count) {
          node match {
            case decision:TrackItem => {
              if (!toFile.isEmpty && toFile.head == thisIdx) {
                toFile = toFile.tail
                if (decision.hasPresent()) {
                  node = decision.present
                } else {
                  decision.present =
                    newTree(1 + thisIdx, elements, toFile)
                  return setsBuf.length-1
                }
              } else {
                if (decision.hasAbsent()) {
                  node = decision.absent
                } else {
                  decision.absent =
                    newTree(1 + thisIdx, elements, toFile)
                  return setsBuf.length-1
                }
              }
              thisIdx += 1
            }
          }
        }
        node match {
          case Present(p) => p
          case _ => throw new Exception("Should not be")
        }
      }
    }
  }

  private def newTree(thisIdx:Int, set:Set[Int],
                      elements:Seq[Int]):TrackTree = {
    if (thisIdx == count) {
      val slot:Int = setsBuf.length
      setsBuf += set
      Present(slot)
    } else if (!elements.isEmpty && thisIdx == elements.head) {
      TrackItem(
        thisIdx, newTree(thisIdx + 1,set,elements.tail),
        Absent())
    } else {
      TrackItem(
        thisIdx, Absent(),
        newTree(thisIdx + 1,set,elements))
    }
  }
}
