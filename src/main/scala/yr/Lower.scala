// Copyright (C) 2021, 2022 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.yr
import scala.collection.mutable.WeakHashMap
import scala.collection.mutable.{HashSet, HashMap}

/**
  * Node labels in the lower graph.
  */
type Label[T, H, S] = Set[HState[T, H, S]] | Ind[T, H, S]

/**
  * Alias for the result of pushing an item onto a lower node stack
  * node, and then performing reduce steps on the new node.
  */
type LowerRecognizeResult[T, H, S] = (List[Lower[T, H, S]], List[T])

/**
  * Lower graph nodes.
  *
  * @param content [[Label][Node label]] of this node.
  * @param parent [[HashSet][Set]] of parents of this node.  This set
  * can grow over time, so the collection is mutable.
  */
class Lower[T, H, S](
  val content: Label[T, H, S],
  val parent: HashSet[Lower[T, H, S]]
) {

  def recognize(term: T): LowerRecognizeResult[T, H, S] = ???

  def push(item: Label[T, H, S], cache: LowerCache[T, H, S]): Lower[T, H, S] =
    cache.get(item) match {
      case Some(result) => {
        result.parent += this
        result
      }
      case None => {
        val result = new Lower(item, HashSet[Lower[T, H, S]](this))
        cache += ((item, result))
        result
      }
    }
}

/**
  * Cache of lower graph nodes across recognition of one observation.
  *
  */
type LowerCache[T, H, S] = HashMap[Label[T, H, S], Lower[T, H, S]]

/**
  * The lower graph itself can be characterized by its graph tops.
  * However this class will probably be unused and dropped; we
  * reference the tops from the upper graph, not from this sort of
  * aggregate structure.
  *
  * @param tops
  */
class LowerGraph[T, H, S](
  val tops: WeakHashMap[Lower[T, H, S], Unit] =
    new WeakHashMap[Lower[T, H, S], Unit]()
)
