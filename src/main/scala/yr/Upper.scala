// Copyright (C) 2021, 2022 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.planrec.yr
import scala.collection.mutable.HashMap

/**
  * Result of traversing the upper graph for recognizing one
  * observation.
  */
type UpperTraversalResult[T, H, S] = (Upper[T, H, S], List[H])

/**
  * Cache of upper graph nodes across recognition of one observation.
  *
  */
type UpperCache[T, H, S] = HashMap[Upper[T, H, S], Upper[T, H, S]]

/**
  * Combined cache upper and lower graph nodes across recognition of
  * one observation.
  *
  */
class Cache[T, H, S](
  val upper: UpperCache[T, H, S] = new UpperCache[T, H, S]) {
  val lower = new LowerCache[T, H, S]
}

/**
  * Common superclass of upper graph nodes.
  */
trait Upper[T, H, S] {
  def recognize(term: T, cache: Cache[T, H, S]): UpperTraversalResult[T, H, S]
}

/**
  * Upper graph node corresponding to the disjunction of several
  * possible parses.
  *
  * @param subs Subgraphs each representing a possible parse.
  */
class UpperAny[T, H, S](subs: List[UpperOne[T, H, S] | UpperLower[T, H, S]])
    extends Upper[T, H, S] {
  override def recognize(term: T, cache: Cache[T, H, S]):
      UpperTraversalResult[T, H, S] = ???
}

/**
  * Upper graph node corresponding to the synchronization point of
  * overlapping subparses.
  *
  * @param pars Upper graphs nodes corresponding to the partial parse
  * of each overlapping subgoal.
  * @param base Lower graph node of the overall parse state.
  */
class UpperOne[T, H, S](pars: List[Upper[T, H, S]], base: Lower[T, H, S])
    extends Upper[T, H, S] {
  override def recognize(term: T, cache: Cache[T, H, S]):
      UpperTraversalResult[T, H, S] = ???
}

/**
  * Upper graph node corresponding to one possible parse.
  *
  * @param stack Lower graph node corresponding at the top of the
  * parse graph.
  */
class UpperLower[T, H, S](stack: Lower[T, H, S]) extends Upper[T, H, S] {
  override def recognize(term: T, cache: Cache[T, H, S]):
      UpperTraversalResult[T, H, S] = ???
}
