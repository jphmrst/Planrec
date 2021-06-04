// Copyright (C) 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.util

/** Utilities for combining and simplifying Iterators and Iterables. */
object Iterators {

  /**
    * Transform an Iterator returning Option-wrapped values into an
    * Iterator of only the values under Some.
    */
  def withoutOption[A](iter: Iterator[Option[A]]): Iterator[A] =
    new WithoutOption[A](iter)

  /**
    * Transform an Iterator returning Option-wrapped values into an
    * Iterator of only the values under Some.
    */
  def withoutOption[A](iter: Iterable[Option[A]]): Iterable[A] =
    new Iterable[A] {
      override def iterator: Iterator[A] = withoutOption(iter.iterator)
    }

  /**
    * Transform an Option-wrapped Iterator into a possibly-empty
    * Iterator.
    */
  def flattenOptionIterator[A](iter: Option[Iterator[A]]): Iterator[A] =
    iter.fold(Iterator.empty[A])(x => x)

  /**
    * Transform an Option-wrapped Iterable into a possibly-empty
    * Iterable.
    */
  def flattenOptionIterable[A](iter: Option[Iterable[A]]): Iterable[A] =
    iter.fold(Iterable.empty[A])(x => x)

  /**
    * Transform several Iterators into a single Iterator.
    */
  def sequenceIterators[A](iters: Iterator[Iterator[A]]): Iterator[A] =
    new MultiIterator[A](iters)

  /**
    * Transform several Iterators into a single Iterator.
    */
  def sequenceIterators[A](iters: Iterable[Iterator[A]]): Iterator[A] =
    new MultiIterator[A](iters.iterator)

  /**
    * Transform an Iterator of Iterables into a single Iterator.
    */
  def iterateIterables[A](iters: Iterator[Iterable[A]]): Iterator[A] =
    new MultiIterator(iters.map(_.iterator))

  /**
    * Transform several Iterables into a single Iterable.
    */
  def sequenceIterables[A](iters: Iterable[Iterable[A]]): Iterable[A] =
    new Iterable[A] {
      override def iterator: Iterator[A] =
        new MultiIterator[A]((for (i <- iters) yield i.iterator).iterator)
    }

  // -----------------------------------------------------------------
  // Private classes implementing the above methods.

  private class WithoutOption[A](private val iter: Iterator[Option[A]])
  extends Iterator[A] {

    private var nextA: Option[A] = findActual()

    private def findActual(): Option[A] = {
      while (iter.hasNext) {
        iter.next() match {
          case opt@Some(_) => return opt
          case None => { }
        }
      }
      return None
    }

    override def hasNext: Boolean = !nextA.isEmpty

    override def next(): A = nextA match {
      case None => throw new NoSuchElementException
      case Some(a) => {
        nextA = findActual()
        a
      }
    }
  }

  private class MultiIterator[A](
    private val metaIterator: Iterator[Iterator[A]]
  )
  extends Iterator[A] {

    private var currentIterator = firstNonempty()
    private var nextA: Option[A] = updateNextA()

    private def firstNonempty(): Iterator[A] = {
      while (metaIterator.hasNext) {
        val cand = metaIterator.next()
        if (cand.hasNext) { return cand }
      }
      Iterator.empty[A]
    }
    private def updateNextA(): Option[A] = currentIterator.hasNext match {
      case true => Some(currentIterator.next())
      case false => None
    }

    /** {@inheritDocs} */
    override def hasNext: Boolean = !nextA.isEmpty

    /** {@inheritDocs} */
    override def next(): A = nextA match {
      case None => throw new NoSuchElementException
      case Some(a) => {
        if (!currentIterator.hasNext) {
          currentIterator = firstNonempty()
        }
        nextA = updateNextA()
        a
      }
    }
  }
}
