
package org.maraist.latex

/** Objects which are convertable to LaTeX via a
 * {@link org.maraist.latex.LaTeXdoc LaTeXdoc} instance.
 */
trait LaTeXRenderable {
  /** Write this object to the given
   * {@link org.maraist.latex.LaTeXdoc LaTeXdoc}.
   */
  def toLaTeX(doc:LaTeXdoc): Unit
}
