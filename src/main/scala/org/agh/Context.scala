package org.agh

case class Context(n: Int, c: Int = 0) {
  val end: Boolean = n == c

  def ++ = Context(n, c + 1)

  def -- = Context(n, c - 1)
}
