import language.experimental.macros

import reflect.macros.Context

package object scrappy {
  def doo(code: _): _ = macro dooImpl

  def dooImpl(c: Context)(code: c.Tree): c.Tree = {
    import c.universe._

    code match {
      case Block((stats, last)) =>
        stats.foldRight(last) { (tree, accum) =>
          tree match {
            case q"$left <-- $right" =>
              left match {
                case Ident(t@TermName(_)) =>
                  q"$right.flatMap($t => $accum)"
                case _ =>
                  q"$right.flatMap { case $left => $accum }"
              }
            case _ => tree
          }
        }

      case _ => c.abort(code.pos, "doo must take a monadic computation")
    }
  }
}
