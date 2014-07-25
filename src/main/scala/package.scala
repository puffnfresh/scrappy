package scrappy

import language.experimental.macros

import reflect.macros.Context
import annotation.StaticAnnotation

class doo extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro doo.impl
}

object doo {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val inputs = annottees.map(_.tree).toList

    object dooTransformer extends Transformer {
      def transformLine(tree: Tree, accum: Tree): Tree =
        tree match {
          case q"$left <-- $right" =>
            left match {
              case Ident(t@TermName(_)) =>
                val valDef = ValDef(Modifiers(Flag.PARAM), t, TypeTree(), EmptyTree)
                val nested = transform(right)
                q"$nested.flatMap($valDef => $accum)"
              case _ =>
                val nested = transform(right)
                q"$nested.flatMap { case $left => $accum }"
            }
          case _ =>
            q"$tree.flatMap { case () => $accum }"
        }

      override def transform(tree: Tree): Tree =
        tree match {
          case q"doo($body)" =>
            body match {
              case Block((stats, last)) =>
                stats.foldRight(last) { (tree, accum) =>
                  tree match {
                    case Function(ValDef(mods, _, _, _) :: Nil, body) if mods.hasFlag(Flag.SYNTHETIC) =>
                      transformLine(body, accum)
                    case _ =>
                      transformLine(tree, accum)
                  }
                }
              case _ => c.abort(annottees.head.tree.pos, "doo must take a monadic computation")
            }
          case _ =>
            super.transform(tree)
        }
    }

    val expandees = inputs.map(t => dooTransformer.transform(t))

    c.Expr(Block(expandees, Literal(Constant(()))))
  }
}
