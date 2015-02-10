package scrappy

import language.experimental.macros

import reflect.macros.Context
import reflect.api.Universe
import annotation.StaticAnnotation

case class DataConstructor[U <: Universe](name: U#TermName, fields: Option[List[(U#TermName, U#Tree)]])
case class TypeParam(p: String)
case class DataDescriptor[U <: Universe](name: U#TypeName, params: List[TypeParam], constructors: List[DataConstructor[U]])

trait DerivingTrait {
  def derive(u: Universe)(freshType: String => u.TypeName, freshTerm: String => u.TermName, descriptor: DataDescriptor[u.type]): u.Tree
}
trait Deriving0[T[_]] extends DerivingTrait
trait Deriving1[T[_[_]]] extends DerivingTrait

trait Show[A] {
  def show(a: A): String
}

object Show {
  implicit val derivingShow: Deriving0[Show] = new Deriving0[Show] {
    def derive(u: Universe)(freshType: String => u.TypeName, freshTerm: String => u.TermName, descriptor: DataDescriptor[u.type]): u.Tree = {
      import u._
      val cases = descriptor.constructors.map {
        case DataConstructor(name, fields) =>
          CaseDef(Ident(name), EmptyTree, Literal(Constant(name.decoded)))
      }
      val tpe = descriptor.name
      val name = freshTerm("derivedShow")
      val a = freshTerm("a")
      val matchCases = Match(Ident(a), cases)
      q"implicit val $name: Show[$tpe] = new Show[$tpe] { def show($a: $tpe) = $matchCases }"
    }
  }
}

trait Functor[F[_]] {
  def fmap[A, B](a: F[A])(f: A => B): F[B]
}

object Functor {
  implicit val derivedFunctor: Deriving1[Functor] = new Deriving1[Functor] {
    def derive(u: Universe)(freshType: String => u.TypeName, freshTerm: String => u.TermName, descriptor: DataDescriptor[u.type]): u.Tree = {
      import u._
      val A = freshType("A")
      val B = freshType("B")
      val f = freshTerm("f")
      val cases = descriptor.constructors.map {
        case DataConstructor(name, None) =>
          CaseDef(Ident(name), EmptyTree, Ident(name))
        case DataConstructor(name, Some(fields)) =>
          val leftParams = fields.map {
            case (name, _) => Bind(name, Ident(termNames.WILDCARD))
          }
          val rightParams = fields.map {
            //case (name, Ident(t)) => Apply(Ident(f), Ident(name) :: Nil)
            case (name, Ident(t)) => Ident(name)
          }
          CaseDef(Apply(Ident(name), leftParams), EmptyTree, Apply(Ident(name), rightParams))
      }
      val tpe = descriptor.name
      val name = freshTerm("derivedFunctor")
      val a = freshTerm("a")
      val matchCases = Match(Ident(a), cases)
      val x = q"""
        implicit val $name: Functor[$tpe] = new Functor[$tpe] {
          def fmap[$A, $B]($a: $tpe[$A])($f: $A => $B): $tpe[$B] = $matchCases
        }
      """
      println(x)
      x
    }
  }
}

class deriving extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro deriving.impl
}

object deriving {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val inputs = annottees.map(_.tree).toList

    val q"new deriving($a)" = c.prefix.tree

    def getDeriving(s: String) = appliedType(
      c.mirror.staticClass(s).toType,
      c.mirror.staticClass(a.toString).toType :: Nil
    )

    val tpe0 = getDeriving("scrappy.Deriving0")
    val tpe1 = getDeriving("scrappy.Deriving1")
    val tree = c.inferImplicitValue(tpe0).orElse(c.inferImplicitValue(tpe1))
    val derivingTrait = c.eval[DerivingTrait](c.Expr(c.resetLocalAttrs(tree.duplicate)))

    val companion = inputs.find {
      case ModuleDef(_) => true
      case _ => false
    }

    val constructors = companion.toList.flatMap {
      case ModuleDef(_, name, Template(_, _, body)) =>
        body.flatMap {
          case ClassDef(mods, constructorName, _, Template(_, _, body)) if mods.hasFlag(Flag.CASE) =>
            val accessors = body.filter {
              case ValDef(mods, _, _, _) if mods.hasFlag(Flag.CASEACCESSOR) =>
                true
              case _ =>
                false
            }
            val fields = accessors.flatMap {
              case ValDef(_, name, tpe, _) =>
                (name, tpe) :: Nil
              case _ =>
                Nil
            }
            DataConstructor[c.universe.type](constructorName.toTermName, Some(fields)) :: Nil
          case ModuleDef(mods, constructorName, _) if mods.hasFlag(Flag.CASE) =>
            DataConstructor[c.universe.type](constructorName, None) :: Nil
          case _ =>
            Nil
        }
      case _ =>
        Nil
    }

    val inputClass = inputs.find {
      case ClassDef(_) => true
      case _ => false
    }

    val typeName = inputClass.fold(c.abort(c.enclosingPosition, "wat")) {
      case ClassDef(_, name, _, _) => name
    }

    val instance = derivingTrait.derive(c.universe)(
      c.freshName(_),
      c.freshName(_),
      DataDescriptor(typeName, List.empty, constructors)
    )
    val newCompanion = companion.map {
      case ModuleDef(a, b, Template(c, d, body)) =>
        ModuleDef(a, b, Template(c, d, instance :: body))
      case a => a
    }

    val expandees = inputs.map {
      case m@ModuleDef(_) =>
        newCompanion.getOrElse(m)
      case a =>
        a
    }

    c.Expr(Block(expandees, Literal(Constant(()))))
  }
}
