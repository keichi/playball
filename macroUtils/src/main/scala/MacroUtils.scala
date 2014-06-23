package play.boy.macros

import language.experimental.macros
import scala.reflect.macros.Context

object MacroUtils {
  def copy[A](obj: A, args: (String, Any)*): A = macro copyImpl[A]
  def copyImpl[A: c.WeakTypeTag](c: Context)(obj: c.Expr[A], args: c.Expr[(String, Any)]*): c.Expr[A] = {
    import c.universe._
    import c.universe.Flag._

    val kvs = args.map(_.tree match {
      case Apply(
        TypeApply(
          Select(
            Apply(
              TypeApply(Select(_, n1: TermName), _),
              List(Literal(Constant(key: String)))
            ),
            n2: TermName
          ),
          _
        ),
        List(value: Tree)
      ) if n1.decoded == "ArrowAssoc" && n2.decoded == "->" => Some(key, value)
      case _ => None
    }).flatten(Option.option2Iterable).toMap

    val ctor = weakTypeOf[A].member(nme.CONSTRUCTOR).asMethod

    val params = ctor.paramss.head.zipWithIndex.map({ case(field, idx) =>
      kvs.get(field.name.decoded).getOrElse({
        Select(obj.tree, newTermName("copy$default$" + (idx + 1).toString))
      })
    })

    c.Expr(Apply(Select(obj.tree, newTermName("copy")), params))
  }
}
