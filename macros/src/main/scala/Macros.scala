package play.boy.macros

import language.experimental.macros
import scala.reflect.macros.Context
import scala.reflect.ClassTag
import play.api.libs.json._

import play.boy.dao._

object Macros {
  def handleIndex: ((String, Any) => JsValue) = macro handleIndexImpl

  def handleIndexImpl(c: Context): c.Expr[(String, Any) => JsValue] = {
    import c.universe._
    import c.universe.Flag._

    val cases = c.mirror.staticPackage("models").typeSignature.members
      .filter(_.isModule)
      .filter(_.typeSignature.baseClasses.contains(typeOf[DAO[_, _]].typeSymbol))
      .map(moduleSymbol => {
        val baseType = moduleSymbol.typeSignature.baseType(typeOf[DAO[_, _]].typeSymbol)
        val TypeRef(_, _, List(modelType, tableType)) = baseType

        val modelSymbol = modelType.typeSymbol.companionSymbol
        val writesSymbol = modelSymbol.typeSignature.members.find(_.typeSignature <:< typeOf[OFormat[_]]).get
        val jsonPkgSymbol = Select(Select(Select(Ident(newTermName("play")), newTermName("api")), newTermName("libs")), newTermName("json"))
        val jsonSymbol = Select(jsonPkgSymbol, newTermName("Json"))
        val arrayWrites = Select(Select(jsonPkgSymbol, newTermName("Writes")), newTermName("arrayWrites"))

        CaseDef(Literal(Constant(modelSymbol.name.decoded.toLowerCase)), EmptyTree,
          Apply(
            Apply(
              Select(jsonSymbol, newTermName("toJson")),
              List(
                TypeApply(
                  Select(Ident(newTermName("y")), newTermName("asInstanceOf")),
                  List(AppliedTypeTree(Ident(newTypeName("Array")), List(TypeTree(modelType))))
                )
              )
            ),
            List(
              Apply(
                TypeApply(arrayWrites, List(TypeTree(modelType))),
                List(
                  Select(Ident(newTermName("Predef")), newTermName("implicitly")),
                  Select(Ident(modelSymbol), newTermName(writesSymbol.name.toString.trim))
                )
              )
            )
          )
        )
      })

    val stringSymbol = Select(Select(Ident(newTermName("scala")), newTermName("Predef")), newTermName("String"))

    val funcExpr = Function(
      List(
        ValDef(Modifiers(PARAM), newTermName("x"), TypeTree(), EmptyTree),
        ValDef(Modifiers(PARAM), newTermName("y"), TypeTree(), EmptyTree)
      ),
      Match(
        Ident(newTermName("x")),
        cases.toList
      )
    )

    println(c.Expr[(String, Any) => JsValue](funcExpr))
    c.Expr[(String, Any) => JsValue](funcExpr)
  }

  def handleGet: ((String, Any) => JsValue) = macro handleGetImpl

  def handleGetImpl(c: Context): c.Expr[(String, Any) => JsValue] = {
    import c.universe._
    import c.universe.Flag._

    val cases = c.mirror.staticPackage("models").typeSignature.members
      .filter(_.isModule)
      .filter(_.typeSignature.baseClasses.contains(typeOf[DAO[_, _]].typeSymbol))
      .map(moduleSymbol => {
        val baseType = moduleSymbol.typeSignature.baseType(typeOf[DAO[_, _]].typeSymbol)
        val TypeRef(_, _, List(modelType, tableType)) = baseType

        val modelSymbol = modelType.typeSymbol.companionSymbol
        val writesSymbol = modelSymbol.typeSignature.members.find(_.typeSignature <:< typeOf[OFormat[_]]).get
        val jsonPkgSymbol = Select(Select(Select(Ident(newTermName("play")), newTermName("api")), newTermName("libs")), newTermName("json"))
        val jsonSymbol = Select(jsonPkgSymbol, newTermName("Json"))

        CaseDef(Literal(Constant(modelSymbol.name.decoded.toLowerCase)), EmptyTree,
          Apply(
            Apply(
              Select(jsonSymbol, newTermName("toJson")),
              List(
                TypeApply(
                  Select(Ident(newTermName("y")), newTermName("asInstanceOf")),
                  List(TypeTree(modelType))
                )
              )
            ),
            List(
              Select(
                Ident(modelSymbol),
                newTermName(writesSymbol.name.toString.trim)
              )
            )
          )
        )
      })

    val stringSymbol = Select(Select(Ident(newTermName("scala")), newTermName("Predef")), newTermName("String"))

    val funcExpr = Function(
      List(
        ValDef(Modifiers(PARAM), newTermName("x"), TypeTree(), EmptyTree),
        ValDef(Modifiers(PARAM), newTermName("y"), TypeTree(), EmptyTree)
      ),
      Match(
        Ident(newTermName("x")),
        cases.toList
      )
    )

    println(c.Expr[(String, Any) => JsValue](funcExpr))
    c.Expr[(String, Any) => JsValue](funcExpr)
  }
}
