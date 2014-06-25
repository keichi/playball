package play.boy.macros

import language.experimental.macros
import scala.reflect.macros.Context
import scala.reflect.ClassTag
import play.api.libs.json._

import play.boy.dao._

object Macros {
  def handleIndex: ((String, Array[_]) => JsValue) = macro handleIndexImpl

  def handleIndexImpl(c: Context): c.Expr[(String, Array[_]) => JsValue] = {
    import c.universe._
    import c.universe.Flag._

    val cases = c.mirror.staticPackage("models").typeSignature.members
      .filter(_.isModule)
      .filter(_.typeSignature.baseClasses.contains(typeOf[DAO[_, _]].typeSymbol))
      .map(moduleSymbol => {
        val baseType = moduleSymbol.typeSignature.baseType(typeOf[DAO[_, _]].typeSymbol)
        val TypeRef(_, _, List(modelType, _)) = baseType

        val modelSymbol = modelType.typeSymbol.companionSymbol
        val caseName = modelSymbol.name.decoded.toLowerCase
        val writesSymbol = modelSymbol.typeSignature.members.find(_.typeSignature <:< typeOf[OFormat[_]]).get
        val writesName = newTermName(writesSymbol.name.toString.trim)

        cq"$caseName => play.api.libs.json.Json.toJson(y.map(_.asInstanceOf[$modelType]))(play.api.libs.json.Writes.arrayWrites[$modelType](implicitly, $modelSymbol.$writesName))"
      })


    c.Expr(q"(x: String, y: Array[_]) => x match { case ..$cases }")
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
        val TypeRef(_, _, List(modelType, _)) = baseType

        val modelSymbol = modelType.typeSymbol.companionSymbol
        val caseName = modelSymbol.name.decoded.toLowerCase
        val writesSymbol = modelSymbol.typeSignature.members.find(_.typeSignature <:< typeOf[OFormat[_]]).get
        val writesName = newTermName(writesSymbol.name.toString.trim)

        cq"$caseName => play.api.libs.json.Json.toJson(y.asInstanceOf[$modelType])($modelSymbol.$writesName)"
      })

    c.Expr(q"(x: String, y: Any) => x match { case ..$cases }")
  }

  def daoMap = macro daoMapImpl

  def daoMapImpl(c: Context): c.Expr[Map[String, DAO[_, _]]] = {
    import c.universe._
    import c.universe.Flag._

    val tuples = c.mirror.staticPackage("models").typeSignature.members
      .filter(_.isModule)
      .filter(_.typeSignature.baseClasses.contains(typeOf[DAO[_, _]].typeSymbol))
      .map(moduleSymbol => {
        val baseType = moduleSymbol.typeSignature.baseType(typeOf[DAO[_, _]].typeSymbol)
        val TypeRef(_, _, List(modelType, _)) = baseType

        val modelSymbol = modelType.typeSymbol.companionSymbol
        val caseName = modelSymbol.name.decoded.toLowerCase

        q"($caseName, $moduleSymbol)"
      }).toList

    c.Expr(q"scala.collection.immutable.Map(..$tuples)")
  }

  def handleCreate: ((String, JsValue) => Any) = macro handleCreateImpl

  def handleCreateImpl(c: Context): c.Expr[(String, JsValue) => Any] = {
    import c.universe._
    import c.universe.Flag._

    val cases = c.mirror.staticPackage("models").typeSignature.members
      .filter(_.isModule)
      .filter(_.typeSignature.baseClasses.contains(typeOf[DAO[_, _]].typeSymbol))
      .map(moduleSymbol => {
        val baseType = moduleSymbol.typeSignature.baseType(typeOf[DAO[_, _]].typeSymbol)
        val TypeRef(_, _, List(modelType, _)) = baseType

        val modelSymbol = modelType.typeSymbol.companionSymbol
        val caseName = modelSymbol.name.decoded.toLowerCase
        val writesSymbol = modelSymbol.typeSignature.members.find(_.typeSignature <:< typeOf[OFormat[_]]).get
        val writesName = newTermName(writesSymbol.name.toString.trim)

        cq"$caseName => play.api.libs.json.Json.fromJson(y)($modelSymbol.$writesName)"
      })

    c.Expr(q"(x: String, y: play.api.libs.json.JsValue) => x match { case ..$cases }")
  }}
