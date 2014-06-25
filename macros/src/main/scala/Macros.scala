package play.boy.macros

import language.experimental.macros
import scala.reflect.macros.Context
import org.joda.time.DateTime
import play.api.libs.json._

import play.boy.dao._
import play.boy.types._

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

  def modelMetaMap = macro modelMetaMapImpl

  def modelMetaMapImpl(c: Context): c.Expr[Map[String, List[ColumnBase]]] = {
    import c.universe._
    import c.universe.Flag._

    val tuples = c.mirror.staticPackage("models").typeSignature.members
      .filter(_.isModule)
      .filter(_.typeSignature.baseClasses.contains(typeOf[DAO[_, _]].typeSymbol))
      .map(moduleSymbol => {
        val baseType = moduleSymbol.typeSignature.baseType(typeOf[DAO[_, _]].typeSymbol)
        val TypeRef(_, _, List(modelType, _)) = baseType
        val modelName = modelType.typeSymbol.name.decoded.toLowerCase

        val ctor = modelType.declarations
          .filter(_.isMethod)
          .map(_.asMethod)
          .find(_.isConstructor)
          .get

        val cols = ctor.asMethod.paramss.head
          .filter(_.isTerm)
          .map({ m => 
            val name = m.name.decoded
            val label = m.annotations
              .find(_.tpe =:= typeOf[play.boy.annotation.label]) match {
                case Some(x) => {
                  val tmp = x.scalaArgs.head
                  q"Some($tmp)"
                }
                case None => q"None"
              }
            
            val colType = m.typeSignature
            val preType = colType.asInstanceOf[TypeRefApi].pre

            if (colType <:< typeOf[String]) {
            val rows = m.annotations
              .find(_.tpe =:= typeOf[play.boy.annotation.text]) match {
                case Some(x) => {
                  val tmp = x.scalaArgs.head
                  q"Some($tmp)"
                }
                case None => q"None"
              }

              q"play.boy.types.StringColumn($name, $label, $rows)"
            } else if (colType =:= typeOf[Boolean]) {
              q"play.boy.types.BooleanColumn($name, $label)"
            } else if (colType =:= typeOf[DateTime]) {
              q"play.boy.types.DateTimeColumn($name, $label)"
            } else if (colType =:= typeOf[Short]) {
              q"play.boy.types.ShortColumn($name, $label)"
            } else if (colType =:= typeOf[Int]) {
              q"play.boy.types.IntColumn($name, $label)"
            } else if (colType =:= typeOf[Long]) {
              q"play.boy.types.LongColumn($name, $label)"
            } else if (colType =:= typeOf[Double]) {
              q"play.boy.types.DoubleColumn($name, $label)"
            } else if (colType =:= typeOf[Float]) {
              q"play.boy.types.FloatColumn($name, $label)"
            } else if (colType.asInstanceOf[TypeRefApi].pre <:< typeOf[Enum]) {
              val enumSymbol = preType.termSymbol.asModule
              
              val options = preType.declarations
                .filter(_.isTerm)
                .filter(_.asTerm.isVal)
                .map({ fieldSymbol =>
                  val itemSymbol = newTermName(fieldSymbol.name.decoded.trim)
                  q"($enumSymbol.$itemSymbol.toString, $enumSymbol.$itemSymbol.id)"
                })
                .toList

              q"play.boy.types.OptionColumn($name, $label, scala.collection.immutable.Map(..$options))"
            } else {
              q"play.boy.types.InvalidColumn($name, $label)"
            }
          }).toList

        q"($modelName, scala.collection.immutable.List(..$cols))"
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
