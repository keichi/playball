package play.boy.macros

import language.experimental.macros
import scala.reflect.macros.Context
import scala.reflect.ClassTag
import play.api.libs.json._

import play.boy.dao._
import play.boy.mapping._

abstract trait ColumnBase {
  val name: String
  val label: Option[String]
}

case class StringColumn(val name: String, val label: Option[String], val rows: Option[Int]) extends ColumnBase
case class BooleanColumn(val name: String, val label: Option[String]) extends ColumnBase
case class DateColumn(val name: String, val label: Option[String]) extends ColumnBase

case class ShortColumn(val name: String, val label: Option[String]) extends ColumnBase
case class IntColumn(val name: String, val label: Option[String]) extends ColumnBase
case class LongColumn(val name: String, val label: Option[String]) extends ColumnBase
case class DoubleColumn(val name: String, val label: Option[String]) extends ColumnBase
case class FloatColumn(val name: String, val label: Option[String]) extends ColumnBase

case class OptionColumn(val name: String, val label: Option[String], options: Map[String, Int]) extends ColumnBase
case class InvalidColumn(val name: String, val label: Option[String]) extends ColumnBase

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
          .filter(!_.annotations.exists(_.tpe <:< typeOf[play.boy.annotation.ignore]))
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

              q"StringColumn($name, $label, $rows)"
            } else if (colType =:= typeOf[Boolean]) {
              q"BooleanColumn($name, $label)"
            } else if (colType =:= typeOf[Short]) {
              q"ShortColumn($name, $label)"
            } else if (colType =:= typeOf[Int]) {
              q"IntColumn($name, $label)"
            } else if (colType =:= typeOf[Long]) {
              q"LongColumn($name, $label)"
            } else if (colType =:= typeOf[Double]) {
              q"DoubleColumn($name, $label)"
            } else if (colType =:= typeOf[Float]) {
              q"FloatColumn($name, $label)"
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

              q"OptionColumn($name, $label, scala.collection.immutable.Map(..$options))"
            } else {
              q"InvalidColumn($name, $label)"
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
