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

  def modelTmpImpl(c: Context)(originalSymbol: c.universe.Symbol, colType: c.universe.Type, name: String, label: c.universe.Tree, optional: Boolean): c.universe.Tree = {
    import c.universe._
    import c.universe.Flag._

    if (colType =:= typeOf[String]) {
      val rows = originalSymbol.annotations
        .find(_.tpe =:= typeOf[play.boy.annotation.text])
        .map(x => q"Some(${x.scalaArgs.head})")
        .getOrElse(q"None")

      q"play.boy.types.StringColumn($name, $label, $optional, $rows)"
    } else if (colType =:= typeOf[Boolean]) {
      q"play.boy.types.BooleanColumn($name, $label, $optional)"
    } else if (colType =:= typeOf[DateTime]) {
      q"play.boy.types.DateTimeColumn($name, $label, $optional)"
    } else if (colType =:= typeOf[Short]) {
      q"play.boy.types.ShortColumn($name, $label, $optional)"
    } else if (colType =:= typeOf[Int]) {
      q"play.boy.types.IntColumn($name, $label, $optional)"
    } else if (colType =:= typeOf[Long]) {
      q"play.boy.types.LongColumn($name, $label, $optional)"
    } else if (colType =:= typeOf[Double]) {
      q"play.boy.types.DoubleColumn($name, $label, $optional)"
    } else if (colType =:= typeOf[Float]) {
      q"play.boy.types.FloatColumn($name, $label, $optional)"
    } else if (colType.asInstanceOf[TypeRefApi].pre <:< typeOf[Enum]) {
      val preType = colType.asInstanceOf[TypeRefApi].pre
      val enumSymbol = preType.termSymbol.asModule
      
      val options = preType.declarations
        .filter(_.isTerm)
        .filter(_.asTerm.isVal)
        .map({ fieldSymbol =>
          val itemSymbol = newTermName(fieldSymbol.name.decoded.trim)
          q"($enumSymbol.$itemSymbol.toString, $enumSymbol.$itemSymbol.id)"
        })
        .toList

      q"play.boy.types.OptionColumn($name, $label, $optional, scala.collection.immutable.Map(..$options))"
    } else if (colType <:< typeOf[Option[_]]) {
      val innerType = colType.asInstanceOf[TypeRefApi].args.head
      modelTmpImpl(c)(originalSymbol, innerType, name, label, true)
    } else {
      q"play.boy.types.InvalidColumn($name, $label, $optional)"
    }
  }

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
            val colType = m.typeSignature
            val name = m.name.decoded
            val label = m.annotations
              .find(_.tpe =:= typeOf[play.boy.annotation.label])
              .map(x => q"Some(${x.scalaArgs.head})")
              .getOrElse(q"None")
            modelTmpImpl(c)(m, colType, name, label, false)
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
