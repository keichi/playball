package play.boy.macros

import language.experimental.macros
import scala.reflect.macros._
import org.joda.time.DateTime
import play.api.libs.json._
import scala.slick.lifted.{AbstractTable, Column, ColumnOrdered}
import scala.annotation.StaticAnnotation

import play.boy._

object Macros {
  def daoMap = macro daoMapImpl

  def daoMapImpl(c: Context): c.Expr[Map[String, DAO[_, _]]] = {
    import c.universe._

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

    if (colType =:= typeOf[String]) {
      q"StringColumn($name, $label, $optional)"
    } else if (colType =:= typeOf[Boolean]) {
      q"BooleanColumn($name, $label, $optional)"
    } else if (colType =:= typeOf[DateTime]) {
      q"DateTimeColumn($name, $label, $optional)"
    } else if (colType =:= typeOf[Short]) {
      q"ShortColumn($name, $label, $optional)"
    } else if (colType =:= typeOf[Int]) {
      q"IntColumn($name, $label, $optional)"
    } else if (colType =:= typeOf[Long]) {
      q"LongColumn($name, $label, $optional)"
    } else if (colType =:= typeOf[Double]) {
      q"DoubleColumn($name, $label, $optional)"
    } else if (colType =:= typeOf[Float]) {
      q"FloatColumn($name, $label, $optional)"
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

      q"OptionColumn($name, $label, $optional, scala.collection.immutable.Map(..$options))"
    } else if (colType <:< typeOf[Option[_]]) {
      val innerType = colType.asInstanceOf[TypeRefApi].args.head
      modelTmpImpl(c)(originalSymbol, innerType, name, label, true)
    } else {
      q"InvalidColumn($name, $label, $optional)"
    }
  }

  def modelMetaMapImpl(c: Context): c.Expr[Map[String, ModelMeta]] = {
    import c.universe._

    val tuples = c.mirror.staticPackage("models").typeSignature.members
      .filter(_.isModule)
      .filter(_.typeSignature.baseClasses.contains(typeOf[DAO[_, _]].typeSymbol))
      .map(moduleSymbol => {
        val baseType = moduleSymbol.typeSignature.baseType(typeOf[DAO[_, _]].typeSymbol)
        val TypeRef(_, _, List(modelType, _)) = baseType
        val modelName = modelType.typeSymbol.name.decoded.toLowerCase
        val modelFullName = modelType.typeSymbol.fullName

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
              .find(_.tpe =:= typeOf[label])
              .map(x => q"Some(${x.scalaArgs.head})")
              .getOrElse(q"None")
            modelTmpImpl(c)(m, colType, name, label, false)
          }).toList

        q"($modelName, ModelMeta($modelFullName, scala.collection.immutable.List(..$cols)))"
      }).toList

    c.Expr(q"scala.collection.immutable.Map(..$tuples)")
  }
}

class rest extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro RESTMacro.impl
}

object RESTMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def handleIndexImpl = {
      import c.universe._

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


      q"private def handleIndex(x: String, y: Array[_]): JsValue = x match { case ..$cases }"
    }

    def handleGetImpl = {
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

      q"private def handleGet(x: String, y: Any): JsValue = x match { case ..$cases }"
    }

    def handleRPCImpl = {
      val cases = c.mirror.staticPackage("models").typeSignature.members
        .filter(_.isModule)
        .filter(_.typeSignature.baseClasses.contains(typeOf[DAO[_, _]].typeSymbol))
        .flatMap(daoSymbol => {
          val baseType = daoSymbol.typeSignature.baseType(typeOf[DAO[_, _]].typeSymbol)
          val TypeRef(_, _, List(modelType, _)) = baseType

          val modelSymbol = modelType.typeSymbol.companionSymbol
          val modelName = modelSymbol.name.decoded.toLowerCase
          val writesSymbol = modelSymbol.typeSignature.members.find(_.typeSignature <:< typeOf[OFormat[_]]).get
          val writesName = newTermName(writesSymbol.name.toString.trim)

          val methods = daoSymbol.typeSignature.members
            .filter(m => m.name.decoded.endsWith("RPC"))
            .map(_.asMethod)

          methods.map(methodSymbol => {
            val methodName = "RPC$".r.replaceAllIn(methodSymbol.name.decoded, "")
            val args = methodSymbol.paramss.head.map(argSymbol => {
              val argType = argSymbol.typeSignature
              val common = q"args.get(${argSymbol.name.decoded}).map(_.as[$argType])"

              if (argType =:= typeOf[String]) {
                q"""$common.getOrElse("")"""
              } else if (argType =:= typeOf[Boolean]) {
                q"$common.getOrElse(false)"
              } else if (argType =:= typeOf[Short]) {
                q"$common.getOrElse(0:Short)"
              } else if (argType =:= typeOf[Int]) {
                q"$common.getOrElse(0)"
              } else if (argType =:= typeOf[Long]) {
                q"$common.getOrElse(0:Long)"
              } else if (argType =:= typeOf[Double]) {
                q"$common.getOrElse(0.0)"
              } else if (argType =:= typeOf[Float]) {
                q"$common.getOrElse(0:Float)"
              } else if (argType =:= typeOf[DateTime]) {
                q"$common.getOrElse(new DateTime)"
              } else if (argType.asInstanceOf[TypeRefApi].pre <:< typeOf[Enum]) {
                val enum = argType.asInstanceOf[TypeRefApi].pre.termSymbol
                q"$common.getOrElse($enum.values.firstKey)"
              } else if (argType <:< typeOf[Option[_]]) {
                q"$common.getOrElse(None)"
              } else {
                c.abort(c.enclosingPosition, "Exposed method takes argument of unsupported type")
              }
            })
            cq"($modelName, $methodName) => Some(play.api.libs.json.Json.toJson($daoSymbol.$methodSymbol(..$args).list()(s).toArray.map(_.asInstanceOf[$modelType]))(play.api.libs.json.Writes.arrayWrites[$modelType](implicitly, $modelSymbol.$writesName)))"
          })
        }).toList :+ cq"_ => None"

      q"private def handleRPC(model: String, method: String, args: Map[String, JsValue], s: play.api.db.slick.Config.driver.simple.Session): Option[JsValue] = (model, method) match { case ..$cases }"
    }

    def handleCreateImpl = {
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

      q"private def handleCreate(x: String, y: play.api.libs.json.JsValue): Any = x match { case ..$cases }"
    }

    def generateSorterImpl = {
      val models = c.mirror.staticPackage("models").typeSignature.members
        .filter(_.isModule)
        .filter(_.typeSignature.baseClasses.contains(typeOf[DAO[_, _]].typeSymbol))
        .map(moduleSymbol => {
          val baseType = moduleSymbol.typeSignature.baseType(typeOf[DAO[_, _]].typeSymbol)
          val TypeRef(_, _, List(modelType, tableType)) = baseType
          val tableName = tableType.typeSymbol

          val ctor = modelType.declarations
            .filter(_.isMethod)
            .map(_.asMethod)
            .find(_.isConstructor)
            .get

          val cols = ctor.asMethod.paramss.head
            .filter(_.isTerm)
            .flatMap({ m => 
              val colName = m.name.toTermName
              val colNameString = colName.decoded

              Seq(
                cq"$colNameString if direction => x.$colName.asc",
                cq"$colNameString if !direction => x.$colName.desc"
              )
            }).toList :+ cq"_ => x.id.asc"


          cq"x:$tableName => col match { case ..$cols }"
        }).toList

      q"private def generateSorter(row: scala.slick.lifted.AbstractTable[_], col: String, direction: Boolean): scala.slick.lifted.ColumnOrdered[_] = row match { case ..$models }"
    }

    def generatePredicateParser(colType: c.Type) = {
      if (colType =:= typeOf[String]) {
        (List("eq", "neq", "like"), q"arg")
      } else if (colType =:= typeOf[Boolean]) {
        (List("eq", "neq"), q"arg.toBoolean")
      } else if (colType =:= typeOf[Short]) {
        (List("eq", "neq", "gt", "lt", "ge", "le"), q"arg.toShort")
      } else if (colType =:= typeOf[Int]) {
        (List("eq", "neq", "gt", "lt", "ge", "le"), q"arg.toInt")
      } else if (colType =:= typeOf[Long]) {
        (List("eq", "neq", "gt", "lt", "ge", "le"), q"arg.toLong")
      } else if (colType =:= typeOf[Double]) {
        (List("eq", "neq", "gt", "lt", "ge", "le"), q"arg.toDouble")
      } else if (colType =:= typeOf[Float]) {
        (List("eq", "neq", "gt", "lt", "ge", "le"), q"arg.toFloat")
      } else if (colType =:= typeOf[DateTime]) {
        (List("eq", "neq", "gt", "lt", "ge", "le"), q"org.joda.time.DateTime.parse(arg, org.joda.time.format.ISODateTimeFormat.dateTime())")
      // } else if (colType <:< typeOf[Option[_]]) {
      //   val (_, inner) = generatePredicateParser(c)(colType.asInstanceOf[TypeRefApi].args.head)
      //   (List("eq", "neq"), q"$inner")
      } else if (colType.asInstanceOf[TypeRefApi].pre <:< typeOf[Enum]) {
        val enumType = colType.asInstanceOf[TypeRefApi].pre
        val enumName = enumType.termSymbol
        (List("eq", "neq"), q"$enumName.withName(arg)")
      } else {
        (List(), null)
      }
    }

    def generatePredicateImpl = {
      val models = c.mirror.staticPackage("models").typeSignature.members
        .filter(_.isModule)
        .filter(_.typeSignature.baseClasses.contains(typeOf[DAO[_, _]].typeSymbol))
        .map(moduleSymbol => {
          val baseType = moduleSymbol.typeSignature.baseType(typeOf[DAO[_, _]].typeSymbol)
          val TypeRef(_, _, List(modelType, tableType)) = baseType
          val tableName = tableType.typeSymbol

          val methodName = newTermName(s"generatePredicateFor${tableName.name.decoded}")
          cq"x:$tableName => $methodName(x, pred, arg)"
        }).toList

      q"private def generatePredicate(row: scala.slick.lifted.AbstractTable[_], pred: String, arg: String): Column[Boolean] = row match { case ..$models }"
    }

    def generatePredicateForImpl = {
      c.mirror.staticPackage("models").typeSignature.members
      .filter(_.isModule)
      .filter(_.typeSignature.baseClasses.contains(typeOf[DAO[_, _]].typeSymbol))
      .map(moduleSymbol => {
        val baseType = moduleSymbol.typeSignature.baseType(typeOf[DAO[_, _]].typeSymbol)
        val TypeRef(_, _, List(modelType, tableType)) = baseType
        val tableName = tableType.typeSymbol

        val ctor = modelType.declarations
          .filter(_.isMethod)
          .map(_.asMethod)
          .find(_.isConstructor)
          .get

        val cols = ctor.asMethod.paramss.head
          .filter(_.isTerm)
          .flatMap({ m => 
            val colType = m.typeSignature
            val colName = m.name.toTermName

            val (opNames, arg) = generatePredicateParser(colType)

            lazy val opMethodMap = Map(
              "eq" -> "$eq$eq$eq",
              "neq" -> "$eq$bang$eq",
              "gt" -> "$greater",
              "lt" -> "$less",
              "ge" -> "$greater$eq",
              "le" -> "$less$eq",
              "like" -> "like"
            )

            opNames.map({opName =>
              val predName = colName.decoded + "_" + opName
              val op = newTermName(opMethodMap.get(opName).get)

              cq"$predName => x.$colName.$op($arg)"
            })
          }).toList :+ cq"_ => true"

        val methodName = newTermName(s"generatePredicateFor${tableName.name.decoded}")
        q"private def $methodName(x: $tableName, pred: String, arg: String): Column[Boolean] = pred match { case ..$cols }"
      }).toList
    }

    def modifyModule(moduleDef: ModuleDef) = {
      val methods = Seq(
        handleIndexImpl,
        handleGetImpl,
        handleRPCImpl,
        handleCreateImpl,
        generateSorterImpl,
        generatePredicateImpl
      ) ++ generatePredicateForImpl

      val q"object $obj extends ..$bases { ..$body }" = moduleDef
      q"""
        object $obj extends ..$bases {
          ..$body
          ..$methods
        }
      """
    }

    annottees.map(_.tree) match {
      case (moduleDef: ModuleDef) :: Nil => c.Expr(modifyModule(moduleDef))
      case _ => c.abort(c.enclosingPosition, "Invalid annottee")
    }
  }
}
