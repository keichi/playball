package controllers

import play.api._
import play.api.mvc._
import play.api.db.slick._
import play.api.libs.json._
import play.api.mvc.BodyParsers.parse
import play.api.db.slick.Config.driver.simple._

import play.boy.dao._
import play.boy.macros._
import play.boy.types._
import play.boy.types.joda._

object REST extends Controller {
  val findDAO = Macros.daoMap.get _
  val findMeta = Macros.modelMetaMap.get _

  val handleIndex = Macros.handleIndex
  val handleGet = Macros.handleGet
  val handleCreate = Macros.handleCreate
  val handleRPC = Macros.handleRPC

  val generatePredicate = Macros.generatePredicate
  val generateSorter = Macros.generateSorter

  def index(model: String) = DBAction { implicit rs =>
    findDAO(model).map({ dao =>
      val qs = rs.queryString
      val kvs = qs.toList.map(kv => (kv._1, kv._2.head))
      val q = dao.query.filter(_ => true)
      val q2 = kvs.foldLeft(q)((q, kv) => q.filter(x => generatePredicate(x, kv._1, kv._2)))
      
      // ソート
      val sortKey = qs.getOrElse("sort_by", Seq("id")).head
      val (col, direction) = if (sortKey.startsWith("-")) {
        (sortKey.substring(1) ,false)
      } else if (sortKey.startsWith("+")) {
        (sortKey.substring(1) ,true)
      } else {
        (sortKey, true)
      }
      val q3 = q2.sortBy(x => generateSorter(x, col, direction))

      // ページネーション
      val q4 = (qs.get("page_size"), qs.get("page")) match {
        case (Some(Seq(pageSize)), Some(Seq(page))) => {
          val ps = pageSize.toInt
          val p = page.toInt

          q3.drop(ps * p).take(ps)
        }
        case _ => q3
      }


      Ok(handleIndex(model, q4.list.toArray))
    }).getOrElse(
      BadRequest(Json.toJson(Map("message" -> s"Model $model not found.")))
    )
  }

  def create(model: String) = DBAction(parse.json) { implicit rs =>
    findDAO(model).map({dao =>
      handleCreate(model, rs.request.body).asInstanceOf[JsResult[_]] .map({ item =>
        val id = dao.insertTypeUnsafe(item)
        Ok(Json.toJson(Map("message" -> s"$model with id:$id created.")))
      }).getOrElse(
        BadRequest(Json.toJson(Map("message" -> s"Request JSON is malformed.")))
      )
    }).getOrElse(
      BadRequest(Json.toJson(Map("message" -> s"Model $model not found.")))
    )
  }

  def get(model: String, id: Long) = DBAction { implicit rs =>
    findDAO(model).map({ dao =>
      dao.findById(id).map({ item =>
        Ok(handleGet(model, item))
      }).getOrElse(
        NotFound(Json.toJson(Map("message" -> s"$model with id:$id not found.")))
      )
    }).getOrElse(
      BadRequest(Json.toJson(Map("message" -> s"Model $model not found.")))
    )
  }

  def update(model: String, id: Long) = DBAction(parse.json) { implicit rs =>
    findDAO(model).map({ dao =>
      dao.findById(id).map({ oldItem =>
        handleCreate(model, rs.request.body).asInstanceOf[JsResult[_]] .map({ item =>
          dao.updateTypeUnsafe(id, item)
          Ok(Json.toJson(Map("message" -> s"$model with id:$id updated.")))
        }).getOrElse(
          BadRequest(Json.toJson(Map("message" -> s"Request JSON is malformed.")))
        )
      }).getOrElse(
        NotFound(Json.toJson(Map("message" -> s"$model with id:$id not found.")))
      )
    }).getOrElse(
      BadRequest(Json.toJson(Map("message" -> s"Model $model not found.")))
    )
  }

  def delete(model: String, id: Long) = DBAction { implicit rs =>
    findDAO(model).map({ dao =>
      dao.delete(id)
      Ok(Json.toJson(Map("message" -> s"$model with id:$id deleted.")))
    }).getOrElse(
      BadRequest(Json.toJson(Map("message" -> s"Model $model not found.")))
    )
  }

  def meta(model: String) = DBAction { implicit rs =>
    findMeta(model).map({ meta =>
      val cols: List[JsValue] = meta.cols.map({ col =>
        val common =
          List(
            "name" -> Json.toJson(col.name),
            "label" -> Json.toJson(col.label),
            "optional" -> Json.toJson(col.optional)
          )

        val specific = col match {
          case StringColumn(_, _, _, rows) =>
            List("type" -> Json.toJson("string"), "rows" -> Json.toJson(rows))
          case BooleanColumn(_, _, _) =>
            List("type" -> Json.toJson("boolean"))
          case DateTimeColumn(_, _, _) =>
            List("type" -> Json.toJson("date"))
          case ShortColumn(_, _, _) =>
            List("type" -> Json.toJson("short"))
          case IntColumn(_, _, _) => 
            List("type" -> Json.toJson("long"))
          case LongColumn(_, _, _) =>
            List("type" -> Json.toJson("long"))
          case DoubleColumn(_, _, _) =>
            List("type" -> Json.toJson("double"))
          case FloatColumn(_, _, _) =>
            List("type" -> Json.toJson("float"))
          case OptionColumn(_, _, _, options) =>
            List("type" -> Json.toJson("float"), "options" -> Json.toJson(options))
          case InvalidColumn(_, _, _) =>
            List("type" -> Json.toJson("invalid"))
        }

        Json.toJson(Map((common ++ specific):_*))
      })

      Ok(Json.toJson(cols))
    }).getOrElse(
      BadRequest(Json.toJson(Map("message" -> s"Model $model not found.")))
    )
  }

  def rpc(model: String, method: String) = DBAction(parse.json) { implicit rs =>
    rs.request.body match {
      case JsObject(fields) => handleRPC(model, method, fields.toMap, rs.dbSession)
                                .map(json => Ok(json))
                                .getOrElse(BadRequest(Json.toJson(Map("message" -> "No matching model or method."))))
      case _ => BadRequest(Json.toJson(Map("message" -> "Request JSON is malformed.")))
    }
  }
}
