package controllers

import play.api._
import play.api.mvc._
import play.api.db.slick._
import play.api.libs.json._
import play.api.mvc.BodyParsers._
import play.api.db.slick.Config.driver.simple._

import models._
import play.boy.dao._
import play.boy.macros._
import play.boy.types._

object REST extends Controller {
  val findDAO = Macros.daoMap.get _
  val findMeta = Macros.modelMetaMap.get _
  val handleIndex = Macros.handleIndex
  val handleGet = Macros.handleGet
  val handleCreate = Macros.handleCreate

  def index(model: String) = DBAction { implicit rs =>
    findDAO(model).map({ dao =>
      Ok(handleIndex(model, dao.list.toArray))
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
}
