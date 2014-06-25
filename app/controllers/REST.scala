package controllers

import play.api._
import play.api.mvc._
import play.api.db.slick._
import play.api.libs.json.{Json, JsResult}
import play.api.mvc.BodyParsers._

import models._
import views._
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
      val cols = meta.map(_ match {
        case StringColumn(name, label, optional, rows) => {
          Json.toJson(
            Map(
              "type" -> Json.toJson("string"),
              "name" -> Json.toJson(name),
              "label" -> Json.toJson(label),
              "optional" -> Json.toJson(optional),
              "rows" -> Json.toJson(rows)
            )
          )
        }
        case BooleanColumn(name, label, optional) => {
          Json.toJson(
            Map(
              "type" -> Json.toJson("boolean"),
              "name" -> Json.toJson(name),
              "label" -> Json.toJson(label),
              "optional" -> Json.toJson(optional)
            )
          )
        }
        case DateTimeColumn(name, label, optional) => {
          Json.toJson(
            Map(
              "type" -> Json.toJson("date"),
              "name" -> Json.toJson(name),
              "label" -> Json.toJson(label),
              "optional" -> Json.toJson(optional)
            )
          )
        }
        case ShortColumn(name, label, optional) => {
          Json.toJson(
            Map(
              "type" -> Json.toJson("short"),
              "name" -> Json.toJson(name),
              "label" -> Json.toJson(label),
              "optional" -> Json.toJson(optional)
            )
          )
        }
        case IntColumn(name, label, optional) => {
          Json.toJson(
            Map(
              "type" -> Json.toJson("int"),
              "name" -> Json.toJson(name),
              "label" -> Json.toJson(label),
              "optional" -> Json.toJson(optional)
            )
          )
        }
        case LongColumn(name, label, optional) => {
          Json.toJson(
            Map(
              "type" -> Json.toJson("long"),
              "name" -> Json.toJson(name),
              "label" -> Json.toJson(label),
              "optional" -> Json.toJson(optional)
            )
          )
        }
        case DoubleColumn(name, label, optional) => {
          Json.toJson(
            Map(
              "type" -> Json.toJson("double"),
              "name" -> Json.toJson(name),
              "label" -> Json.toJson(label),
              "optional" -> Json.toJson(optional)
            )
          )
        }
        case FloatColumn(name, label, optional) => {
          Json.toJson(
            Map(
              "type" -> Json.toJson("float"),
              "name" -> Json.toJson(name),
              "label" -> Json.toJson(label),
              "optional" -> Json.toJson(optional)
            )
          )
        }
        case OptionColumn(name, label, optional, optionals) => {
          Json.toJson(
            Map(
              "type" -> Json.toJson("optional"),
              "name" -> Json.toJson(name),
              "label" -> Json.toJson(label),
              "optional" -> Json.toJson(optional),
              "optionals" -> Json.toJson(optionals)
            )
          )
        }
        case InvalidColumn(name, label, optional) => {
          Json.toJson(
            Map(
              "type" -> Json.toJson("invalid"),
              "name" -> Json.toJson(name),
              "label" -> Json.toJson(label),
              "optional" -> Json.toJson(optional)
            )
          )
        }
      })

      Ok(Json.toJson(cols))
    }).getOrElse(
      BadRequest(Json.toJson(Map("message" -> s"Model $model not found.")))
    )
  }
}
