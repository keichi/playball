package controllers

import play.api._
import play.api.mvc._
import play.api.db.slick._
import play.api.libs.json.{Json, JsResult}
import play.api.libs.json.Json._
import play.api.mvc.BodyParsers._

import models._
import views._
import play.boy.dao._
import play.boy.macros.Macros

object REST extends Controller {
  val findDAO = Macros.daoMap.get _
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
    Ok("")
  }
}
