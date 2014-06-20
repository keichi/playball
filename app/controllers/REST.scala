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
  val modelMapper = Macros.modelMapper
  val handleIndex = Macros.handleIndex
  val handleGet = Macros.handleGet
  val handleCreate = Macros.handleCreate

  def index(model: String) = DBAction { implicit rs =>
    modelMapper(model) match {
      case Some(dao) => Ok(handleIndex(model, dao.list.toArray))
      case None => NotFound(s"Model $model not found.")
    }
  }

  def create(model: String) = DBAction(parse.json) { implicit rs =>
    val item = handleCreate(model, rs.request.body).asInstanceOf[JsResult[_]].get

    modelMapper(model).get.insertTypeUnsafe(item)

    Ok("")
  }

  def get(model: String, id: Long) = DBAction { implicit rs =>
    modelMapper(model) match {
      case Some(dao) => dao.findById(id) match {
        case Some(item) => Ok(handleGet(model, item))
        case None => NotFound(s"$model with id:$id not found.")
      }
      case None => NotFound(s"Model $model not found.")
    }
  }

  def update(model: String, id: Long) = DBAction { implicit rs =>
    Ok("")
  }

  def delete(model: String, id: Long) = DBAction { implicit rs =>
    Ok("")
  }
}
