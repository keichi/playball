package controllers

import play.api._
import play.api.mvc._
import play.api.db.slick._
import play.api.libs.json.Json

import models._
import views._
import play.boy.dao._
import play.boy.macros.Macros

object REST extends Controller {
  val modelMapper = Macros.modelMapper
  val handleIndex = Macros.handleIndex
  val handleGet = Macros.handleGet

  def index(model: String) = DBAction { implicit s =>
    val x = modelMapper(model).get.list.toArray

    Ok(handleIndex(model, x))
  }
  def create(model: String) = DBAction { implicit s =>
    Ok("")
  }
  def get(model: String, id: Long) = DBAction { implicit s =>
    val x = modelMapper(model).get.findById(id).get
    
    Ok(handleGet(model, x))
  }
  def update(model: String, id: Long) = DBAction { implicit s =>
    Ok("")
  }
  def delete(model: String, id: Long) = DBAction { implicit s =>
    Ok("")
  }
}
