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
  def index(model: String) = DBAction { implicit s =>
    val f = Macros.handleIndex
    val x = BeerBrands.list.toArray

    Ok(f("beerbrand", x))
  }
  def create(model: String) = DBAction { implicit s =>
    Ok("")
  }
  def get(model: String, id: Long) = DBAction { implicit s =>
    val f = Macros.handleGet
    val x = BeerBrands.findById(id).get
    
    Ok(f("beerbrand", x))
  }
  def update(model: String, id: Long) = DBAction { implicit s =>
    Ok("")
  }
  def delete(model: String, id: Long) = DBAction { implicit s =>
    Ok("")
  }
}
