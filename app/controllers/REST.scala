package controllers

import play.api._
import play.api.mvc._
import play.api.db.slick._
import play.api.libs.json.Json

import models._
import views._
import play.boy.dao._

object REST extends Controller {
  def index(dao: DAO[_, _]) = DBAction { implicit s =>
    Ok(Json.toJson(dao.list.asInstanceOf[List[BeerBrand]]))
  }
  def create(dao: DAO[_, _]) = DBAction { implicit s =>
    Ok("")
  }
  def get(dao: DAO[_, _], id: Long) = DBAction { implicit s =>
    Ok(Json.toJson(dao.findById(id).asInstanceOf[Option[BeerBrand]]))
  }
  def update(dao: DAO[_, _], id: Long) = DBAction { implicit s =>
    Ok("")
  }
  def delete(dao: DAO[_, _], id: Long) = DBAction { implicit s =>
    Ok("")
  }
}
