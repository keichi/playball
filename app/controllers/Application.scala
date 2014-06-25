package controllers

import play.api._
import play.api.mvc._
import play.api.db.slick._

import play.boy.macros._
import play.boy.macros.ColumnBase

import models._
import views._

object Application extends Controller {
  def index = DBAction { implicit s =>
    Ok(views.html.index(BeerBrands.list))
  }

  def showForm = Action {
    val cols = Macros.modelMetaMap.get("beerbrand").get

    Ok(views.html.test(cols))
  }

  def postForm = Action { request =>
    println(request.body)

    Redirect("/")
  }
}
