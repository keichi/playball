package controllers

import play.api._
import play.api.mvc._
import play.api.db.slick._

import models._
import views._
import models.Users._
import play.boy.auth.Extensions._

object Application extends Controller {
  def index = DBAction { implicit rs =>
    println(rs.currentId)
    println(rs.currentUser)
    println(rs.currentRole)

    Ok(views.html.index())
  }
}
