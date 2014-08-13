package controllers

import play.api._
import play.api.mvc._

import views._

object LogInOut extends Controller {
  def login = Action { implicit s =>
    Ok(views.html.login())
  }

  def authenticate = Action { implicit s =>
    Redirect(routes.Application.index).flashing(
      "success" -> "ログインしました。"
    )
  }

  def logout = Action { implicit s =>
    Redirect(routes.Application.index).flashing(
      "success" -> "ログアウトしました。"
    )
  }
}
