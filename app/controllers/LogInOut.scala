package controllers

import play.api._
import play.api.mvc._

import play.boy.auth._

import views._

object LogInOut extends Controller with AuthController {
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
