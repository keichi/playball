package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.Crypto
import play.api.cache.Cache
import play.api.db.slick._
import play.api.db.slick.Config.driver.simple._
import play.api.Play.current

import views._
import models.Users

object LogInOut extends Controller {
  val loginForm = Form(
  tuple(
    "username" -> text,
    "password" -> text
    )
  )

  def login = Action { implicit s =>
    Ok(views.html.login())
  }

  def authenticate = DBAction { implicit rs =>
    val (username, password) = loginForm.bindFromRequest.get

    Users.findByUsernameAndPassword(username, password)
      .flatMap(user => {
        user.id match {
          case Some(id) => {
            val token = Crypto.generateToken
            Cache.set(s"session.$token", id)

            Some(Redirect(routes.Application.index).flashing(
              "success" -> s"ログインしました。ようこそ${user.name}さん。"
            ).withSession(
              rs.session + ("token", token)
            ))
          }
          case _ => None
        }
      })
      .getOrElse(
        Redirect(routes.LogInOut.login).flashing(
          "error" -> s"ログインできませんでした。"
        )
      )
  }

  def logout = Action { implicit s =>
    Redirect(routes.Application.index).flashing(
      "success" -> "ログアウトしました。"
    )
  }
}
