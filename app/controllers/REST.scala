import play.api._
import play.api.mvc._
import play.api.db.slick._

import models._
import views._

object Application extends Controller {
  def index = DBAction { implicit s =>
    Ok("")
  }
  def create = DBAction { implicit s =>
    Ok("")
  }
  def get(id: Long) = DBAction { implicit s =>
    Ok("")
  }
  def update(id: Long) = DBAction { implicit s =>
    Ok("")
  }
  def delete(id: Long) = DBAction { implicit s =>
    Ok("")
  }
}
