import play.api._
import play.api.mvc.{Handler, RequestHeader}
import play.api.Play.current
import play.api.db.slick._

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._

import models._
import play.boy.dao._

object Global extends GlobalSettings {

  override def onStart(app: Application) {
    InitialData.insert()
  }

  object InitialData {
    def insert(): Unit = {
      DB.withSession { implicit s: Session =>
        if (BeerBrands.count == 0) {
          Seq(
            BeerBrand(None, "Zillertal", "Austria", BeerStyle.Weissbier, true, 5.0, ""),
            BeerBrand(None, "Heineken", "Netherlands", BeerStyle.Pilsener, false, 5.0, ""),
            BeerBrand(None, "Bitburger", "German", BeerStyle.Pilsener, true, 4.6, "")
          ).foreach(BeerBrands.insert)
        }
      }
    }
  }

  override def onRouteRequest(request: RequestHeader): Option[Handler] = {
    if (request.path.startsWith("/api/")) {
      lazy val regex = """/api/([^/]+)(/([^/]+))?/?""".r
      val regex(model, _, id) = request.path

      if (model == "meta") {
          Some(controllers.REST.meta(id))
        } else {
          request.method match {
            case "GET" if id == null => Some(controllers.REST.index(model))
            case "GET" => Some(controllers.REST.get(model, id.toLong))
            case "POST" => Some(controllers.REST.create(model))
            case "PUT" if id  != null => Some(controllers.REST.update(model, id.toLong))
            case "DELETE" => Some(controllers.REST.delete(model, id.toLong))
            case _ => super.onRouteRequest(request)
          }
        }
    } else {
      super.onRouteRequest(request)
    }    
  }

}
