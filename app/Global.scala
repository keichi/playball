import play.api._
import play.api.mvc.{Handler, RequestHeader}
import play.api.Play.current
import play.api.db.slick._

import models._
import play.ball._

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
            BeerBrand(None, "Bitburger", "German", BeerStyle.Pilsener, true, 4.6, ""),
            BeerBrand(None, "Alpha", "Greek", BeerStyle.Pilsener, true, 5.0, ""),
            BeerBrand(None, "Guinness", "Ireland", BeerStyle.Porter, true, 5.0, ""),
            BeerBrand(None, "Asahi Super Dry", "Japan", BeerStyle.Pilsener, false, 5.6, "")
          ).foreach(BeerBrands.insert)
        }

        if (Users.count == 0) {
          Seq(
            User(None, "山田太郎", "yamada1", "yamada1@example.com", Role.Administrator, "123-4567", "ダミー住所", "test", false),
            User(None, "山田花子", "yamada2", "yamada2@example.com", Role.NormalUser, "123-4567", "ダミー住所", "test", false),
            User(None, "山田次郎", "yamada3", "yamada3@example.com", Role.NormalUser, "123-4567", "ダミー住所", "test", false),
            User(None, "山田三郎", "yamada4", "yamada4@example.com", Role.NormalUser, "123-4567", "ダミー住所", "test", false),
            User(None, "田中太郎", "tanaka1", "tanaka1@example.com", Role.NormalUser, "123-4567", "ダミー住所", "test", false),
            User(None, "田中花子", "tanaka2", "tanaka2@example.com", Role.Administrator, "123-4567", "ダミー住所", "test", false)
          ).foreach(Users.insert)
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
            case "POST" if id != null => Some(controllers.REST.rpc(model, id))
            case "POST" if id == null => Some(controllers.REST.create(model))

            case "GET" if id.toLongOpt.isDefined => Some(controllers.REST.get(model, id.toLong))
            case "PUT" if id.toLongOpt.isDefined => Some(controllers.REST.update(model, id.toLong))
            case "DELETE" if id.toLongOpt.isDefined => Some(controllers.REST.delete(model, id.toLong))
            case _ => super.onRouteRequest(request)
          }
        }
    } else {
      super.onRouteRequest(request)
    }    
  }
}
