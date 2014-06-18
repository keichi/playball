import play.api._
import play.api.mvc.{Handler, RequestHeader}
import play.api.Play.current
import play.api.db.slick._

import models._

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
    lazy val regex = """/api/""".r

    super.onRouteRequest(request)
  }

}
