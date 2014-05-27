import play.api._
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
            BeerBrand(None, "Zillertal", "Austria", BeerStyle.Weissbier, 5.0),
            BeerBrand(None, "Heineken", "Netherlands", BeerStyle.Pilsener, 5.0),
            BeerBrand(None, "Bitburger", "German", BeerStyle.Pilsener, 4.6)
          ).foreach(BeerBrands.insert)
        }
      }
    }
  }

}
