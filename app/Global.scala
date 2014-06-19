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
      lazy val regex = """/api/([^/]+)(/(\d+))?/?""".r
      val regex(model, _, id) = request.path

      val moduleSymbol = currentMirror.staticPackage("models").typeSignature.members
        .filter(_.isModule)
        .filter(_.typeSignature.baseClasses.contains(typeOf[DAO[_, _]].typeSymbol))
        .find(moduleSymbol => {
          val baseType = moduleSymbol.typeSignature.baseType(typeOf[DAO[_, _]].typeSymbol)
          val TypeRef(_, _, List(modelType, tableType)) = baseType

          modelType.typeSymbol.name.decoded.toLowerCase == model
        }).get.asModule

      val dao = currentMirror.reflectModule(moduleSymbol).instance.asInstanceOf[DAO[_, _]]

      request.method match {
        case "GET" if id == null => Some(controllers.REST.index(dao))
        case "GET" => Some(controllers.REST.get(dao, id.toLong))
        case "POST" => Some(controllers.REST.create(dao))
        case "DELETE" => Some(controllers.REST.delete(dao, id.toLong))
        case _ => super.onRouteRequest(request)
      }
    } else {
      super.onRouteRequest(request)
    }    
  }

}
