package play.boy.macros

import language.experimental.macros
import scala.reflect.macros.Context
import play.api.libs.json._

object Macros {
  def handleIndex(model: String): JsValue = macro handleIndexImpl

  def handleIndexImpl(c: Context)(model: c.Expr[String]): c.Expr[JsValue] = {
    import c.universe._

    reify { JsNumber(123) }
  }
}
