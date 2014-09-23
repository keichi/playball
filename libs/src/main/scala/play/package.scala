package play

import com.github.tototoshi.slick.GenericJodaSupport

package object ball extends GenericJodaSupport(play.api.db.slick.Config.driver) {
  import org.joda.time.DateTime
  import play.api.libs.json._

  implicit object DefaultJodaDateWrites extends Writes[DateTime] {
    def writes(d: DateTime): JsValue = JsString(d.toString)
  }

  implicit class StringImprovements(val s: String) {
    import scala.util.control.Exception._
    def toIntOpt = catching(classOf[NumberFormatException]) opt s.toInt
    def toLongOpt = catching(classOf[NumberFormatException]) opt s.toLong
  }
}
