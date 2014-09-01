package play

import com.github.tototoshi.slick.GenericJodaSupport

package object boy extends GenericJodaSupport(play.api.db.slick.Config.driver) {
  import org.joda.time.DateTime
  import play.api.libs.json._

  implicit object DefaultJodaDateWrites extends Writes[DateTime] {
    def writes(d: DateTime): JsValue = JsString(d.toString)
  }
}
