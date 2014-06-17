package play.boy.mapping

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.db.slick.Config.driver.simple._

// EnumerationからTable[A]へのmappingを自動的に行うためのクラス
abstract class Enum extends Enumeration {
  implicit val enumColumnType = MappedColumnType.base[Value, Int](
    _.id,
    this.apply _
  )
  implicit val writes = new Writes[Value] {
    def writes(c: Value): JsValue = {
      JsNumber(c.id)
    }
  }
  implicit val reads = new Reads[Value] {
    def reads(js: JsValue): JsResult[Value] = {
      JsSuccess(Value(js.as[Int]))
    }
  }
  implicit val format = Format(reads, writes)
}
