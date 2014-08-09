package play.boy.types

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
      JsString(c.toString)
    }
  }
  implicit val reads = new Reads[Value] {
    def reads(js: JsValue): JsResult[Value] = {
      JsSuccess(withName(js.as[String]))
    }
  }
  implicit val format = Format(reads, writes)
}

abstract trait ColumnBase {
  val name: String
  val label: Option[String]
  val optional: Boolean
}

case class ModelMeta(name: String, cols: List[ColumnBase])

case class StringColumn(val name: String, val label: Option[String], val optional: Boolean, val rows: Option[Int]) extends ColumnBase
case class BooleanColumn(val name: String, val label: Option[String], val optional: Boolean) extends ColumnBase
case class DateTimeColumn(val name: String, val label: Option[String], val optional: Boolean) extends ColumnBase

case class ShortColumn(val name: String, val label: Option[String], val optional: Boolean) extends ColumnBase
case class IntColumn(val name: String, val label: Option[String], val optional: Boolean) extends ColumnBase
case class LongColumn(val name: String, val label: Option[String], val optional: Boolean) extends ColumnBase
case class DoubleColumn(val name: String, val label: Option[String], val optional: Boolean) extends ColumnBase
case class FloatColumn(val name: String, val label: Option[String], val optional: Boolean) extends ColumnBase

case class OptionColumn(val name: String, val label: Option[String], val optional: Boolean, options: Map[String, Int]) extends ColumnBase
case class InvalidColumn(val name: String, val label: Option[String], val optional: Boolean) extends ColumnBase
