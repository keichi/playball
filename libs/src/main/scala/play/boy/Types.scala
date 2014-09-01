package play.boy

import play.api.libs.json._
import play.api.db.slick.Config.driver.simple._
import play.api.data.FormError
import play.api.data.format.Formatter
import play.api.data.format.Formats.intFormat

// EnumerationからTable[A]へのmappingを自動的に行うためのクラス
abstract class Enum extends Enumeration {
  def options: Seq[(String, String)] = {
    values.map(v => (v.id.toString, v.toString)).toSeq
  }

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
  implicit val formatter = new Formatter[Value] {
    def bind(key: String, data: Map[String, String]) = {
      intFormat.bind(key, data).right.flatMap { value =>
        scala.util.control.Exception.allCatch[Value]
          .either(apply(value))
          .left.map(e => Seq(FormError(key, "error.enum", Nil)))
      }
    }

    def unbind(key: String, value: Value) = Map(
      key -> value.id.toString
    )
  }
}

abstract trait ColumnBase {
  val name: String
  val label: Option[String]
  val optional: Boolean
}

case class ModelMeta(name: String, cols: List[ColumnBase])

case class StringColumn(val name: String, val label: Option[String], val optional: Boolean) extends ColumnBase
case class BooleanColumn(val name: String, val label: Option[String], val optional: Boolean) extends ColumnBase
case class DateTimeColumn(val name: String, val label: Option[String], val optional: Boolean) extends ColumnBase

case class ShortColumn(val name: String, val label: Option[String], val optional: Boolean) extends ColumnBase
case class IntColumn(val name: String, val label: Option[String], val optional: Boolean) extends ColumnBase
case class LongColumn(val name: String, val label: Option[String], val optional: Boolean) extends ColumnBase
case class DoubleColumn(val name: String, val label: Option[String], val optional: Boolean) extends ColumnBase
case class FloatColumn(val name: String, val label: Option[String], val optional: Boolean) extends ColumnBase

case class OptionColumn(val name: String, val label: Option[String], val optional: Boolean, options: Map[String, Int]) extends ColumnBase
case class InvalidColumn(val name: String, val label: Option[String], val optional: Boolean) extends ColumnBase
