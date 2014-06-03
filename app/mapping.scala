package play.boy.mapping

import play.api.db.slick.Config.driver.simple._

// EnumerationからTable[A]へのmappingを自動的に行うためのクラス
abstract class Enum extends Enumeration {
  implicit val enumColumnType = MappedColumnType.base[Value, Int](
    _.id,
    this.apply _
  )
}
