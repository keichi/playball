package models

import play.api.db.slick.Config.driver.simple._
import scala.slick.lifted.Tag

import scala.annotation.StaticAnnotation

// modelクラスのメンバに使えるアノテーション
case class ignore() extends StaticAnnotation
case class label(message: String) extends StaticAnnotation
case class text(rows: Int) extends StaticAnnotation

// EnumerationからTable[A]へのmappingを自動的に行うためのクラス
abstract class Enum extends Enumeration {
  implicit val enumColumnType = MappedColumnType.base[Value, Int](
    _.id,
    this.apply _
  )
}

// 列挙型はEnumを継承する。使い方はEnumerationと同じ。
object BeerStyle extends Enum {
  type BeerStyle = Value
  val Pilsener = Value("ピルスナー")
  val Weissbier = Value("白ビール")
  val IPA = Value("IPA")
  val Stout = Value("スタウト")
  val Porter = Value("ポーター")
  val AmberAle = Value("アンバーエール")
}

import BeerStyle.BeerStyle

trait ModelBase {
  val id: Option[Long]
  val updatedAt: java.sql.Date
  val createdAt: java.sql.Date
}

// case classをTable[A]にmappingして使うことを前提にしているので、
// まずはcase classを定義する。
case class BeerBrand(
  @ignore
  id: Option[Long],
  @label("名前")
  name: String,
  @label("原産国")
  country: String,
  @label("種類")
  style: BeerStyle,
  @label("美味しい")
  tasty: Boolean,
  @label("アルコール度数")
  strength: Double,
  @label("コメント") @text(5)
  comment: String
)

// case classをmappingするTable[A]を定義する
class BeerBrands(tag: Tag) extends Table[BeerBrand](tag, "BEER") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name", O.NotNull)
  def country = column[String]("country")
  def style = column[BeerStyle]("style")
  def tasty = column[Boolean]("tasty")
  def strength = column[Double]("strength")
  def comment = column[String]("comment")
  def * = (id.?, name, country, style, tasty, strength, comment) <> (BeerBrand.tupled, BeerBrand.unapply _)
}

// A: モデルクラス, B: Aをmappingするテーブル定義クラス
abstract class DOA[A, B <: Table[A]] {
  protected val query: TableQuery[B]

  def list(implicit s: Session): Seq[A] = {
    query.list
  }

  def count(implicit s: Session): Int = {
    Query(query.length).first
  }

  def insert(item: A)(implicit s: Session) {
    query.insert(item)
  }

  def toCSV(implicit s: Session): String = {
    ""
  }

  // def update(id: Long, item: A)(implicit s: Session) {
  //   val newItem: A = item.copy(Some(id))
  //   query.where(_.id === id).update(newItem)
  // }

  // def delete(id: Long)(implicit s: Session) {
  //   query.where(_.id === id).delete
  // }
}

object BeerBrands extends DOA[BeerBrand, BeerBrands] {
  val query = TableQuery[BeerBrands]
}
