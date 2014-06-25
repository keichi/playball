package models

import play.api.db.slick.Config.driver.simple._
import scala.slick.lifted.Tag

import org.joda.time.DateTime
import com.github.tototoshi.slick.H2JodaSupport._

import play.api.libs.json._
import play.boy.dao._
import play.boy.annotation._
import play.boy.types._

// 列挙型はEnumを継承する。使い方はEnumerationと同じ。
object BeerStyle2 extends Enum {
  type BeerStyle2 = Value
  val Pilsener = Value("ピルスナー")
  val Weissbier = Value("白ビール")
  val IPA = Value("IPA")
  val Stout = Value("スタウト")
  val Porter = Value("ポーター")
  val AmberAle = Value("アンバーエール")
}

import BeerStyle2.BeerStyle2

// case classをTable[A]にmappingして使うことを前提にしているので、
// まずはcase classを定義する。
case class BeerBrand2(
  @ignore
  id: Option[Long],
  @label("名前")
  name: String,
  @label("原産国")
  country: String,
  @label("種類")
  style: BeerStyle2,
  @label("美味しい")
  tasty: Boolean,
  @label("アルコール度数")
  strength: Double,
  @label("コメント") @text(5)
  comment: String,
  @ignore
  createdAt: DateTime = new DateTime,
  @ignore
  updatedAt: DateTime = new DateTime
)

object BeerBrand2 {
  implicit val format = Json.format[BeerBrand2]
}

// case classをmappingするTable[A]を定義する
class BeerBrands2(tag: Tag) extends Table[BeerBrand2](tag, "beer2") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name", O.NotNull)
  def country = column[String]("country")
  def style = column[BeerStyle2]("style")
  def tasty = column[Boolean]("tasty")
  def strength = column[Double]("strength")
  def comment = column[String]("comment")
  def createdAt = column[DateTime]("created_at")
  def updatedAt = column[DateTime]("updated_at")
  def * = (id.?, name, country, style, tasty, strength, comment, createdAt, updatedAt) <> ((BeerBrand2.apply _).tupled, BeerBrand2.unapply _)
}

object BeerBrands2 extends DAO[BeerBrand2, BeerBrands2] {
  val query = TableQuery[BeerBrands2]
}
