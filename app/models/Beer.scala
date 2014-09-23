package models

import play.api.db.slick.Config.driver.simple._
import scala.slick.lifted.Tag

import org.joda.time.DateTime

import play.api.libs.json.Json
import play.ball._

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

// case classをTable[A]にmappingして使うことを前提にしているので、
// まずはcase classを定義する。
@authorizeDefault(true, true)
case class BeerBrand(
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
  @label("コメント")
  comment: String,
  createdBy: Option[Long] = None,
  updatedBy: Option[Long] = None,
  createdAt: DateTime = new DateTime,
  updatedAt: DateTime = new DateTime
)

object BeerBrand {
  implicit val format = Json.format[BeerBrand]
}

// case classをmappingするTable[A]を定義する
class BeerBrands(tag: Tag) extends Table[BeerBrand](tag, "beer") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name", O.NotNull)
  def country = column[String]("country")
  def style = column[BeerStyle]("style")
  def tasty = column[Boolean]("tasty")
  def strength = column[Double]("strength")
  def comment = column[String]("comment")
  def createdBy = column[Option[Long]]("created_by")
  def updatedBy = column[Option[Long]]("updated_by")
  def createdAt = column[DateTime]("created_at")
  def updatedAt = column[DateTime]("updated_at")
  def * = (id.?, name, country, style, tasty, strength, comment, createdBy, updatedBy, createdAt, updatedAt) <> ((BeerBrand.apply _).tupled, BeerBrand.unapply _)
}

object BeerBrands extends DAO[BeerBrand, BeerBrands] {
  val query = TableQuery[BeerBrands]

  def findStrongerRPC(strength: Double) = {
    query.filter(_.strength >= strength)
  }

  def findByStyleAndCountryRPC(style: BeerStyle, country: String) = {
    query.filter(b => b.style === style && b.country === country)
  }
}
