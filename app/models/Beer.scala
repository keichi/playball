package models

import play.api.db.slick.Config.driver.simple._
import scala.slick.lifted.Tag

import scala.annotation.StaticAnnotation

case class ignore() extends StaticAnnotation
case class label(message: String) extends StaticAnnotation
case class text(rows: Int) extends StaticAnnotation

abstract class Enum extends Enumeration {
  implicit val enumColumnType = MappedColumnType.base[Value, Int](
    _.id,
    this.apply _
  )
}

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

object BeerBrands {
  val beerbrands = TableQuery[BeerBrands]

  def list(implicit s: Session): Seq[BeerBrand] = {
    beerbrands.list
  }

  def count(implicit s: Session): Int = {
    Query(beerbrands.length).first
  }

  def insert(beer: BeerBrand)(implicit s: Session) {
    beerbrands.insert(beer)
  }

  def update(id: Long, beer: BeerBrand)(implicit s: Session) {
    val newBeer: BeerBrand = beer.copy(Some(id))
    beerbrands.where(_.id === id).update(newBeer)
  }

  def delete(id: Long)(implicit s: Session) {
    beerbrands.where(_.id === id).delete
  }
}
