package models

import play.api.db.slick.Config.driver.simple._
import scala.slick.lifted.Tag

import scala.annotation.StaticAnnotation

case class ignore() extends StaticAnnotation
case class label(message: String) extends StaticAnnotation

abstract class Enum extends Enumeration {
  implicit val enumColumnType = MappedColumnType.base[Value, Int](
    _.id,
    this.apply _
  )
}

object BeerStyle extends Enum {
  type BeerStyle = Value
  val Pilsener, Weissbier, IPA, Stout, Porter, AmberAle = Value
}

import BeerStyle.BeerStyle

case class BeerBrand(
  id: Option[Long],
  name: String,
  country: String,
  style: BeerStyle,
  strength: Double
)

class BeerBrands(tag: Tag) extends Table[BeerBrand](tag, "BEER") {
  @ignore
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  @label("名前")
  def name = column[String]("name", O.NotNull)
  @label("原産国")
  def country = column[String]("country", O.NotNull)
  @label("種類")
  def style = column[BeerStyle]("style", O.NotNull)
  @label("アルコール度数")
  def strength = column[Double]("strength", O.NotNull)
  def * = (id.?, name, country, style, strength) <> (BeerBrand.tupled, BeerBrand.unapply _)
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
