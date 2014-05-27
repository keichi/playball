package controllers

import play.api._
import play.api.mvc._
import play.api.db.slick._
import scala.reflect.runtime.{universe => ru}

import models._
import views._

sealed trait ColumnType

case class StringColumn(description: String) extends ColumnType
case class BooleanColumn(description: String) extends ColumnType
case class DateColumn(description: String) extends ColumnType
case class TimeColumn(description: String) extends ColumnType

case class ShortColumn(description: String) extends ColumnType
case class IntColumn(description: String) extends ColumnType
case class LongColumn(description: String) extends ColumnType
case class DoubleColumn(description: String) extends ColumnType
case class FloatColumn(description: String) extends ColumnType

case class OptionColumn(description: String, options: Map[String, Int]) extends ColumnType
case class InvalidColumn(description: String) extends ColumnType

object Application extends Controller {
  def index = DBAction { implicit rs =>
    Ok(views.html.index(BeerBrands.list))
  }

  def test = Action {
    val cols = ru.typeOf[BeerBrands].declarations
            .filter(_.isMethod)
            .map(_.asMethod)
            .filter(_.returnType <:< ru.typeOf[slick.lifted.Column[_]])
            .filter(!_.annotations.exists(_.tpe <:< ru.typeOf[ignore]))
            .map({ m => 
              val name = m.name.toString
              val colType = m.returnType.asInstanceOf[ru.TypeRefApi].args.head

              if (colType <:< ru.typeOf[String]) {
                StringColumn(name)
              } else if (colType <:< ru.typeOf[Boolean]) {
                BooleanColumn(name)
              } else if (colType <:< ru.typeOf[Short]) {
                ShortColumn(name)
              } else if (colType <:< ru.typeOf[Int]) {
                IntColumn(name)
              } else if (colType <:< ru.typeOf[Long]) {
                LongColumn(name)
              } else if (colType <:< ru.typeOf[Double]) {
                DoubleColumn(name)
              } else if (colType <:< ru.typeOf[Float]) {
                FloatColumn(name)
              } else if (colType.baseClasses.exists(s => s.asClass.toType <:< ru.typeOf[Enum])) {
                OptionColumn(name, Map())
              } else {
                InvalidColumn(name)
              }
            })
            .toSeq

    Ok(views.html.test(cols))
  }
}
