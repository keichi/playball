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
              val preType = colType.asInstanceOf[ru.TypeRefApi].pre

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
              } else if (colType.asInstanceOf[ru.TypeRefApi].pre <:< ru.typeOf[Enum]) {
                val runtimeMirror = ru.typeTag[preType.type].mirror 
                val moduleMirror = runtimeMirror.reflectModule(preType.termSymbol.asModule)
                val obj = moduleMirror.instance
                
                val map = preType.declarations
                  .filter(_.isTerm)
                  .filter(_.asTerm.isVal)
                  .map({t =>
                    val o = runtimeMirror.reflect(obj).reflectField(t.asTerm).get
                    val item = o.asInstanceOf[Enumeration#Value]
                    (item.toString, item.id)
                  })
                  .toMap


                OptionColumn(name, map)
              } else {
                InvalidColumn(name)
              }
            })
            .toSeq

    Ok(views.html.test(cols))
  }
}
