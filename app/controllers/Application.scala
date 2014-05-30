package controllers

import play.api._
import play.api.mvc._
import play.api.db.slick._
import scala.reflect.runtime.{universe => ru}

import models._
import views._

sealed abstract class ColumnBaseType(val name: String, val label: Option[String])

case class StringColumn(override val name: String, override val label: Option[String], val rows: Option[Int]) extends ColumnBaseType(name, label)
case class BooleanColumn(override val name: String, override val label: Option[String]) extends ColumnBaseType(name, label)
case class DateColumn(override val name: String, override val label: Option[String]) extends ColumnBaseType(name, label)

case class ShortColumn(override val name: String, override val label: Option[String]) extends ColumnBaseType(name, label)
case class IntColumn(override val name: String, override val label: Option[String]) extends ColumnBaseType(name, label)
case class LongColumn(override val name: String, override val label: Option[String]) extends ColumnBaseType(name, label)
case class DoubleColumn(override val name: String, override val label: Option[String]) extends ColumnBaseType(name, label)
case class FloatColumn(override val name: String, override val label: Option[String]) extends ColumnBaseType(name, label)

case class OptionColumn(override val name: String, override val label: Option[String], options: Map[String, Int]) extends ColumnBaseType(name, label)
case class InvalidColumn(override val name: String, override val label: Option[String]) extends ColumnBaseType(name, label)

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
              val label = m.annotations
                .find(_.tpe <:< ru.typeOf[label])
                .flatMap(_.scalaArgs.head match {
                  case ru.Literal(ru.Constant(s)) => Some(s.asInstanceOf[String])
                  case _ => None
                })
              
              val colType = m.returnType.asInstanceOf[ru.TypeRefApi].args.head
              val preType = colType.asInstanceOf[ru.TypeRefApi].pre

              if (colType <:< ru.typeOf[String]) {
                val rows = m.annotations
                  .find(_.tpe <:< ru.typeOf[text])
                  .flatMap(_.scalaArgs.head match {
                    case ru.Literal(ru.Constant(s)) => Some(s.asInstanceOf[Int])
                    case _ => None
                  })

                StringColumn(name, label, rows)
              } else if (colType =:= ru.typeOf[Boolean]) {
                BooleanColumn(name, label)
              } else if (colType =:= ru.typeOf[Short]) {
                ShortColumn(name, label)
              } else if (colType =:= ru.typeOf[Int]) {
                IntColumn(name, label)
              } else if (colType =:= ru.typeOf[Long]) {
                LongColumn(name, label)
              } else if (colType =:= ru.typeOf[Double]) {
                DoubleColumn(name, label)
              } else if (colType =:= ru.typeOf[Float]) {
                FloatColumn(name, label)
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


                OptionColumn(name, label, map)
              } else {
                InvalidColumn(name, label)
              }
            })
            .toSeq

    Ok(views.html.test(cols))
  }
}
