package controllers

import play.api._
import play.api.mvc._
import play.api.db.slick._
import scala.reflect.runtime.{universe => ru}

import play.api.data._
import play.api.data.Forms._
import play.api.data.format.Formats._

import play.boy.annotation._

import models._
import views._

sealed abstract trait ColumnBase {
  val name: String
  val label: Option[String]
}

case class StringColumn(val name: String, val label: Option[String], val rows: Option[Int]) extends ColumnBase
case class BooleanColumn(val name: String, val label: Option[String]) extends ColumnBase
case class DateColumn(val name: String, val label: Option[String]) extends ColumnBase

case class ShortColumn(val name: String, val label: Option[String]) extends ColumnBase
case class IntColumn(val name: String, val label: Option[String]) extends ColumnBase
case class LongColumn(val name: String, val label: Option[String]) extends ColumnBase
case class DoubleColumn(val name: String, val label: Option[String]) extends ColumnBase
case class FloatColumn(val name: String, val label: Option[String]) extends ColumnBase

case class OptionColumn(val name: String, val label: Option[String], options: Map[String, Int]) extends ColumnBase
case class InvalidColumn(val name: String, val label: Option[String]) extends ColumnBase

object Application extends Controller {
  def index = DBAction { implicit s =>
    Ok(views.html.index(BeerBrands.list))
  }

  def showForm = Action {
    val ctor = ru.typeOf[BeerBrand].declarations
            .filter(_.isMethod)
            .map(_.asMethod)
            .find(_.isConstructor)
            .get

    val cols = ctor.asMethod.paramss.head
            .filter(_.isTerm)
            .filter(!_.annotations.exists(_.tpe <:< ru.typeOf[ignore]))
            .map({ m => 
              val name = m.name.toString
              val label = m.annotations
                .find(_.tpe <:< ru.typeOf[label])
                .flatMap(_.scalaArgs.head match {
                  case ru.Literal(ru.Constant(s)) => Some(s.asInstanceOf[String])
                  case _ => None
                })
              
              val colType = m.typeSignature
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

  def postForm = Action { request =>
    println(request.body)

    Redirect("/")
  }
}
