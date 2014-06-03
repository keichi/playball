package play.boy.doa

import scala.reflect.runtime.{universe => ru}
import play.api.db.slick.Config.driver.simple._

private[doa] object Duck {  
  type Model[A] = {
    val id: Option[Long]
    def productIterator: Iterator[Any]
  }
  type Table = {
    def id: Column[Long]
  }
}

// A: モデルクラス, B: Aをmappingするテーブル定義クラス
abstract class DOA[A <: Duck.Model[A] : ru.TypeTag, B <: Table[A] with Duck.Table] {
  protected val query: TableQuery[B]

  def list(implicit s: Session): List[A] = {
    query.list
  }

  def count(implicit s: Session): Int = {
    Query(query.length).first
  }

  def insert(item: A)(implicit s: Session) {
    query.insert(item)
  }

  def toCSV(implicit s: Session): String = {
    val builder = new StringBuilder()
    
    query.list.foreach({ row: A =>
      row.productIterator.foreach(c => builder ++= c.toString += ',')
      builder += '\n'
    })

    builder.toString
  }

  def findById(id: Long)(implicit s: Session): Option[A] = {
    query.where(_.id === id).firstOption
  }

  def update(id: Long, item: A)(implicit s: Session, ct: scala.reflect.ClassTag[A]) = {
    lazy val runtimeMirror = ru.typeTag[A].mirror
    val instanceMirror = runtimeMirror.reflect(item)

    lazy val method = ru.typeOf[A].member(ru.newTermName("copy")).asMethod
    val methodMirror = instanceMirror.reflectMethod(method)

    lazy val fields = ru.typeOf[A].members.filter(_.isTerm).map(_.asTerm).filter(_.isAccessor)
    val fieldValues = fields.map(f =>
      if (f.name.decoded == "id") {
          Some(id)
        } else {
          instanceMirror.reflectField(f).get
      }).toSeq.reverse

    val newItem = methodMirror(fieldValues:_*).asInstanceOf[A]

    query.where(_.id === id).update(newItem)
  }

  def delete(id: Long)(implicit s: Session) {
    query.where(_.id === id).delete
  }
}
