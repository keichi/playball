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

  lazy val runtimeMirror = ru.typeTag[A].mirror
  lazy val classMirror = runtimeMirror.reflectClass(ru.typeOf[A].typeSymbol.asClass)

  lazy val constructorSymbol = ru.typeOf[A].declaration(ru.nme.CONSTRUCTOR)
  lazy val defaultConstructor =
    if (constructorSymbol.isMethod) {
      constructorSymbol.asMethod
    } else {
      val ctors = constructorSymbol.asTerm.alternatives
      ctors.map { _.asMethod }.find { _.isPrimaryConstructor }.get
    }

  lazy val constructorMirror = classMirror.reflectConstructor(defaultConstructor)

  def list(implicit s: Session): List[A] = {
    query.list
  }

  def count(implicit s: Session): Int = {
    Query(query.length).first
  }

  def insert(item: A)(implicit s: Session): Long = {
    (query returning query.map(_.id)) += item
  }

  def create(args: Any*)(implicit s: Session): Long = {
    val item = constructorMirror(args:_*).asInstanceOf[A]

    (query returning query.map(_.id)) += item
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

  def findByPK(id: Long)(implicit s: Session): Option[A] = findById(id)

  def update(id: Long, item: A)(implicit s: Session, ct: scala.reflect.ClassTag[A]) = {
    val instanceMirror = runtimeMirror.reflect(item)

    lazy val fields = ru.typeOf[A].members.filter(_.isTerm).map(_.asTerm).filter(_.isAccessor)
    val fieldValues = fields.map(f =>
      if (f.name.decoded == "id") {
          Some(id)
        } else {
          instanceMirror.reflectField(f).get
      }).toSeq.reverse

    val newItem = constructorMirror(fieldValues:_*).asInstanceOf[A]

    query.where(_.id === id).update(newItem)
  }

  def delete(id: Long)(implicit s: Session) {
    query.where(_.id === id).delete
  }
}
