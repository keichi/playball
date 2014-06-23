package play.boy.dao

import scala.reflect.runtime.universe._
import org.joda.time.DateTime
import play.api.db.slick.Config.driver.simple._

// ダックタイピング用のstructural typeをまとめたクラス
object Duck {
  // モデルクラスが実装すべきメソッド・フィールド
  type Model[A] = {
    val id: Option[Long]
    val updatedAt: DateTime
    val createdAt: DateTime
    def productIterator: Iterator[Any]
  }

  // テーブル定義クラスが実装すべきメソッド・フィールド
  type Table = {
    def id: Column[Long]
  }
}

// A: モデルクラス, B: Aをmappingするテーブル定義クラス
abstract class DAO[A <: Duck.Model[A]: TypeTag : scala.reflect.ClassTag, B <: Table[A] with Duck.Table] {
  protected val query: TableQuery[B]

  lazy val runtimeMirror = typeTag[A].mirror
  lazy val classMirror = runtimeMirror.reflectClass(typeOf[A].typeSymbol.asClass)

  lazy val constructorSymbol = typeOf[A].declaration(nme.CONSTRUCTOR)
  lazy val defaultConstructor =
    if (constructorSymbol.isMethod) {
      constructorSymbol.asMethod
    } else {
      val ctors = constructorSymbol.asTerm.alternatives
      ctors.map(_.asMethod).find(_.isPrimaryConstructor).get
    }
  lazy val constructorMirror = classMirror.reflectConstructor(defaultConstructor)

  lazy val fieldSymbols = typeOf[A].members.filter(_.isTerm).map(_.asTerm).filter(_.isAccessor)

  private def updateField(item: A, kvs: Map[String, Any]): A = {
    val instanceMirror = runtimeMirror.reflect(item)

    val fieldValues = fieldSymbols.map(f =>
        kvs.get(f.name.decoded).getOrElse(instanceMirror.reflectField(f).get)
      ).toSeq.reverse

    constructorMirror(fieldValues:_*).asInstanceOf[A]
  }

  def list(implicit s: Session): List[A] = {
    query.list
  }

  def count(implicit s: Session): Int = {
    Query(query.length).first
  }

  def insert(item: A)(implicit s: Session): Long = {
    (query returning query.map(_.id)) += item
  }

  def insertAll(items: Iterable[A])(implicit s: Session): Option[Int] = {
    query ++= items
  }

  def insertTypeUnsafe(item: Any)(implicit s: Session): Long = {
    insert(item.asInstanceOf[A])
  }

  def create(args: Any*)(implicit s: Session): Long = {
    val item = constructorMirror((args :+ new DateTime :+ new DateTime ):_*).asInstanceOf[A]

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
    val newItem = updateField(item, Map("id" -> Some(id), "updatedAt" -> new DateTime))

    query.where(_.id === id).update(newItem)
  }

  def updateTypeUnsafe(id: Long, item: Any)(implicit s: Session) = {
    update(id, item.asInstanceOf[A])
  }

  def delete(id: Long)(implicit s: Session) {
    query.where(_.id === id).delete
  }
}
