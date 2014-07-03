package play.boy.dao

import scala.reflect.runtime.universe._
import org.joda.time.DateTime
import play.api.db.slick.Config.driver.simple._
import com.github.tototoshi.csv.CSVWriter

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
  val query: TableQuery[B]

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

  private def colToString(col: Any): String = {
    col match {
      case x: String => x
      case x: Short => x.toString
      case x: Int => x.toString
      case x: Long => x.toString
      case x: Float => x.toString
      case x: Double => x.toString
      case x: DateTime => x.toString
      case x: Boolean => x.toString
      case Some(x) => colToString(x)
      case None => ""
      case x => col.toString
    }
  }

  def toCSV(implicit s: Session): String = {
    // StringWriterは閉じなくてよいらしい
    val sw = new java.io.StringWriter()
    val writer = CSVWriter.open(sw)

    try {
      query.list.foreach({ row: A =>
        writer.writeRow(row.productIterator.map(colToString).toSeq)
      })
    } finally {
      writer.close()
    }

    sw.toString
  }

  def fromCSV(csv: String)(implicit s: Session): Unit = {
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
