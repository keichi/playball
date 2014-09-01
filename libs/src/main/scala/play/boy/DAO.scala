package play.boy

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._
import scala.language.reflectiveCalls
import org.joda.time.DateTime
import play.api.db.slick.Config.driver.simple._
import com.github.tototoshi.csv.{CSVReader, CSVWriter}

// ダックタイピング用のstructural typeをまとめたクラス
object Duck {
  // モデルクラスが実装すべきメソッド・フィールド
  type Model = {
    val id: Option[Long]
    val createdBy: Option[Long]
    val updatedBy: Option[Long]
    val createdAt: DateTime
    val updatedAt: DateTime
    def productIterator: Iterator[Any]
  }

  // テーブル定義クラスが実装すべきメソッド・フィールド
  type Table = {
    def id: Column[Long]
  }
}

// これは別ファイルに分けた方がいいかも
object Util {
  // リソース開放のためのLoan Pattern
  def using[A, R <: { def close() }](r : R)(f : R => A) : A =
    try {
      f(r)
    } finally {
      r.close()
  }
}

// A: モデルクラス, B: Aをmappingするテーブル定義クラス
abstract class DAO[A <: Duck.Model: TypeTag : scala.reflect.ClassTag, B <: Table[A] with Duck.Table] {
  type Model = A
  type Table = B

  val query: TableQuery[B]
  lazy val acl = typeOf[A].typeSymbol.annotations
              .filter(_.tpe <:< typeOf[authorize])
              .map({ a =>
                val args = a.scalaArgs
                val enumType = args.head.tpe.asInstanceOf[TypeRefApi].pre
                val moduleMirror = currentMirror.reflectModule(enumType.termSymbol.asModule)
                val instanceMirror = currentMirror.reflect(moduleMirror.instance)
                val value = instanceMirror.reflectField(args.head.symbol.asTerm).get

                val read = if (args.length >= 2) args(1) match {
                  case Literal(Constant(x)) => x.asInstanceOf[Boolean]
                  case _ => false
                } else false
                val write = if (args.length >= 3) args(2) match {
                  case Literal(Constant(x)) => x.asInstanceOf[Boolean]
                  case _ => false
                } else false

                (value.asInstanceOf[RoleLike#Value], (read, write))
              })
              .toMap
  lazy val aclDefault = typeOf[A].typeSymbol.annotations
              .find(_.tpe <:< typeOf[authorizeDefault])
              .map({ a =>
                val args = a.scalaArgs
                val read = if (args.length >= 1) args(0) match {
                  case Literal(Constant(x)) => x.asInstanceOf[Boolean]
                  case _ => false
                } else false
                val write = if (args.length >= 2) args(1) match {
                  case Literal(Constant(x)) => x.asInstanceOf[Boolean]
                  case _ => false
                } else false

                (read, write)
              })
              .getOrElse((false, false))

  private lazy val classMirror = currentMirror.reflectClass(typeOf[A].typeSymbol.asClass)

  private lazy val constructorSymbol = typeOf[A].declaration(nme.CONSTRUCTOR)
  private lazy val defaultConstructor =
    if (constructorSymbol.isMethod) {
      constructorSymbol.asMethod
    } else {
      val ctors = constructorSymbol.asTerm.alternatives
      ctors.map(_.asMethod).find(_.isPrimaryConstructor).get
    }
  private lazy val constructorMirror = classMirror.reflectConstructor(defaultConstructor)

  private lazy val fieldSymbols = typeOf[A].members.filter(_.isTerm).map(_.asTerm).filter(_.isAccessor)

  private lazy val paramSymbols = defaultConstructor.paramss.head

  private def updateField(item: A, kvs: Map[String, Any]): A = {
    val instanceMirror = currentMirror.reflect(item)

    val fieldValues = fieldSymbols.map(f =>
        kvs.get(f.name.decoded).getOrElse(instanceMirror.reflectField(f).get)
      ).toSeq.reverse

    constructorMirror(fieldValues:_*).asInstanceOf[A]
  }

  def list(implicit s: Session): List[A] = {
    query.list
  }

  def list(page: Int, pageSize: Int = 10)(implicit s: Session): List[A] = {
    query.drop(pageSize * page).take(pageSize).list
  }

  def count(implicit s: Session): Int = {
    Query(query.length).first
  }

  def insert(item: A)(implicit ds: Session, s: play.api.mvc.Session = play.api.mvc.Session()): Long = {
    val newItem = updateField(item, Map(
      "createdBy" -> Auth.currentId,
      "updatedBy" -> Auth.currentId
    ))

    (query returning query.map(_.id)) += item
  }

  def insertAll(items: Iterable[A])(implicit ds: Session, s: play.api.mvc.Session = play.api.mvc.Session()): Option[Int] = {
    val newItems = items.map(
      item => updateField(item, Map(
        "createdBy" -> Auth.currentId,
        "updatedBy" -> Auth.currentId
      ))
    )

    query ++= newItems
  }

  def insertTypeUnsafe(item: Any)(implicit ds: Session, s: play.api.mvc.Session = play.api.mvc.Session()): Long = {
    insert(item.asInstanceOf[A])
  }

  def create(args: Any*)(implicit ds: Session, s: play.api.mvc.Session = play.api.mvc.Session()): Long = {
    val item = constructorMirror((args :+ Auth.currentId :+ Auth.currentId :+ new DateTime :+ new DateTime ):_*).asInstanceOf[A]

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
    Util.using(CSVWriter.open(sw)) { w =>
      query.list.foreach({ row: A =>
        w.writeRow(row.productIterator.map(colToString).toSeq)
      })
    }

    sw.toString
  }

  private def colFromString(col: String, colType: Type): Any = {
    if (colType =:= typeOf[String]) {
      col
    } else if (colType =:= typeOf[Boolean]) {
      col.toBoolean
    } else if (colType =:= typeOf[DateTime]) {
      new DateTime(col)
    } else if (colType =:= typeOf[Short]) {
      col.toShort
    } else if (colType =:= typeOf[Int]) {
      col.toInt
    } else if (colType =:= typeOf[Long]) {
      col.toLong
    } else if (colType =:= typeOf[Double]) {
      col.toDouble
    } else if (colType =:= typeOf[Float]) {
      col.toFloat
    } else if (colType.asInstanceOf[TypeRefApi].pre <:< typeOf[Enumeration]) {
      val enumType = colType.asInstanceOf[TypeRefApi].pre
      val moduleMirror = currentMirror.reflectModule(enumType.termSymbol.asModule)
      val obj = moduleMirror.instance.asInstanceOf[Enumeration]

      try {
        obj.withName(col)
      } catch {
        case e: java.util.NoSuchElementException => obj.apply(col.toInt)
      }
    } else if (colType <:< typeOf[Option[_]]) {
      if (col == "") {
        None
      } else {
        Some(colFromString(col, colType.asInstanceOf[TypeRefApi].args.head))
      }
    } else {
      null
    }
  }

  def fromCSV(csv: String)(implicit ds: Session, s: play.api.mvc.Session = play.api.mvc.Session()): Unit = {
    // StringReaderは閉じなくてよいらしい
    val sr = new java.io.StringReader(csv)
    val types = paramSymbols.map(_.typeSignature)
    Util.using(CSVReader.open(sr)) { r =>
      val items = r.iterator.flatMap({ cols =>
        val args = cols.zip(types).map({ case(col, t) =>
          try {
            colFromString(col, t)
          } catch {
            case e: Throwable => null
          }
        })

        try {
          Some(constructorMirror(args:_*).asInstanceOf[A])
        } catch {
          case e: Throwable => None
        }
      })

      insertAll(items.toList)
    }
  }

  def findById(id: Long)(implicit s: Session): Option[A] = {
    query.where(_.id === id).firstOption
  }

  def findByPK(id: Long)(implicit s: Session): Option[A] = findById(id)

  def update(id: Long, item: A)(implicit ds: Session, s: play.api.mvc.Session = play.api.mvc.Session(), ct: scala.reflect.ClassTag[A]): Int = {
    val newItem = updateField(item, Map(
      "id" -> Some(id),
      "updatedAt" -> new DateTime,
      "updatedBy" -> Auth.currentId
    ))

    query.where(_.id === id).update(newItem)
  }

  def updateTypeUnsafe(id: Long, item: Any)(implicit ds: Session, s: play.api.mvc.Session = play.api.mvc.Session()): Int = {
    update(id, item.asInstanceOf[A])
  }

  def delete(id: Long)(implicit s: Session): Int = {
    query.where(_.id === id).delete
  }
}
