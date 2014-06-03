package play.boy.doa

import play.api.db.slick.Config.driver.simple._

private[doa] object PlayboyDuck {  
  type Model[A] = {
    val id: Option[Long]
  }
  type Table = {
    def id: Column[Long]
  }
}

// A: モデルクラス, B: Aをmappingするテーブル定義クラス
abstract class DOA[A <: PlayboyDuck.Model[A], B <: Table[A] with PlayboyDuck.Table] {
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
    ""
  }

  def findById(id: Long)(implicit s: Session): Option[A] = {
    query.where(_.id === id).firstOption
  }

  // def update(id: Long, item: A)(implicit s: Session) {
  //   val newItem: A = item.copy(Some(id))
  //   query.where(_.id === id).update(newItem)
  // }

  def delete(id: Long)(implicit s: Session) {
    query.where(_.id === id).delete
  }
}
