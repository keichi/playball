package play.boy.auth

import play.api.Play.current
import play.api.cache.Cache
import scala.annotation.implicitNotFound

trait RoleLike extends Enumeration {
}

trait UserLike {
  val role: RoleLike#Value
}

trait UserDAOLike {
  def findById(id: Long)(implicit ds: play.api.db.slick.Session): Option[UserLike]
}

@implicitNotFound("Both DB session and HTTP session is required.")
object Auth {
  def isLoggedIn(implicit ds: play.api.db.slick.Session, s: play.api.mvc.Session): Boolean = {
    currentId.isDefined
  }

  def currentUser(implicit dao: UserDAOLike, ds: play.api.db.slick.Session, s: play.api.mvc.Session): Option[UserLike] = {
    currentId.flatMap(id => dao.findById(id))
  }

  def currentRole(implicit dao: UserDAOLike, ds: play.api.db.slick.Session, s: play.api.mvc.Session): Option[RoleLike#Value] = {
    currentUser.map(user => user.role)
  }

  def currentId(implicit s: play.api.mvc.Session): Option[Long] = {
    s.get("token").flatMap(token => Cache.getAs[Long](s"session.$token"))
  }
}
