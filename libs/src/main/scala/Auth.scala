package play.boy.auth

import play.api.db.slick.DBSessionRequest
import play.api.Play.current
import play.api.cache.Cache

trait RoleLike extends Enumeration {
}

trait UserLike {
  val role: RoleLike#Value
}

trait UserDAOLike {
  def findById(id: Long)(implicit s: play.api.db.slick.Session): Option[UserLike]
}

object Extensions {
  implicit class DBSessionRequestExtensions(val rs: DBSessionRequest[_]) {
    def currentUser(implicit dao: UserDAOLike, s: play.api.db.slick.Session): Option[UserLike] = {
      currentId.flatMap(id => dao.findById(id))
    }

    def currentRole(implicit dao: UserDAOLike, s: play.api.db.slick.Session): Option[RoleLike#Value] = {
      currentUser.map(user => user.role)
    }

    def currentId: Option[Long] = {
      rs.session
        .get("token")
        .flatMap(token => Cache.getAs[Long](s"session.$token"))
    }
  }
}
