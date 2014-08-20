package play.boy.auth

import play.api.Play.current
import play.api.cache.Cache
import scala.annotation.implicitNotFound
import play.api.db.slick._
import play.api.mvc._

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

  def Authenticated[A](roles: RoleLike#Value*)(bodyParser: BodyParser[A])(requestHandler: DBSessionRequest[A] => SimpleResult)(implicit dao: UserDAOLike): Action[A] = {
    DBAction(bodyParser) { rs: DBSessionRequest[A] =>
      implicit val ds = rs.dbSession
      implicit val s = rs.session

      currentRole match {
        case Some(role) if (roles.contains(role) || roles.isEmpty) => requestHandler(rs)
        case _ => Results.Unauthorized("このページを閲覧する権限がありません。")
      }
    }
  }

  def Authenticated(roles: RoleLike#Value*)(requestHandler: DBSessionRequest[AnyContent] => SimpleResult)(implicit dao: UserDAOLike): Action[AnyContent] = {
    Authenticated[AnyContent](roles:_*)(BodyParsers.parse.anyContent)(requestHandler)
  }

  def Authenticated(requestHandler: DBSessionRequest[AnyContent] => SimpleResult)(implicit dao: UserDAOLike): Action[AnyContent] = {
    Authenticated[AnyContent]()(BodyParsers.parse.anyContent)(requestHandler)
  }
}
