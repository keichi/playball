package play.boy.annotation

import scala.annotation.StaticAnnotation
import play.boy.auth.RoleLike

object Permission {
  case object Read extends Permission
  case object Write extends Permission
}

sealed abstract trait Permission {
}

// modelクラスのメンバに使えるアノテーション
case class label(message: String) extends StaticAnnotation
case class text(rows: Int) extends StaticAnnotation
case class owner(perms: Permission*) extends StaticAnnotation
case class authorize(role: RoleLike#Value, perms: Permission*) extends StaticAnnotation
