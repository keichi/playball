package play.boy.annotation

import scala.annotation.StaticAnnotation
import play.boy.auth.RoleLike

// modelクラスのメンバに使えるアノテーション
case class label(message: String) extends StaticAnnotation
case class text(rows: Int) extends StaticAnnotation
case class authorize(role: RoleLike#Value, read: Boolean, write: Boolean) extends StaticAnnotation
