package play.ball

import scala.annotation.StaticAnnotation

// modelクラスのメンバに使えるアノテーション
case class label(message: String) extends StaticAnnotation
case class authorize(role: RoleLike#Value, read: Boolean, write: Boolean) extends StaticAnnotation
case class authorizeDefault(read: Boolean, write: Boolean) extends StaticAnnotation
