package play.boy.annotation

import scala.annotation.StaticAnnotation

// modelクラスのメンバに使えるアノテーション
case class label(message: String) extends StaticAnnotation
case class text(rows: Int) extends StaticAnnotation
