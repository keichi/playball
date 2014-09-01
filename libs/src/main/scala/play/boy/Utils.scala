package play.boy

object StringUtils {
  implicit class StringImprovements(val s: String) {
    import scala.util.control.Exception._
    def toIntOpt = catching(classOf[NumberFormatException]) opt s.toInt
    def toLongOpt = catching(classOf[NumberFormatException]) opt s.toLong
  }
}
