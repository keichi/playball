package play.ball

object Util {
  import scala.language.reflectiveCalls
  // リソース開放のためのLoan Pattern
  def using[A, R <: { def close() }](r : R)(f : R => A) : A =
    try {
      f(r)
    } finally {
      r.close()
  }
}
