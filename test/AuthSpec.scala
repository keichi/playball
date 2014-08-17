import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._
import play.api.libs.json._

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */
@RunWith(classOf[JUnitRunner])
class AuthSpec extends Specification {
  "Auth module" should {

    "create and return session token at login" in new WithApplication {
      val result = route(
          FakeRequest(POST, "/login")
          .withFormUrlEncodedBody(
            "username" -> "yamada1",
            "password" -> "test"
          )
        ).get

      session(result).data must haveKey("token")
    }

    "create/delete session token at login/logout" in new WithApplication {
      val result = route(
          FakeRequest(POST, "/login")
          .withFormUrlEncodedBody(
            "username" -> "yamada1",
            "password" -> "test"
          )
        ).get

      session(result).data must haveKey("token")

      val result2 = route(FakeRequest(GET, "/logout")).get

      session(result2).data must not haveKey("token")
    }

    "not allow login twice" in new WithApplication {
      val result = route(
          FakeRequest(POST, "/login")
          .withFormUrlEncodedBody(
            "username" -> "yamada1",
            "password" -> "test"
          )
        ).get

      session(result).data must haveKey("token")

      val token = session(result).data.get("token").get

      val result2 = route(
          FakeRequest(POST, "/login")
          .withFormUrlEncodedBody(
            "username" -> "yamada2",
            "password" -> "test"
          ).withSession(session(result).data.toSeq:_*)
        ).get

      session(result2).data must haveKey("token")

      val token2 = session(result2).data.get("token").get

      token must equalTo(token2)
    }
  }
}
