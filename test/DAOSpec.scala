import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._

import models._
import play.api.db.slick.DB
import play.api.db.slick.Config.driver.simple._

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */
@RunWith(classOf[JUnitRunner])
class DAOSpec extends Specification {
  "DAO interface" should {

    "list all items" in new WithApplication {
      DB.withSession{ implicit s =>
        BeerBrands.list.size must equalTo(3)
      }
    }

    "return number of items" in new WithApplication {
      DB.withSession{ implicit s =>
        BeerBrands.count must equalTo(3)
      }
    }

    "insert item" in new WithApplication {
      DB.withSession{ implicit s =>
        val item = BeerBrand(None, "TestBeer", "Japan", BeerStyle.Pilsener, false, 5.0, "")
        val id = BeerBrands.insert(item)
        BeerBrands.findById(id) must beSome.like({ case item2 => 
          item2.name must equalTo(item.name)
          item2.country must equalTo(item.country)
          item2.style must equalTo(item.style)
          item2.tasty must equalTo(item.tasty)
          item2.strength must equalTo(item.strength)
          item2.comment must equalTo(item.comment)
        })
      }
    }

    "insert multiple items" in new WithApplication {
      DB.withSession{ implicit s =>
        val count = BeerBrands.count
        val items = List(
          BeerBrand(None, "TestBeerA", "Japan", BeerStyle.Pilsener, false, 5.0, ""),
          BeerBrand(None, "TestBeerB", "Austria", BeerStyle.IPA, false, 6.0, ""),
          BeerBrand(None, "TestBeerC", "France", BeerStyle.Pilsener, true, 7.0, "")
        )
        BeerBrands.insertAll(items)
        BeerBrands.count must equalTo(count + 3)
      }
    }

    "return none for non-existing item" in new WithApplication {
      DB.withSession{ implicit s =>
        BeerBrands.findById(9999) must beNone
      }
    }

    "return single item" in new WithApplication {
      DB.withSession{ implicit s =>
        BeerBrands.findById(1) must beSome
        BeerBrands.findByPK(1) must beSome
      }
    }

    "delete item" in new WithApplication {
      DB.withSession{ implicit s =>
        val id = BeerBrands.create(None, "TestBeer2", "Japan", BeerStyle.Pilsener, false, 5.0, "")
        BeerBrands.findById(id) must beSome
        BeerBrands.delete(id)
        BeerBrands.findById(id) must beNone
      }
    }

    "create item" in new WithApplication {
      DB.withSession{ implicit s =>
        val id = BeerBrands.create(None, "TestBeer3", "Japan", BeerStyle.Pilsener, false, 5.0, "")
        BeerBrands.findById(id) must beSome.like({ case item => 
          item.name must equalTo("TestBeer3")
          item.country must equalTo("Japan")
          item.style must equalTo(BeerStyle.Pilsener)
          item.tasty must equalTo(false)
          item.strength must equalTo(5.0)
          item.comment must equalTo("")
        })
      }
    }

    "update item" in new WithApplication {
      DB.withSession{ implicit s =>
        val id = BeerBrands.create(None, "TestBeer3", "Japan", BeerStyle.Pilsener, false, 5.0, "")
        BeerBrands.update(id, BeerBrand(None, "UpdatedBeer", "America", BeerStyle.Pilsener, true, 15.0, "updated"))
        BeerBrands.findById(id) must beSome.like({ case item => 
          item.name must equalTo("UpdatedBeer")
          item.country must equalTo("America")
          item.style must equalTo(BeerStyle.Pilsener)
          item.tasty must equalTo(true)
          item.strength must equalTo(15.0)
          item.comment must equalTo("updated")
        })
      }
    }
  }
}
