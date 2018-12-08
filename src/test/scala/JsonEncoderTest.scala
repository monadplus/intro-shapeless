import Model.IceCream
import org.scalatest.FreeSpec
import Json._, JsEncoder._
import DerivingLabelled._

class JsonEncoderTest extends FreeSpec {
  "An ice cream" - {
    val vanilla = IceCream("vanilla", inCone = true)
    "when encoded to json" - {
      "should be" in {
        assert {
          JsEncoder[IceCream].encode(vanilla) === JsObject {
            List(
              "name"   -> JsString("vanilla"),
              "inCone" -> JsBool(true)
            )
          }
        }
      }
    }
  }
}
