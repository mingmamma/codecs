package codecs

// Implicit Conversion lecture: https://www.coursera.org/learn/effective-scala/lecture/YmFcf/implicit-conversions
sealed trait Json3
case class JNumber(value: BigDecimal) extends Json3
case class JBool(value: Boolean) extends Json3
case class JString(value: String) extends Json3
case class JArray(elements: List[Json3]) extends Json3
case class JObject(fields: (String, Json3)*) extends Json3

case class JObjectFieldValue(json: Json3) extends Json3
object JObjectFieldValue:
  given Conversion[String, JObjectFieldValue] with
    def apply(x: String): JObjectFieldValue = JObjectFieldValue(JString(x))

  given Conversion[Int, JObjectFieldValue] with
    def apply(x: Int): JObjectFieldValue = JObjectFieldValue(JNumber(x))

object Json3:
  // use of repeated parameter syntax
  def obj(fields: (String, Json3)*): Json3 =
    JObject(fields*)

  def obj2(fields: (String, JObjectFieldValue)*): Json3 =
    JObject(fields.map(field => (field._1, field._2.json))*)

def renderJson3(json3: Json3): String = json3 match
  case JBool(value) => value.toString
  case JNumber(value) => value.toString
  case JString(value) => value
  case JArray(elements) => elements.map(renderJson3).mkString("[", ",", "]")
  case JObject(fields*) => fields.map(field => s"${field._1}:${renderJson3(field._2)}").mkString("{", ",", "}")       

// @main def run(): Unit =
//   println(renderJson3(Json3.obj("name" -> JString("Paul"), "age" -> JNumber(42))))
//   // The following is an example of the provision of more ergonomic API to construct a Json3.JObject AST instance
//   // With the introduction of JObjectFieldValue into the Json3.obj2 utility method for Json3.JObject creation
//   // the additional implicit coversion definitions allow for the input types liek String and Int to be automatically
//   // converted to JObjectFieldValue type expected by the Json3.obj2 method by the compiler
//   import scala.language.implicitConversions
//   println(renderJson3(Json3.obj2("name" -> "Paul", "age" -> 42)))
