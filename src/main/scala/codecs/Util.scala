package codecs

import org.typelevel.jawn.{ Parser, Facade }

// Utility methods that decode values from `String` JSON blobs, and
// render values to `String` JSON blobs
object Util:

  /**
   * Parse a JSON document contained in a `String` value into a `Json` value, returns
   * `None` in case the supplied `s` value is not a valid JSON document.
   */
  // with Facade[Json] instance in place, one is ready to use jawn's parser. In this instance
  // parseFromString variant of the parse signiture is used. The parse calls return a Try[Json]
  // instance, which is converted to an Option with toOption
  // https://github.com/typelevel/jawn/?tab=readme-ov-file#parsing
  def parseJson(s: String): Option[Json] = Parser.parseFromString[Json](s).toOption

  /**
   * Parse the JSON value from the supplied `s` parameter, and then try to decode
   * it as a value of type `A` using the given `decoder`.
   *
   * Returns `None` if JSON parsing failed, or if decoding failed.
   */
  def parseAndDecode[A](s: String)(using decoder: Decoder[A]): Option[A] =
    for
      json <- parseJson(s)
      a <- decoder.decode(json)
    yield a

  /**
   * Render the supplied `value` into JSON using the given `encoder`.
   */
  // the renderJson method completes the end-to-end serialization:
  // instance of generic Scala data type -> an instance of respective Json Data type, via encoder.encode() call
  // an instance of Json data type -> proper Json String representation, via render() call
  def renderJson[A](value: A)(using encoder: Encoder[A]): String =
    render(encoder.encode(value))

  private def render(json: Json): String = json match
    case Json.Null    => "null"
    case Json.Bool(b) => b.toString
    case Json.Num(n)  => n.toString
    case Json.Str(s)  => renderString(s)
    // The following two make use of the mkString() method from the IterableOnce Trait
    // which use three parameters to make a String representation of a collection
    // namely, a start symbol, a seperater symbol (for in-between elements), and a end symbol
    case Json.Arr(vs) => vs.map(render).mkString("[", ",", "]")
    case Json.Obj(vs) => vs.map((k, v) => s"${renderString(k)}:${render(v)}").mkString("{", ",", "}")

  // Going over a String s character by character to handle specific characters for rendering
  private def renderString(s: String): String =
    val sb = new StringBuilder
    sb.append('"')
    var i = 0
    val len = s.length
    while i < len do
      s.charAt(i) match
        // the following cases SEEM to be related to escaping
        // unclear:
        // 1. why " needs to be escaped as \\\", \" SEEMS suficient
        // 2. why single \ and / are NOT escaped
        // 3. otherwise the list is generally sensible
        // Assuming the authoritative source, see the escape section: https://www.json.org/json-en.html
        case '"'  => sb.append("\\\"")
        case '\\' => sb.append("\\\\")
        // Annotating just for reference: https://en.wikipedia.org/wiki/ASCII#Control_characters
        case '\b' => sb.append("\\b") // backspace
        case '\f' => sb.append("\\f") // form feed
        case '\n' => sb.append("\\n") // line feed
        case '\r' => sb.append("\\r") // carriage return
        case '\t' => sb.append("\\t") // horizontal tabb
        case c =>
          // this line SEEMS to mean that for the cases not covered previously
          // if the character in question is before the whitespace character (Dec 32) as ref'ed in ASCII: https://en.wikipedia.org/wiki/ASCII#Control_characters
          // which equate to the control characters of Dec 0 to 31, use the C Lang integer format specifier %04x, prefixed with \\u
          // to obtain the formatted result of the character's corresponding integer. Specifically %04x means padded to at least 4
          // digits of the given integer in hexadecimal, e.g. the result on 15 is 000f
          if c < ' ' then sb.append("\\u%04x" format c.toInt)
          else sb.append(c)
      i += 1
    sb.append('"').toString

  // In order to use our own Json AST while still using Jawn's parser, we provide a Facade[Json] instance.
  // Our case is simple s.t. the SimpleFacade helper is sufficient, following a similar example
  // https://github.com/typelevel/jawn/#do-it-yourself-parsing 
  given Facade.SimpleFacade[Json] with
    def jnull = Json.Null
    def jtrue = Json.Bool(true)
    def jfalse = Json.Bool(false)
    def jnum(s: CharSequence, decIndex: Int, expIndex: Int) = Json.Num(BigDecimal(s.toString))
    def jstring(s: CharSequence) = Json.Str(s.toString)
    def jarray(vs: List[Json]) = Json.Arr(vs)
    def jobject(vs: Map[String, Json]) = Json.Obj(vs)

end Util
