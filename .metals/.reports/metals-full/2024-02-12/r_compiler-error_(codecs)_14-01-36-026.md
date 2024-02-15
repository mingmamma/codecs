file://<WORKSPACE>/src/main/scala/codecs/Codecs.scala
### java.lang.AssertionError: assertion failed: position error, parent span does not contain child span
parent      = [A](using protected given val codecA: Json2Codec[A]) extends Json2Codec[
  Vector[A]] {
  def encode(value: Vector[A]): Json2 =
    Json2Arr(value.map(element => codecA.encode(element)))
  def decode(json: Json2): Vector[A] = null
} # -1,
parent span = <5594..5791>,
child       = def decode(json: Json2): Vector[A] = null # -1,
child span  = [5755..5759..6844]

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.0
Classpath:
<WORKSPACE>/.bloop/codecs/bloop-bsp-clients-classes/classes-Metals-ExMt2PeiQ4Kr75wHRp9ODw== [exists ], <HOME>/Library/Caches/bloop/semanticdb/com.sourcegraph.semanticdb-javac.0.9.9/semanticdb-javac-0.9.9.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.3.0/scala3-library_3-3.3.0.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/jawn-parser_3/1.1.2/jawn-parser_3-1.1.2.jar [exists ], <HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.10/scala-library-2.13.10.jar [exists ]
Options:
-deprecation -Xsemanticdb -sourceroot <WORKSPACE>


action parameters:
uri: file://<WORKSPACE>/src/main/scala/codecs/Codecs.scala
text:
```scala
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

/**
  * A data type modeling JSON values.
  *
  * For example, the `42` integer JSON value can be modeled as `Json.Num(42)`
  */
sealed trait Json:
  /**
   * Try to decode this JSON value into a value of type `A` by using
   * the given decoder.
   *
   * Note that you have to explicitly fix `A` type parameter when you call the method:
   *
   * {{{
   *   someJsonValue.decodeAs[User] // OK
   *   someJsonValue.decodeAs       // Wrong!
   * }}}
   */
  def decodeAs[A](using decoder: Decoder[A]): Option[A] = decoder.decode(this)

// The Json data types, following the Json spec
// https://en.wikipedia.org/wiki/JSON#Data_types
object Json:
  /** The JSON `null` value */
  case object Null extends Json
  /** JSON boolean values */
  case class Bool(value: Boolean) extends Json
  /** JSON numeric values */
  case class Num(value: BigDecimal) extends Json
  /** JSON string values */
  case class Str(value: String) extends Json
  /** JSON objects */
  case class Obj(fields: Map[String, Json]) extends Json
  /** JSON arrays */
  case class Arr(items: List[Json]) extends Json

// trait is preferred over abstract class in Scala3
// https://docs.scala-lang.org/scala3/book/domain-modeling-tools.html#abstract-classes
// sealed abstract class Json2:
sealed trait Json2:
  def decode[T](using json2decoder: Json2Decoder[T]): T = json2decoder.decode(this)

// object Json2:
// companion object in the original definition help with namespace management but not essential
// https://docs.scala-lang.org/scala3/book/domain-modeling-tools.html#other-uses 

sealed trait Json2Bool extends Json2:
  def value: Boolean
  
case object Json2True extends Json2Bool:
  def value = true

case object Json2False extends Json2Bool:
  def value = false

case class Json2Num(value: BigDecimal) extends Json2
case class Json2Str(value: String) extends Json2
case class Json2Arr(elements: Vector[Json2]) extends Json2:
  // The following would make an auxillary constructor: https://docs.scala-lang.org/scala3/book/domain-modeling-tools.html#auxiliary-constructors
  // Vector is a more "balanced" choice of a sequential data structure than List
  def this(elements: List[Json2]) = this(elements.toVector)

case object Json2Null extends Json2

trait Json2Decoder[T]:
  def decode(json: Json2): T

trait Json2Encoder[T]:
  def encode(value: T): Json2

// type-class trait Json2Codec that has two abstract methods for any Codec instance
trait Json2Codec[T] extends Json2Decoder[T], Json2Encoder[T]:

// trait primitiveCodes:
  
  // the following line will give a unparameterised given instances in Scala3 equivalent to implicit objects in Scala2, the with in Scala3
  // or extends in Scala2 is followed by the object body where the required abstract methods are implemented
  // given UnitCodec: Json2Codec[Unit] with
  // implicit object UnitCodec extends Json2Codec[Unit] (Scala2)
  // https://docs.scala-lang.org/scala3/reference/contextual/relationship-implicits.html#given-instances-1
  // In favor of implicit val over implicit object, the implicit val is used instead
  // https://docs.scala-lang.org/tutorials/FAQ/index.html#why-is-implicit-val-usually-recommended-over-implicit-object 

  given UnitCodec: Json2Codec[Unit] = new Json2Codec[Unit] {
    def encode(value: Unit) = Json2Null
    def decode(json: Json2) = 
      json match
        case Json2Null => ()
  }

  given BooleanCodec: Json2Codec[Boolean] = new Json2Codec[Boolean] {
    def encode(value: Boolean) = if value == true then Json2True else Json2False
    def decode(json: Json2) = 
      json match
        case Json2False => false
        case Json2True => true 
  }

  given Json2Codec[Char] with
    def encode(value: Char) = Json2Str(String.valueOf(value))
    def decode(json: Json2) =
      json match
        case Json2Str(value) if value.length() == 1 => value.head


  given Json2Codec[String] with
    def encode(value: String) = Json2Str(value)
    def decode(json: Json2) = 
      json match
        case Json2Str(value) => value

  given IntCodec: Json2Codec[Int] with
    def encode(value: Int) = Json2Num(value)
    def decode(json: Json2) = 
      json match
        case Json2Num(value) if value.isValidInt => value.intValue

  given DoubleCodec: Json2Codec[Double] with
    def encode(value: Double) = Json2Num(value)
    def decode(json: Json2) = 
      json match
        case Json2Num(value) => value.doubleValue

  // would-be equivalent with context bound syntax sugar: https://docs.scala-lang.org/scala3/book/ca-context-bounds.html
  // given VectorCodec[A: Json2Codec]: Json2Codec[Vector[A]] with
  given VectorCodec[A](using codecA: Json2Codec[A]): Json2Codec[Vector[A]] with
    def encode(value: Vector[A]): Json2 = Json2Arr(value.map(element => codecA.encode(element)))
    def decode(json: Json2): Vector[A] = 


  // given ArrayCodec[A: Json2Codec]: Json2Codec[Array[A]] with

  // given ListCodec[A: Json2Codec]: Json2Codec[List[A]] with

  // given SetCodec[A: Json2Codec]: Json2Codec[Set[A]] with
  


      


/**
  * A type class that turns a value of type `A` into its JSON representation.
  */
// This example illuminates the concept of type class. The first step to declare a type class is to declare a parameterised
// trait, in this example, `trait Encoder[-A]` followed by one of more defining abstract methods within, namely in this case, 
// `def encode(value: A): Json`. Type class differs from a more regular trait in that the regular trait definition of Encoder,
// presumably `trait Encoder` followed by `def encode(): Json` abstract method, would require sub-typing to provide implementation for the 
// concreate type A, i.e. `A extends Encoder`. Type class spares the need of sub-typing like that.
// https://docs.scala-lang.org/scala3/book/ca-type-classes.html
// https://docs.scala-lang.org/scala3/book/ca-type-classes.html#the-type-class
trait Encoder[-A]:
  
  // Defining the role of an encoder of a given type A s.t. it is an instance responsible for
  // maping the an instance of Scala data type A to an instance of one of the
  // Json data type variants as given in the Json object  
  /** Encodes a value of type `A` into JSON */
  def encode(value: A): Json

    /**
    * Transforms this `Encoder[A]` into an `Encoder[B]`, given a transformation function
    * from `B` to `A`.
    *
    * For instance, given a `Encoder[String]`, we can get an `Encoder[UUID]`:
    *
    * {{{
    *   def uuidEncoder(given stringEncoder: Encoder[String]): Encoder[UUID] =
    *     stringEncoder.transform[UUID](uuid => uuid.toString)
    * }}}
    *
    * This operation is also known as “contramap”.
    */
    // As examplied in the actual usage later, transform op is useful in providing an
    // Encoder[B] instance by delegating the defining op to encode a value of type B
    // to work by first applying f: B =>A on that value to get an instance of type A, s.t. Encoder[A]
    // would already exist. Thus the total effect of encoding a value of type B is to
    // encode the corresponding value in type A using an already known Encoder[A]
  def transform[B](f: B => A): Encoder[B] =
    Encoder.fromFunction[B]((value: B) => this.encode(f(value)))

end Encoder

object Encoder extends EncoderInstances:

  /**
   * Convenient method for creating an instance of encoder from a function `f`
   */
  def fromFunction[A](f: A => Json) = new Encoder[A] {
    def encode(value: A): Json = f(value)
  }

end Encoder

// The following encoders are created with the covienience of the fromFunction() defined in the Encoder companion objects
// which a Encoder construction helper that is generic over types. The crust of a concrete enconder of a type A (of Scala data type) 
// is to be able to map an instance of A to the resective "Json" data type instance. Hence the only defining info of a encoder
// is such map f as the argument to fromFunction
trait EncoderInstances:

  // Having been spared to directly sub-typing the Encoder abstract type by using type class instead,
  // Here comes the concreate definition of Encoder for various Scala data types
  // the `given` is used to declare the canonical value for a particular type
  // since there is just one principal encoder instance needed for each data type
  // https://docs.scala-lang.org/scala3/book/ca-context-parameters.html#given-instances-implicit-definitions-in-scala-2
  
  /** An encoder for the `Unit` value */
  given unitEncoder: Encoder[Unit] =
    Encoder.fromFunction((_: Unit) => Json.Null)

  /** An encoder for `Int` values */
  given intEncoder: Encoder[Int] =
    Encoder.fromFunction((n: Int) => Json.Num(BigDecimal(n)))

  // An encoder for `Float` values
  given floatEncoder: Encoder[Double] =
    Encoder.fromFunction((f: Double) => Json.Num(BigDecimal(f)))  

  /** An encoder for `String` values */
  given stringEncoder: Encoder[String] =
    Encoder.fromFunction((s: String) => Json.Str(s))

  /** An encoder for `Boolean` values */
  given booleanEncoder: Encoder[Boolean] =
    Encoder.fromFunction((b: Boolean) => Json.Bool(b))

  /**
    * Encodes a list of values of type `A` into a JSON array containing
    * the list elements encoded with the given `encoder`
    */
  // the encoder for the scala data type List[A] should map to the Json data type Json.Arr()
  // As per the definition given: `case class Arr(items: List[Json]) extends Json`, we see
  // Json.Arr() call would take items: List[Json] as the argument. Hence a map is used to 
  // map each instance of type A to its respective Json by an encode() call
  given listEncoder[A](using encoder: Encoder[A]): Encoder[List[A]] =
    Encoder.fromFunction(as => Json.Arr(as.map((a: A) => encoder.encode(a))))

end EncoderInstances

/**
  * A specialization of `Encoder` that returns JSON objects only
  */
trait ObjectEncoder[-A] extends Encoder[A]:
  // Refines the encoding result to `Json.Obj`
  def encode(value: A): Json.Obj

  /**
    * Combines `this` encoder with `that` encoder.
    * Returns an encoder producing a JSON object containing both
    * fields of `this` encoder and fields of `that` encoder.
    */
  def zip[B](that: ObjectEncoder[B]): ObjectEncoder[(A, B)] =
    ObjectEncoder.fromFunction { (a, b) =>
      Json.Obj(this.encode(a).fields ++ that.encode(b).fields)
    }
end ObjectEncoder

object ObjectEncoder:

  /**
    * Convenient method for creating an instance of object encoder from a function `f`
    */
  def fromFunction[A](f: A => Json.Obj): ObjectEncoder[A] = new ObjectEncoder[A] {
    def encode(value: A): Json.Obj = f(value)
  }

  /**
    * An encoder for values of type `A` that produces a JSON object with one field
    * named according to the supplied `name` and containing the encoded value.
    */
  def field[A](name: String)(using encoder: Encoder[A]): ObjectEncoder[A] =
    ObjectEncoder.fromFunction(a => Json.Obj(Map(name -> encoder.encode(a))))

end ObjectEncoder

/**
  * The dual of an encoder. Decodes a serialized value into its initial type `A`.
  */
// `trait Decoder[+A]` means the subtyping of Decoder[A] is covariant in its type parameter A,
// hence annotated as Decoder[A]. Traits that are covariant to its type parameters are dinstinct
// by the fact that the abstract methods in the trait involve the type parameter ONLY in the return
// position. Convariance relation annotated by Decoder[+A] entails that given type parameters A, B s.t 
// A is a subtype of B, then Decoder[A] is a subtype of Decoder[B]
// https://docs.scala-lang.org/tour/variances.html#covariance

// Subtyping relation means the ability to supply a subtype of a trait to the call site where the call site
// has required an instance of that trait. This definition captures high-level essence of the Liskov 
// substitution principle for a lay reader
// https://docs.scala-lang.org/tour/traits.html#subtyping
// https://en.wikipedia.org/wiki/Liskov_substitution_principle
trait Decoder[+A]:
  /**
    * @param data The data to de-serialize
    * @return The decoded value wrapped in `Some`, or `None` if decoding failed
    */
  // this defines the role of the decoder, to turn an instance of Json data type to an Option
  // instance of Scala data type
  def decode(data: Json): Option[A]

  /**
    * Combines `this` decoder with `that` decoder.
    * Returns a decoder that invokes both `this` decoder and `that`
    * decoder and returns a pair of decoded value in case both succeed,
    * or `None` if at least one failed.
    */
  def zip[B](that: Decoder[B]): Decoder[(A, B)] =
    Decoder.fromFunction { json =>
      this.decode(json).zip(that.decode(json))
    }

  /**
    * Transforms this `Decoder[A]` into a `Decoder[B]`, given a transformation function
    * from `A` to `B`.
    *
    * This operation is also known as “map”.
    */
  def transform[B](f: A => B): Decoder[B] =
    Decoder.fromFunction(json => this.decode(json).map(f))

end Decoder

object Decoder extends DecoderInstances:

  /**
    * Convenient method to build a decoder instance from a function `f`
    */
  // This is the standard Decoder construction method to create a Decoder
  // by taking its respective defining decoding higher order function as an argument
  def fromFunction[A](f: Json => Option[A]): Decoder[A] = new Decoder[A] {
    def decode(data: Json): Option[A] = f(data)
  }

  /**
    * Alternative method for creating decoder instances
    */
  // A partial function is a unary higher-order function A => B whose domain
  // does not necessarily cover the all the values of type A
  // lift() is a convinient method to turn a partial function A => B to a its regular 
  // higher-order-function dual A => Option[B], depending on whether the input value x is defined
  // on the domain of the partial function
  def fromPartialFunction[A](pf: PartialFunction[Json, A]): Decoder[A] =
    fromFunction(pf.lift)

  def fromPartialFunction2[A](pf: PartialFunction[Json, Option[A]]): Decoder[A] = new Decoder[A] {
    def decode(data: Json): Option[A] = pf.lift(data).getOrElse(None)
  }

  def field2[A](name: String)(using decoder: Decoder[A]): Decoder[A] =
  Decoder.fromPartialFunction2 {
    case Json.Obj(e) => e.get(name).flatMap(e => decoder.decode(e))
  }

end Decoder

trait DecoderInstances:

  /** A decoder for the `Unit` value */
  given unitDecoder: Decoder[Unit] =
    Decoder.fromPartialFunction({ case Json.Null => () })

  /** A decoder for `Int` values. Hint: use the `isValidInt` method of `BigDecimal`. */
  given intDecoder: Decoder[Int] =
    Decoder.fromPartialFunction {
      case Json.Num(n: BigDecimal) if n.isValidInt => n.toInt
    }

  /** A decoder for `String` values */
  given strDecoder: Decoder[String] = 
    Decoder.fromPartialFunction {
      case Json.Str(s: String) => s
    }

  /** A decoder for `Boolean` values */
  given booleanDecoder: Decoder[Boolean] = 
    Decoder.fromPartialFunction {
      case Json.Bool(b: Boolean) => b
    }

  /**
    * A decoder for JSON arrays. It decodes each item of the array
    * using the given `decoder`. The resulting decoder succeeds only
    * if all the JSON array items are successfully decoded.
    */
  given listDecoder[A](using decoder: Decoder[A]): Decoder[List[A]] =
    
    // Decode the provided `item` with the provided `decoder`. If this succeeds,
    // return the decoded item prepended to the `previouslyDecodedItems`.
    def decodeAndPrepend(item: Json, previouslyDecodedItems: List[A]): Option[List[A]] =
      // would-be equivalent
      // decoder.decode(item) match

      item.decodeAs[A].map(decodedItem => decodedItem +: previouslyDecodedItems)
      
      // would-be equivalent
      // item.decodeAs[A] match
      //   case Some(decodedItem) => Some(decodedItem +: previouslyDecodedItems)
      //   case None => None
    
    // Decode the provided `item` only if the previous items were successfully decoded.
    // In case `maybePreviouslyDecodedItems` is `None` (which means that at least
    // one of the previous items failed to be decoded), return `None`.
    // Otherwise, decode the provided `item` and prepend it to the previously
    // decoded items (use the method `decodeAndPrepend`).
    def processItem(item: Json, maybePreviouslyDecodedItems: Option[List[A]]): Option[List[A]] =
      
      maybePreviouslyDecodedItems.flatMap(previouslyDecodedItems => decodeAndPrepend(item, previouslyDecodedItems))
      
      // would-be equivalent
      // maybePreviouslyDecodedItems match
      //   case None => None
      //   case Some(_) => decodeAndPrepend(item, maybePreviouslyDecodedItems.get)
      
    // Decodes all the provided JSON items. Fails if any item fails to
    // be decoded.
    // Iterates over the items, and tries to decode each item if the
    // previous items could be successfully decoded.
    def decodeAllItems(items: List[Json]): Option[List[A]] =
      items.foldRight(Some(List.empty[A]))(processItem)
    
    // Finally, write a decoder that checks whether the JSON value to decode
    // is a JSON array.
    //   - if it is the case, call `decodeAllItems` on the array items,
    //   - otherwise, return a failure (`None`)
    Decoder.fromFunction {
      case Json.Arr(items: List[Json]) => decodeAllItems(items)
      case _ => None
    }

  /**
    * A decoder for JSON objects. It decodes the value of a field of
    * the supplied `name` using the given `decoder`.
    */
  // the field method is to create a Decoder whose defining decode method's domain
  // is to work with a Json Obj variant instance represented by an internal Map
  // and whose return is (the Option of) decoded value of the given field in the Map
  def field[A](name: String)(using decoder: Decoder[A]): Decoder[A] =
    Decoder.fromFunction{ e => e match 
      case Json.Obj(e) => e.get(name).flatMap(e => decoder.decode(e))
      case _ => None
    }

  // def field2[A](name: String)(using decoder: Decoder[A]): Decoder[A] =
  //   Decoder.fromPartialFunction2 {
  //     case Json.Obj(e) => e.get(name).flatMap(e => decoder.decode(e))
  //   }
    
end DecoderInstances

case class Person(name: String, age: Int)

object Person extends PersonCodecs

trait PersonCodecs:

  /** The encoder for `Person` */
  // Person is a case class taking two aruments name and age. We relegate the issue of encoding a Person instance
  // to encoding a two-element tuple of (String/(person.name), Int(person.age)). This regation is specifed by the "contramap"
  // tranform[Person](f:(Person => (String, Int))) call, s.t. the defining encode method of Encoder[Person] becomes
  // a encode((String, Int)) method call. 
  
  
  // The zip() method in the ObjectEncoder provides the means to create an Encoder for encoding a parameterised tuple.
  // Specifically, the ObjectEncoder field call creates a ObjectEncoder that turns an instance of Scala type A to a Json Obj variant
  // whose internal is represented by a Map. The zip call combines two ObjectEncoders to a new ObjectEncoder represented by 
  // the two interal Maps combined, s.t. the defining encode of such ObjectEncoder is a higher order function from the
  // parameterised tuple, to the Json Obbj variant containing the Map as described
  given Encoder[Person] =
    ObjectEncoder.field[String]("name")
      .zip(ObjectEncoder.field[Int]("age"))
      .transform[Person](person => (person.name, person.age))

  /** The corresponding decoder for `Person`.
    * Hint: create the decoders for the `name` and `age` JSON fields
    *       by using the method `Decoder.field`
    * Hint: combine the decoders by using their methods `zip` and
    *       `transform`.
    */
  given Decoder[Person] =
    // the application of zip call in this case of two Decoders from field calls results in a Decoder
    // whose defining decode methods's domain is to work with a Json Obj variant and whose return is
    // (the Options of) decoded values from the two fields zipped
    Decoder.field2[String]("name")
           .zip(Decoder.field2[Int]("age"))
           .transform[Person]((name, age) => Person(name, age))

end PersonCodecs

case class Contacts(people: List[Person])

object Contacts extends ContactsCodecs

trait ContactsCodecs:

  // Relegate the Contacts encoder to an encoder instance of List[Person] with the transform call
  // Provide the encoder of List[Person] to variant Json.Obj with the field call
  // The internal map of Json.Obj has a key of "people" and a value of Json.Arr variant
  given Encoder[Contacts] = 
    ObjectEncoder.field[List[Person]]("people")
                 .transform[Contacts]((contact: Contacts) => (contact.people: List[Person]))

  given Decoder[Contacts] = 
    Decoder.field2[List[Person]]("people")
           .transform[Contacts](peopeoList => Contacts(peopeoList))

end ContactsCodecs

// In case you want to try your code, here is a simple `Main`
// that can be used as a starting point. Otherwise, you can use
// the REPL (use the `console` sbt task).

// "importing a package" is imprecise since import statement bring specified members of the package into scope
// in this case, we are specifically bring the Util object into the namespace s.t. the method calls are short
import codecs.Util.*

@main def run(): Unit =
  println(renderJson(())) // () works but Unit doesn't
  println(renderJson(4.2))
  println(renderJson(42))
  println(renderJson("foo"))
  println(renderJson(Person("Bob", 66)))
  println(renderJson(Contacts(people = List(Person("Bob", 66), Person("Alice", 42)))))
  // println(renderJson(Vector(Person("Bob", 66), Person("Alice", 42))))

  println(renderJson3(Json3.obj("name" -> JString("Paul"), "age" -> JNumber(42))))
  // The following is an example of the provision of more ergonomic API to construct a Json3.JObject AST instance
  // With the introduction of JObjectFieldValue into the Json3.obj2 utility method for Json3.JObject creation
  // the additional implicit coversion definitions allow for the input types liek String and Int to be automatically
  // converted to JObjectFieldValue type expected by the Json3.obj2 method by the compiler
  import scala.language.implicitConversions
  println(renderJson3(Json3.obj2("name" -> "Paul", "age" -> 42)))

  // val maybeJsonString = parseJson(""" "foo" """)
  // val maybeJsonObj    = parseJson(""" { "name": "Alice", "age": 42 } """)
  // val maybeJsonObj2   = parseJson(""" { "name": "Alice", "age": "42" } """)
  // Uncomment the following lines as you progress in the assignment
  // println(maybeJsonString.flatMap(_.decodeAs[Int]))
  // println(maybeJsonString.flatMap(_.decodeAs[String]))
  // println(maybeJsonObj.flatMap(_.decodeAs[Person]))
  // println(maybeJsonObj2.flatMap(_.decodeAs[Person]))


```



#### Error stacktrace:

```
scala.runtime.Scala3RunTime$.assertFailed(Scala3RunTime.scala:8)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:175)
	dotty.tools.dotc.ast.Positioned.check$1$$anonfun$3(Positioned.scala:205)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:205)
	dotty.tools.dotc.ast.Positioned.checkPos(Positioned.scala:226)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:200)
	dotty.tools.dotc.ast.Positioned.checkPos(Positioned.scala:226)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:200)
	dotty.tools.dotc.ast.Positioned.check$1$$anonfun$3(Positioned.scala:205)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:205)
	dotty.tools.dotc.ast.Positioned.checkPos(Positioned.scala:226)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:200)
	dotty.tools.dotc.ast.Positioned.checkPos(Positioned.scala:226)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:200)
	dotty.tools.dotc.ast.Positioned.check$1$$anonfun$3(Positioned.scala:205)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:205)
	dotty.tools.dotc.ast.Positioned.checkPos(Positioned.scala:226)
	dotty.tools.dotc.parsing.Parser.parse$$anonfun$1(ParserPhase.scala:38)
	dotty.tools.dotc.parsing.Parser.parse$$anonfun$adapted$1(ParserPhase.scala:39)
	scala.Function0.apply$mcV$sp(Function0.scala:42)
	dotty.tools.dotc.core.Phases$Phase.monitor(Phases.scala:437)
	dotty.tools.dotc.parsing.Parser.parse(ParserPhase.scala:39)
	dotty.tools.dotc.parsing.Parser.runOn$$anonfun$1(ParserPhase.scala:48)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.parsing.Parser.runOn(ParserPhase.scala:48)
	dotty.tools.dotc.Run.runPhases$1$$anonfun$1(Run.scala:247)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.ArrayOps$.foreach$extension(ArrayOps.scala:1321)
	dotty.tools.dotc.Run.runPhases$1(Run.scala:263)
	dotty.tools.dotc.Run.compileUnits$$anonfun$1(Run.scala:271)
	dotty.tools.dotc.Run.compileUnits$$anonfun$adapted$1(Run.scala:280)
	dotty.tools.dotc.util.Stats$.maybeMonitored(Stats.scala:67)
	dotty.tools.dotc.Run.compileUnits(Run.scala:280)
	dotty.tools.dotc.Run.compileSources(Run.scala:195)
	dotty.tools.dotc.interactive.InteractiveDriver.run(InteractiveDriver.scala:165)
	scala.meta.internal.pc.MetalsDriver.run(MetalsDriver.scala:45)
	scala.meta.internal.pc.PcCollector.<init>(PcCollector.scala:44)
	scala.meta.internal.pc.PcSemanticTokensProvider$Collector$.<init>(PcSemanticTokensProvider.scala:61)
	scala.meta.internal.pc.PcSemanticTokensProvider.Collector$lzyINIT1(PcSemanticTokensProvider.scala:61)
	scala.meta.internal.pc.PcSemanticTokensProvider.Collector(PcSemanticTokensProvider.scala:61)
	scala.meta.internal.pc.PcSemanticTokensProvider.provide(PcSemanticTokensProvider.scala:90)
	scala.meta.internal.pc.ScalaPresentationCompiler.semanticTokens$$anonfun$1(ScalaPresentationCompiler.scala:109)
```
#### Short summary: 

java.lang.AssertionError: assertion failed: position error, parent span does not contain child span
parent      = [A](using protected given val codecA: Json2Codec[A]) extends Json2Codec[
  Vector[A]] {
  def encode(value: Vector[A]): Json2 =
    Json2Arr(value.map(element => codecA.encode(element)))
  def decode(json: Json2): Vector[A] = null
} # -1,
parent span = <5594..5791>,
child       = def decode(json: Json2): Vector[A] = null # -1,
child span  = [5755..5759..6844]