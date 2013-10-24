package scalaio

import shapeless._, poly._

object list extends (Id ~> List) {
  def apply[T](t : T) = List(t)
}

object headOption extends (List ~> Option) {
  def apply[T](l : List[T]) = l.headOption
}

object plus extends Poly2 {
  implicit val caseInt = at[Int, Int](_ + _)
}

object inc extends ->((i: Int) => i+1)

object hinc extends Poly1 {
  implicit val caseInt = at[Int](_+1)
  implicit val caseString = at[String](_+"*")
  implicit val caseBoolean = at[Boolean](!_)
}

object TupleDemo {

  import syntax.std.tuple._

  val t = (23, "foo", true)

  // Heads and tails
  t.head
  t.tail

  t.last
  t.init

  // : t t.tail.tail.tail ... Unit

  // Come back to indexing/updating/splitting

  // Append, prepend, concat
  1.0 +: t
  t :+ 1.0

  val u = (1.0, 'a')
  t ++ u

  () ++ t == t  // tuples are monoids wrt Unit and ++
  t ++ () == t

  // map, flatMap
  val t1 = t map list
  t1 map headOption

  val v = ((23, "foo"), (), (1.0, true, 'c'))
  v flatMap identity

  val w = (1, 2, 3, 4, 5)
  w.foldLeft(0)(plus)

  // zip, unzip, transpose
  val x = ((23, "foo"), (true, 1.0), ('c', 13))
  val x1 = x.zip
  val x2 = x1.unzip

  x.transpose // .transpose.transpose

  val v0 = (1.0, 0.0, 0.0)
  val v1 = (2.0, 3.0, 0.0)
  val v2 = (4.0, 5.0, 6.0)
  val m = (v0, v1, v2)
  m.transpose

  // toList
  v0.toList

  // productElements
  t.productElements
}

object GenericDemo {

  case class Address(street: String, number: Int, city: String, country: String)
  case class Person(name: String, age: Int, address: Address)

  val suesAddress = Address("North Street", 23, "Brighton", "UK")
  val sue = Person("Sue", 32, suesAddress)

  val pgen = Generic[Person]
  val agen = Generic[Address]

  // to
  agen.to(suesAddress)
  pgen.to(sue)

  // from
  val johnsAddressl = "South Street" :: 13 :: "Bristol" :: "UK" :: HNil
  val johnsAddress = agen.from(johnsAddressl)
  val johnl = "John" :: 31 :: johnsAddress :: HNil
  val john = pgen.from(johnl)

  // low level slicing and dicing
  val mtownl = agen.to(pgen.to(sue).tail.tail.head).drop(2)
  val jstreetl = agen.to(pgen.to(john).tail.tail.head).take(2)
  val fixedl = jstreetl ++ mtownl
  val fixed = agen.from(fixedl)

  // Lenses
  val streetLens = Lens[Person] >> 2 >> 0
  streetLens.get(john)
  streetLens.set(john)("East Street")

  // Zipper
  import Zipper._

  val jz = john.toZipper
  jz.right.right.down.put("West Street").root.reify

  // Coproducts, Scrap Your Boilerplate, recursion

  sealed trait Tree[T]
  case class Leaf[T](t: T) extends Tree[T]
  case class Node[T](left: Tree[T], right: Tree[T], name: String = "Foo") extends Tree[T]

  Generic[Leaf[Int]]
  Generic[Node[Int]]
  Generic[Tree[Int]] // Abstract, recursive

  // Compare: product
  val isb = 23 :: "foo" :: true :: HNil
  isb map hinc
  everywhere(hinc)(isb)

  // Compare: coproduct/sum
  type ISB = Int :+: String :+: Boolean :+: CNil
  val i = Coproduct[ISB](23)
  val s = Coproduct[ISB]("foo")
  val b = Coproduct[ISB](true)

  i map hinc
  s map hinc
  b map hinc

  everywhere(hinc)(i)
  everywhere(hinc)(s)
  everywhere(hinc)(b)

  // Sum of products ... with recursion!
  val tree: Tree[Int] =
    Node(
      Node(
        Node(
          Leaf(1),
          Node(
            Leaf(2),
            Leaf(3)
          )
        ),
        Leaf(4)
      ),
      Node(
        Leaf(5),
        Leaf(6)
      )
    )

  everywhere(inc)(tree)
}

object SingletonDemo {

  val foo = "foo"
  val foo2 : foo.type = foo // nb. not "foo"

  // No 23.type/"foo".type/true.type

  import syntax.singleton._

  23.narrow
  "foo".narrow
  true.narrow

  val wFoo = Witness("foo")
  val wBar = Witness("bar")
  val wBaz = Witness("baz")
  type Foo = wFoo.T
  type Bar = wBar.T
  type Baz = wBaz.T

  val w0 = Witness(0)
  val w1 = Witness(1)
  val w2 = Witness(2)
  type _0 = w0.T
  type _1 = w1.T
  type _2 = w2.T

  val wT = Witness(true)
  val wF = Witness(false)
  type True = wT.T
  type False = wF.T

  // Type classes indexed by values
  trait Choose[B <: Boolean, T] {
    def apply() : T
  }

  object Choose {
    implicit val chooseTrue = new Choose[True, String] {
      def apply() = "Foo"
    }

    implicit val chooseFalse = new Choose[False, Int] {
      def apply() = 23
    }
  }

  def choose[T](b: Witness.Lt[Boolean])(implicit choose: Choose[b.T, T]): T = choose()

  // HLists and tuples indexed by Int literals

  object Index {
    import syntax.std.tuple._

    val t = (23, "foo", true)
    t(1)
    t.updatedAt(1, 2.0)
    t.split(1)

    val l = 23 :: "foo" :: true :: HNil
    l(1)
    l.updatedAt(1, 2.0)
    l.split(1)   // ._1, ._2 vs. resX(0), resX(1)
  }

  // Records with singleton literal keys

  import record._, syntax.singleton._

  val field = "foo" -> 23
  val sfield = "foo" ->> 23

  def value[K, V](f: FieldType[K, V]): V = f
  def key[K, V](f: FieldType[K, V])(implicit wKey: Witness.Aux[K]): K = wKey.value

  val book =
    ("author" ->> "Benjamin Pierce") ::
    ("title"  ->> "Types and Programming Languages") ::
    ("id"     ->>  262162091) ::
    ("price"  ->>  44.99) ::
    HNil

  book("author")
  book("price")
  val book1 = book + ("price" ->> 39.99)
  book("price")

  book.values // .tupled
  book.keys   // .tupled
}

object IllTypedDemo {
  import test.illTyped

  //illTyped(""" 23 : Int """)
  illTyped(""" 23 : String """)

  //val liar : Unit = illTyped(""" liar """)

}


// Sized take and drop
