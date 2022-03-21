package experiment

import cats.Show
import cats.implicits._
import smithy4s._
import smithy4s.schema.{Schema, _}

trait ShowInstances {
  implicit val sId: Show[ShapeId] = new Show[ShapeId]() {
    def show(s: ShapeId): String = s"${s.namespace}#${s.name}"
  }
  implicit val byteArray: Show[ByteArray] = new Show[ByteArray]() {
    def show(s: ByteArray): String = s"ByteArray(length=${s.array.length})"
  }
  implicit val document: Show[Document] = new Show[Document]() {
    def show(s: Document): String = s.show
  }
  implicit val ts: Show[Timestamp] = new Show[Timestamp]() {
    def show(s: Timestamp): String = s.epochSecond.toString()
  }
}
object ShowInstances extends ShowInstances

object Do extends App {
  import ShowInstances._

  final case class ThisIsAnAttempt(
      st: String,
      i: Int,
      either: Either[String, Int],
      list: List[ThisIsAnAttempt]
  )

  val sv: SchemaVisitor[Show] = new SchemaVisitor[Show]() {
    private val primShowPf = Primitive.deriving[Show]
    override def primitive[P](
        shapeId: ShapeId,
        hints: Hints,
        tag: Primitive[P]
    ): Show[P] = Show.show { p =>
      val valueShow = primShowPf(tag).show(p)
      s"Primitive { value = $valueShow }"
    }

    override def list[A](
        shapeId: ShapeId,
        hints: Hints,
        member: Schema[A]
    ): Show[List[A]] = {
      val showSchemaA = sv(member)
      Show.show { u =>
        val values = u.map(a => showSchemaA.show(a)).mkString("[", ", ", "]")
        s"List { values = $values }"
      }
    }

    override def set[A](
        shapeId: ShapeId,
        hints: Hints,
        member: Schema[A]
    ): Show[Set[A]] = Show.show { u =>
      val showSchemaA = sv(member)
      val values = u.map(a => showSchemaA.show(a)).mkString("[", ", ", "]")
      s"Set { values = $values }"
    }

    override def map[K, V](
        shapeId: ShapeId,
        hints: Hints,
        key: Schema[K],
        value: Schema[V]
    ): Show[Map[K, V]] = Show.show { m =>
      val showKey = sv(key)
      val showValue = sv(value)
      val values = m
        .map { case (k, v) => s"${showKey.show(k)} -> ${showValue.show(v)}" }
        .mkString("[", ", ", "]")
      s"Map { values = $values }"
    }

    override def enumeration[E](
        shapeId: ShapeId,
        hints: Hints,
        values: List[EnumValue[E]],
        total: E => EnumValue[E]
    ): Show[E] = Show.show { e =>
      val value = total(e)
      s"Enum { value = ${value.stringValue} }"
    }

    override def struct[S](
        shapeId: ShapeId,
        hints: Hints,
        fields: Vector[SchemaField[S, _]],
        make: IndexedSeq[Any] => S
    ): Show[S] = {
      def compileField[A](schemaField: SchemaField[S, A]): S => String = {
        val folder = new Field.FolderK[Schema, S, Show]() {
          override def onRequired[AA](
              label: String,
              instance: Schema[AA],
              get: S => AA
          ): Show[AA] = sv(instance)
          override def onOptional[AA](
              label: String,
              instance: Schema[AA],
              get: S => Option[AA]
          ): Show[Option[AA]] = Show.show { opt =>
            opt match {
              case None =>
                s"None"
              case Some(value) =>
                val show = sv(instance)
                s"Optional { ${show.show(value)} }"
            }
          }
        }
        val showField = schemaField.foldK(folder)
        s => showField.show(schemaField.get(s))
      }
      val functions = fields.map { f => compileField(f) }
      Show.show { s =>
        val values = functions
          .map(f => f(s))
          .mkString("[", ", ", "]")
        s"Struct { shapeId = ${sId.show(shapeId)}, fields = $values }"
      }
    }

    override def union[U](
        shapeId: ShapeId,
        hints: Hints,
        alternatives: Vector[SchemaAlt[U, _]],
        dispatch: U => Alt.SchemaAndValue[U, _]
    ): Show[U] = {
      val compileAlt: SchemaAlt[U, *] ~> Show = {
        new (SchemaAlt[U, *] ~> Show) {
          def apply[A](fa: Alt[Schema, U, A]): Show[A] =
            fa.instance.compile(sv)
        }
      }

      val precomputed =
        compileAlt.unsafeCache(alternatives.map(Existential.wrap(_)))
      def showIt[A](altV: Alt.SchemaAndValue[U, A]): String =
        precomputed(altV.alt).show(altV.value)

      Show.show { u =>
        val withV = dispatch(u)
        val value = showIt(withV)
        s"Union { shapeId = ${sId.show(shapeId)}, value = $value }"
      }
    }

    override def biject[A, B](
        schema: Schema[A],
        to: A => B,
        from: B => A
    ): Show[B] = Show.show { b =>
      sv(schema).show(from(b))
    }

    override def lazily[A](suspend: Lazy[Schema[A]]): Show[A] = Show.show[A] {
      val ss = suspend.map { sv(_) }
      a => s"Lazy = ${ss.value.show(a)}"
    }

  }

  // #region running
  // this could have a value in the main branch as `Schema.either`
  implicit val either: smithy4s.Schema[Either[String, Int]] = {
    val lSchema: smithy4s.Schema[Left[String, Int]] =
      Schema.bijection(Schema.string, Left(_: String), _.value)

    val rSchema: smithy4s.Schema[Right[String, Int]] =
      Schema.bijection(Schema.int, Right(_: Int), _.value)

    val l = lSchema.oneOf[Either[String, Int]]("left")
    val r = rSchema.oneOf[Either[String, Int]]("right")
    Schema
      .union(l, r) {
        case lv: Left[String, Int]  => l(lv)
        case ri: Right[String, Int] => r(ri)
      }
      .withId(ShapeId("smithy4s.api", "Either"))
      .addHints(smithy4s.Hints.empty)
  }

  implicit val thisIsAnAttemptSchema: Schema[ThisIsAnAttempt] = Schema
    .struct(
      Schema.string
        .required[ThisIsAnAttempt]("st", _.st),
      Schema.int
        .required[ThisIsAnAttempt]("i", _.i),
      either.required[ThisIsAnAttempt]("either", _.either),
      Schema
        .list(Schema.LazySchema(Lazy(thisIsAnAttemptSchema)))
        .required[ThisIsAnAttempt]("list", _.list)
    ) {
      ThisIsAnAttempt.apply
    }
    .withId(ShapeId("my.namespace", "ThisIsAnAttempt"))
  val value = ThisIsAnAttempt(
    "hey",
    42,
    Right(42 * 2),
    List(ThisIsAnAttempt("oh no", 1, Left("yes"), Nil))
  )

  println(
    thisIsAnAttemptSchema
      .compile[Show](sv)
      .show(value)
  )

  // #endregion
}
