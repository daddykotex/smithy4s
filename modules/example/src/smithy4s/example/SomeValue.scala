package smithy4s.example

import smithy4s.Newtype
import smithy4s.syntax._

object SomeValue extends Newtype[String] {
  val id: smithy4s.ShapeId = smithy4s.ShapeId("smithy4s.example", "SomeValue")
  val hints : smithy4s.Hints = smithy4s.Hints(
    id,
  )
  val underlyingSchema : smithy4s.Schema[String] = string.withHints(hints)
  val schema : smithy4s.Schema[SomeValue] = bijection(underlyingSchema, SomeValue(_), (_ : SomeValue).value)
  implicit val staticSchema : schematic.Static[smithy4s.Schema[SomeValue]] = schematic.Static(schema)
}