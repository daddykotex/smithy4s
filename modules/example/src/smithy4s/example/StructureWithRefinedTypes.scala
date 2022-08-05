package smithy4s.example

import smithy4s.schema.Schema._

case class StructureWithRefinedTypes(age: Option[Age] = None, personAge: Option[PersonAge] = None, fancyList: Option[FancyList] = None, unwrappedFancyList: Option[smithy4s.example.refined.FancyList] = None, name: Option[Name] = None, dogName: Option[smithy4s.example.refined.Name] = None)
object StructureWithRefinedTypes extends smithy4s.ShapeTag.Companion[StructureWithRefinedTypes] {
  val id: smithy4s.ShapeId = smithy4s.ShapeId("smithy4s.example", "StructureWithRefinedTypes")

  val hints : smithy4s.Hints = smithy4s.Hints.empty

  implicit val schema: smithy4s.Schema[StructureWithRefinedTypes] = struct(
    Age.schema.optional[StructureWithRefinedTypes]("age", _.age),
    PersonAge.schema.optional[StructureWithRefinedTypes]("personAge", _.personAge),
    FancyList.schema.optional[StructureWithRefinedTypes]("fancyList", _.fancyList),
    UnwrappedFancyList.underlyingSchema.optional[StructureWithRefinedTypes]("unwrappedFancyList", _.unwrappedFancyList),
    Name.schema.optional[StructureWithRefinedTypes]("name", _.name),
    DogName.underlyingSchema.optional[StructureWithRefinedTypes]("dogName", _.dogName),
  ){
    StructureWithRefinedTypes.apply
  }.withId(id).addHints(hints)
}