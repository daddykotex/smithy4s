# Exercise

The code in this module is the result of an exercise to understand to important component of `smithy4s`:

- `Schema[A]`
- `Schematic`

The exercise is to write a natural transformation `Schema ~> Show` so that we can turn a `Schema[A]` into a `Show[A]` Given the natural transformation (`nt: Schema ~> Show`) and an `instance: Schema[A]` we can call `instance.compile(nt).show(aValue)` to produce a `String`.

The goal being to experiment and explore different implementation for such an implementation.

## the visitor pattern

The smithy4s library provides a `SchemaVisitor` abstraction. This abstraction extends `Schema ~> Show` so if we're able to build a `SchemaVisitor`, then we'll be able to call `compile` with it.

The visitor pattern is an OOP pattern. It helps transforming a data structure. In our case, it helps us to go from a `Schema` to a `Show`. To do this, the `SchemaVisitor` defines one method per different `Schema` encoding. `Schema` is an Algebraic Data Type (ADT) and there are only a handful different encoding. Our `SchemaVisitor` implementation forces us to implement a transformation from each of them into a `Show`. Here is the exhaustive list of methods:

```scala
trait SchemaVisitor[F[_]] extends (Schema ~> F){
  def primitive[P](shapeId: ShapeId, hints: Hints, tag: Primitive[P]) : F[P]
  def list[A](shapeId: ShapeId, hints: Hints, member: Schema[A]) : F[List[A]]
  def set[A](shapeId: ShapeId, hints: Hints, member: Schema[A]): F[Set[A]]
  def map[K, V](shapeId: ShapeId, hints: Hints, key: Schema[K], value: Schema[V]): F[Map[K, V]]
  def enumeration[E](shapeId: ShapeId, hints: Hints, values: List[EnumValue[E]], total: E => EnumValue[E]) : F[E]
  def struct[S](shapeId: ShapeId, hints: Hints, fields: Vector[SchemaField[S, _]], make: IndexedSeq[Any] => S) : F[S]
  def union[U](shapeId: ShapeId, hints: Hints, alternatives: Vector[SchemaAlt[U, _]], dispatch: U => Alt.SchemaAndValue[U, _]) : F[U]
  def biject[A, B](schema: Schema[A], to: A => B, from: B => A) : F[B]
  def surject[A, B](schema: Schema[A], to: A => Either[Throwable, B], from: B  => A) : F[B]
  def lazily[A](suspend: Lazy[Schema[A]]) : F[A]
  //...
}
```

When implementing each method, there is only one trick I can give: look at all the types you've got and try to wire things together. Follow the types. Sometimes the solution is quite complex and the only reason I got it to work was by looking at what I had and try to connect things together to produce a `Show`,

For example, lets first look at a simple one and then we'll try a more complex one. The simple one could be `list`:

```scala
val sv: SchemaVisitor[Show] = new SchemaVisitor[Show]() {
  //...
  def list[A](shapeId: ShapeId, hints: Hints, member: Schema[A]) : Show[List[A]]
  //...
}
```

We want to produce a `Show[List[A]]` given those 3 parameters. Let's try to create that `Show` instance:

```scala
val sv: SchemaVisitor[Show] = new SchemaVisitor[Show]() {
  //...
  def list[A](shapeId: ShapeId, hints: Hints, member: Schema[A]): Show[List[A]] =
    new Show[List[A]]() {
      def show(value: List[A]): String = ???
    }
  //...
}
```

We could be lazy and do something like `value.map(_.toString).mkString(", ")` and call it a day! But the goal is to provide some relevant information regarding the schema of the values in the list. So how can we achieve that? We have the `List[A]`, we have a `Schema[A]` and the usual `shapeId` and `hints`. Let's not forget the `SchemaVisitor[Show]` that we are currently implementing. So, we'd like to have a relevant `Show` instance for our `A`, and then call it for every value in the `List`. If we take a minute to look at the `SchemaVisitor`, we'll realize that behind the curtains, it implements a method `Schema[A] => Show[A]` and we have a `Schema[A]`. Let's get that:

```scala
val sv: SchemaVisitor[Show] = new SchemaVisitor[Show]() {
  //...
  def list[A](shapeId: ShapeId, hints: Hints, member: Schema[A]): Show[List[A]] =
    new Show[List[A]]() {
      val showForA: Show[A] = sv(member)
      def show(value: List[A]): String = ???
    }
  //...
}
```

Once we have that show, we can turn the values in the list into `String` and finish our `Show` implementation:

```scala
val sv: SchemaVisitor[Show] = new SchemaVisitor[Show]() {
  //...
  def list[A](shapeId: ShapeId, hints: Hints, member: Schema[A]): Show[List[A]] =
    new Show[List[A]]() {
      val showForA: Show[A] = sv(member)
      def show(values: List[A]): String = {
        val valuesStr = value.map(showForA.show(_))).mkString(", ")
        s"List { values = [$valuesStr] }"
      }
    }
  //...
}
```

It's a simple one, yet there is quite a bit of complexity. We're dealing with typeclass, generic types, recursive calls just to produce a `String`. But rest assure, this kind of pattern is usually the way to go when implementing such things.

Now let's look at a more complex one, the struct method:

```scala
val sv: SchemaVisitor[Show] = new SchemaVisitor[Show]() {
  //...
  def struct[S](shapeId: ShapeId, hints: Hints, fields: Vector[SchemaField[S, _]], make: IndexedSeq[Any] => S) : F[S]
  //...
}
```

As you can see, there's again more complexity. We have our usual rather useless `shapeId` and `hints`. But we also have a list of fields. Because in essence, a struct is a product type. It's a type type that contains multiple members of potentially multiple types. In Scala, product type are usually modelled with case class. For example: `Person(name: String, age: Int)` is a product type.

How would we implement a `Schema[Person] => Show[Person]`? Well, let's find out. As you can probably guess, the important thing for us here is the `fields` parameter. It contains a sequence of all the fields of our product type. Let's look at `SchemaField` to see what it offers:

```scala
// first a type alias
type SchemaField[S, A] = Field[Schema, S, A]
// so SchemaField[S, _] is Field[Schema, S, A] and a Field is
sealed abstract class Field[F[_], S, A] {
  type T
  def label: String
  def get: S => A
  def instance: F[T]
  def isRequired: Boolean
  def isOptional: Boolean = !isRequired
  def hints: Hints

  def mapK[G[_]](f: F ~> G): Field[G, S, A]
  def transformHintsLocally(f: Hints => Hints): Field[F, S, A]
  def addHints(hints: Hint*): Field[F, S, A] = ???
  def instanceA(onOptional: Field.ToOptional[F]): F[A]
  def fold[B](folder: Field.Folder[F, S, B]): B
  def foldK[G[_]](folder: Field.FolderK[F, S, G]): G[A]
  def leftFolder[B](folder: Field.LeftFolder[F, B]): (B, S) => B
  def foreachT(s: S)(f: T => Unit): Unit
  final def foreachA(s: S)(f: A => Unit): Unit = ???
}

object Field {
  trait FolderK[F[_], S, G[_]]{
    def onRequired[A](label: String, instance: F[A], get: S => A): G[A]
    def onOptional[A](label: String, instance: F[A], get: S => Option[A]): G[Option[A]]
  }

  trait Folder[F[_], S, B] {
    def onRequired[A](label: String, instance: F[A], get: S => A): B
    def onOptional[A](label: String, instance: F[A], get: S => Option[A]): B
  }
}
```

So... we have a sequence of `Field[Schema, S, _]`, and what we ultimately need is a `Show` instance for all of those fields. To produce a a `Show[S]`, we need to grab the `Show[_]` instance for every field. The underscore in this type definition means that any of those schema field may have a different type. The same way our `Person` type class has a `String` member and an `Int` member. For example, for our dummy `Person` product type, we would have the following fields: `Vector(Field[Schema, Person, String], Field[Schema, Person, Int])`. One instance for each member and they have different type.

There are two tricky parts here. The first one is that `Field` is an abstract class, and the implementations are private so we can't even pattern match on them. The second one is the compiler can have issues with existential type. It will have trouble compiling even if you're code is right. But nevertheless, we can do it.

Same start as earlier:

```scala
val sv: SchemaVisitor[Show] = new SchemaVisitor[Show]() {
  //...
  def struct[S](shapeId: ShapeId, hints: Hints, fields: Vector[SchemaField[S, _]], make: IndexedSeq[Any] => S): Show[S] =
    new Show[S]() {
      def show(value: S): String = ???
    }
  //...
}
```

So what can we do with a `S` and a sequence of all the fields in our structure. Well we can see that `Field` has a `get` method to turn the `S` into a `A`. Let's iterate on the fields, get to the `Field` instance and try to get a `Show` intance for each of them. It's similar to what we did `list` above, but we have extra machinery to work with because each field may have a different type. My first instinct was to do something like this:

```scala
val sv: SchemaVisitor[Show] = new SchemaVisitor[Show]() {
  //...
  def struct[S](shapeId: ShapeId, hints: Hints, fields: Vector[SchemaField[S, _]], make: IndexedSeq[Any] => S): Show[S] =
    new Show[S]() {
      def show(value: S): String = {
        fields.map { field =>
          val showField = sv(field.instance)
        }
        ???
      }
    }
  //...
}
```

But what is the type of `showField` here? We have a `Schema[T]` under `instance`, so we can call `sv` with it and get a `Show[T]`. This is great, but how do we actually get a `T` then? We can't, there is nothing in scope that can provide us with a `T`, we only know that `T` is used in `instance: Schema[T]`, but that's it. We have to find another way.

In the `Field` definition above, we have other references to `instance` but they have a different type: `F[A]` as opposed to this _existential type_ `T`. On each field, there are are `fold` and `foldK` functions that take respectively a `Folder` and `FolderK`. These two traits would give us access to `F[A]` that we could use to get a `Show[A]` (if we align our types correctly). Should we use `fold` or `foldK` then? Well, because we want to go from `Schema` to `Show` or in this case `F` to `G`, then let's try with `foldK`:

```scala
val sv: SchemaVisitor[Show] = new SchemaVisitor[Show]() {
  //...
  def struct[S](shapeId: ShapeId, hints: Hints, fields: Vector[SchemaField[S, _]], make: IndexedSeq[Any] => S): Show[S] =
    new Show[S]() {
      def show(value: S): String = {
        fields.map { field =>
          val showField = field.foldK(new Field.FolderK[Schema, S, Show]() {
            override def onRequired[A](label: String, instance: Schema[A], get: S => A): Show[A] = sv(instance)
            override def onOptional[A](label: String, instance: Schema[A], get: S => Option[A]): Show[Option[A]] = {
              val showA = sv(instance)
              Show.show {
                case None => "None"
                case Some(value) =>
                  s"Optional { value = ${showA.show(value)} }"
              }
            }
          })
        }
        ???
      }
    }
  //...
}
```

In each method, we have a `Schema[A]`, that means we can get a `Show[A]` via `sv`. And `Show[A]` is what we need to return, so the implementation is easy, especially in the `onRequired` case. The `onOptional` case is a bit more tedious but should be straight forward nonetheless.

So now what we have called `foldK`, we can assume that `showField` is a valid `Show[A]`. With that, we should be able to call the `show` method on each instance with `A` we retrieve from calling `field.get`:

```scala
val sv: SchemaVisitor[Show] = new SchemaVisitor[Show]() {
  //...
  def struct[S](shapeId: ShapeId, hints: Hints, fields: Vector[SchemaField[S, _]], make: IndexedSeq[Any] => S): Show[S] =
    new Show[S]() {
      def show(value: S): String = {
        val values = fields.map { field =>
          val showField = field.foldK(new Field.FolderK[Schema, S, Show]() {
            override def onRequired[A](label: String, instance: Schema[A], get: S => A): Show[A] = sv(instance)
            override def onOptional[A](label: String, instance: Schema[A], get: S => Option[A]): Show[Option[A]] = {
              val showA = sv(instance)
              Show.show {
                case None => "None"
                case Some(value) =>
                  s"Optional { value = ${showA.show(value)} }"
              }
            }
          })
          showField.show(field.get(value))
        }
        s"Struct { values = ${values.mkString(", ")} }"
      }
    }
  //...
}
```

This is it! It works! Well, it depends, because it may not work for some scala version because of the trickyness around existential type. To help the compiler, we'll extract all the logic for a `Field` inside a method where the last type parameter is fixed. This will help the compiler follow along:

```scala
val sv: SchemaVisitor[Show] = new SchemaVisitor[Show]() {
  //...
  def struct[S](shapeId: ShapeId, hints: Hints, fields: Vector[SchemaField[S, _]], make: IndexedSeq[Any] => S): Show[S] =
    new Show[S]() {
      def show(value: S): String = {
        // we use A2 to avoid the conflict with `A` inside the foldK
        def forField[A2](field: Field[Schema, S, A2]): String = {
          val showField = field.foldK(new Field.FolderK[Schema, S, Show]() {
            override def onRequired[A](label: String, instance: Schema[A], get: S => A): Show[A] = sv(instance)
            override def onOptional[A](label: String, instance: Schema[A], get: S => Option[A]): Show[Option[A]] = {
              val showA = sv(instance)
              Show.show {
                case None => "None"
                case Some(value) =>
                  s"Optional { value = ${showA.show(value)} }"
              }
            }
          })
          showField.show(field.get(value))
        }
        val values = fields.map { field =>
          forField(field)
        }
        s"Struct { values = ${values.mkString(", ")} }"
      }
    }
  //...
}
```
