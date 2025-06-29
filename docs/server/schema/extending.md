---
title: Extending schemas
---
The AST in gql is subject to extension.
In particular, the schema can be used to write arbitary information that can later be used for various purposes.

Integrations that use schema extensions are the [goi](../integrations/goi) and [relational](../integrations/relational) integrations.

Lets get some imports ready before we start.
```scala mdoc
import gql._
import gql.dsl.all._
import gql.ast._
import gql.resolver._
import cats.effect._
import cats._
import cats.data._
import cats.implicits._
```

For this showcase, our goal will be to add authorization to any schema.

To extend the schema with new attributes we must define what attribute we wish to embed into the schema.
We can extend a special trait based on the ast node we wish to extend.
```scala mdoc
case class AuthorizedField(
  permissions: List[String]
) extends FieldAttribute[fs2.Pure]
```

Lets also introduce some functions related to authorization.
```scala mdoc
def checkPermissions(token: String, permissions: List[String]): IO[Boolean] = ???
```

Now we will use our new attribute to create a dsl for out extension.
```scala mdoc
def authed[A, B](perms: String*)(field: Field[IO, A, B]): Field[IO, A, B] = {
  val permissions = perms.toList
  field
    .addAttributes(AuthorizedField(permissions))
    .compose(Resolver.id[IO, A].arg(arg[String]("secretToken")).evalMap{ case (token, a) =>
      checkPermissions(token, permissions).map{
        case false => s"your token didn't satisfy the permissions ${permissions.mkString(", ")}".leftIor
        case true => a.rightIor
      }
    }.rethrow)
    .document(s"Requires permissions ${permissions.mkString(", ")}")
}
```

We can now use our authorization function.
```scala mdoc
case class Person(name: String, age: Int)
implicit lazy val person: Type[IO, Person] = tpe[IO, Person](
  "Person",
  "name" -> authed("read:name") {
    lift(_.name)
  },
  "age" -> lift(_.name),
  "name2" -> authed("read:name", "read:name2") {
    authed("read:name") {
      lift(_.age)
    }
  },
)
```
Now notice two things:
  1. We forgot to add authorization to the `age` field.
  2. We added authorization twice to the `name2` field by mistake.

We will catch both of these errors by validating our schema.
```scala mdoc
sealed trait Error
object Error {
  case class MultiplePermissionLists(field: String, perms: List[List[String]]) extends Error {
    override def toString = 
      s"Field '$field' has multiple permission lists: ${perms.map(ps => s"{${ps.mkString(",")}}").mkString(", ")}"
  }
  case class MissingPermission(field: String) extends Error {
    override def toString = s"Field '$field' is missing a permission list"
  }
}

def validate(schema: SchemaShape[IO, ?, ?, ?]): Chain[Error] = {
  import SchemaShape._
  import VisitNode._
  val fa = schema.visitOnce[Eval, Chain[Error]]{
    case FieldNode(name, f: Field[IO, ?, ?]) =>
      Some {
          Eval.now {
            f.attributes.collect{ case a: AuthorizedField => a } match {
              case Nil => Chain(Error.MissingPermission(name))
              case a :: Nil => Chain.empty
              case ys => Chain(Error.MultiplePermissionLists(name, ys.map(_.permissions)))
            }
          }
      }
  }

  fa.value
}
```

Lets see what happens when we validate our schema.
```scala mdoc
lazy val s = SchemaShape.unit[IO](
  fields[IO, Unit](
    "person" -> lift(_ => Person("John", 42))
  )
)

validate(s).toList.foreach(println)
```

Notice that the errors we expected were caught by our validation.
