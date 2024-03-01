---
title: Structuring large applications
---
The documentation explores smaller examples.
To host larger graphs there are some considerations that must be addressed.
* What up-front work can be done to minimize the overhead in introducing new types.
* How is (mutual) recursion handled between different domains.

## Seperating domains
Partially applying all needed dependencies can be expressed with a class.
```scala mdoc
import cats.effect._
import gql._
import gql.ast._
import gql.dsl._

final case class Organization(
  id: String,
  name: String
)

final case class User(
  id: String,
  name: String,
  organizationId: String
)

trait Repo {
  def getUser(id: String): IO[User]
  def getOrganization(id: String): IO[Organization]
  def getOrganizationUsers(organizationId: String): IO[List[User]]
}

class UserTypes(repo: Repo) {
  val dsl = new GqlDsl[IO] {}
  import dsl._

  implicit val organization: Type[IO, Organization] = 
    tpe[Organization](
      "Organization",
      "id" -> lift(_.id),
      "name" -> lift(_.name),
      "users" -> eff(x => repo.getOrganizationUsers(x.id))
    )

  implicit val user: Type[IO, User] =
    tpe[User](
      "User",
      "id" -> lift(_.id),
      "name" -> lift(_.name),
      "organization" -> eff(x => repo.getOrganization(x.organizationId))
    )
}
```


<details>
  <summary>You can also extend the dsl if you prefer a more object oriented style.</summary>

```scala mdoc:nest
class UserTypes(repo: Repo) extends GqlDsl[IO] {
  // ...
}
```

</details>

### Mutually recursive domains
Subgraphs can neatly packaged into classes, but that does not address the issue of recursion between different domains.
#### Call by name constructor parameters
A compositional approach is to use call by name constructor parameters to lazily pass mutually recursive dependencies.
```scala mdoc:nest
class UserTypes(
  paymentTypes: => PaymentTypes,
) {
  lazy val p = paymentTypes
  import p._
  // ...
}

class PaymentTypes(
  userTypes: => UserTypes,
) {
  lazy val u = userTypes
  import u._
  // ...
}

lazy val userTypes = new UserTypes(paymentTypes)
lazy val paymentTypes = new PaymentTypes(userTypes)
```
When domain types are defined in seperate projects, OOP interfaces can be used to implement mutual recursion.
```scala mdoc:nest
trait UserTypes {
  // we can also choose to only expose the datatypes that are necessary
  implicit def userType: Type[IO, User]
}
```