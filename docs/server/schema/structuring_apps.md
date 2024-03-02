---
title: Structuring large applications
---
The documentation explores smaller examples.
To host larger graphs there are some considerations that must be addressed.
* What up-front work can be done to minimize the overhead in introducing new types.
* How is (mutual) recursion handled between different domains.

Recursive datatypes are notoriously difficult to deal with.
In functional programming lazyness is often exploited as a solution to introduce cyclic data, but can easily accidentally introduce infinite recursion.

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
  // notice how we bind the effect (IO) so that we can omit this parameter in the dsl
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

## Mutually recursive domains
Subgraphs can neatly packaged into classes, but that does not address the issue of recursion between different domains.
### Call by name constructor parameters
A compositional approach is to use call by name constructor parameters to lazily pass mutually recursive dependencies.
```scala mdoc:nest
class UserTypes(paymentTypes: => PaymentTypes) {
  lazy val p = paymentTypes
  import p._
  // ...
}

class PaymentTypes(userTypes: => UserTypes) {
  lazy val u = userTypes
  import u._
  // ...
}

lazy val userTypes: UserTypes = new UserTypes(paymentTypes)
lazy val paymentTypes: PaymentTypes = new PaymentTypes(userTypes)
```
:::tip
When domain types are defined in seperate projects, OOP interfaces can be used to implement mutual recursion.
```scala mdoc:nest
// core project
trait User
trait UserTypes {
  // we can also choose to only expose the datatypes that are necessary
  implicit def userType: Type[IO, User]
}
trait Payment
trait PaymentTypes {
  implicit def paymentType: Type[IO, Payment]
}

// user project
class UserTypesImpl(paymentTypes: => PaymentTypes) extends UserTypes {
  lazy val p = paymentTypes
  import p._
  def userType: Type[IO, User] = ???
}

// payment project
class PaymentTypesImpl(userTypes: => UserTypes) extends PaymentTypes {
  lazy val u = userTypes
  import u._
  def paymentType: Type[IO, Payment] = ???
}

// main project
lazy val userTypes: UserTypes = new UserTypesImpl(paymentTypes)
lazy val paymentTypes: PaymentTypes = new PaymentTypesImpl(userTypes)
```
:::

### Cake
The cake pattern can also be used to define mutually recursive dependencies, at the cost of composability.
```scala mdoc:nest
// core project
trait User
trait UserTypes {
  // we can also choose to only expose the datatypes that are necessary
  implicit def userType: Type[IO, User]
}
trait Payment
trait PaymentTypes {
  implicit def paymentType: Type[IO, Payment]
}

// user project
trait UserTypesImpl extends UserTypes { self: PaymentTypes =>
  import self._
  def userType: Type[IO, User] = ???
}

// payment project
trait PaymentTypesImpl extends PaymentTypes { self: UserTypes =>
  import self._
  def paymentType: Type[IO, Payment] = ???
}

// main project
val allTypes = new UserTypesImpl with PaymentTypesImpl { }
```
