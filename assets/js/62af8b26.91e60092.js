"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[508],{3905:(e,n,a)=>{a.d(n,{Zo:()=>c,kt:()=>m});var t=a(7294);function o(e,n,a){return n in e?Object.defineProperty(e,n,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[n]=a,e}function l(e,n){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var t=Object.getOwnPropertySymbols(e);n&&(t=t.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),a.push.apply(a,t)}return a}function i(e){for(var n=1;n<arguments.length;n++){var a=null!=arguments[n]?arguments[n]:{};n%2?l(Object(a),!0).forEach((function(n){o(e,n,a[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):l(Object(a)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(a,n))}))}return e}function r(e,n){if(null==e)return{};var a,t,o=function(e,n){if(null==e)return{};var a,t,o={},l=Object.keys(e);for(t=0;t<l.length;t++)a=l[t],n.indexOf(a)>=0||(o[a]=e[a]);return o}(e,n);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);for(t=0;t<l.length;t++)a=l[t],n.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(o[a]=e[a])}return o}var s=t.createContext({}),p=function(e){var n=t.useContext(s),a=n;return e&&(a="function"==typeof e?e(n):i(i({},n),e)),a},c=function(e){var n=p(e.components);return t.createElement(s.Provider,{value:n},e.children)},u={inlineCode:"code",wrapper:function(e){var n=e.children;return t.createElement(t.Fragment,{},n)}},d=t.forwardRef((function(e,n){var a=e.components,o=e.mdxType,l=e.originalType,s=e.parentName,c=r(e,["components","mdxType","originalType","parentName"]),d=p(a),m=o,g=d["".concat(s,".").concat(m)]||d[m]||u[m]||l;return a?t.createElement(g,i(i({ref:n},c),{},{components:a})):t.createElement(g,i({ref:n},c))}));function m(e,n){var a=arguments,o=n&&n.mdxType;if("string"==typeof e||o){var l=a.length,i=new Array(l);i[0]=d;var r={};for(var s in n)hasOwnProperty.call(n,s)&&(r[s]=n[s]);r.originalType=e,r.mdxType="string"==typeof e?e:o,i[1]=r;for(var p=2;p<l;p++)i[p]=a[p];return t.createElement.apply(null,i)}return t.createElement.apply(null,a)}d.displayName="MDXCreateElement"},8812:(e,n,a)=>{a.r(n),a.d(n,{assets:()=>s,contentTitle:()=>i,default:()=>u,frontMatter:()=>l,metadata:()=>r,toc:()=>p});var t=a(7462),o=(a(7294),a(3905));const l={title:"Relational"},i=void 0,r={unversionedId:"server/integrations/relational",id:"server/integrations/relational",title:"Relational",description:"This integration is fairly new and sofisticated so it can be subject to change.",source:"@site/docs/server/integrations/relational.md",sourceDirName:"server/integrations",slug:"/server/integrations/relational",permalink:"/gql/docs/server/integrations/relational",draft:!1,editUrl:"https://github.com/valdemargr/gql/tree/main/docs/server/integrations/relational.md",tags:[],version:"current",frontMatter:{title:"Relational"},sidebar:"docs",previous:{title:"Global object identification",permalink:"/gql/docs/server/integrations/goi"},next:{title:"Query DSL",permalink:"/gql/docs/client/dsl"}},s={},p=[{value:"Skunk example",id:"skunk-example",level:2},{value:"Simplifying relationships",id:"simplifying-relationships",level:3},{value:"Runtime semantics",id:"runtime-semantics",level:2},{value:"Implementing your own integration",id:"implementing-your-own-integration",level:2},{value:"Adding arguments",id:"adding-arguments",level:2},{value:"Sum types",id:"sum-types",level:2},{value:"Declaring complex subqueries",id:"declaring-complex-subqueries",level:2},{value:"Using relational without tables",id:"using-relational-without-tables",level:2},{value:"Running transactions",id:"running-transactions",level:2},{value:"Handling N+1",id:"handling-n1",level:2}],c={toc:p};function u(e){let{components:n,...a}=e;return(0,o.kt)("wrapper",(0,t.Z)({},c,a,{components:n,mdxType:"MDXLayout"}),(0,o.kt)("admonition",{type:"caution"},(0,o.kt)("p",{parentName:"admonition"},"This integration is fairly new and sofisticated so it can be subject to change.")),(0,o.kt)("p",null,"gql also comes with an optional integration for relational databases."),(0,o.kt)("p",null,"The relational integration is library agnostic and is based on query fragments that can be composed into a full query."),(0,o.kt)("p",null,"The relational module ships with two implementations, one for ",(0,o.kt)("inlineCode",{parentName:"p"},"skunk")," and another for ",(0,o.kt)("inlineCode",{parentName:"p"},"doobie"),".\nThey can be found in the ",(0,o.kt)("a",{parentName:"p",href:"../../overview/modules"},"modules")," section."),(0,o.kt)("admonition",{type:"tip"},(0,o.kt)("p",{parentName:"admonition"},"Integrating a new library requires very little code.\nThe skunk integration only spans 18 lines of code.")),(0,o.kt)("h2",{id:"skunk-example"},"Skunk example"),(0,o.kt)("p",null,"For this example we will use ",(0,o.kt)("inlineCode",{parentName:"p"},"skunk"),".\nWe will start off with some imports."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},"import skunk._\nimport skunk.codec.all._\nimport skunk.implicits._\nimport gql.ast._\nimport gql.dsl.all._\nimport gql.relational._\nimport gql.relational.skunk.dsl._\nimport gql.relational.skunk.dsl.algebra.QueryContext\nimport cats._\nimport cats.data._\nimport cats.arrow._\nimport cats.effect._\nimport cats.implicits._\n")),(0,o.kt)("p",null,"Before we start declaring fragments, we need to define our domain."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},"final case class Home(name: String, address: String)\n// many homes belong to many people\nfinal case class Person(name: String, age: Int)\n// a pet has one owner\nfinal case class Pet(name: String, age: Int, owner: Int)\n")),(0,o.kt)("p",null,"The realtional module also ships with a dsl that makes declaration use conscise.\nWe will start off just declaring the home table."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'case class HomeTable(\n  // When a table is queried it must have an alias\n  alias: String\n) extends SkunkTable {\n  // Note that we use only skunk tools to declare the contents of this structure\n\n  // We can declare how this table is referenced in sql (or some other query language)\n  def table = void"home"\n\n  // The SkunkTable trait gives some convinience methods for declaring columns\n  val (idCol, id) = sel("id", int4)\n  val (nameCol, name) = sel("name", text)\n  val (addressCol, address) = sel("address", text)\n\n  // The projection that uniquely identifies a row in the table\n  def tableKey = id\n}\n// We get some methods if show how given an alias we can get a table\nval homeTable = skunkTable(HomeTable)\n')),(0,o.kt)("p",null,"We will also need to declare the other two tables, this time with less comments."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'case class PersonTable(alias: String) extends SkunkTable {\n  def table = void"person"\n\n  val (idCol, id) = sel("id", int4)\n  val (nameCol, name) = sel("name", text)\n  val (ageCol, age) = sel("age", int4)\n\n  def tableKey = id\n}\nval personTable = skunkTable(PersonTable)\n\ncase class PetTable(alias: String) extends SkunkTable {\n  def table = void"pet"\n\n  val (idCol, id) = sel("id", int4)\n  val (nameCol, name) = sel("name", text)\n  val (ageCol, age) = sel("age", int4)\n  val (ownerCol, owner) = sel("owner", int4)\n\n  def tableKey = id\n}\nval petTable = skunkTable(PetTable)\n')),(0,o.kt)("p",null,"Since ",(0,o.kt)("inlineCode",{parentName:"p"},"Home")," and ",(0,o.kt)("inlineCode",{parentName:"p"},"Person")," have a many to many relationship, we will have to go through another table table to get the relationship."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'case class HomePersonTable(alias: String) extends SkunkTable {\n  def table = void"home_person"\n\n  val (homeCol, home) = sel("home_id", int4)\n  val (personCol, person) = sel("person_id", int4)\n\n  def tableKey = (home, person).tupled\n}\nval homePersonTable = skunkTable(HomePersonTable)\n')),(0,o.kt)("p",null,"Now we can start declaring our graphql schema."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'implicit lazy val pet: Type[IO, QueryContext[PetTable]] = \n  tpe[IO, QueryContext[PetTable]](\n    "PetTable",\n    "name" -> query(_.name), // query is a method that compiles to a projection in the query language (sql)\n    "age" -> query(_.age)\n  )\n\nimplicit lazy val person: Type[IO, QueryContext[PersonTable]] = \n  tpe[IO, QueryContext[PersonTable]](\n    "PersonTable",\n    "name" -> query(_.name),\n    "age" -> query(_.age),\n    "pets" -> cont{ person => // cont is a continuation that will create a new table from the current one\n      // The join method takes a type parameter that declares the multiplicity of the join\n      // If no type parameter is given, the join is assumed to be one to one\n      petTable.join[List]{ pet =>\n        // Given an instance of the pet table, we can declare a join predicate\n        sql"${pet.ownerCol} = ${person.idCol}"\n      }\n    }\n  )\n\nimplicit lazy val home: Type[IO, QueryContext[HomeTable]] = \n  tpe[IO, QueryContext[HomeTable]](\n    "HomeTable",\n    "name" -> query(_.name),\n    "address" -> query(_.address),\n    "caption" -> query(h => (h.name, h.address).mapN(_ + " at " + _)), // projections form an applicative\n    "people" -> cont{ home =>\n      // Tables can be flatmapped together\n      for {\n        hp <- homePersonTable.join[List](hp => sql"${home.idCol} = ${hp.homeCol}")\n        p <- personTable.join(p => sql"${hp.personCol} = ${p.idCol}")\n      } yield p\n    }\n  )\n')),(0,o.kt)("p",null,"Now we are done declaring our schema."),(0,o.kt)("p",null,"Before querying it we will need our database up and running."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'import cats.effect.unsafe.implicits.global\nimport natchez.noop._ // needed for skunk connection\nimplicit val trace: natchez.Trace[IO] = NoopTrace[IO]()\n\ndef connection = Session.single[IO](\n  host = "127.0.0.1",\n  port = 5432,\n  user = "postgres",\n  database = "postgres"\n)\n')),(0,o.kt)("details",null,(0,o.kt)("summary",null,"We will also need to create our tables and insert some data."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'connection.use{ ses =>\n  val queries = List(\n    sql"drop table if exists pet",\n    sql"drop table if exists home_person",\n    sql"drop table if exists person",\n    sql"drop table if exists home",\n    sql"""create table home_person (\n      home_id int not null,\n      person_id int not null\n    )""",\n    sql"""create table pet (\n      id int4 primary key,\n      name text not null,\n      age int not null,\n      owner int not null\n    )""",\n    sql"""create table person (\n      id int4 primary key,\n      name text not null,\n      age int not null\n    )""",\n    sql"""create table home (\n      id int4 primary key,\n      name text not null,\n      address text not null\n    )""",\n    sql"""insert into home (id, name, address) values (1, \'Doe Home\', \'123 Main St\')""",\n    sql"""insert into person (id, name, age) values (1, \'John Doe\', 42)""",\n    sql"""insert into person (id, name, age) values (2, \'Jane Doe\', 40)""",\n    sql"""insert into home_person (home_id, person_id) values (1, 1)""", \n    sql"""insert into home_person (home_id, person_id) values (1, 2)""",\n    sql"""insert into pet (id, name, age, owner) values (1, \'Fluffy\', 2, 1)""",\n  )\n\n  queries.traverse(x => ses.execute(x.command))\n}.unsafeRunSync()\n// res0: List[<none>.<root>.skunk.data.Completion] = List(\n//   DropTable,\n//   DropTable,\n//   DropTable,\n//   DropTable,\n//   CreateTable,\n//   CreateTable,\n//   CreateTable,\n//   CreateTable,\n//   Insert(count = 1),\n//   Insert(count = 1),\n//   Insert(count = 1),\n//   Insert(count = 1),\n//   Insert(count = 1),\n//   Insert(count = 1)\n// )\n'))),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'def schema = gql.Schema.query(\n  tpe[IO, Unit](\n    "Query",\n    "homes" -> runFieldSingle(connection) { (_: Unit) => \n      homeTable.join[List](_ => sql"true")\n    }\n  )\n)\n\ndef q = """\nquery {\n  homes {\n    name\n    address\n    caption\n    people {\n      name\n      age\n      pets {\n        name\n        age\n      }\n    }\n  }\n}\n"""\n\nimport io.circe.syntax._\nimport gql.{Compiler, Application}\nschema\n  .map(Compiler[IO].compile(_, q))\n  .flatMap { case Right(Application.Query(run)) => run.map(_.handleErrors{e => println(e.getMessage()); ""}.asJson.spaces2) }\n  .unsafeRunSync()\n// res1: String = """{\n//   "data" : {\n//     "homes" : [\n//       {\n//         "address" : "123 Main St",\n//         "caption" : "Doe Home at 123 Main St",\n//         "name" : "Doe Home",\n//         "people" : [\n//           {\n//             "age" : 42,\n//             "name" : "John Doe",\n//             "pets" : [\n//               {\n//                 "age" : 2,\n//                 "name" : "Fluffy"\n//               }\n//             ]\n//           },\n//           {\n//             "age" : 40,\n//             "name" : "Jane Doe",\n//             "pets" : [\n//             ]\n//           }\n//         ]\n//       }\n//     ]\n//   }\n// }"""\n')),(0,o.kt)("p",null,"And thats it!"),(0,o.kt)("p",null,"Just for fun, we check out the generated sql."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'import gql.relational.skunk._\nimplicit def logQueries[F[_]: MonadCancelThrow]: SkunkIntegration.Queryable[F] = \n  new SkunkIntegration.Queryable[F] {\n    def apply[A](\n      query: AppliedFragment,\n      decoder: Decoder[A], \n      connection: SkunkIntegration.Connection[F]\n    ): F[List[A]] = {\n      println(query.fragment.sql)\n      SkunkIntegration.skunkQueryable[F].apply(query, decoder, connection)\n    }\n}\n\ndef schema = gql.Schema.query(\n  tpe[IO, Unit](\n    "Query",\n    "homes" -> runFieldSingle(connection) { (_: Unit) => \n      homeTable.join[List](_ => sql"true")\n    }\n  )\n)\n\nschema\n  .map(Compiler[IO].compile(_, q))\n  .flatMap { case Right(Application.Query(run)) => run.void }\n  .unsafeRunSync()\n// select t1.id, t1.address, t1.name, t1.address, t1.name, t2.home_id, t2.person_id, t3.id, t3.age, t3.name, t4.id, t4.age, t4.name\n// from home as t1\n// left join home_person as t2 on t1.id = t2.home_id\n// left join person as t3 on t2.person_id = t3.id\n// left join pet as t4 on t4.owner = t3.id\n// where true\n')),(0,o.kt)("h3",{id:"simplifying-relationships"},"Simplifying relationships"),(0,o.kt)("p",null,"The join between ",(0,o.kt)("inlineCode",{parentName:"p"},"home")," and ",(0,o.kt)("inlineCode",{parentName:"p"},"person")," can be a bit daunting, since you have to keep track of multiplicity yourself.\nInstead we can use the database to handle some of the multiplicity for us by generalizing the person table."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'case class SharedPersonTable(alias: String, table: AppliedFragment) extends SkunkTable {\n  val (idCol, id) = sel("id", int4)\n  val (nameCol, name) = sel("name", text)\n  val (ageCol, age) = sel("age", int4)\n\n  def tableKey = id\n}\n\nval sharedPersonTable = skunkTable(SharedPersonTable(_, void"person"))\n\nval homePersonQuery = void"(select * from home_person inner join person on home_person.person_id = person.id)"\nval sharedHomePersonTable = skunkTable(SharedPersonTable(_, homePersonQuery))\n\n// And now using our subquery we can simplify the join.\nimplicit lazy val person: Type[IO, QueryContext[SharedPersonTable]] = ???\n\ntpe[IO, QueryContext[HomeTable]](\n  "HomeTable",\n  "name" -> query(_.name),\n  "address" -> query(_.address),\n  "caption" -> query(h => (h.name, h.address).mapN(_ + " at " + _)), // projections form an applicative\n  "people" -> cont{ h => \n    sharedHomePersonTable.join[List](hp => sql"${h.idCol} = ${hp.aliased(sql"home_id")}")\n  }\n)\n')),(0,o.kt)("h2",{id:"runtime-semantics"},"Runtime semantics"),(0,o.kt)("admonition",{type:"info"},(0,o.kt)("p",{parentName:"admonition"},"This section is a technical reference, and not necessary to use the library.")),(0,o.kt)("p",null,"Data emitted by SQL is not hierarchical, but instead flat; for it to map well to graphql, which is hierarchical some work must be performed.\nMost use-cases are covered by simply invoking the ",(0,o.kt)("inlineCode",{parentName:"p"},"join")," method with the proper multiplicity parameter."),(0,o.kt)("p",null,"When your AST is inspected to build a query, a recursive AST walk composes a big reassociation function that can translate flat query results into the proper hierarchical structure.\nThis composed function also tracks the visited columns and their decoders."),(0,o.kt)("p",null,"The query algebra has a special operation that lets the caller modify the state however they wish.\nThe dsl uses this state modification for various tasks, such as providing a convinient ",(0,o.kt)("inlineCode",{parentName:"p"},"join")," method that both joins a table and performs the proper reassociation of results.\nConsider the following example that joins a table more explicitly."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},"val q1 = for {\n  ht <- homeTable.simpleJoin(_ => void\"true\")\n  _ <- reassociate[List](ht.tableKey)\n  // some other reassociation criteria\n  _ <- reassociate[Option](select(int4, void\"42\"))\n} yield ht\n// q1: algebra.Query[[X]List[Option[X]], HomeTable] = FlatMap(\n//   fa = FlatMap(\n//     fa = LiftEffect(fa = EitherT(value = cats.data.IndexedStateT@2315d97d)),\n//     f = gql.relational.QueryDsl$$Lambda$14075/0x000000080382a040@2cdcf3b1\n//   ),\n//   f = <function1>\n// )\n\n// we can perform reassociation before performing the actions in 'q1'\nval q2 = reassociate[Option](select(text, void\"'john doe'\")).flatMap(_ => q1)\n// q2: algebra.Query[[X]Option[List[Option[X]]], HomeTable] = FlatMap(\n//   fa = LiftEffect(fa = EitherT(value = cats.data.IndexedStateT@33597353)),\n//   f = <function1>\n// )\n\n// we can also change the result structure after performing the actions in 'q2'\nq2.mapK[List](new (\u03bb[X => Option[List[Option[X]]]] ~> List) {\n  def apply[A](fa: Option[List[Option[A]]]): List[A] = fa.toList.flatten.flatMap(_.toList)\n})\n// res4: algebra.Query[List, HomeTable] = LiftEffect(\n//   fa = EitherT(value = cats.data.IndexedStateT@31587ec9)\n// )\n")),(0,o.kt)("p",null,"Accessing the lowlevel state also lets the user perform other tasks such as unique id (new alias) generation."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},"for {\n  alias1 <- newAlias\n  alias2 <- newAlias\n} yield ()\n// res5: algebra.Query[[X]X, Unit] = FlatMap(\n//   fa = LiftEffect(fa = EitherT(value = cats.data.IndexedStateT@3c0b97ae)),\n//   f = <function1>\n// )\n")),(0,o.kt)("h2",{id:"implementing-your-own-integration"},"Implementing your own integration"),(0,o.kt)("p",null,"The entire dsl and query compiler is available if you implement a couple of methods."),(0,o.kt)("p",null,"Here is the full skunk integration."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'import _root_.{skunk => sk}\nobject MyIntegration extends QueryAlgebra {\n  // What is a fragment\n  type Frag = sk.AppliedFragment\n  // How do we transform a string into a fragment\n  def stringToFrag(s: String): Frag = sql"#${s}".apply(Void)\n  // Combine and create empty fragments\n  implicit def appliedFragmentMonoid: Monoid[Frag] = sk.AppliedFragment.MonoidAppFragment\n  // How do we decode values\n  type Decoder[A] = sk.Decoder[A]\n  // How can we combine decoders\n  implicit def applicativeForDecoder: Applicative[Decoder] = Decoder.ApplicativeDecoder\n  // How do we make an optional decoder\n  def optDecoder[A](d: Decoder[A]): Decoder[Option[A]] = d.opt\n  // What is needed to perform a query\n  type Connection[F[_]] = Resource[F, Session[F]]\n  // Given a connection, how do we use it\n  implicit def skunkQueryable[F[_]: MonadCancelThrow]: Queryable[F] = new Queryable[F] {\n    def apply[A](query: AppliedFragment, decoder: Decoder[A], connection: Connection[F]): F[List[A]] =\n      connection.use(_.execute(query.fragment.query(decoder))(query.argument))\n  }\n}\n')),(0,o.kt)("p",null,"The dsl can be instantiated for any query algebra."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},"object myDsl extends QueryDsl(MyIntegration)\n")),(0,o.kt)("p",null,"you can also add integration specific methods to your dsl."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},"object myDsl extends QueryDsl(MyIntegration) {\n  def someOperationSpecificToMyIntegration = ???\n}\n")),(0,o.kt)("h2",{id:"adding-arguments"},"Adding arguments"),(0,o.kt)("p",null,"All field combinators allow arguments to be provided naturally, regardless of where the field is in the query."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'implicit lazy val pt: Type[IO, QueryContext[PersonTable]] = ???\n\ntpe[IO, QueryContext[HomeTable]](\n  "HomeTable",\n  "people" -> cont(arg[List[Int]]("ids")) { (home, ids) =>\n    for {\n      hp <- homePersonTable.join[List](hp => sql"${home.idCol} = ${hp.homeCol}")\n      p <- personTable.join(p => sql"${hp.personCol} = ${p.idCol} and ${p.idCol} in (${int4.list(ids)})".apply(ids))\n    } yield p\n  }\n)\n')),(0,o.kt)("h2",{id:"sum-types"},"Sum types"),(0,o.kt)("p",null,"Sum types can naturally be declared also."),(0,o.kt)("details",null,(0,o.kt)("summary",null,"Lets set up some tables for sum types."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'connection.use{ ses =>\n  val queries = List(\n    sql"drop table if exists owner",\n    sql"drop table if exists dog",\n    sql"drop table if exists cat",\n    sql"""create table owner (\n      id int4 primary key\n    )""",\n    sql"""create table dog (\n      id int4 primary key,\n      owner_id int4 not null,\n      name text not null,\n      age int not null\n    )""",\n    sql"""create table cat (\n      id int4 primary key,\n      owner_id int4 not null,\n      name text not null,\n      age int not null\n    )""",\n    sql"""insert into owner (id) values (1)""",\n    sql"""insert into owner (id) values (2)""",\n    sql"""insert into dog (id, owner_id, name, age) values (1, 1, \'Dog\', 42)""",\n    sql"""insert into cat (id, owner_id, name, age) values (2, 2, \'Cat\', 22)""",\n  )\n\n  queries.traverse(x => ses.execute(x.command))\n}.unsafeRunSync()\n// res7: List[<none>.<root>.skunk.data.Completion] = List(\n//   DropTable,\n//   DropTable,\n//   DropTable,\n//   CreateTable,\n//   CreateTable,\n//   CreateTable,\n//   Insert(count = 1),\n//   Insert(count = 1),\n//   Insert(count = 1),\n//   Insert(count = 1)\n// )\n'))),(0,o.kt)("p",null,"And now we can run it."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'sealed trait Animal { \n  def name: String\n}\ncase class Dog(owner: String, name: String, age: Int) extends Animal\ncase class Cat(owner: String, name: String, age: Int) extends Animal\n\ntrait OwnerTable extends SkunkTable {\n  def table = void"owner"\n  val (idCol, id) = sel("id", int4)\n  def tableKey = id\n}\ncase class OwnerTableUnion(alias: String) extends OwnerTable\ncase class OwnerTableInterface(alias: String) extends OwnerTable\nval ownerTableUnion = skunkTable(OwnerTableUnion)\n// ownerTableUnion: SkunkTableAlg[OwnerTableUnion] = gql.relational.skunk.dsl$$anon$2@7701bae5\nval ownerTableInterface = skunkTable(OwnerTableInterface)\n// ownerTableInterface: SkunkTableAlg[OwnerTableInterface] = gql.relational.skunk.dsl$$anon$2@f3a8cbd\n\ncase class DogTable(alias: String) extends SkunkTable {\n  def table = void"dog"\n\n  val (idCol, id) = sel("id", int4)\n  val (ownerCol, owner) = sel("owner_id", int4)\n  val (nameCol, name) = sel("name", text)\n  val (ageCol, age) = sel("age", int4)\n\n  def tableKey = id\n}\nval dogTable = skunkTable(DogTable)\n// dogTable: SkunkTableAlg[DogTable] = gql.relational.skunk.dsl$$anon$2@7d6615a5\n\ncase class CatTable(alias: String) extends SkunkTable {\n  def table = void"cat"\n\n  val (idCol, id) = sel("id", int4)\n  val (ownerCol, owner) = sel("owner_id", int4)\n  val (nameCol, name) = sel("name", text)\n  val (ageCol, age) = sel("age", int4)\n\n  def tableKey = id\n}\nval catTable = skunkTable(CatTable)\n// catTable: SkunkTableAlg[CatTable] = gql.relational.skunk.dsl$$anon$2@1d305b40\n\nimplicit lazy val animalInterface = interface[IO, QueryContext[OwnerTableInterface]](\n  "AnimalInterface",\n  "owner" -> abst[IO, String]\n)\n\nimplicit lazy val cat = tpe[IO, QueryContext[CatTable]](\n  "Cat",\n  "owner" -> query(_.owner),\n  "name" -> query(_.name),\n  "age" -> query(_.age)\n).contImplements[OwnerTableInterface]{ owner => \n  catTable.join[Option](cat => sql"${owner.idCol} = ${cat.ownerCol}")\n}\n\nimplicit lazy val dog = tpe[IO, QueryContext[DogTable]](\n  "Dog",\n  "owner" -> query(_.owner),\n  "name" -> query(_.name),\n  "age" -> query(_.age)\n).contImplements[OwnerTableInterface]{ owner => \n  dogTable.join[Option](dog => sql"${owner.idCol} = ${dog.ownerCol}")\n}\n\n// we use the builder to create a union type\nimplicit lazy val animal = relBuilder[IO, OwnerTableUnion] { b =>\n  b\n    .union("Animal")\n    .contVariant(owner => dogTable.join[Option](dog => sql"${owner.idCol} = ${dog.ownerCol}"))\n    .contVariant(owner => catTable.join[Option](cat => sql"${owner.idCol} = ${cat.ownerCol}"))\n}\n\ndef schema = gql.Schema.query(\n  tpe[IO, Unit](\n    "Query",\n    "animals" -> runFieldSingle(connection) { (_: Unit) =>\n      ownerTableUnion.join[List](_ => sql"true")\n    },\n    "animalInterfaces" -> runFieldSingle(connection) { (_: Unit) =>\n      ownerTableInterface.join[List](_ => sql"true")\n    }\n  )\n)\n\ndef animalQuery = """\n  query {\n    animals {\n      __typename\n      ... on Dog {\n        owner\n        name\n        age\n      }\n      ... on Cat {\n        owner\n        name\n        age\n      }\n    }\n    animalInterfaces {\n      __typename\n      ... on Dog {\n        owner\n        name\n        age\n      }\n      ... on Cat {\n        owner\n        name\n        age\n      }\n    }\n  }\n"""\n\nschema\n  .map(Compiler[IO].compile(_, animalQuery))\n  .flatMap { case Right(Application.Query(run)) => run.map(_.handleErrors{e => println(e.getMessage()); ""}.asJson.spaces2) }\n  .unsafeRunSync()\n// select t1.id, t2.id, t2.age, t2.name, t2.owner_id, t3.id, t3.age, t3.name, t3.owner_id\n// from owner as t1\n// left join dog as t2 on t1.id = t2.owner_id\n// left join cat as t3 on t1.id = t3.owner_id\n// where true\n// select t1.id, t2.id, t2.age, t2.name, t2.owner_id, t3.id, t3.age, t3.name, t3.owner_id\n// from owner as t1\n// left join dog as t2 on t1.id = t2.owner_id\n// left join cat as t3 on t1.id = t3.owner_id\n// where true\n// res8: String = """{\n//   "data" : {\n//     "animalInterfaces" : [\n//       {\n//         "__typename" : "Cat",\n//         "age" : 22,\n//         "name" : "Cat",\n//         "owner" : 2\n//       },\n//       {\n//         "__typename" : "Dog",\n//         "age" : 42,\n//         "name" : "Dog",\n//         "owner" : 1\n//       }\n//     ],\n//     "animals" : [\n//       {\n//         "__typename" : "Cat",\n//         "age" : 22,\n//         "name" : "Cat",\n//         "owner" : 2\n//       },\n//       {\n//         "__typename" : "Dog",\n//         "age" : 42,\n//         "name" : "Dog",\n//         "owner" : 1\n//       }\n//     ]\n//   }\n// }"""\n')),(0,o.kt)("h2",{id:"declaring-complex-subqueries"},"Declaring complex subqueries"),(0,o.kt)("p",null,"Sometimes your tables must have complex filtering, limiting, ordering and so on.\nThe most obvious way to declare such parameters is simply to use a subquery."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'case class ParameterizedPersonTable(alias: String, table: AppliedFragment) extends SkunkTable {\n  val (idCol, id) = sel("id", int4)\n  val (nameCol, name) = sel("name", text)\n  val (ageCol, age) = sel("age", int4)\n  \n  def tableKey = id\n}\ndef parameterizedPersonTable(\n  limitOffset: Option[(Int, Int)],\n  order: Option[AppliedFragment],\n  filter: Option[AppliedFragment]\n) = skunkTable{ alias => \n  val filt = filter.foldMap(f => sql"where ${f.fragment}".apply(f.argument))\n  val ord = order.foldMap(f => sql"order by ${f.fragment}".apply(f.argument))\n  val lim = \n    limitOffset.foldMap{ case (limit, offset) => sql"limit ${int4} offset ${int4}".apply((limit, offset))}\n  ParameterizedPersonTable(\n    alias,\n    sql"""|(\n          |  select *\n          |  from person\n          |  ${filt.fragment}\n          |  ${ord.fragment}\n          |  ${lim.fragment}\n          |)""".stripMargin.apply((filt.argument, ord.argument, lim.argument))\n  )\n}\n')),(0,o.kt)("p",null,"And now we can use our new table."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'implicit lazy val ppt: Type[IO, QueryContext[ParameterizedPersonTable]] = ???\n\nval personQueryArgs = (\n  arg[Option[Int]]("limit"),\n  arg[Option[Int]]("offset"),\n  arg[Option[Boolean]]("order"),\n  arg[Option[Int]]("ageFilter")\n).tupled\ntpe[IO, QueryContext[HomeTable]](\n  "HomeTable",\n  "people" -> cont(personQueryArgs) { case (home, (lim, off, ord, af)) =>\n    for {\n      hp <- homePersonTable.join[List](hp => sql"${home.idCol} = ${hp.homeCol}")\n      p <- parameterizedPersonTable(\n        limitOffset = (lim, off).tupled,\n        order = ord.map{\n          case true => void"age desc"\n          case false => void"age asc"\n        },\n        filter = af.map(age => sql"age > ${int4}".apply(age))\n      ).join(p => sql"${hp.personCol} = ${p.idCol}")\n    } yield p\n  }\n)\n')),(0,o.kt)("h2",{id:"using-relational-without-tables"},"Using relational without tables"),(0,o.kt)("p",null,"There is no restriction on how you can implement a table, so you can choose your own strategy.\nFor instance say we just wanted to declare everything up-front and select fields ad-hoc."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'import gql.relational.skunk.SkunkIntegration.Query.Select\n\ncase class AdHocTable(\n  alias: String, \n  table: AppliedFragment,\n  tableKey: Select[?],\n) extends SkunkTable\n\ntpe[IO, QueryContext[HomeTable]](\n  "HomeTable",\n  "people" -> cont(arg[List[Int]]("ids")) { (home, ids) =>\n    for {\n      hp <- skunkTable(alias => \n          AdHocTable(\n            alias, \n            sql"#${alias}.home_person".apply(Void), \n            select(\n              int4 ~ int4,\n              sql"#${alias}.home_id".apply(Void), \n              sql"#${alias}.person_id".apply(Void)\n            )\n          )\n        ).join[List](hp => sql"${home.idCol} = ${hp.aliased(sql"home_id")}")\n      p <- personTable.join(p => sql"${hp.aliased(sql".person_id")} = ${p.idCol} and ${p.idCol} in (${int4.list(ids)})".apply(ids))\n    } yield p\n  }\n)\n')),(0,o.kt)("p",null,"Since there is no dsl for this, constructing the query is a bit gruesome.\nConsider if a dsl is possible for your formulation."),(0,o.kt)("h2",{id:"running-transactions"},"Running transactions"),(0,o.kt)("p",null,"Most usecases involve running all queries in a transaction, but none of the examples so far have introduces this.\nThe implementation of transactions depends on the database library, but many implementations share common properties."),(0,o.kt)("p",null,"If your database library supports opening transactions as a resource then the you can lazily open a transaction.\nHere is an example using skunk."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'trait SessionContext {\n  def getSession: Resource[IO, Session[IO]]\n}\n\nobject SessionContext {\n  def fromIOLocal(iol: IOLocal[Option[Resource[IO, Session[IO]]]]) = new SessionContext {\n    def getSession = Resource.eval(iol.get).flatMap{\n      case None => Resource.eval(IO.raiseError(new Exception("No session in context")))\n      case Some(sc) => sc\n    }\n  }\n}\n\ndef myConnection: Resource[IO, Session[IO]] = Session.single[IO](\n  host = "127.0.0.1",\n  port = 5432,\n  user = "postgres",\n  database = "postgres"\n)\n\n// The outer resource manages the lifecycle of the connection\n// The inner resource leases the connection, if the inner resource is not closed, the outer waits\ndef lazyConnection: Resource[IO, LazyResource[IO, Session[IO]]] = \n  gql.relational.LazyResource.fromResource(myConnection)\n\n// We define our schema as requiring a connection\ndef myQuery(ctx: SessionContext): Type[IO, Unit] = {\n  implicit lazy val homeTableTpe: Out[IO, QueryContext[HomeTable]] = ???\n  tpe[IO, Unit](\n    "Query",\n    "homes" -> runFieldSingle(ctx.getSession) { (_: Unit) => \n      homeTable.join[List](_ => sql"true")\n    }\n  )\n}\n\ndef runQuery: IO[String => Compiler.Outcome[IO]] = \n  gql.Statistics[IO].flatMap{ stats => \n    IOLocal[Option[Resource[IO, Session[IO]]]](None).map{ loc =>\n      val sc = SessionContext.fromIOLocal(loc)\n\n      val schema = gql.Schema.query(stats)(myQuery(sc))\n\n      val setResource = lazyConnection.evalMap(x => loc.set(Some(x.get)))\n\n      (query: String) => \n        Compiler[IO]\n          .compile(schema, q)\n          .map{\n            case gql.Application.Query(fa) => gql.Application.Query(setResource.surround(fa))\n            case gql.Application.Mutation(fa) => gql.Application.Mutation(setResource.surround(fa))\n            // Subscription is a bit more complex since we would like to close the transaction on every event\n            case gql.Application.Subscription(fa) => \n              gql.Application.Subscription{\n                fs2.Stream.resource(lazyConnection).flatMap{ x =>\n                  fs2.Stream.exec(loc.set(Some(x.get))) ++\n                    fa.evalTap(_ => x.forceClose)\n                }\n              }\n          }\n    }\n  }\n')),(0,o.kt)("details",null,(0,o.kt)("summary",null,"You can also use MTL for passing the transaction around"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'import cats.mtl._\n\ndef myConnection: Resource[IO, Session[IO]] = Session.single[IO](\n  host = "127.0.0.1",\n  port = 5432,\n  user = "postgres",\n  database = "postgres"\n)\n\n// The outer resource manages the lifecycle of the connection\n// The inner resource leases the connection, if the inner resource is not closed, the outer waits\ndef lazyConnection: Resource[IO, LazyResource[IO, Session[IO]]] = \n  gql.relational.LazyResource.fromResource(myConnection)\n\nval liftK = Kleisli.liftK[IO, Resource[IO, Session[IO]]]\n\ntype GetConn[F[_]] = Ask[F, Resource[F, Session[F]]]\n\ndef makeConn[F[_]](conn: GetConn[F]): Resource[F, Session[F]] = \n  Resource.eval(conn.ask[Resource[F, Session[F]]]).flatten\n\n// We define our schema as requiring a connection\ndef myQuery[F[_]: Async](conn: GetConn[F]): Type[F, Unit] = {\n  implicit lazy val homeTableTpe: Type[F, QueryContext[HomeTable]] = ???\n  tpe[F, Unit](\n    "Query",\n    "homes" -> runFieldSingle(makeConn(conn)) { (_: Unit) => \n      homeTable.join[List](_ => sql"true")\n    }\n  )\n}\n\nimplicit def functorForAsk[F[_]]: Functor[Ask[F, *]] = ???\ndef kleisliAsk[F[_]: Applicative, A] = Ask[Kleisli[F, A, *], A]\n\ndef runQuery: IO[String => Compiler.Outcome[IO]] = \n  gql.Statistics[IO].map{ stats => \n    type G[A] = Kleisli[IO, Resource[IO, Session[IO]], A]\n\n    val liftK = Kleisli.liftK[IO, Resource[IO, Session[IO]]]\n\n    val ask: Ask[G, Resource[G, Session[G]]] = \n      kleisliAsk[IO, Resource[IO, Session[IO]]].map(_.mapK(liftK).map(_.mapK(liftK)))\n\n    val schema = gql.Schema.query(stats.mapK(liftK))(myQuery[G](ask))\n\n    val oneshot = lazyConnection.map(_.get.flatTap(_.transaction))\n\n    (query: String) => \n      Compiler[G]\n        .compile(schema, q)\n        .map{ \n          case gql.Application.Query(fa) => gql.Application.Query(oneshot.useKleisli(fa))\n          case gql.Application.Mutation(fa) => gql.Application.Mutation(oneshot.useKleisli(fa))\n          // Subscription is a bit more complex since we would like to close the transaction on every event\n          case gql.Application.Subscription(fa) => \n            gql.Application.Subscription{\n              fs2.Stream.resource(lazyConnection).flatMap{ lc =>\n                fa\n                  .translate(Kleisli.applyK[IO, Resource[IO, Session[IO]]](lc.get.flatTap(_.transaction)))\n                  .evalTap(_ => lc.forceClose)\n              }\n            }\n        }\n  }\n'))),(0,o.kt)("h2",{id:"handling-n1"},"Handling N+1"),(0,o.kt)("p",null,"The relational module can handle N+1 queries and queries that can cause cartesian products.\nTo solve N+1, the user must use the ",(0,o.kt)("inlineCode",{parentName:"p"},"runField")," method instead of the ",(0,o.kt)("inlineCode",{parentName:"p"},"runFieldSingle"),".\nThe ",(0,o.kt)("inlineCode",{parentName:"p"},"runField")," method takes a list of inputs ",(0,o.kt)("inlineCode",{parentName:"p"},"I")," and produces ",(0,o.kt)("inlineCode",{parentName:"p"},"Query[G, (Select[I], B)]"),", such that query results can be reassociated with the inputs."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'def myBatchedHomeQuery(conn: Resource[IO, Session[IO]]) = {\n  case class MyDatatype(homeId: Int)\n\n  tpe[IO, MyDatatype](\n    "MyDatatype",\n    "home" -> runField[IO, List, MyDatatype, HomeTable](conn) { xs => \n      val lst = xs.toList.map(_.homeId)\n      for {\n        ht <- homeTable.join[List](ht => sql"${ht.idCol} in (${int4.list(lst)})".apply(lst))\n      } yield (ht.id.fmap(MyDatatype), ht)\n    }\n  )\n}\n')),(0,o.kt)("p",null,"To solve the query multiplicity explosions you can use the ",(0,o.kt)("inlineCode",{parentName:"p"},"contBoundary")," which works almost like ",(0,o.kt)("inlineCode",{parentName:"p"},"cont"),", except the query will be split up into two queries."),(0,o.kt)("p",null,"The ",(0,o.kt)("inlineCode",{parentName:"p"},"contBoundary")," function takes two interesting parameters.\nThe first parameter will be a projection of the current query, decoded into ",(0,o.kt)("inlineCode",{parentName:"p"},"B"),".\nThe second parameter turns this ",(0,o.kt)("inlineCode",{parentName:"p"},"B")," into another query, which will be the root of the new query."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-scala"},'def boundaryQuery(conn: Resource[IO, Session[IO]]) = {\n  case class MyDatatype(homeId: Int)\n\n  relBuilder[IO, HomeTable]{ rb =>\n    rb.tpe(\n      "HomeTable",\n      "people" -> rb.contBoundary(conn){ home =>\n        homePersonTable.join[List](hp => sql"${home.idCol} = ${hp.homeCol}").map(_.person)\n      }{ (xs: NonEmptyList[Int]) =>\n        val lst = xs.toList\n        personTable.join(p => sql"${p.idCol} in (${int4.list(lst)})".apply(lst)).map(p => p.id -> p)\n      }\n    )\n  }\n}\n')),(0,o.kt)("admonition",{type:"info"},(0,o.kt)("p",{parentName:"admonition"},"The ",(0,o.kt)("inlineCode",{parentName:"p"},"contBoundary")," is only available in when using the ",(0,o.kt)("inlineCode",{parentName:"p"},"relBuilder"),", since type inference does not work very well."),(0,o.kt)("p",{parentName:"admonition"},"Inference troubles with ",(0,o.kt)("inlineCode",{parentName:"p"},"runField")," can also be alleviated by using the ",(0,o.kt)("inlineCode",{parentName:"p"},"relBuilder"),".")))}u.isMDXComponent=!0}}]);