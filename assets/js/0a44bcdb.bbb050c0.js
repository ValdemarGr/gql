"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[436],{3905:(e,t,n)=>{n.d(t,{Zo:()=>c,kt:()=>d});var a=n(7294);function i(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function r(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function l(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?r(Object(n),!0).forEach((function(t){i(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):r(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function o(e,t){if(null==e)return{};var n,a,i=function(e,t){if(null==e)return{};var n,a,i={},r=Object.keys(e);for(a=0;a<r.length;a++)n=r[a],t.indexOf(n)>=0||(i[n]=e[n]);return i}(e,t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);for(a=0;a<r.length;a++)n=r[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(i[n]=e[n])}return i}var s=a.createContext({}),p=function(e){var t=a.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):l(l({},t),e)),n},c=function(e){var t=p(e.components);return a.createElement(s.Provider,{value:t},e.children)},m={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},u=a.forwardRef((function(e,t){var n=e.components,i=e.mdxType,r=e.originalType,s=e.parentName,c=o(e,["components","mdxType","originalType","parentName"]),u=p(n),d=i,f=u["".concat(s,".").concat(d)]||u[d]||m[d]||r;return n?a.createElement(f,l(l({ref:t},c),{},{components:n})):a.createElement(f,l({ref:t},c))}));function d(e,t){var n=arguments,i=t&&t.mdxType;if("string"==typeof e||i){var r=n.length,l=new Array(r);l[0]=u;var o={};for(var s in t)hasOwnProperty.call(t,s)&&(o[s]=t[s]);o.originalType=e,o.mdxType="string"==typeof e?e:i,l[1]=o;for(var p=2;p<r;p++)l[p]=n[p];return a.createElement.apply(null,l)}return a.createElement.apply(null,n)}u.displayName="MDXCreateElement"},2801:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>s,contentTitle:()=>l,default:()=>m,frontMatter:()=>r,metadata:()=>o,toc:()=>p});var a=n(7462),i=(n(7294),n(3905));const r={title:"The DSL"},l=void 0,o={unversionedId:"server/schema/dsl",id:"server/schema/dsl",title:"The DSL",description:"gql's dsl is a lightweight set of smart-constructors.",source:"@site/docs/server/schema/dsl.md",sourceDirName:"server/schema",slug:"/server/schema/dsl",permalink:"/gql/docs/server/schema/dsl",draft:!1,editUrl:"https://github.com/valdemargr/gql/tree/main/docs/server/schema/dsl.md",tags:[],version:"current",frontMatter:{title:"The DSL"},sidebar:"docs",previous:{title:"Input types",permalink:"/gql/docs/server/schema/input_types"},next:{title:"Resolvers",permalink:"/gql/docs/server/schema/resolvers"}},s={},p=[{value:"Fields",id:"fields",level:2},{value:"Builders",id:"builders",level:3},{value:"Value resolution",id:"value-resolution",level:3},{value:"Unification instances",id:"unification-instances",level:2},{value:"Interface inheritance",id:"interface-inheritance",level:3},{value:"Input types",id:"input-types",level:2},{value:"Other output structures",id:"other-output-structures",level:2},{value:"Covariant effects",id:"covariant-effects",level:3}],c={toc:p};function m(e){let{components:t,...n}=e;return(0,i.kt)("wrapper",(0,a.Z)({},c,n,{components:t,mdxType:"MDXLayout"}),(0,i.kt)("p",null,"gql's dsl is a lightweight set of smart-constructors.\nIf you have a particular usecase or even coding style that conflicts with the dsl, you can always introduce your own schema definition syntax or build on top of the existing dsl."),(0,i.kt)("p",null,"Lets begin by importing what we need."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},"import cats.data._\nimport cats.effect._\nimport cats.implicits._\nimport gql.dsl.all._\nimport gql.ast._\nimport gql.resolver._\n")),(0,i.kt)("h2",{id:"fields"},"Fields"),(0,i.kt)("p",null,"The simplest form of field construction comes from the ",(0,i.kt)("inlineCode",{parentName:"p"},"build.from")," smart constructor.\nIt simply lifts a resolver into a field, given that a gql output type exists for the resolver output."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},"def r: Resolver[IO, Int, String] = Resolver.lift(i => i.toString())\n\nval f: Field[IO, Int, String] = build.from(r)\n// f: Field[IO, Int, String] = Field(\n//   resolve = gql.resolver.Resolver@3e02c60,\n//   output = cats.Later@4640998d,\n//   description = None,\n//   attributes = List()\n// )\n")),(0,i.kt)("p",null,"Sometimes type inference cannot find the proper type for a field:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},"build.from(Resolver.liftF(i => IO(i.toString())))\n// error: value liftF is not a member of object gql.resolver.Resolver\n// did you mean lift? or perhaps liftFull?\n// build.from(Resolver.liftF(i => IO(i.toString())))\n//            ^^^^^^^^^^^^^^\n")),(0,i.kt)("p",null,"The type parameters for ",(0,i.kt)("inlineCode",{parentName:"p"},"build")," are partially applied, such that when type inference isn't enough, types can be supplied explicitly."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},"build[IO, Int].from(Resolver.effect(i => IO(i.toString())))\n\nbuild.from(Resolver.effect((i: Int) => IO(i.toString())))\n")),(0,i.kt)("p",null,"For some fields, there is an even more concise syntax.\nInvoking the ",(0,i.kt)("inlineCode",{parentName:"p"},"apply")," method of ",(0,i.kt)("inlineCode",{parentName:"p"},"build"),", takes a higher order function that goes from the identity resolver (",(0,i.kt)("inlineCode",{parentName:"p"},"Resolver[F, A, A]"),") to some output."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},"build[IO, Int](_.map(i => i * 2).evalMap(i => IO(i))): Field[IO, Int, Int]\n")),(0,i.kt)("h3",{id:"builders"},"Builders"),(0,i.kt)("p",null,"Complex structures may require many special resolver compositions.\nThe dsl also introduces a something akin to a builder pattern.\nThe ",(0,i.kt)("inlineCode",{parentName:"p"},"build")," function from the previous section, creates a builder that has more constructors than just ",(0,i.kt)("inlineCode",{parentName:"p"},"from")," and ",(0,i.kt)("inlineCode",{parentName:"p"},"apply"),"."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},"import gql.dsl.FieldBuilder\nval b: FieldBuilder[IO, Int] = build[IO, Int]\n")),(0,i.kt)("p",null,"Often a builder is only relevant within a scope, thus one can end up having many unused builders in scope.\nThe ",(0,i.kt)("inlineCode",{parentName:"p"},"builder")," makes such code more concise:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},"builder[IO, Int]{ (fb: FieldBuilder[IO, Int]) =>\n  fb\n}\n")),(0,i.kt)("p",null,"The builder dsl contains most of the field related constructors:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},'builder[IO, Int]{ fb =>\n  fb.tpe(\n    "Query",\n    "answer" -> lift(i => i * 0 + 42),\n    "pong" -> fb(_.map(_ => "pong"))\n  ): Type[IO, Int]\n  \n  fb.fields(\n    "answer" -> fb.lift(i => i * 0 + 42),\n    "ping" -> fb.from(Resolver.lift(_ => "pong"))\n  )\n}\n')),(0,i.kt)("h3",{id:"value-resolution"},"Value resolution"),(0,i.kt)("p",null,"Wrapping every field in a ",(0,i.kt)("inlineCode",{parentName:"p"},"build")," smart constructor and then defining the resolver seperately is a bit verbose.\nThere are smart constructors for two common variants of field resolvers, that lift a resolver function directly to a ",(0,i.kt)("inlineCode",{parentName:"p"},"Field"),"."),(0,i.kt)("p",null,"We must decide if the field is pure or effectful:"),(0,i.kt)("admonition",{type:"note"},(0,i.kt)("p",{parentName:"admonition"},"The effect constructor is named ",(0,i.kt)("inlineCode",{parentName:"p"},"eff")," to avoid collisions with cats-effect.")),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},'final case class Person(\n  name: String\n)\n\ntpe[IO, Person](\n  "Person",\n  "name" -> lift(_.name),\n  "nameEffect" -> eff(x => IO(x.name))\n)\n')),(0,i.kt)("p",null,"The ",(0,i.kt)("inlineCode",{parentName:"p"},"lift")," and ",(0,i.kt)("inlineCode",{parentName:"p"},"eff")," constructors can also also be supplied with arguments:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},'def familyName = arg[String]("familyName")\n\ntpe[IO, Person](\n  "Person",\n  "name" -> lift(familyName)(_ + _.name),\n  "nameEffect" -> eff(familyName)((f, p) => IO(p.name + f))\n)\n')),(0,i.kt)("h2",{id:"unification-instances"},"Unification instances"),(0,i.kt)("p",null,(0,i.kt)("inlineCode",{parentName:"p"},"Union"),"s and ",(0,i.kt)("inlineCode",{parentName:"p"},"Interface"),"s are abstract types that have implementations."),(0,i.kt)("p",null,(0,i.kt)("inlineCode",{parentName:"p"},"Union")," declares it's implementations up-front, like a ",(0,i.kt)("inlineCode",{parentName:"p"},"sealed trait"),".\nHowever, ",(0,i.kt)("inlineCode",{parentName:"p"},"Interface")," implementations are declared on the types that implement the interface, like a ",(0,i.kt)("inlineCode",{parentName:"p"},"trait")," or an ",(0,i.kt)("inlineCode",{parentName:"p"},"abstract class"),"."),(0,i.kt)("p",null,"Before continuing, lets setup the environment."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},"trait Vehicle { \n  def name: String\n}\nfinal case class Car(name: String) extends Vehicle\nfinal case class Boat(name: String) extends Vehicle\nfinal case class Truck(name: String) extends Vehicle\n\n")),(0,i.kt)("p",null,"For the ",(0,i.kt)("inlineCode",{parentName:"p"},"Union"),", variants can be declared using the ",(0,i.kt)("inlineCode",{parentName:"p"},"variant")," function, which takes a ",(0,i.kt)("inlineCode",{parentName:"p"},"PartialFunction")," from the unifying type to the implementation."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},'implicit def car: Type[IO, Car] = ???\nimplicit def boat: Type[IO, Boat] = ???\nimplicit def truck: Type[IO, Truck] = ???\n\nunion[IO, Vehicle]("Vehicle")\n  .variant[Car] { case c: Car => c }\n  .variant[Boat] { case b: Boat => b }\n  .variant[Truck] { case t: Truck => t }\n')),(0,i.kt)("p",null,"A shorthand function exists, if the type of the variant is a subtype of the unifying type."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},'union[IO, Vehicle]("Vehicle")\n  .subtype[Car] \n  .subtype[Boat] \n  .subtype[Truck] \n')),(0,i.kt)("p",null,"For an ",(0,i.kt)("inlineCode",{parentName:"p"},"Interface")," the same dsl exists, but is placed on the types that can implement the interface (a ",(0,i.kt)("inlineCode",{parentName:"p"},"Type")," or another ",(0,i.kt)("inlineCode",{parentName:"p"},"Interface"),")."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},'implicit lazy val vehicle: Interface[IO, Vehicle] = interface[IO, Vehicle](\n  "Vehicle",\n  "name" -> abst[IO, String]\n)\n\ntpe[IO, Car]("Car", "name" -> lift(_.name))\n  .implements[Vehicle]{ case c: Car => c }\n  \ntpe[IO, Boat]("Boat", "name" -> lift(_.name))\n  .subtypeOf[Vehicle]\n  \ntrait OtherVehicle extends Vehicle {\n  def weight: Int\n}\n\ninterface[IO, OtherVehicle](\n  "OtherVehicle",\n  "weight" -> abst[IO, Int],\n  // Since OtherVehicle is a subtype of Vehicle\n  // we can directly embed the Vehicle fields\n  vehicle.abstractFields: _*\n).implements[Vehicle]\n')),(0,i.kt)("h3",{id:"interface-inheritance"},"Interface inheritance"),(0,i.kt)("p",null,"It can be a bit cumbersome to implement an interface's fields every time it is extended.\nInterfaces accept any field type (abstract or concrete) as input.\nThis is convinient since it allows a safe type of inheritance.\nWhen using the ",(0,i.kt)("inlineCode",{parentName:"p"},"subtypeImpl")," function, all possible fields are added to the type."),(0,i.kt)("admonition",{type:"info"},(0,i.kt)("p",{parentName:"admonition"},"gql's inheritance has some implications:"),(0,i.kt)("ul",{parentName:"admonition"},(0,i.kt)("li",{parentName:"ul"},"If you're working an a ",(0,i.kt)("inlineCode",{parentName:"li"},"Type"),", only concrete fields can be inherited."),(0,i.kt)("li",{parentName:"ul"},"If you're working on an ",(0,i.kt)("inlineCode",{parentName:"li"},"Interface"),", all fields, concrete and abstract can be inherited.")),(0,i.kt)("p",{parentName:"admonition"},"gql picks the best field when you inherit from an interface.\nFor two fields with the same name, gql will always pick the concrete field.\nIf both are concrete, it will prioritize the field from the subtype (the type you're working on).")),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},'trait Pet {\n  def name: String\n  def age: Int\n  def weight: Double\n}\n\ncase class Dog(name: String, age: Int, weight: Double) extends Pet\n\nimplicit lazy val pet: Interface[IO, Pet] = interface[IO, Pet](\n  "Pet",\n  "name" -> lift(_.name),\n  "age" -> lift(_.age),\n  "weight" -> lift(_.weight)\n)\n\nlazy val overwirttenName = lift[Dog](_.name)\n\nimplicit lazy val dog: Type[IO, Dog] = tpe[IO, Dog](\n  "Dog",\n  "bark" -> lift(_ => "woof!"),\n  "name" -> overwirttenName\n).subtypeImpl[Pet]\n\ndog.fields.map{ case (k, _) => k}.mkString_(", ")\n// res13: String = "bark, name, age, weight"\n\n// The Dog type has it\'s own implementation of the name field\ndog.fields.exists{ case (_, v) => v == overwirttenName }\n// res14: Boolean = true\n')),(0,i.kt)("p",null,"To showcase the inheritance a bit further, consider the following invalid schema."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},'implicit lazy val pet: Interface[IO, Pet] = interface[IO, Pet](\n  "Pet",\n  "name" -> lift(_.name),\n  "age" -> lift(_.age),\n  // Notice that weight is abstract\n  "weight" -> abst[IO, Double]\n)\n\nimplicit lazy val dog: Type[IO, Dog] = tpe[IO, Dog](\n  "Dog",\n  "bark" -> lift(_ => "woof!")\n).subtypeImpl[Pet]\n\n// We are missing the weight field\ndog.fields.map{ case (k, _) => k}.mkString_(", ")\n// res15: String = "bark, name, age"\n')),(0,i.kt)("admonition",{type:"tip"},(0,i.kt)("p",{parentName:"admonition"},(0,i.kt)("a",{parentName:"p",href:"/gql/docs/server/schema/#validation"},"Schema validation")," will catch such errors.")),(0,i.kt)("h2",{id:"input-types"},"Input types"),(0,i.kt)("p",null,"Review the ",(0,i.kt)("a",{parentName:"p",href:"/gql/docs/server/schema/input_types"},"Input types")," section for more information."),(0,i.kt)("h2",{id:"other-output-structures"},"Other output structures"),(0,i.kt)("p",null,"Examples of other structures can be in the ",(0,i.kt)("a",{parentName:"p",href:"/gql/docs/server/schema/output_types"},"Output types")," section."),(0,i.kt)("h3",{id:"covariant-effects"},"Covariant effects"),(0,i.kt)("p",null,"Output types in gql are covariant in ",(0,i.kt)("inlineCode",{parentName:"p"},"F"),", such that output types written in different effects seamlessly weave together.\n",(0,i.kt)("inlineCode",{parentName:"p"},"fs2")," provides a type that we can reuse for pure effects defined as ",(0,i.kt)("inlineCode",{parentName:"p"},"type Pure[A] <: Nothing"),"."),(0,i.kt)("p",null,"With this trick, we can define gql types for trivial cases of our domain:"),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-scala"},'final case class Entity(\n  name: String,\n  age: Int\n)\n\nobject Entity {\n  implicit lazy val gqlType: Type[fs2.Pure, Entity] = tpe[fs2.Pure, Entity](\n    "Entity",\n    "name" -> lift(_.name),\n    "age" -> lift(_.age)\n  )\n}\n\ntrait Example\n\ntpe[IO, Example](\n  "Example",\n  "entity" -> lift(_ => Entity("John Doe", 42))\n)\n')))}m.isMDXComponent=!0}}]);