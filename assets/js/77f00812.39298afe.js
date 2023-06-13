"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[782],{3905:(e,n,t)=>{t.d(n,{Zo:()=>s,kt:()=>m});var a=t(7294);function r(e,n,t){return n in e?Object.defineProperty(e,n,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[n]=t,e}function i(e,n){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);n&&(a=a.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),t.push.apply(t,a)}return t}function o(e){for(var n=1;n<arguments.length;n++){var t=null!=arguments[n]?arguments[n]:{};n%2?i(Object(t),!0).forEach((function(n){r(e,n,t[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):i(Object(t)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(t,n))}))}return e}function l(e,n){if(null==e)return{};var t,a,r=function(e,n){if(null==e)return{};var t,a,r={},i=Object.keys(e);for(a=0;a<i.length;a++)t=i[a],n.indexOf(t)>=0||(r[t]=e[t]);return r}(e,n);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(a=0;a<i.length;a++)t=i[a],n.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(r[t]=e[t])}return r}var c=a.createContext({}),d=function(e){var n=a.useContext(c),t=n;return e&&(t="function"==typeof e?e(n):o(o({},n),e)),t},s=function(e){var n=d(e.components);return a.createElement(c.Provider,{value:n},e.children)},p={inlineCode:"code",wrapper:function(e){var n=e.children;return a.createElement(a.Fragment,{},n)}},u=a.forwardRef((function(e,n){var t=e.components,r=e.mdxType,i=e.originalType,c=e.parentName,s=l(e,["components","mdxType","originalType","parentName"]),u=d(t),m=r,g=u["".concat(c,".").concat(m)]||u[m]||p[m]||i;return t?a.createElement(g,o(o({ref:n},s),{},{components:t})):a.createElement(g,o({ref:n},s))}));function m(e,n){var t=arguments,r=n&&n.mdxType;if("string"==typeof e||r){var i=t.length,o=new Array(i);o[0]=u;var l={};for(var c in n)hasOwnProperty.call(n,c)&&(l[c]=n[c]);l.originalType=e,l.mdxType="string"==typeof e?e:r,o[1]=l;for(var d=2;d<i;d++)o[d]=t[d];return a.createElement.apply(null,o)}return a.createElement.apply(null,t)}u.displayName="MDXCreateElement"},7801:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>c,contentTitle:()=>o,default:()=>p,frontMatter:()=>i,metadata:()=>l,toc:()=>d});var a=t(7462),r=(t(7294),t(3905));const i={title:"Global object identification"},o=void 0,l={unversionedId:"server/integrations/goi",id:"server/integrations/goi",title:"Global object identification",description:"gql also supports global object identification.",source:"@site/docs/server/integrations/goi.md",sourceDirName:"server/integrations",slug:"/server/integrations/goi",permalink:"/gql/docs/server/integrations/goi",draft:!1,editUrl:"https://github.com/valdemargr/gql/tree/main/docs/server/integrations/goi.md",tags:[],version:"current",frontMatter:{title:"Global object identification"},sidebar:"docs",previous:{title:"Natchez (tracing)",permalink:"/gql/docs/server/integrations/natchez"},next:{title:"Query DSL",permalink:"/gql/docs/client/dsl"}},c={},d=[{value:"Codecs",id:"codecs",level:2},{value:"Schema builder dsl",id:"schema-builder-dsl",level:2}],s={toc:d};function p(e){let{components:n,...t}=e;return(0,r.kt)("wrapper",(0,a.Z)({},s,t,{components:n,mdxType:"MDXLayout"}),(0,r.kt)("p",null,"gql also supports global object identification."),(0,r.kt)("admonition",{type:"info"},(0,r.kt)("p",{parentName:"admonition"},"Global object identification is primarily used by Relay clients to refetch objects.")),(0,r.kt)("p",null,"Global object identification requires two things:"),(0,r.kt)("ol",null,(0,r.kt)("li",{parentName:"ol"},"An id field on the object type."),(0,r.kt)("li",{parentName:"ol"},"A node field on the query type to look objects up.")),(0,r.kt)("h2",{id:"codecs"},"Codecs"),(0,r.kt)("p",null,"gql's GOI module introduces a codec type ",(0,r.kt)("inlineCode",{parentName:"p"},"IDCodec[A]")," decodes an array of strings into some type ",(0,r.kt)("inlineCode",{parentName:"p"},"A")," and encodes an ",(0,r.kt)("inlineCode",{parentName:"p"},"A")," into an array of strings."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},"import cats.implicits._\nimport gql._\nimport gql.goi._\nimport gql.goi.codec\n\nfinal case class UserId(\n  id1: String,\n  id2: Int\n)\n\nval userIdCodec: IDCodec[UserId] = (codec.string *: codec.int).to[UserId]\n")),(0,r.kt)("admonition",{type:"info"},(0,r.kt)("p",{parentName:"admonition"},"The ",(0,r.kt)("inlineCode",{parentName:"p"},"*:")," composition syntax is provided on top of the ",(0,r.kt)("inlineCode",{parentName:"p"},"twiddles")," library to map tuples to and from case classes.\nConsider taking a look at the ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/typelevel/twiddles"},"twiddles documentation"))),(0,r.kt)("p",null,"You won't be calling the encode and decode functions explicitly, but now that we have a codec for our ",(0,r.kt)("inlineCode",{parentName:"p"},"UserId"),", let's try it out:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'val encoded = userIdCodec.encode(UserId("abc", 123)).mkString_(":")\n// encoded: String = "abc:123"\nval decoded = userIdCodec.decode(encoded.split(":"))\n// decoded: cats.data.package.ValidatedNec[String, UserId] = Valid(\n//   a = UserId(id1 = "abc", id2 = 123)\n// )\n')),(0,r.kt)("p",null,"Optional fields can also be modeled with ",(0,r.kt)("inlineCode",{parentName:"p"},"opt")," method:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'final case class WithOpt(id1: String, id2: Option[String])\n\nval c = (codec.string *: codec.string.opt).to[WithOpt]\n// c: IDCodec[WithOpt] = gql.goi.IDCodec$$anon$2$$anon$3@3e85d0c\nc.encode(WithOpt("abc", Some("def"))).mkString_(":")\n// res0: String = "abc:def"\nc.encode(WithOpt("abc", None)).mkString_(":")\n// res1: String = "abc:null"\n')),(0,r.kt)("p",null,"Codecs can also handle errors:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'import java.util.UUID\nval uuidCodec = codec.string.eimap[UUID](\n  str => Either.catchNonFatal(UUID.fromString(str)).leftMap(_ => s"Invalid UUID \'$str\'"),\n)(_.toString())\n// uuidCodec: IDCodec[UUID] = gql.goi.IDCodec$$anon$1@e8c3eac\n\nuuidCodec.decode(Array("abc"))\n// res2: cats.data.package.ValidatedNec[String, UUID] = Invalid(\n//   e = Singleton(a = "Invalid UUID \'abc\'")\n// )\n')),(0,r.kt)("h2",{id:"schema-builder-dsl"},"Schema builder dsl"),(0,r.kt)("p",null,"GOI provides a dsl when building na object or interface that requires global object identification."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'import gql.ast._\nimport gql.dsl._\nimport gql.goi.dsl._\nimport cats.effect._\n\nfinal case class MyId(id: String)\nobject MyId {\n  implicit val myIdCodec: IDCodec[MyId] = codec.string.to[MyId]\n}\n\nfinal case class MyData(id: MyId, name: String)\ndef getData(id: MyId): IO[Option[MyData]] = IO.pure(Some(MyData(id, "name")))\n\nval myDataGid = gid[IO, MyData, MyId](\n  "MyData",\n  (d: MyData) => d.id,\n  (id: MyId) => getData(id)\n)\n\nimplicit val myData: Type[IO, MyData] = myDataGid.tpe(\n  "name" -> lift(_.name)\n)\n')),(0,r.kt)("p",null,"Once you are done declaring all of your types, you must accumulate a list of global object id's that the ",(0,r.kt)("inlineCode",{parentName:"p"},"node")," field can fetch:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'import cats.effect.unsafe.implicits.global\n\ndef schemaWithGoi: IO[Schema[IO, Unit, Unit, Unit]] = Schema.simple(\n    Goi.node(\n      SchemaShape.unit[IO](fields("data" -> eff(_ => getData(MyId("abc"))))), \n      List(myDataGid)\n    )\n  )\n  \ndef runWith(id: String) = {\n  def compiled = schemaWithGoi.map{ schema =>\n    Compiler[IO].compile(\n      schema,\n      s"""\n        query {\n          node(id: "$id") {\n            ... on MyData {\n              id\n              name\n            }\n          }\n        }\n      """\n    )\n  }\n    \n  compiled.flatMap(_.traverse{ case Application.Query(fa) => fa }).unsafeRunSync()\n}\n\ndef makeId(str: String) = new String(java.util.Base64.getEncoder.encode(str.getBytes()))\n\nrunWith(makeId("MyData:abc"))\n// res3: Either[CompilationError, QueryResult] = Right(\n//   value = QueryResult(\n//     data = object[node -> {\n//   "id" : "TXlEYXRhOmFiYw==",\n//   "name" : "name"\n// }],\n//     errors = Chain()\n//   )\n// )\n\nrunWith(makeId(""))\n// res4: Either[CompilationError, QueryResult] = Right(\n//   value = QueryResult(\n//     data = object[node -> null],\n//     errors = Singleton(\n//       a = Error(\n//         error = Right(value = "Invalid id parts \'\'"),\n//         path = Singleton(a = JString(value = "node"))\n//       )\n//     )\n//   )\n// )\n\nrunWith(makeId("Other"))\n// res5: Either[CompilationError, QueryResult] = Right(\n//   value = QueryResult(\n//     data = object[node -> null],\n//     errors = Singleton(\n//       a = Error(\n//         error = Right(value = "Invalid id parts \'Other\'"),\n//         path = Singleton(a = JString(value = "node"))\n//       )\n//     )\n//   )\n// )\n\nrunWith(makeId("MyData:abc:extra"))\n// res6: Either[CompilationError, QueryResult] = Right(\n//   value = QueryResult(\n//     data = object[node -> null],\n//     errors = Singleton(\n//       a = Error(\n//         error = Right(\n//           value = "Invalid Global object identifier size expected size 1 but got 2: string."\n//         ),\n//         path = Singleton(a = JString(value = "node"))\n//       )\n//     )\n//   )\n// )\n')))}p.isMDXComponent=!0}}]);