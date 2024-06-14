"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[776],{3905:(e,n,a)=>{a.d(n,{Zo:()=>c,kt:()=>d});var t=a(7294);function r(e,n,a){return n in e?Object.defineProperty(e,n,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[n]=a,e}function i(e,n){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var t=Object.getOwnPropertySymbols(e);n&&(t=t.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),a.push.apply(a,t)}return a}function l(e){for(var n=1;n<arguments.length;n++){var a=null!=arguments[n]?arguments[n]:{};n%2?i(Object(a),!0).forEach((function(n){r(e,n,a[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):i(Object(a)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(a,n))}))}return e}function o(e,n){if(null==e)return{};var a,t,r=function(e,n){if(null==e)return{};var a,t,r={},i=Object.keys(e);for(t=0;t<i.length;t++)a=i[t],n.indexOf(a)>=0||(r[a]=e[a]);return r}(e,n);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(t=0;t<i.length;t++)a=i[t],n.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(r[a]=e[a])}return r}var s=t.createContext({}),p=function(e){var n=t.useContext(s),a=n;return e&&(a="function"==typeof e?e(n):l(l({},n),e)),a},c=function(e){var n=p(e.components);return t.createElement(s.Provider,{value:n},e.children)},u={inlineCode:"code",wrapper:function(e){var n=e.children;return t.createElement(t.Fragment,{},n)}},m=t.forwardRef((function(e,n){var a=e.components,r=e.mdxType,i=e.originalType,s=e.parentName,c=o(e,["components","mdxType","originalType","parentName"]),m=p(a),d=r,g=m["".concat(s,".").concat(d)]||m[d]||u[d]||i;return a?t.createElement(g,l(l({ref:n},c),{},{components:a})):t.createElement(g,l({ref:n},c))}));function d(e,n){var a=arguments,r=n&&n.mdxType;if("string"==typeof e||r){var i=a.length,l=new Array(i);l[0]=m;var o={};for(var s in n)hasOwnProperty.call(n,s)&&(o[s]=n[s]);o.originalType=e,o.mdxType="string"==typeof e?e:r,l[1]=o;for(var p=2;p<i;p++)l[p]=a[p];return t.createElement.apply(null,l)}return t.createElement.apply(null,a)}m.displayName="MDXCreateElement"},4823:(e,n,a)=>{a.r(n),a.d(n,{assets:()=>s,contentTitle:()=>l,default:()=>u,frontMatter:()=>i,metadata:()=>o,toc:()=>p});var t=a(7462),r=(a(7294),a(3905));const i={title:"Query DSL"},l=void 0,o={unversionedId:"client/dsl",id:"client/dsl",title:"Query DSL",description:"gql provides a dsl for building graphql queries and response parsers.",source:"@site/docs/client/dsl.md",sourceDirName:"client",slug:"/client/dsl",permalink:"/gql/docs/client/dsl",draft:!1,editUrl:"https://github.com/valdemargr/gql/tree/main/docs/client/dsl.md",tags:[],version:"current",frontMatter:{title:"Query DSL"},sidebar:"docs",previous:{title:"Relational",permalink:"/gql/docs/server/integrations/relational"},next:{title:"Code generation",permalink:"/gql/docs/client/code-generation"}},s={},p=[{value:"Selections",id:"selections",level:2},{value:"Fragments",id:"fragments",level:2},{value:"Variables",id:"variables",level:2},{value:"Execution",id:"execution",level:2}],c={toc:p};function u(e){let{components:n,...a}=e;return(0,r.kt)("wrapper",(0,t.Z)({},c,a,{components:n,mdxType:"MDXLayout"}),(0,r.kt)("p",null,"gql provides a dsl for building graphql queries and response parsers.\nWhen you compose your query with the dsl, you automatically compose both a query and a json decoder for the query response."),(0,r.kt)("h2",{id:"selections"},"Selections"),(0,r.kt)("p",null,"The simplest combinator is ",(0,r.kt)("inlineCode",{parentName:"p"},"sel")," which declares a field selection:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'import gql.client._\nimport gql.client.dsl._\nimport cats.implicits._\n\nsel[Option[String]]("name")\n// res0: SelectionSet[Option[String]] = SelectionSet(\n//   impl = Fmap(\n//     fa = Lift(\n//       fa = Field(\n//         fieldName = "name",\n//         alias0 = None,\n//         args0 = List(),\n//         subQuery = OptionModifier(\n//           subQuery = Terminal(decoder = io.circe.Decoder$$anon$26@7a1d22e4)\n//         ),\n//         directives0 = List()\n//       )\n//     ),\n//     f = gql.client.SelectionSet$$$Lambda$12499/0x000000080328e840@52b64c9d\n//   )\n// )\n')),(0,r.kt)("p",null,"Most combinators in the dsl have multiple overloads to provide various features."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'sel.build[Option[String]]("name", _.alias("n"))\n\nsel.build[Option[String]]("name", _.args(arg("id", 42)))\n')),(0,r.kt)("p",null,"Every selection related structure forms an ",(0,r.kt)("inlineCode",{parentName:"p"},"Applicative")," such that you can compose multiple selections together:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'val s1 = sel[Option[String]]("name")\n\nval s2 = sel[Option[Int]]("age")\n\nval s3: SelectionSet[(Option[String], Option[Int])] = (s1, s2).tupled\n\nfinal case class PersonQuery(name: Option[String], age: Option[Int])\n\nval pq: SelectionSet[PersonQuery] = (s1, s2).mapN(PersonQuery.apply)\n')),(0,r.kt)("p",null,"Queries can also act as sub-selections (",(0,r.kt)("inlineCode",{parentName:"p"},"SubQuery")," in gql):"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'sel[PersonQuery]("person") {\n    pq\n}\n')),(0,r.kt)("p",null,"In the first examples the sub-query is captured implicitly.\nWe can also do this for custom types:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'implicit val pq2: SelectionSet[PersonQuery] = pq\n\nsel[PersonQuery]("person")\n')),(0,r.kt)("h2",{id:"fragments"},"Fragments"),(0,r.kt)("p",null,"Like in graphql we can define fragments to reuse selections:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'val frag = fragment[String]("MyFragment", on="Person") {\n    sel[String]("name")\n}\n\nval fragmentSpreads = sel[(Option[String], Option[Int])]("person") {\n    (\n        fragment.spread(frag),\n        inlineFrag[Int]("Person") {\n            sel[Int]("age")\n        }\n    ).tupled\n}\n')),(0,r.kt)("p",null,"Notice that both ",(0,r.kt)("inlineCode",{parentName:"p"},"fragment")," and ",(0,r.kt)("inlineCode",{parentName:"p"},"inlineFrag")," return an optional result.\nThis is because the spread may not match on the type (if the spread condition is a sub-type of the spread-on type).\nThis is not always the desired behavior, and as such, fragments can be required:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},"fragment.spread(frag).required: SelectionSet[String]\n")),(0,r.kt)("p",null,"You can provide additional information, should the fragment turn out to actually be missing:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'fragment.spread(frag).requiredFragment("MyFragment", on="Person")\n')),(0,r.kt)("admonition",{type:"info"},(0,r.kt)("p",{parentName:"admonition"},"Fragments should be preferred over re-using selections directly to reduce the rendered query size.")),(0,r.kt)("h2",{id:"variables"},"Variables"),(0,r.kt)("p",null,"Variables are accumulated into a sort of writer monad, such that they can be declared ad-hoc:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'variable[String]("name")\n// res7: Var[String, VariableName[String]] = Var(\n//   impl = WriterT(\n//     run = (\n//       Singleton(\n//         a = One(\n//           name = VariableName(name = "name"),\n//           tpe = "String!",\n//           default = None\n//         )\n//       ),\n//       io.circe.Encoder$AsObject$$anon$68@20dd3a5c\n//     )\n//   ),\n//   variableNames = VariableName(name = "name")\n// )\n')),(0,r.kt)("p",null,"Variables can be combined with the ",(0,r.kt)("inlineCode",{parentName:"p"},"~")," operator:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'variable[String]("name") ~ variable[Int]("age")\n// res8: Var[(String, Int), (VariableName[String], VariableName[Int])] = Var(\n//   impl = WriterT(\n//     run = (\n//       Append(\n//         leftNE = Singleton(\n//           a = One(\n//             name = VariableName(name = "name"),\n//             tpe = "String!",\n//             default = None\n//           )\n//         ),\n//         rightNE = Singleton(\n//           a = One(\n//             name = VariableName(name = "age"),\n//             tpe = "Int!",\n//             default = None\n//           )\n//         )\n//       ),\n//       io.circe.Encoder$AsObject$$anon$68@4039705a\n//     )\n//   ),\n//   variableNames = (VariableName(name = "name"), VariableName(name = "age"))\n// )\n')),(0,r.kt)("p",null,"Variables can also be declared as omittable, optionally with a default value:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'omittableVariable[String]("name", value("John")) ~\n    omittableVariable[Int]("age")\n// res9: Var[(Option[String], Option[Int]), (VariableName[String], VariableName[Int])] = Var(\n//   impl = WriterT(\n//     run = (\n//       Append(\n//         leftNE = Singleton(\n//           a = One(\n//             name = VariableName(name = "name"),\n//             tpe = "String!",\n//             default = Some(value = StringValue(v = "John", c = ()))\n//           )\n//         ),\n//         rightNE = Singleton(\n//           a = One(\n//             name = VariableName(name = "age"),\n//             tpe = "Int!",\n//             default = None\n//           )\n//         )\n//       ),\n//       io.circe.Encoder$AsObject$$anon$68@53c07eba\n//     )\n//   ),\n//   variableNames = (VariableName(name = "name"), VariableName(name = "age"))\n// )\n')),(0,r.kt)("p",null,'Variables can be "materialized" into a ',(0,r.kt)("inlineCode",{parentName:"p"},"VariableClosure")," by introducing them to a query:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'// Given a variable of type String, we can construct a query that returns an Int\nval queryWithVariable: VariableClosure[String, Int] = \n    variable[String]("name").introduce{ name: VariableName[String] =>\n        sel.build[Int]("id", _.args(arg("name", name)))\n    }\n')),(0,r.kt)("p",null,(0,r.kt)("inlineCode",{parentName:"p"},"VariableClosure")," can be combined via ",(0,r.kt)("inlineCode",{parentName:"p"},"~")," and have their selections modified via ",(0,r.kt)("inlineCode",{parentName:"p"},"modify"),":"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'def subQuery1: VariableClosure[String, Int] = queryWithVariable\n\ndef subQuery2: VariableClosure[String, Int] = \n    variable[String]("name2").introduce{ name: VariableName[String] =>\n        sel.build[Int]("id2", _.args(arg("name", name)))\n    }\n\ndef combined: VariableClosure[(String, String), Int] = \n (subQuery1 ~ subQuery2).modify(_.map{ case (v1, v2) => v1 + v2 })\n\n// VariableClosure also forms a profunctor so we can also use rmap\n(subQuery1 ~ subQuery2).rmap{ case (v1, v2) => v1 + v2 }\n')),(0,r.kt)("h2",{id:"execution"},"Execution"),(0,r.kt)("p",null,"Once a query has been constructed, there are three ways to wrap it together.\n",(0,r.kt)("inlineCode",{parentName:"p"},"simple")," if the query is parameter-less and name-less, ",(0,r.kt)("inlineCode",{parentName:"p"},"named")," if your query is named and ",(0,r.kt)("inlineCode",{parentName:"p"},"parameterized")," if it is both named and parameterized:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'import gql.parser.QueryAst.OperationType\ndef simpleQuery = Query.simple(\n    OperationType.Query,\n    sel[Unit]("person") {\n        (\n            sel[Int]("id"),\n            sel.build[Int]("age", _.args(arg("numbers", List(42))))\n        ).tupled.void\n    }\n)\n\nsimpleQuery.compile.query\n// res11: String = "query { person { age( numbers: [42] ), id } }"\n\nQuery.named(\n    OperationType.Mutation,\n    "MyMutation",\n    sel[String]("name")\n).compile.query\n// res12: String = "mutation MyMutation { name }"\n\ndef paramQuery = Query.parameterized(\n    OperationType.Subscription,\n    "MySubscription",\n    combined\n)\n\ndef compiledParamQuery = paramQuery.compile(("first", "second"))\ncompiledParamQuery.query\n// res13: String = """subscription MySubscription( $name : String!, $name2 : String! ) {\n//   id2( name: $name2 ),\n//   id( name: $name )\n// }"""\n\ncompiledParamQuery.variables\n// res14: Option[io.circe.JsonObject] = Some(\n//   value = object[name -> "first",name2 -> "second"]\n// )\n')))}u.isMDXComponent=!0}}]);