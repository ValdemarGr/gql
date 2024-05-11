"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[672],{3905:(e,n,a)=>{a.d(n,{Zo:()=>c,kt:()=>u});var t=a(7294);function r(e,n,a){return n in e?Object.defineProperty(e,n,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[n]=a,e}function i(e,n){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var t=Object.getOwnPropertySymbols(e);n&&(t=t.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),a.push.apply(a,t)}return a}function l(e){for(var n=1;n<arguments.length;n++){var a=null!=arguments[n]?arguments[n]:{};n%2?i(Object(a),!0).forEach((function(n){r(e,n,a[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):i(Object(a)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(a,n))}))}return e}function o(e,n){if(null==e)return{};var a,t,r=function(e,n){if(null==e)return{};var a,t,r={},i=Object.keys(e);for(t=0;t<i.length;t++)a=i[t],n.indexOf(a)>=0||(r[a]=e[a]);return r}(e,n);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(t=0;t<i.length;t++)a=i[t],n.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(r[a]=e[a])}return r}var s=t.createContext({}),p=function(e){var n=t.useContext(s),a=n;return e&&(a="function"==typeof e?e(n):l(l({},n),e)),a},c=function(e){var n=p(e.components);return t.createElement(s.Provider,{value:n},e.children)},d={inlineCode:"code",wrapper:function(e){var n=e.children;return t.createElement(t.Fragment,{},n)}},m=t.forwardRef((function(e,n){var a=e.components,r=e.mdxType,i=e.originalType,s=e.parentName,c=o(e,["components","mdxType","originalType","parentName"]),m=p(a),u=r,v=m["".concat(s,".").concat(u)]||m[u]||d[u]||i;return a?t.createElement(v,l(l({ref:n},c),{},{components:a})):t.createElement(v,l({ref:n},c))}));function u(e,n){var a=arguments,r=n&&n.mdxType;if("string"==typeof e||r){var i=a.length,l=new Array(i);l[0]=m;var o={};for(var s in n)hasOwnProperty.call(n,s)&&(o[s]=n[s]);o.originalType=e,o.mdxType="string"==typeof e?e:r,l[1]=o;for(var p=2;p<i;p++)l[p]=a[p];return t.createElement.apply(null,l)}return t.createElement.apply(null,a)}m.displayName="MDXCreateElement"},3983:(e,n,a)=>{a.r(n),a.d(n,{assets:()=>s,contentTitle:()=>l,default:()=>d,frontMatter:()=>i,metadata:()=>o,toc:()=>p});var t=a(7462),r=(a(7294),a(3905));const i={title:"Monadic Resolver DSL"},l=void 0,o={unversionedId:"server/schema/arrow_dsl",id:"server/schema/arrow_dsl",title:"Monadic Resolver DSL",description:"Modelling complex evaluation with Resolvers can be tricky.",source:"@site/docs/server/schema/arrow_dsl.md",sourceDirName:"server/schema",slug:"/server/schema/arrow_dsl",permalink:"/gql/docs/server/schema/arrow_dsl",draft:!1,editUrl:"https://github.com/valdemargr/gql/tree/main/docs/server/schema/arrow_dsl.md",tags:[],version:"current",frontMatter:{title:"Monadic Resolver DSL"},sidebar:"docs",previous:{title:"The DSL",permalink:"/gql/docs/server/schema/dsl"},next:{title:"Resolvers",permalink:"/gql/docs/server/schema/resolvers"}},s={},p=[{value:"Technical details",id:"technical-details",level:3},{value:"Builder extensions",id:"builder-extensions",level:3},{value:"Composition",id:"composition",level:3},{value:"Toplevel expressions",id:"toplevel-expressions",level:4},{value:"Lifting arguments",id:"lifting-arguments",level:2},{value:"Choice",id:"choice",level:2},{value:"Batching example",id:"batching-example",level:2},{value:"Arrowless final?",id:"arrowless-final",level:2}],c={toc:p};function d(e){let{components:n,...a}=e;return(0,r.kt)("wrapper",(0,t.Z)({},c,a,{components:n,mdxType:"MDXLayout"}),(0,r.kt)("p",null,"Modelling complex evaluation with ",(0,r.kt)("inlineCode",{parentName:"p"},"Resolver"),"s can be tricky.\nIt often involves using ",(0,r.kt)("inlineCode",{parentName:"p"},"first")," to pair up an arrow's result with it's input and proceeding with ",(0,r.kt)("inlineCode",{parentName:"p"},"map")," or ",(0,r.kt)("inlineCode",{parentName:"p"},"contramap"),"."),(0,r.kt)("p",null,"Gql introduces a in-language monadic arrow dsl that re-writes a monadic arrow expression into a series of ",(0,r.kt)("inlineCode",{parentName:"p"},"map"),", ",(0,r.kt)("inlineCode",{parentName:"p"},"contramap")," and ",(0,r.kt)("inlineCode",{parentName:"p"},"first")," invocations."),(0,r.kt)("admonition",{type:"info"},(0,r.kt)("p",{parentName:"admonition"},"This feature is akin to the ",(0,r.kt)("inlineCode",{parentName:"p"},"proc")," ",(0,r.kt)("a",{parentName:"p",href:"https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial#Arrow_proc_notation"},"notation in Haskell"),".")),(0,r.kt)("p",null,"Using the notation is straightforward, the same (covariant) combinators for ",(0,r.kt)("inlineCode",{parentName:"p"},"Resolver")," exist in the arrow dsl."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},"import gql.resolver._\nimport cats.implicits._\nimport cats.effect._\nimport gql.arrow._\n\n// Bind the effect type (IO) to aid with compiler errors and inference\nval d = dsl[IO]\nimport d._\nval r: Resolver[IO, Int, String] = \n  proc[Int] { i: Var[Int] =>\n    for {\n      a <- i.evalMap(x => IO(x + 2))\n      b <- a.evalMap(x => IO(x * 3))\n      c <- (a, b).tupled.evalMap{ case (aa, bb) => IO(aa + bb) }\n    } yield c.map(_.toString)\n  }\n")),(0,r.kt)("details",null,(0,r.kt)("summary",null,"Most syntatic extensions don't make much sense unless the arrow type (Resolver) is bound which requires knowing the effect type. The full monadic arrows language is available as toplevel functions also."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},"import gql.arrow.{Language => L}\nL.proc[Resolver[IO, *, *], Int, String] { i =>\n  for {\n    x <- L.declare[Resolver[IO, *, *], Int, Int](i)(Resolver.lift[IO, Int](z => z * 2))\n    y <- L.declare[Resolver[IO, *, *], (Int, Int), String]((x, x).tupled)(Resolver.lift[IO, (Int, Int)]{ case (a, b) => (a + b).toString() })\n  } yield y\n}\n// res0: Resolver[IO, Int, String] = gql.resolver.Resolver@a5c2401\n"))),(0,r.kt)("p",null,"The underlying arrow is also available for composition via ",(0,r.kt)("inlineCode",{parentName:"p"},"apply"),"."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},"proc[Int] { i =>\n  for {\n    x <- i(_.evalMap(z => IO(z + 1)))\n    out <- x.apply(_.map(_.toString))\n  } yield out\n}\n")),(0,r.kt)("h3",{id:"technical-details"},"Technical details"),(0,r.kt)("p",null,"The dsl introduces two datatypes, ",(0,r.kt)("inlineCode",{parentName:"p"},"Var")," and ",(0,r.kt)("inlineCode",{parentName:"p"},"Decl"),"."),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"Var")," is a reference to a set of variables that occur in the arrow. ",(0,r.kt)("inlineCode",{parentName:"li"},"Var")," forms an ",(0,r.kt)("inlineCode",{parentName:"li"},"Applicative"),"."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"Decl")," is used to re-write the monadic (",(0,r.kt)("inlineCode",{parentName:"li"},"flatMap"),") structure into an arrow. ",(0,r.kt)("inlineCode",{parentName:"li"},"Decl")," forms a ",(0,r.kt)("inlineCode",{parentName:"li"},"Monad"),".")),(0,r.kt)("p",null,"The primary use of ",(0,r.kt)("inlineCode",{parentName:"p"},"Decl")," is to bind variables.\nEvery transformation on a ",(0,r.kt)("inlineCode",{parentName:"p"},"Var"),"iable introduces a new ",(0,r.kt)("inlineCode",{parentName:"p"},"Var"),"iable which is stored in the ",(0,r.kt)("inlineCode",{parentName:"p"},"Decl")," structure."),(0,r.kt)("admonition",{type:"info"},(0,r.kt)("p",{parentName:"admonition"},"Since ",(0,r.kt)("inlineCode",{parentName:"p"},"Var")," forms an ",(0,r.kt)("inlineCode",{parentName:"p"},"Applicative")," that implies that ",(0,r.kt)("inlineCode",{parentName:"p"},"map")," is available for ",(0,r.kt)("inlineCode",{parentName:"p"},"Var"),".\n",(0,r.kt)("inlineCode",{parentName:"p"},"map")," for ",(0,r.kt)("inlineCode",{parentName:"p"},"Var")," is not memoized since it does not lift ",(0,r.kt)("inlineCode",{parentName:"p"},"Var")," into ",(0,r.kt)("inlineCode",{parentName:"p"},"Decl"),".\n",(0,r.kt)("inlineCode",{parentName:"p"},"Var")," has an extension ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/typelevel/cats/blob/c8aabcacd6045b9aed5c8626c4bf5308dd3f4912/core/src/main/scala/cats/arrow/Profunctor.scala#L59"},(0,r.kt)("inlineCode",{parentName:"a"},"rmap"))," which introduces a new ",(0,r.kt)("inlineCode",{parentName:"p"},"Var"),"iable that memoizes the result.\nThat is, the following equivalences holds:"),(0,r.kt)("pre",{parentName:"admonition"},(0,r.kt)("code",{parentName:"pre",className:"language-scala"},"declare((v: Var[A]).map(f))(Resolver.id[F, A]) <-> \n  (v: Var[A]).rmap(f) <->\n  (v: Var[A]).apply(_.map(f))\n"))),(0,r.kt)("p",null,"Closures are illegal in the dsl, as they are refer to variables that are not guaranteed to be available, so prefer invoking ",(0,r.kt)("inlineCode",{parentName:"p"},"proc")," once per ",(0,r.kt)("inlineCode",{parentName:"p"},"Resolver"),"."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},"println {\n  scala.util.Try {\n    proc[Int] { i =>\n      for {\n        x <- i.evalMap(x => IO(x + 2))\n        o <- x.andThen(proc[Int]{ _ =>\n          x.rmap(y => y + 2)\n        })\n      } yield o\n    }\n  }.toEither.leftMap(_.getMessage)\n}\n// Left(Variable closure error.\n// Variable declared at arrow_dsl.md:70.\n// Compilation initiated at arrow_dsl.md:68.\n// Variables that were not declared in this scope may not be referenced.\n// Example:\n// ```\n// proc[Int]{ i =>\n//   for {\n//     x <- i.apply(_.map(_ + 1))\n//     y <- i.apply(_.andThen(proc[Int]{ _ =>\n//       // referencing 'x' here is an error\n//       x.apply(_.map(_ + 1))\n//     }))\n//   } yield y\n// }\n// ```)\n")),(0,r.kt)("h3",{id:"builder-extensions"},"Builder extensions"),(0,r.kt)("p",null,"The dsl includes an extension method to ",(0,r.kt)("inlineCode",{parentName:"p"},"FieldBuilder")," that eases construction of ",(0,r.kt)("inlineCode",{parentName:"p"},"Field"),"s.\nThe dsl also enhances any resolver with a ",(0,r.kt)("inlineCode",{parentName:"p"},"proc")," extension method."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'import gql.ast._\n\nval gqlDsl = gql.dsl.GqlDsl[IO]\nimport gqlDsl._\n\nbuilder[Unit]{ b =>\n  b.tpe(\n    "MyType",\n    "field" -> b.proc{ i =>\n      for {\n        x <- i.evalMap(_ => IO(1 + 2))\n        y <- x.rmap(_ + 3)\n      } yield y\n    },\n    "otherField" -> b(_.proc{ i =>\n      i.evalMap(_ => IO(1 + 2))\n    })\n  )\n}\n')),(0,r.kt)("h3",{id:"composition"},"Composition"),(0,r.kt)("p",null,"Sharing common sub-arrows is a desirable property.\nThis can is expressed naturally with the dsl."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},"def mulDiv(i: Var[Int]): Decl[Var[Int]] = for {\n  x <- i.rmap(_ * 2)\n  y <- x.rmap(_ / 2)\n} yield y\n\nproc[Int](mulDiv(_) >>= mulDiv)\n// res4: Resolver[IO, Int, Int] = gql.resolver.Resolver@46126f11\n\nproc[Int](mulDiv(_) >>= mulDiv >>= mulDiv)\n// res5: Resolver[IO, Int, Int] = gql.resolver.Resolver@5d3108e1\n")),(0,r.kt)("h4",{id:"toplevel-expressions"},"Toplevel expressions"),(0,r.kt)("p",null,"It is recommended to always work in a scope with your effect type (",(0,r.kt)("inlineCode",{parentName:"p"},"F"),") bound, to ease inference and type signatures.\nThere is however support for toplevel proc resolver expressions."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},"def toplevelMulDiv[F[_]](i: Var[Int]): ResolverDecl[F, Var[Int]] = {\n  val d = dsl[F]\n  import d._\n  for {\n    x <- i.rmap(_ * 2)\n    y <- x.rmap(_ / 2)\n  } yield y\n}\n")),(0,r.kt)("p",null,"Passing the dsl as an implicit parameter is also an option."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},"def toplevelMulDiv[F[_]](i: Var[Int])(implicit d: ResolverArrowDsl[F]): ResolverDecl[F, Var[Int]] = {\n  import d._\n  for {\n    x <- i.rmap(_ * 2)\n    y <- x.rmap(_ / 2)\n  } yield y\n}\n")),(0,r.kt)("h2",{id:"lifting-arguments"},"Lifting arguments"),(0,r.kt)("p",null,"Request arguments is made easier by the arrow dsl."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'proc[Int] { i =>\n  for {\n    x <- i.evalMap(x => IO(x + 2))\n    y <- argument(arg[Int]("age"))\n    z <- (x, y).tupled.evalMap { case (a, b) => IO(a + b) }\n  } yield z\n}\n')),(0,r.kt)("h2",{id:"choice"},"Choice"),(0,r.kt)("p",null,"The dsl also covers ",(0,r.kt)("inlineCode",{parentName:"p"},"ArrowChoice"),"'s ",(0,r.kt)("inlineCode",{parentName:"p"},"choice")," combinator."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'proc[Int] { i =>\n  for {\n    x <- i.rmap(v => if (v > 5) Left(v) else Right(v))\n    y <- x.choice(\n      l => l.rmap(_ * 2),\n      r => for {\n        a <- argument(arg[Int]("age"))\n        out <- (a, r, i).tupled.rmap{ case (a, b, c) => a + b + c }\n      } yield out\n    )\n  } yield y\n}\n')),(0,r.kt)("h2",{id:"batching-example"},"Batching example"),(0,r.kt)("p",null,"Some steps commonly occur when writing batched resolvers:"),(0,r.kt)("ol",null,(0,r.kt)("li",{parentName:"ol"},"Pulling an id out of the parent datatype."),(0,r.kt)("li",{parentName:"ol"},"Passing the id to a batching resolver."),(0,r.kt)("li",{parentName:"ol"},"Pairing the batched output with the parent datatype.")),(0,r.kt)("p",null,"This pairing requires some clever use of ",(0,r.kt)("inlineCode",{parentName:"p"},"first")," and ",(0,r.kt)("inlineCode",{parentName:"p"},"contramap/lmap"),".\nThis behaviour is much easier to express monadically since we have access to closures."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'def getAddresses(ids: Set[Int]): IO[Map[Int, String]] =\n  IO(ids.toList.map(id => id -> s"Address $id").toMap)\n\ncase class DataType(id: Int, name: String)\nproc[DataType] { i =>\n  for {\n    id <- i.rmap(_.id)\n    r = Resolver.inlineBatch[IO, Int, String](getAddresses).opt\n    (addr: Var[Option[String]]) <- id.andThen(r)\n    p = (i, addr).tupled\n    out <- p.rmap{ case (dt, a) => s"${dt.name} @ ${a.getOrElse("<unknown>")}" }\n  } yield out\n}\n')),(0,r.kt)("h2",{id:"arrowless-final"},"Arrowless final?"),(0,r.kt)("p",null,"Expressions can be declared for any arrow, not just ",(0,r.kt)("inlineCode",{parentName:"p"},"Resolver"),".\nThe usefullness of this property is not significant, but an interesting property nonetheless."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},"import cats.free._\nimport cats.arrow._\ndef mulDiv[F2[_, _]](v: Var[Int]): Free[DeclAlg[F2, *], Var[Int]] = {\n  val d = new Language[F2] {}\n  import d._\n  // We can ask for the arrow evidence that must occur when some proc compiles us\n  askArrow.flatMap{ implicit arrow: Arrow[F2] =>\n    for {\n      x <- v.rmap(_ * 2)\n      y <- x.rmap(_ / 2)\n    } yield y\n  }\n}\n\nproc[Int] { i =>\n  for {\n    x <- i.rmap(_ * 2)\n    y <- mulDiv(x)\n  } yield y\n}\n")))}d.isMDXComponent=!0}}]);