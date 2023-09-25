"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[143],{3905:(e,a,t)=>{t.d(a,{Zo:()=>o,kt:()=>d});var n=t(7294);function s(e,a,t){return a in e?Object.defineProperty(e,a,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[a]=t,e}function r(e,a){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);a&&(n=n.filter((function(a){return Object.getOwnPropertyDescriptor(e,a).enumerable}))),t.push.apply(t,n)}return t}function l(e){for(var a=1;a<arguments.length;a++){var t=null!=arguments[a]?arguments[a]:{};a%2?r(Object(t),!0).forEach((function(a){s(e,a,t[a])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):r(Object(t)).forEach((function(a){Object.defineProperty(e,a,Object.getOwnPropertyDescriptor(t,a))}))}return e}function m(e,a){if(null==e)return{};var t,n,s=function(e,a){if(null==e)return{};var t,n,s={},r=Object.keys(e);for(n=0;n<r.length;n++)t=r[n],a.indexOf(t)>=0||(s[t]=e[t]);return s}(e,a);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);for(n=0;n<r.length;n++)t=r[n],a.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(s[t]=e[t])}return s}var p=n.createContext({}),i=function(e){var a=n.useContext(p),t=a;return e&&(t="function"==typeof e?e(a):l(l({},a),e)),t},o=function(e){var a=i(e.components);return n.createElement(p.Provider,{value:a},e.children)},c={inlineCode:"code",wrapper:function(e){var a=e.children;return n.createElement(n.Fragment,{},a)}},h=n.forwardRef((function(e,a){var t=e.components,s=e.mdxType,r=e.originalType,p=e.parentName,o=m(e,["components","mdxType","originalType","parentName"]),h=i(t),d=s,u=h["".concat(p,".").concat(d)]||h[d]||c[d]||r;return t?n.createElement(u,l(l({ref:a},o),{},{components:t})):n.createElement(u,l({ref:a},o))}));function d(e,a){var t=arguments,s=a&&a.mdxType;if("string"==typeof e||s){var r=t.length,l=new Array(r);l[0]=h;var m={};for(var p in a)hasOwnProperty.call(a,p)&&(m[p]=a[p]);m.originalType=e,m.mdxType="string"==typeof e?e:s,l[1]=m;for(var i=2;i<r;i++)l[i]=t[i];return n.createElement.apply(null,l)}return n.createElement.apply(null,t)}h.displayName="MDXCreateElement"},2598:(e,a,t)=>{t.r(a),t.d(a,{assets:()=>p,contentTitle:()=>l,default:()=>c,frontMatter:()=>r,metadata:()=>m,toc:()=>i});var n=t(7462),s=(t(7294),t(3905));const r={title:"Planning"},l=void 0,m={unversionedId:"server/execution/planning",id:"server/execution/planning",title:"Planning",description:"Planner algorithm",source:"@site/docs/server/execution/planning.md",sourceDirName:"server/execution",slug:"/server/execution/planning",permalink:"/gql/docs/server/execution/planning",draft:!1,editUrl:"https://github.com/valdemargr/gql/tree/main/docs/server/execution/planning.md",tags:[],version:"current",frontMatter:{title:"Planning"},sidebar:"docs",previous:{title:"Extending schemas",permalink:"/gql/docs/server/schema/extending"},next:{title:"Statistics",permalink:"/gql/docs/server/execution/statistics"}},p={},i=[{value:"Planner algorithm",id:"planner-algorithm",level:2},{value:"The high-level idea",id:"the-high-level-idea",level:3},{value:"Default planner intuition",id:"default-planner-intuition",level:3},{value:"Converting a query to a problem",id:"converting-a-query-to-a-problem",level:3}],o={toc:i};function c(e){let{components:a,...r}=e;return(0,s.kt)("wrapper",(0,n.Z)({},o,r,{components:a,mdxType:"MDXLayout"}),(0,s.kt)("h2",{id:"planner-algorithm"},"Planner algorithm"),(0,s.kt)("h3",{id:"the-high-level-idea"},"The high-level idea"),(0,s.kt)("p",null,"When planning for a query the planner assigns weights to every edge/field, optionally labels them with their batch names (if a batch resolver was used) and finally converts the problem to a simpler DAG (directed asyclic graph) form."),(0,s.kt)("admonition",{type:"tip"},(0,s.kt)("p",{parentName:"admonition"},"For information on how the planner assigns weights, check out the ",(0,s.kt)("a",{parentName:"p",href:"/gql/docs/server/execution/statistics"},"statistics"),".")),(0,s.kt)("p",null,"The goal now is to form batches by contracting nodes that are batchable (jobs of the same family in scheduling/OR jargon)."),(0,s.kt)("p",null,"For instance, assume the following DAG is in question:"),(0,s.kt)("mermaid",{value:"flowchart LR\n    Query((Query)) ---\x3e a(a<br/>batch: z<br/>cost: 2)\n    a --\x3e A((A))\n\n    Query --\x3e b(b<br/>cost: 1)\n    b --\x3e B((B))\n    \n      B ---\x3e c(c<br/>batch: z<br/>cost: 2)\n      c --\x3e C((C))"}),(0,s.kt)("p",null,"Now consider the following plan, where a possible contraction is colored red:"),(0,s.kt)("mermaid",{value:"flowchart LR\n    Query((Query)) -----\x3e a(a<br/>batch: z<br/>cost: 2)\n    a --\x3e A((A))\n\n    Query --\x3e b(b<br/>cost: 1)\n    b --\x3e B((B))\n    \n      B ---\x3e c(c<br/>batch: z<br/>cost: 2)\n      c --\x3e C((C))\n\nstyle a stroke:#f66,stroke-dasharray: 5 5\nstyle c stroke:#f66,stroke-dasharray: 5 5"}),(0,s.kt)("p",null,"And contracted it becomes:"),(0,s.kt)("mermaid",{value:'flowchart LR\n    Query((Query)) --\x3e b(b<br/>cost: 1)\n    b --\x3e B((B))\n    \n      B ---\x3e ac("{a,c}"<br/>batch: z<br/>cost: 2)\n      ac --\x3e A((A))\n      ac --\x3e C((C))\n\nstyle ac stroke:#f66,stroke-dasharray: 5 5'}),(0,s.kt)("h3",{id:"default-planner-intuition"},"Default planner intuition"),(0,s.kt)("p",null,"The default planner heuristic in gql lazily enumerates all plans, imposing a locally greedy order to the enumerated plans.\nThe default planner also employs some simple but powerful pruning rules to eliminate trivially uninteresting plan variantions."),(0,s.kt)("p",null,'The planner works through the problem from the root(s) and down through the DAG.\nThe algorithm keeps some state regarding what batches have been visited and what nodes are scheduled in the "current plan".\nIn a round of planning the algorithm will figure out what nodes are schedulable by looking at it\'s state.'),(0,s.kt)("p",null,"The planner will lazily generate all combinations of possible batches of schedulable nodes."),(0,s.kt)("admonition",{type:"note"},(0,s.kt)("p",{parentName:"admonition"},"One can easily cause a combinatorial explosion by generation of combinations.\nFortunately we don't consider every plan (and in fact, the default algorithm only pulls ",(0,s.kt)("span",{parentName:"p",className:"math math-inline"},(0,s.kt)("span",{parentName:"span",className:"katex"},(0,s.kt)("span",{parentName:"span",className:"katex-mathml"},(0,s.kt)("math",{parentName:"span",xmlns:"http://www.w3.org/1998/Math/MathML"},(0,s.kt)("semantics",{parentName:"math"},(0,s.kt)("mrow",{parentName:"semantics"},(0,s.kt)("mi",{parentName:"mrow"},"O"),(0,s.kt)("mo",{parentName:"mrow",stretchy:"false"},"("),(0,s.kt)("mi",{parentName:"mrow",mathvariant:"normal"},"\u2223"),(0,s.kt)("mi",{parentName:"mrow"},"V"),(0,s.kt)("mi",{parentName:"mrow",mathvariant:"normal"},"\u2223"),(0,s.kt)("mo",{parentName:"mrow",stretchy:"false"},")")),(0,s.kt)("annotation",{parentName:"semantics",encoding:"application/x-tex"},"O(|V|)")))),(0,s.kt)("span",{parentName:"span",className:"katex-html","aria-hidden":"true"},(0,s.kt)("span",{parentName:"span",className:"base"},(0,s.kt)("span",{parentName:"span",className:"strut",style:{height:"1em",verticalAlign:"-0.25em"}}),(0,s.kt)("span",{parentName:"span",className:"mord mathnormal",style:{marginRight:"0.02778em"}},"O"),(0,s.kt)("span",{parentName:"span",className:"mopen"},"("),(0,s.kt)("span",{parentName:"span",className:"mord"},"\u2223"),(0,s.kt)("span",{parentName:"span",className:"mord mathnormal",style:{marginRight:"0.22222em"}},"V"),(0,s.kt)("span",{parentName:"span",className:"mord"},"\u2223"),(0,s.kt)("span",{parentName:"span",className:"mclose"},")")))))," plans).\nFurthermore, most problems will have less than n plans.")),(0,s.kt)("p",null,'The planner will always generate the largest batches first, hence the "locally greedy" ordering.'),(0,s.kt)("p",null,"Trivially schedulable nodes are always scheduled first if possible; a pruning rules makes sure of this.\nFor a given scheduleable node, if no other un-scheduled node exists of the same family (excluding it's own descendants), then that node's only and optimal batch is the singleton batch containing only that node."),(0,s.kt)("p",null,"There are other pruning rules that have been considered, but don't seem necessary for practical problems since most problems produce very few plans."),(0,s.kt)("p",null,'One such pruning rule consideres "optimal" generated batch combinations.\nIf the largest batch that the planner can generate ',(0,s.kt)("span",{parentName:"p",className:"math math-inline"},(0,s.kt)("span",{parentName:"span",className:"katex"},(0,s.kt)("span",{parentName:"span",className:"katex-mathml"},(0,s.kt)("math",{parentName:"span",xmlns:"http://www.w3.org/1998/Math/MathML"},(0,s.kt)("semantics",{parentName:"math"},(0,s.kt)("mrow",{parentName:"semantics"},(0,s.kt)("mo",{parentName:"mrow",fence:"true"},"("),(0,s.kt)("mfrac",{parentName:"mrow",linethickness:"0px"},(0,s.kt)("mi",{parentName:"mfrac"},"n"),(0,s.kt)("mi",{parentName:"mfrac"},"n")),(0,s.kt)("mo",{parentName:"mrow",fence:"true"},")")),(0,s.kt)("annotation",{parentName:"semantics",encoding:"application/x-tex"},"n \\choose n")))),(0,s.kt)("span",{parentName:"span",className:"katex-html","aria-hidden":"true"},(0,s.kt)("span",{parentName:"span",className:"base"},(0,s.kt)("span",{parentName:"span",className:"strut",style:{height:"1.2em",verticalAlign:"-0.35em"}}),(0,s.kt)("span",{parentName:"span",className:"mord"},(0,s.kt)("span",{parentName:"span",className:"mopen delimcenter",style:{top:"0em"}},(0,s.kt)("span",{parentName:"span",className:"delimsizing size1"},"(")),(0,s.kt)("span",{parentName:"span",className:"mfrac"},(0,s.kt)("span",{parentName:"span",className:"vlist-t vlist-t2"},(0,s.kt)("span",{parentName:"span",className:"vlist-r"},(0,s.kt)("span",{parentName:"span",className:"vlist",style:{height:"0.7454em"}},(0,s.kt)("span",{parentName:"span",style:{top:"-2.355em"}},(0,s.kt)("span",{parentName:"span",className:"pstrut",style:{height:"2.7em"}}),(0,s.kt)("span",{parentName:"span",className:"sizing reset-size6 size3 mtight"},(0,s.kt)("span",{parentName:"span",className:"mord mtight"},(0,s.kt)("span",{parentName:"span",className:"mord mathnormal mtight"},"n")))),(0,s.kt)("span",{parentName:"span",style:{top:"-3.144em"}},(0,s.kt)("span",{parentName:"span",className:"pstrut",style:{height:"2.7em"}}),(0,s.kt)("span",{parentName:"span",className:"sizing reset-size6 size3 mtight"},(0,s.kt)("span",{parentName:"span",className:"mord mtight"},(0,s.kt)("span",{parentName:"span",className:"mord mathnormal mtight"},"n"))))),(0,s.kt)("span",{parentName:"span",className:"vlist-s"},"\u200b")),(0,s.kt)("span",{parentName:"span",className:"vlist-r"},(0,s.kt)("span",{parentName:"span",className:"vlist",style:{height:"0.345em"}},(0,s.kt)("span",{parentName:"span"}))))),(0,s.kt)("span",{parentName:"span",className:"mclose delimcenter",style:{top:"0em"}},(0,s.kt)("span",{parentName:"span",className:"delimsizing size1"},")"))))))),' contains nodes that all have the same "latest ending parent", then all other combinations ',(0,s.kt)("span",{parentName:"p",className:"math math-inline"},(0,s.kt)("span",{parentName:"span",className:"katex"},(0,s.kt)("span",{parentName:"span",className:"katex-mathml"},(0,s.kt)("math",{parentName:"span",xmlns:"http://www.w3.org/1998/Math/MathML"},(0,s.kt)("semantics",{parentName:"math"},(0,s.kt)("mrow",{parentName:"semantics"},(0,s.kt)("mrow",{parentName:"mrow"},(0,s.kt)("mo",{parentName:"mrow",fence:"true"},"("),(0,s.kt)("mfrac",{parentName:"mrow",linethickness:"0px"},(0,s.kt)("mi",{parentName:"mfrac"},"n"),(0,s.kt)("mi",{parentName:"mfrac"},"k")),(0,s.kt)("mo",{parentName:"mrow",fence:"true"},")")),(0,s.kt)("mtext",{parentName:"mrow"},"\xa0where\xa0"),(0,s.kt)("mi",{parentName:"mrow"},"k"),(0,s.kt)("mo",{parentName:"mrow"},"<"),(0,s.kt)("mi",{parentName:"mrow"},"n")),(0,s.kt)("annotation",{parentName:"semantics",encoding:"application/x-tex"},"{n \\choose k} \\text{ where } k < n")))),(0,s.kt)("span",{parentName:"span",className:"katex-html","aria-hidden":"true"},(0,s.kt)("span",{parentName:"span",className:"base"},(0,s.kt)("span",{parentName:"span",className:"strut",style:{height:"1.2em",verticalAlign:"-0.35em"}}),(0,s.kt)("span",{parentName:"span",className:"mord"},(0,s.kt)("span",{parentName:"span",className:"mord"},(0,s.kt)("span",{parentName:"span",className:"mopen delimcenter",style:{top:"0em"}},(0,s.kt)("span",{parentName:"span",className:"delimsizing size1"},"(")),(0,s.kt)("span",{parentName:"span",className:"mfrac"},(0,s.kt)("span",{parentName:"span",className:"vlist-t vlist-t2"},(0,s.kt)("span",{parentName:"span",className:"vlist-r"},(0,s.kt)("span",{parentName:"span",className:"vlist",style:{height:"0.7454em"}},(0,s.kt)("span",{parentName:"span",style:{top:"-2.355em"}},(0,s.kt)("span",{parentName:"span",className:"pstrut",style:{height:"2.7em"}}),(0,s.kt)("span",{parentName:"span",className:"sizing reset-size6 size3 mtight"},(0,s.kt)("span",{parentName:"span",className:"mord mtight"},(0,s.kt)("span",{parentName:"span",className:"mord mathnormal mtight",style:{marginRight:"0.03148em"}},"k")))),(0,s.kt)("span",{parentName:"span",style:{top:"-3.144em"}},(0,s.kt)("span",{parentName:"span",className:"pstrut",style:{height:"2.7em"}}),(0,s.kt)("span",{parentName:"span",className:"sizing reset-size6 size3 mtight"},(0,s.kt)("span",{parentName:"span",className:"mord mtight"},(0,s.kt)("span",{parentName:"span",className:"mord mathnormal mtight"},"n"))))),(0,s.kt)("span",{parentName:"span",className:"vlist-s"},"\u200b")),(0,s.kt)("span",{parentName:"span",className:"vlist-r"},(0,s.kt)("span",{parentName:"span",className:"vlist",style:{height:"0.345em"}},(0,s.kt)("span",{parentName:"span"}))))),(0,s.kt)("span",{parentName:"span",className:"mclose delimcenter",style:{top:"0em"}},(0,s.kt)("span",{parentName:"span",className:"delimsizing size1"},")")))),(0,s.kt)("span",{parentName:"span",className:"mord text"},(0,s.kt)("span",{parentName:"span",className:"mord"},"\xa0where\xa0")),(0,s.kt)("span",{parentName:"span",className:"mord mathnormal",style:{marginRight:"0.03148em"}},"k"),(0,s.kt)("span",{parentName:"span",className:"mspace",style:{marginRight:"0.2778em"}}),(0,s.kt)("span",{parentName:"span",className:"mrel"},"<"),(0,s.kt)("span",{parentName:"span",className:"mspace",style:{marginRight:"0.2778em"}})),(0,s.kt)("span",{parentName:"span",className:"base"},(0,s.kt)("span",{parentName:"span",className:"strut",style:{height:"0.4306em"}}),(0,s.kt)("span",{parentName:"span",className:"mord mathnormal"},"n")))))," are trivially fruitless."),(0,s.kt)("p",null,"Once the planner has constructed a lazy list of batches, it then consideres every plan that ",(0,s.kt)("em",{parentName:"p"},"could")," exist for every batch, hence a computational difficulty of finding the ",(0,s.kt)("strong",{parentName:"p"},"best")," plan."),(0,s.kt)("admonition",{type:"info"},(0,s.kt)("p",{parentName:"admonition"},"If you want to understand the algorithm better, consider taking a look at the source code.")),(0,s.kt)("h3",{id:"converting-a-query-to-a-problem"},"Converting a query to a problem"),(0,s.kt)("p",null,"gql considers only resolvers when running query planning.\nEvery field that is traversed in a query is expanded to all the resolvers it consists such that it becomes a digraph."),(0,s.kt)("p",null,"As an example, consider the following instance:"),(0,s.kt)("pre",null,(0,s.kt)("code",{parentName:"pre",className:"language-scala"},'import gql._\nimport gql.dsl.all._\nimport gql.ast._\nimport gql.server.planner._\nimport gql.resolver._\nimport scala.concurrent.duration._\nimport cats.implicits._\nimport cats.effect._\nimport cats.effect.unsafe.implicits.global\n\ncase object Child\n\ndef wait[I](ms: Int) = Resolver.liftF[IO, I](_ => IO.sleep(50.millis))\n\nval schem = Schema.stateful{\n  Resolver.batch[IO, Unit, Int](_ => IO.sleep(10.millis) as Map(() -> 42)).flatMap{ b1 =>\n    Resolver.batch[IO, Unit, String](_ => IO.sleep(15.millis) as Map(() -> "42")).map{ b2 =>\n      implicit lazy val child: Type[IO, Child.type] = builder[IO, Child.type]{ b =>\n        b.tpe(\n          "Child",\n          "b1" -> b.from(wait(50) andThen b1.optional map (_.get)),\n          "b2" -> b.from(wait(100) andThen b2.optional map (_.get)),\n        )\n      }\n\n      SchemaShape.unit[IO](\n        builder[IO, Unit]{ b =>\n          b.fields(\n            "child" -> b.from(wait(42) as Child),\n            "b2" -> b.from(wait(25) andThen b2.optional map (_.get))\n          )\n        }\n      )\n    }\n  }\n}.unsafeRunSync()\n')),(0,s.kt)("p",null,"Now let's define our query and modify our schema so the planner logs:"),(0,s.kt)("pre",null,(0,s.kt)("code",{parentName:"pre",className:"language-scala"},'val qry = """\n  query {\n    child {\n      b1\n      b2\n    }\n    b2\n  }\n"""\n\nval withLoggedPlanner = schem.copy(planner = new Planner[IO] {\n  def plan(naive: NodeTree): IO[OptimizedDAG] =\n    schem.planner.plan(naive).map { output =>\n      println(output.show(ansiColors = false))\n      println(s"naive: ${output.totalCost}")\n      println(s"optimized: ${output.optimizedCost}")\n      output\n    }\n})\n')),(0,s.kt)("p",null,"And we plan for it inspect the result:"),(0,s.kt)("pre",null,(0,s.kt)("code",{parentName:"pre",className:"language-scala"},"def runQry() = {\n  Compiler[IO]\n    .compile(withLoggedPlanner, qry)\n    .traverse_{ case Application.Query(fa) => fa }\n    .unsafeRunSync()\n}\n\nrunQry()\n// name: Query_b2.compose-left.compose-left.compose-right, cost: 100.00, end: 100.00, batch: 2\n//              name: batch_1, cost: 100.00, end: 200.00, batch: 4\n//              >>>>>>>>>>>>>name: batch_1, cost: 100.00, end: 300.00, batch: 4\n// name: Query_child.compose-right.compose-left.compose-right.first.compose-right.compose-right, cost: 100.00, end: 100.00, batch: 1\n//              name: Child_b1.compose-left.compose-left.compose-right, cost: 100.00, end: 200.00, batch: 5\n//                           name: batch_0, cost: 100.00, end: 300.00, batch: 0\n//              name: Child_b2.compose-left.compose-left.compose-right, cost: 100.00, end: 200.00, batch: 3\n//                           name: batch_1, cost: 100.00, end: 300.00, batch: 4\n// \n// naive: 700.0\n// optimized: 600.0\n")),(0,s.kt)("p",null,"We can warm up the weights (statistics) a bit by running the query a few times:"),(0,s.kt)("pre",null,(0,s.kt)("code",{parentName:"pre",className:"language-scala"},"(0 to 10).toList.foreach(_ => runQry())\n")),(0,s.kt)("p",null,"Now we can see how the weights are assigned:"),(0,s.kt)("pre",null,(0,s.kt)("code",{parentName:"pre",className:"language-scala"},"runQry()\n// name: Query_b2.compose-left.compose-left.compose-right, cost: 50407.46, end: 50407.46, batch: 4\n//                  name: batch_1, cost: 15207.91, end: 65615.37, batch: 0\n//                  >>>>>>>>>>>>>>>>>name: batch_1, cost: 15207.91, end: 115967.64, batch: 0\n// name: Query_child.compose-right.compose-left.compose-right.first.compose-right.compose-right, cost: 50454.64, end: 50454.64, batch: 5\n//                  name: Child_b1.compose-left.compose-left.compose-right, cost: 50250.10, end: 100704.73, batch: 1\n//                                   name: batch_0, cost: 10348.00, end: 111052.73, batch: 2\n//                  name: Child_b2.compose-left.compose-left.compose-right, cost: 50305.10, end: 100759.73, batch: 3\n//                                   name: batch_1, cost: 15207.91, end: 115967.64, batch: 0\n// \n// naive: 242181.09090909088\n// optimized: 226973.18181818182\n")),(0,s.kt)("p",null,"Plans can also be shown nicely in a terminal with ANSI colors:\n",(0,s.kt)("img",{alt:"Terminal output",src:t(1745).Z,width:"1144",height:"333"})))}c.isMDXComponent=!0},1745:(e,a,t)=>{t.d(a,{Z:()=>n});const n=t.p+"assets/images/plan_image-bacfe186ade480842758a5d754111dfd.png"}}]);