"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[287],{3905:(e,t,r)=>{r.d(t,{Zo:()=>l,kt:()=>d});var n=r(7294);function a(e,t,r){return t in e?Object.defineProperty(e,t,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[t]=r,e}function c(e,t){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),r.push.apply(r,n)}return r}function i(e){for(var t=1;t<arguments.length;t++){var r=null!=arguments[t]?arguments[t]:{};t%2?c(Object(r),!0).forEach((function(t){a(e,t,r[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):c(Object(r)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(r,t))}))}return e}function o(e,t){if(null==e)return{};var r,n,a=function(e,t){if(null==e)return{};var r,n,a={},c=Object.keys(e);for(n=0;n<c.length;n++)r=c[n],t.indexOf(r)>=0||(a[r]=e[r]);return a}(e,t);if(Object.getOwnPropertySymbols){var c=Object.getOwnPropertySymbols(e);for(n=0;n<c.length;n++)r=c[n],t.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(e,r)&&(a[r]=e[r])}return a}var s=n.createContext({}),p=function(e){var t=n.useContext(s),r=t;return e&&(r="function"==typeof e?e(t):i(i({},t),e)),r},l=function(e){var t=p(e.components);return n.createElement(s.Provider,{value:t},e.children)},u={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},m=n.forwardRef((function(e,t){var r=e.components,a=e.mdxType,c=e.originalType,s=e.parentName,l=o(e,["components","mdxType","originalType","parentName"]),m=p(r),d=a,f=m["".concat(s,".").concat(d)]||m[d]||u[d]||c;return r?n.createElement(f,i(i({ref:t},l),{},{components:r})):n.createElement(f,i({ref:t},l))}));function d(e,t){var r=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var c=r.length,i=new Array(c);i[0]=m;var o={};for(var s in t)hasOwnProperty.call(t,s)&&(o[s]=t[s]);o.originalType=e,o.mdxType="string"==typeof e?e:a,i[1]=o;for(var p=2;p<c;p++)i[p]=r[p];return n.createElement.apply(null,i)}return n.createElement.apply(null,r)}m.displayName="MDXCreateElement"},3481:(e,t,r)=>{r.r(t),r.d(t,{assets:()=>s,contentTitle:()=>i,default:()=>u,frontMatter:()=>c,metadata:()=>o,toc:()=>p});var n=r(7462),a=(r(7294),r(3905));const c={title:"Natchez (tracing)"},i=void 0,o={unversionedId:"server/integrations/natchez",id:"server/integrations/natchez",title:"Natchez (tracing)",description:"The natchez package provides functions to trace your query execution and planning.",source:"@site/docs/server/integrations/natchez.md",sourceDirName:"server/integrations",slug:"/server/integrations/natchez",permalink:"/gql/docs/server/integrations/natchez",draft:!1,editUrl:"https://github.com/facebook/docusaurus/tree/main/packages/create-docusaurus/templates/shared/docs/server/integrations/natchez.md",tags:[],version:"current",frontMatter:{title:"Natchez (tracing)"},sidebar:"docs",previous:{title:"GraphQL-WS",permalink:"/gql/docs/server/integrations/graphqlws"},next:{title:"Query DSL",permalink:"/gql/docs/client/dsl"}},s={},p=[],l={toc:p};function u(e){let{components:t,...r}=e;return(0,a.kt)("wrapper",(0,n.Z)({},l,r,{components:t,mdxType:"MDXLayout"}),(0,a.kt)("p",null,"The natchez package provides functions to trace your query execution and planning."),(0,a.kt)("p",null,"The tracing functions include information such as the query plan and query in case of an invalid query."),(0,a.kt)("p",null,"The easiest way to add tracing to your app is by tracing the schema via ",(0,a.kt)("inlineCode",{parentName:"p"},"traceSchema")," and incoming queries via ",(0,a.kt)("inlineCode",{parentName:"p"},"traceQuery"),".\nFor instance, consider the following tracing implementation for a http server:"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-scala"},"import natchez._\nimport gql._\nimport gql.natchez.NatchezTracer\nimport cats.effect.{Trace => _, _}\nimport gql.http4s.Http4sRoutes\n\nimplicit def trace: Trace[IO] = ???\n\ndef schema: Schema[IO, Unit, Unit, Unit] = ???\n\ndef tracedSchema = NatchezTracer.traceSchema(schema)\n\ndef traceAndRunHttpRequest(qp: QueryParameters) =\n    NatchezTracer.traceQuery[IO](qp.query, qp.variables.getOrElse(Map.empty), qp.operationName)(\n        Compiler[IO].compileWith(tracedSchema, qp)\n    )\n\ndef routes = Http4sRoutes.syncSimple[IO](traceAndRunHttpRequest(_).map(Right(_)))\n")))}u.isMDXComponent=!0}}]);