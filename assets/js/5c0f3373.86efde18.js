"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[317],{3905:(e,t,n)=>{n.d(t,{Zo:()=>s,kt:()=>m});var a=n(7294);function r(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function p(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){r(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function i(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},o=Object.keys(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var l=a.createContext({}),c=function(e){var t=a.useContext(l),n=t;return e&&(n="function"==typeof e?e(t):p(p({},t),e)),n},s=function(e){var t=c(e.components);return a.createElement(l.Provider,{value:t},e.children)},u={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},d=a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,o=e.originalType,l=e.parentName,s=i(e,["components","mdxType","originalType","parentName"]),d=c(n),m=r,y=d["".concat(l,".").concat(m)]||d[m]||u[m]||o;return n?a.createElement(y,p(p({ref:t},s),{},{components:n})):a.createElement(y,p({ref:t},s))}));function m(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var o=n.length,p=new Array(o);p[0]=d;var i={};for(var l in t)hasOwnProperty.call(t,l)&&(i[l]=t[l]);i.originalType=e,i.mdxType="string"==typeof e?e:r,p[1]=i;for(var c=2;c<o;c++)p[c]=n[c];return a.createElement.apply(null,p)}return a.createElement.apply(null,n)}d.displayName="MDXCreateElement"},6944:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>l,contentTitle:()=>p,default:()=>u,frontMatter:()=>o,metadata:()=>i,toc:()=>c});var a=n(7462),r=(n(7294),n(3905));const o={title:"Output types"},p=void 0,i={unversionedId:"output_types",id:"output_types",title:"Output types",description:"An output type Out[F[_], A] is a node in the graph that can take some A as input and produce a graphql value in F.",source:"@site/docs/output_types.md",sourceDirName:".",slug:"/output_types",permalink:"/gql/docs/output_types",draft:!1,editUrl:"https://github.com/facebook/docusaurus/tree/main/packages/create-docusaurus/templates/shared/docs/output_types.md",tags:[],version:"current",frontMatter:{title:"Output types"},sidebar:"docs",previous:{title:"Getting started",permalink:"/gql/docs/test"},next:{title:"Context",permalink:"/gql/docs/context"}},l={},c=[{value:"Scalar",id:"scalar",level:2},{value:"Enum",id:"enum",level:2}],s={toc:c};function u(e){let{components:t,...n}=e;return(0,r.kt)("wrapper",(0,a.Z)({},s,n,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("p",null,"An output type ",(0,r.kt)("inlineCode",{parentName:"p"},"Out[F[_], A]")," is a node in the graph that can take some ",(0,r.kt)("inlineCode",{parentName:"p"},"A")," as input and produce a graphql value in ",(0,r.kt)("inlineCode",{parentName:"p"},"F"),".\nThe output types of gql are defined in ",(0,r.kt)("inlineCode",{parentName:"p"},"gql.ast")," and are named after their respective GraphQL types."),(0,r.kt)("h2",{id:"scalar"},"Scalar"),(0,r.kt)("p",null,(0,r.kt)("inlineCode",{parentName:"p"},"Scalar")," types are composed of a name and a codec\nThe ",(0,r.kt)("inlineCode",{parentName:"p"},"Scalar")," type can encode ",(0,r.kt)("inlineCode",{parentName:"p"},"A => Json")," and decode ",(0,r.kt)("inlineCode",{parentName:"p"},"Json => Either[Error, A]"),".\ngql comes with a few predefined scalars, but you can also define your own."),(0,r.kt)("p",null,"For instance, the ",(0,r.kt)("inlineCode",{parentName:"p"},"ID")," type is defined for any ",(0,r.kt)("inlineCode",{parentName:"p"},"Scalar")," as follows:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'import cats.implicits._\n\nimport gql.ast.{stringScalar, Scalar}\n\nfinal case class ID[A](value: A)\nimplicit def idScalar[F[_], A](implicit inner: Scalar[F, A]): Scalar[F, ID[A]] =\n  Scalar("ID", inner.codec.imap(ID(_))(_.value))\n')),(0,r.kt)("h2",{id:"enum"},"Enum"),(0,r.kt)("p",null,(0,r.kt)("inlineCode",{parentName:"p"},"Enum")," types, like ",(0,r.kt)("inlineCode",{parentName:"p"},"Scalar")," types, are terminal types that consist of a name and non-empty bi-directional mapping from a scala type to a ",(0,r.kt)("inlineCode",{parentName:"p"},"String"),":"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'import cats.data._\nimport gql.ast.Enum\n\nsealed trait Color\nobject Color {\n  case object Red extends Color\n  case object Green extends Color\n  case object Blue extends Color\n}\n\nimplicit def color[F[_]] = \n  Enum(\n    "Color",\n    NonEmptyList.of(\n      "RED" -> Color.Red,\n      "GREEN" -> Color.Green,\n      "BLUE" -> Color.Blue\n    )\n  )\n')),(0,r.kt)("p",null,(0,r.kt)("inlineCode",{parentName:"p"},"Enum")," types have no constraints on the values they can encode or decode, so they can in fact, be dynamically typed:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-scala"},'import cats.data._\nimport gql.ast.Enum\n\nfinal case class UntypedEnum(s: String)\n\nimplicit def untypedEnum[F[_]] = \n  Enum(\n    "UntypedEnum",\n    NonEmptyList.of(\n      "FIRST" -> UntypedEnum("FIRST")\n    )\n  )\n')))}u.isMDXComponent=!0}}]);