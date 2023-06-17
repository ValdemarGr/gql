import React from 'react';
import clsx from 'clsx';
import styles from './styles.module.css';
import CodeBlock from '@theme/CodeBlock';

type Feature = {
  title: string,
  description: JSX.Element,
  code: JSX.Element, 
  Svg?: React.FunctionComponent<React.SVGProps<SVGSVGElement>>
}

const FeatureList: Feature[] = [
  /*{
    title: 'Declarative',
    Svg: require('@site/static/img/undraw_docusaurus_mountain.svg').default,
    description: (
      <>
        Docusaurus was designed from the ground up to be easily installed and
        used to get your website up and running quickly.
      </>
    ),
  },*/
  {
    title: 'Powerful algebraic resolvers',
    // Svg: require('@site/static/img/undraw_docusaurus_tree.svg').default,
    description: (
      <>
        gql distills what it means to be a GraphQL resolver into a concise, well-behaved algebra that composes.
      </>
    ),
    code: (
      <CodeBlock language='scala'>
        {`"friends" -> resolve(_
  .evalMap(getFriends)
  .streamMap(is => peopleEvents(is.map(_.id)))
  .rethrow
  .arg(limitArg) andThen batchGetPeople
)`}
      </CodeBlock>
    )
  },
  {
    title: 'Declarative schema definition',
    //Svg: require('@site/static/img/undraw_docusaurus_react.svg').default,
    description: (
      <>
        gql comes with syntax and a DSL for succinctly defining schemas.
      </>
    ),
    code: (
      <CodeBlock language='scala'>
        {`tpe[IO, Person](
  "Person",
  "name" -> lift(_.name),
  "friends" -> eff(p => getFriends(p.id))
)`}
      </CodeBlock>
    )
  },
  {
    title: 'Typed functional graphql',
    // Svg: require('@site/static/img/undraw_docusaurus_mountain.svg').default,
    description: (
      <>
        gql adopts a simple and predictable approach to GraphQL.
        Every aspect has been crafted to minimize friction by employing the proper constructions.
      </>
    ),
    code: (
      <CodeBlock language='scala'>
        {`(
  arg[String]("firstName"),
  arg[String]("lastName")
).mapN(_ + " " + _)`}
      </CodeBlock>
    )
  },
  {
    title: 'Query planning',
    // Svg: require('@site/static/img/undraw_docusaurus_mountain.svg').default,
    description: (
      <>
        gql features a query planner heuristic that enables better-than-naive query performance and an expressive batching api that helps the user optimize their schema in a complely typed functional manner.
      </>
    ),
    code: (
      <CodeBlock language='scala'>
        {`slowFields ::: fastFields.contramap(...)`}
      </CodeBlock>
    )
  },
  {
    title: 'Signal based subscriptions',
    // Svg: require('@site/static/img/undraw_docusaurus_mountain.svg').default,
    description: (
      <>
        gql features an unusual subscription model that is instead based on signals.
        That is, streams or even resources of data can appear <b>anywhere</b> in the schema and gql will efficiently re-execute the query and handle resource leasing.
      </>
    ),
    code: (
      <CodeBlock language='scala'>
        {`"data" -> resolve(_
  .streamMap(subscribeToIds)
  .andThen(batchGetData)
  .streamMap(subscribeToSubIds)
)`}
      </CodeBlock>
    )
  },
  {
    title: 'Easy to extend',
    // Svg: require('@site/static/img/undraw_docusaurus_mountain.svg').default,
    description: (
      <>
        gql is designed to be easily extended with new features.
        Want to provide a custom query planner or calculate cost estimates yourself? No problem.
        <br/>
        <br/>
        gql also comes with some pre-built extensions such as http4s integration, graphql-ws, tracing and global object identification.
      </>
    ),
    code: (
      <CodeBlock language='scala'>
        {`gql.http4s.Http4sRoutes.ws(queryCompiler, _)`}
      </CodeBlock>
    )
  },
  {
    title: 'Client-side dsl',
    // Svg: require('@site/static/img/undraw_docusaurus_mountain.svg').default,
    description: (
      <>
      gql also features a client which can either be declared via the dsl or code generated from a graphql query.
      <br/>
      <br/>
      gql is modular and as such, client queries can be validated against the same implementation rules as a gql server.
      </>
    ),
    code: (
      <CodeBlock language='scala'>
        {`fragment("PersonFragment", "Person") {
  (
    sel[String]("name"),
    sel[Option[Int]]("age")
  ).mapN(Person.apply)
}`}
      </CodeBlock>
    )
  },
];

function Feature({ Svg, title, description, code }: Feature) {
  const showSvg = Svg === undefined ? <></> : <Svg className={styles.featureSvg} role="img" />;
  return (
    <div className={clsx('col col--4')}>
      <div className="text--center">
        {showSvg}
      </div>
      <div className="text--center padding-horiz--md">
        <h3>{title}</h3>
      </div>
      {code}
      <div className="text--center padding-horiz--md">
        <p>{description}</p>
      </div>
    </div>
  );
}

export default function HomepageFeatures() {
  return (
    <section className={styles.features}>
    <div className="container">
      <div className="row">
        {FeatureList.map((props, idx) => (
          <Feature key={idx} {...props} />
        ))}
      </div>
    </div>
  </section>
  );
}
