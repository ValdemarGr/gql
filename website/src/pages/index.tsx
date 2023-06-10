import React from 'react';
import clsx from 'clsx';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import HomepageFeatures from '@site/src/components/HomepageFeatures';

import styles from './index.module.css';

function HomepageHeader() {
  const {siteConfig} = useDocusaurusContext();
  return (
    <header className={clsx('hero hero--primary', styles.heroBanner)}>
      <div className="container">
        <h1 className="hero__title">{siteConfig.title}</h1>
        <p className="hero__subtitle">{siteConfig.tagline}</p>
        {/*<p className="hero__subtitle">
        gql is a GraphQL implementation.
        gql comes with many of the standard features of a GraphQL implementation such as a dsl, parser and interpreter.
        But also some unique features such as, herustic query planning and signals.
        The most important goals of gql is to be simple, predictable and composable.  
  </p>*/}
        <div className={styles.buttons}>
          <Link
            className="button button--secondary button--lg"
            to="/docs/gql">
            Documentation
    {/*Docusaurus Tutorial - 5min ⏱️*/}
          </Link>
        </div>
      </div>
    </header>
  );
}

export default function Home() {
  const {siteConfig} = useDocusaurusContext();
  return (
    <Layout
      // title={`gql`}//`Hello from ${siteConfig.title}`}
      description="Scala GraphQL DSL">
      <HomepageHeader />
      <main>
        <HomepageFeatures />
      </main>
    </Layout>
  );
}
