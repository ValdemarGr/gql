package gql.relational

import cats.effect._
import gql.ast._
import gql.dsl._
import cats.implicits._
import fs2.Pure
import cats._
import gql.resolver.Resolver
import cats.data._
import scala.reflect.ClassTag
import java.util.UUID
import gql.Arg
import gql.EmptyableArg
import gql.resolver.FieldMeta
import cats.mtl.Stateful
import cats.mtl.Tell
import gql.{preparation => prep}
import cats.mtl.Raise
import gql.Schema
import gql.SchemaShape
import gql.Application
import natchez.TraceValue
import natchez.Kernel
import natchez.Span
import java.net.URI
import cats.arrow.FunctionK

trait QueryInterpreter extends QueryAlgebra {
   
}
