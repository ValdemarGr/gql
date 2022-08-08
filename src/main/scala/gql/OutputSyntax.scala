package gql

import cats.implicits._
import cats.data._
import cats._
import shapeless.Lazy
import scala.reflect.ClassTag

abstract class OutputSyntax {
  def obj[F[_], A](
      name: String,
      hd: (String, Output.Fields.Field[F, A, _]),
      tl: (String, Output.Fields.Field[F, A, _])*
  ) = Output.Obj[F, A](name, NonEmptyList(hd, tl.toList))

  def union[F[_], A](
      name: String,
      hd: Output.Unification.Instance[F, A, _],
      tl: Output.Unification.Instance[F, A, _]*
  ) = Output.Union[F, A](
    name,
    NonEmptyList(hd, tl.toList).map(x => x.ol.name -> x.asInstanceOf[Output.Unification.Instance[F, A, Any]]).toNem
  )

  def interface[F[_], A](
      o: Output.Obj[F, A],
      hd: Output.Unification.Instance[F, A, _],
      tl: Output.Unification.Instance[F, A, _]*
  ) = Output.Interface[F, A](
    o.name,
    NonEmptyList(hd, tl.toList).map(x => x.ol.name -> x.asInstanceOf[Output.Unification.Instance[F, A, Any]]).toList.toMap,
    o.fields
  )

  // def instance[F[_], A, B <: A: ClassTag](ol: ObjectLike[F, B]): Output.Unification.Instance[F, A, B] =
  //   Output.Unification.Instance(ol)(Output.Unification.Specify.specifyForSubtype[A, B])

  // def contraInstance[F[_], A, B <: A: ClassTag, C](ol: ObjectLike[F, C], f: B => C): Output.Unification.Instance[F, A, C] =
  //   Output.Unification.Instance[F, A, C](ol)(Output.Unification.Specify.specifyForSubtype[A, B].map(f))

  def contra[B] = OutputSyntax.PartiallyAppliedContra[B]()

  def effect[F[_], I, T](resolver: I => F[T])(implicit tpe: => Output[F, T]): Output.Fields.Field[F, I, T] =
    Output.Fields.SimpleField[F, I, T](
      Output.Fields.DeferredResolution(resolver),
      Eval.later(tpe)
    )

  def effectArg[F[_], I, T, A](arg: Output.Fields.Arg[A])(resolver: (I, A) => F[T])(implicit
      tpe: => Output[F, T]
  ): Output.Fields.Field[F, I, T] =
    Output.Fields.ArgField[F, I, T, A](
      arg,
      Output.Fields.DeferredResolution { case (i, a) => resolver(i, a) },
      Eval.later(tpe)
    )

  def pure[F[_], I, T](resolver: I => T)(implicit tpe: => Output[F, T]): Output.Fields.Field[F, I, T] =
    Output.Fields.SimpleField[F, I, T](
      Output.Fields.PureResolution(resolver),
      Eval.later(tpe)
    )

  def pureArg[F[_], I, T, A](
      arg: Output.Fields.Arg[A]
  )(resolver: (I, A) => T)(implicit tpe: => Output[F, T]): Output.Fields.Field[F, I, T] =
    Output.Fields.ArgField[F, I, T, A](
      arg,
      Output.Fields.PureResolution { case (i, a) => resolver(i, a) },
      Eval.later(tpe)
    )

  def arg[A](name: String, default: Option[A] = None)(implicit tpe: Input[A]): Output.Fields.Arg[A] =
    Output.Fields.Arg.initial[A](Output.Fields.ArgParam(name, tpe, default))

  def batchResolver[F[_], K, T](batchName: String, resolver: Set[K] => F[Map[K, T]]): OutputSyntax.BatchResolver[F, K, T] =
    OutputSyntax.BatchResolver(batchName, resolver)

  def batchPure[F[_], I, T, K](batchRes: OutputSyntax.BatchResolver[F, K, T])(key: I => K)(implicit
      tpe: => Output[F, T],
      F: Applicative[F]
  ): Output.Fields.Field[F, I, T] = {
    implicit lazy val tpe2 = tpe
    batchEffect[F, I, T, K](batchRes)(key.andThen(F.pure))
  }

  def batchEffect[F[_]: Functor, I, T, K](batchRes: OutputSyntax.BatchResolver[F, K, T])(key: I => F[K])(implicit
      tpe: => Output[F, T]
  ): Output.Fields.Field[F, I, T] =
    Output.Fields.SimpleField[F, I, T](
      Output.Fields.BatchedResolution[F, I, T, K](batchRes.batchName, key.andThen(_.map(List(_))), batchRes.resolver),
      Eval.later(tpe)
    )

  def batchEffectG[F[_]: Functor, G[_]: Traverse, I, T, K](batchRes: OutputSyntax.BatchResolver[F, K, T])(key: I => F[G[K]])(implicit
      tpe: => Output[F, G[T]]
  ): Output.Fields.Field[F, I, G[T]] =
    Output.Fields.SimpleField[F, I, G[T]](
      Output.Fields
        .BatchedResolution[F, I, T, K](batchRes.batchName, key.andThen(_.map(_.toList)), batchRes.resolver)
        .asInstanceOf[Output.Fields.Resolution[F, I, G[T]]],
      Eval.later(tpe)
    )
}

object OutputSyntax {
  final case class BatchNode[F[_], I, K, A, T](
      batch: I => F[Batch[F, K, A, T]],
      resolver: BatchResolver[F, K, T]
  )

  final case class BatchResolver[F[_], K, T](
      batchName: String,
      resolver: Set[K] => F[Map[K, T]]
  )

  final case class Batch[F[_], K, A, T](
      keys: List[K],
      post: List[(K, T)] => F[A]
  )

  final case class Key(key: String)

  final case class InputData(data: String)

  final case class OutputData(data: String)

  final case class TestOutput(data: List[OutputData])

  final case class TestInput(data: List[InputData])

  def keysFromInputData[F[_]](id: InputData)(implicit F: Applicative[F]): F[List[Key]] =
    F.pure(id.data.split(" | ").toList.map(Key(_)))

  def getDataFromApi[F[_]](keys: Set[Key]): F[Map[Key, OutputData]] = ???

  def batchResolver[F[_]] = BatchResolver("mock", getDataFromApi[F])

  def mockBatchNode[F[_]](implicit F: Applicative[F]) =
    BatchNode[F, TestInput, Key, TestOutput, OutputData](
      _.data
        .flatTraverse(keysFromInputData[F])
        .map(keys => Batch(keys, xs => F.pure(TestOutput(xs.map { case (_, v) => v })))),
      batchResolver[F]
    )

  object Batch {
    implicit def applyForBatch[F[_]: Applicative, K, T] = {
      type G[A] = Batch[F, K, A, T]
      new Applicative[G] {
        override def pure[A](x: A): G[A] = Batch(List.empty, _ => x.pure[F])

        override def ap[A, B](ff: G[A => B])(fa: G[A]): G[B] =
          Batch(
            ff.keys ++ fa.keys,
            { m =>
              val f = ff.post(m.take(ff.keys.size))
              val a = fa.post(m.drop(ff.keys.size))
              f.ap(a)
            }
          )
      }
    }
  }

  case class PartiallyAppliedContra[B](val dummy: Boolean = false) extends AnyVal {
    def apply[F[_], A](pf: PartialFunction[A, B])(implicit ol: ObjectLike[F, B]): Output.Unification.Instance[F, A, B] =
      Output.Unification.Instance[F, A, B](ol)(Output.Unification.Specify.make(pf.lift))
  }
}
