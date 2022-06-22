package gql

object conversions {
  // implicit def outputScalarForCodec[F[_], A](implicit sc: ScalarCodec[A]): Output.Scalar[F, A] =
  //   Output.Scalar[F, A](sc)

  implicit def inputScalarForCodec[A](implicit sc: ScalarCodec[A]): Input.Scalar[A] =
    Input.Scalar(sc)
}
