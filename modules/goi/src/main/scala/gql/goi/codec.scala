package gql.goi

object codec {
  val string: IDCodec[String] = IDCodec.stringInstance

  val int: IDCodec[Int] = IDCodec.intInstance

  val long: IDCodec[Long] = IDCodec.longInstance

  val float: IDCodec[Float] = IDCodec.floatInstance

  val double: IDCodec[Double] = IDCodec.doubleInstance

  val boolean: IDCodec[Boolean] = IDCodec.booleanInstance

  val uuid: IDCodec[java.util.UUID] = IDCodec.uuidInstance

  val date: IDCodec[java.time.LocalDate] = IDCodec.dateInstance
}
