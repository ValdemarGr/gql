package gql.goi

import cats.implicits._

object codec {
  def enumCodec[A <: enumeratum.EnumEntry](e: enumeratum.Enum[A], name: String): IDCodec[A] =
    IDCodec.make(s => e.withNameEither(s).leftMap(_.getMessage()), _.entryName, name)

  val string: IDCodec[String] = IDCodec.stringInstance

  val int: IDCodec[Int] = IDCodec.intInstance

  val long: IDCodec[Long] = IDCodec.longInstance

  val float: IDCodec[Float] = IDCodec.floatInstance

  val double: IDCodec[Double] = IDCodec.doubleInstance

  val boolean: IDCodec[Boolean] = IDCodec.booleanInstance

  val uuid: IDCodec[java.util.UUID] = IDCodec.uuidInstance

  val localDate: IDCodec[java.time.LocalDate] = IDCodec.dateInstance
}
