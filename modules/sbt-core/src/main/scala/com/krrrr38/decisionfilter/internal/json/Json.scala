package com.krrrr38.decisionfilter.internal.json

/** The JSON AST the plugins build and read. Parsing and rendering go through [[JsonBackend]], so the library doing
  * the work can be swapped in one place ([[JsonBackend.current]]) without touching any caller.
  */
sealed abstract class Json extends Product with Serializable {
  def render: String = JsonBackend.current.render(this, pretty = false)
  def pretty: String = JsonBackend.current.render(this, pretty = true)

  def field(key: String): Option[Json] = this match {
    case Json.JObj(fields) => fields.collectFirst { case (k, v) if k == key => v }
    case _                 => None
  }
  def asString: Option[String] = this match {
    case Json.JStr(v) => Some(v)
    case _            => None
  }
  def asNumber: Option[BigDecimal] = this match {
    case Json.JNum(v) => Some(v)
    case _            => None
  }
  def asDouble: Option[Double] = asNumber.map(_.toDouble)
  def asBoolean: Option[Boolean] = this match {
    case Json.JBool(v) => Some(v)
    case _             => None
  }
  def asArray: Option[Vector[Json]] = this match {
    case Json.JArr(v) => Some(v)
    case _            => None
  }
  def asObject: Option[Vector[(String, Json)]] = this match {
    case Json.JObj(v) => Some(v)
    case _            => None
  }
}

object Json {
  case object JNull extends Json
  final case class JBool(value: Boolean) extends Json
  final case class JNum(value: BigDecimal) extends Json
  final case class JStr(value: String) extends Json
  final case class JArr(items: Vector[Json]) extends Json
  final case class JObj(fields: Vector[(String, Json)]) extends Json

  final case class ParseError(message: String)

  def parse(input: String): Either[ParseError, Json] = JsonBackend.current.parse(input)

  def obj(fields: (String, Json)*): JObj = JObj(fields.toVector)
  def arr(items: Json*): JArr = JArr(items.toVector)
  def str(v: String): Json = JStr(v)
  def num(v: Double): Json = JNum(BigDecimal(v))
  def num(v: Long): Json = JNum(BigDecimal(v))
  def bool(v: Boolean): Json = JBool(v)
  def opt[A](v: Option[A])(f: A => Json): Json = v.fold[Json](JNull)(f)
  def strs(vs: Iterable[String]): Json = JArr(vs.map(v => JStr(v): Json).toVector)
}

/** Parses and renders [[Json]]. Implement this to use another JSON library. */
trait JsonBackend {
  def parse(input: String): Either[Json.ParseError, Json]
  def render(json: Json, pretty: Boolean): String
}

object JsonBackend {

  /** The one place that decides which library parses and renders. */
  private[decisionfilter] val current: JsonBackend = SjsonNewBackend
}
