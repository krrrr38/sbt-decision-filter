package com.krrrr38.decisionfilter.internal.json

import scala.util.{Failure, Success}

import sjsonnew.shaded.scalajson.ast.unsafe._
import sjsonnew.support.scalajson.unsafe.{CompactPrinter, Parser, PrettyPrinter}

/** [[JsonBackend]] on sjson-new, which sbt 1.x and 2.x both ship (for their caches and server), so it adds no
  * dependency to the build using the plugin. The parts used here are the same in both.
  */
private[json] object SjsonNewBackend extends JsonBackend {

  def parse(input: String): Either[Json.ParseError, Json] =
    Parser.parseFromString(input) match {
      case Success(v) => Right(fromAst(v))
      case Failure(e) => Left(Json.ParseError(String.valueOf(e.getMessage)))
    }

  def render(json: Json, pretty: Boolean): String = {
    val ast = toAst(json)
    if (pretty) PrettyPrinter(ast) else CompactPrinter(ast)
  }

  private def fromAst(v: JValue): Json = v match {
    case JNull       => Json.JNull
    case b: JBoolean => Json.JBool(b.get)
    case n: JNumber  => Json.JNum(BigDecimal(n.value))
    case s: JString  => Json.JStr(s.value)
    case a: JArray   => Json.JArr(a.value.toVector.map(fromAst))
    case o: JObject  => Json.JObj(o.value.toVector.map(f => f.field -> fromAst(f.value)))
  }

  private def toAst(j: Json): JValue = j match {
    case Json.JNull     => JNull
    case Json.JBool(v)  => if (v) JTrue else JFalse
    case Json.JNum(v)   => JNumber(numberText(v))
    case Json.JStr(v)   => JString(v)
    case Json.JArr(vs)  => JArray(vs.map(toAst).toArray)
    case Json.JObj(fs) => JObject(fs.map { case (k, v) => JField(k, toAst(v)) }.toArray)
  }

  /** Integers without a fraction ("21400", not "21400.0"); everything else as BigDecimal prints it. */
  private def numberText(v: BigDecimal): String = {
    val s = v.bigDecimal.stripTrailingZeros
    if (s.scale <= 0 && s.precision - s.scale <= 18) s.toBigIntegerExact.toString else s.toString
  }
}
