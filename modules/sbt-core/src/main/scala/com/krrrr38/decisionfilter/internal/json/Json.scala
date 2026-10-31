package com.krrrr38.decisionfilter.internal.json

/** A minimal JSON AST, parser and renderer. Kept dependency-free on purpose: this file is compiled into both
  * the test runtime (users' test classpath) and the sbt plugins.
  *
  * Compiles on Scala 2.12, 2.13 and 3.
  */
sealed abstract class Json extends Product with Serializable {
  def render: String = Json.render(this, pretty = false)
  def pretty: String = Json.render(this, pretty = true)

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

  def obj(fields: (String, Json)*): JObj = JObj(fields.toVector)
  def arr(items: Json*): JArr = JArr(items.toVector)
  def str(v: String): Json = JStr(v)
  def num(v: Double): Json = JNum(BigDecimal(v))
  def num(v: Long): Json = JNum(BigDecimal(v))
  def bool(v: Boolean): Json = JBool(v)
  def opt[A](v: Option[A])(f: A => Json): Json = v.fold[Json](JNull)(f)
  def strs(vs: Iterable[String]): Json = JArr(vs.map(v => JStr(v): Json).toVector)

  final case class ParseError(message: String, offset: Int)

  def parse(input: String): Either[ParseError, Json] =
    try {
      val p = new Parser(input)
      p.skipWs()
      val v = p.value()
      p.skipWs()
      if (!p.atEnd) Left(ParseError("trailing characters", p.pos)) else Right(v)
    } catch {
      case e: Parser.Failure => Left(ParseError(e.getMessage, e.offset))
    }

  def render(json: Json, pretty: Boolean): String = {
    val sb = new java.lang.StringBuilder
    write(sb, json, if (pretty) 0 else -1)
    sb.toString
  }

  private def write(sb: java.lang.StringBuilder, json: Json, indent: Int): Unit = {
    def newline(level: Int): Unit =
      if (indent >= 0) {
        sb.append('\n')
        var i = 0
        while (i < level) { sb.append("  "); i += 1 }
      }
    json match {
      case JNull     => sb.append("null")
      case JBool(v)  => sb.append(if (v) "true" else "false")
      case JNum(v)   => sb.append(renderNumber(v))
      case JStr(v)   => quote(sb, v)
      case JArr(vs) if vs.isEmpty => sb.append("[]")
      case JArr(vs) =>
        sb.append('[')
        var first = true
        vs.foreach { v =>
          if (!first) sb.append(',')
          first = false
          newline(indent + 1)
          write(sb, v, if (indent >= 0) indent + 1 else -1)
        }
        newline(indent)
        sb.append(']')
      case JObj(fs) if fs.isEmpty => sb.append("{}")
      case JObj(fs) =>
        sb.append('{')
        var first = true
        fs.foreach { case (k, v) =>
          if (!first) sb.append(',')
          first = false
          newline(indent + 1)
          quote(sb, k)
          sb.append(if (indent >= 0) ": " else ":")
          write(sb, v, if (indent >= 0) indent + 1 else -1)
        }
        newline(indent)
        sb.append('}')
    }
  }

  private def renderNumber(v: BigDecimal): String = {
    val s = v.bigDecimal.stripTrailingZeros
    if (s.scale <= 0 && s.precision - s.scale <= 18) s.toBigIntegerExact.toString
    else s.toString
  }

  private def quote(sb: java.lang.StringBuilder, s: String): Unit = {
    sb.append('"')
    var i = 0
    while (i < s.length) {
      val c = s.charAt(i)
      c match {
        case '"'  => sb.append("\\\"")
        case '\\' => sb.append("\\\\")
        case '\n' => sb.append("\\n")
        case '\r' => sb.append("\\r")
        case '\t' => sb.append("\\t")
        case '\b' => sb.append("\\b")
        case '\f' => sb.append("\\f")
        case _ if c < 0x20 => sb.append(f"\\u${c.toInt}%04x")
        case _    => sb.append(c)
      }
      i += 1
    }
    sb.append('"')
  }

  private object Parser {
    final class Failure(msg: String, val offset: Int) extends RuntimeException(msg)
  }

  private final class Parser(s: String) {
    var pos = 0

    def atEnd: Boolean = pos >= s.length
    private def fail(msg: String): Nothing = throw new Parser.Failure(msg, pos)
    private def peek: Char = if (atEnd) fail("unexpected end of input") else s.charAt(pos)

    def skipWs(): Unit =
      while (!atEnd && { val c = s.charAt(pos); c == ' ' || c == '\n' || c == '\r' || c == '\t' }) pos += 1

    private def expect(c: Char): Unit =
      if (peek != c) fail(s"expected '$c'") else pos += 1

    private def literal(word: String, v: Json): Json =
      if (s.startsWith(word, pos)) { pos += word.length; v }
      else fail(s"expected $word")

    def value(): Json = peek match {
      case '{'                          => obj()
      case '['                          => arr()
      case '"'                          => JStr(string())
      case 't'                          => literal("true", JBool(true))
      case 'f'                          => literal("false", JBool(false))
      case 'n'                          => literal("null", JNull)
      case c if c == '-' || c.isDigit   => number()
      case c                            => fail(s"unexpected character '$c'")
    }

    private def obj(): Json = {
      expect('{')
      skipWs()
      val b = Vector.newBuilder[(String, Json)]
      if (peek == '}') { pos += 1; return JObj(b.result()) }
      var more = true
      while (more) {
        skipWs()
        val k = string()
        skipWs()
        expect(':')
        skipWs()
        b += (k -> value())
        skipWs()
        peek match {
          case ',' => pos += 1
          case '}' => pos += 1; more = false
          case _   => fail("expected ',' or '}'")
        }
      }
      JObj(b.result())
    }

    private def arr(): Json = {
      expect('[')
      skipWs()
      val b = Vector.newBuilder[Json]
      if (peek == ']') { pos += 1; return JArr(b.result()) }
      var more = true
      while (more) {
        skipWs()
        b += value()
        skipWs()
        peek match {
          case ',' => pos += 1
          case ']' => pos += 1; more = false
          case _   => fail("expected ',' or ']'")
        }
      }
      JArr(b.result())
    }

    private def string(): String = {
      expect('"')
      val sb = new java.lang.StringBuilder
      var done = false
      while (!done) {
        val c = peek
        pos += 1
        c match {
          case '"' => done = true
          case '\\' =>
            val e = peek
            pos += 1
            e match {
              case '"'  => sb.append('"')
              case '\\' => sb.append('\\')
              case '/'  => sb.append('/')
              case 'b'  => sb.append('\b')
              case 'f'  => sb.append('\f')
              case 'n'  => sb.append('\n')
              case 'r'  => sb.append('\r')
              case 't'  => sb.append('\t')
              case 'u' =>
                if (pos + 4 > s.length) fail("bad unicode escape")
                val hex = s.substring(pos, pos + 4)
                try sb.append(Integer.parseInt(hex, 16).toChar)
                catch { case _: NumberFormatException => fail("bad unicode escape") }
                pos += 4
              case other => fail(s"bad escape '\\$other'")
            }
          case _ => sb.append(c)
        }
      }
      sb.toString
    }

    private def number(): Json = {
      val start = pos
      if (peek == '-') pos += 1
      while (!atEnd && { val c = s.charAt(pos); c.isDigit || c == '.' || c == 'e' || c == 'E' || c == '+' || c == '-' })
        pos += 1
      val text = s.substring(start, pos)
      try JNum(BigDecimal(text))
      catch { case _: NumberFormatException => fail(s"bad number '$text'") }
    }
  }
}
