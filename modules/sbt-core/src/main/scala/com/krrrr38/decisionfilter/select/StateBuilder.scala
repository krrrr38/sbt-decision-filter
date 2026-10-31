package com.krrrr38.decisionfilter.select

import scala.util.matching.Regex

/** Builds the state sent to the backend (spec 6.5). */
object StateBuilder {

  val DefaultDiffLimit: Int = 96000

  val Redacted: String = "[REDACTED]"

  /** Patterns for values that look like secrets. Users can replace the list with `decisionFilterRedactPatterns`. */
  val DefaultRedactPatterns: Vector[String] = Vector(
    // AWS access key ids
    """\b(?:AKIA|ASIA)[0-9A-Z]{16}\b""",
    // PEM private keys
    """-----BEGIN [A-Z ]*PRIVATE KEY-----[\s\S]*?-----END [A-Z ]*PRIVATE KEY-----""",
    // Credentials embedded in URLs: scheme://user:password@host
    """(?<=://)[^/\s:@]+:[^/\s@]+(?=@)""",
    // key = "value" style assignments of things named like secrets
    """(?i)(?<=(?:password|passwd|secret|token|api[_-]?key|access[_-]?key)["']?\s{0,3}[:=]\s{0,3}["']?)[^\s"']{6,}"""
  )

  def compile(patterns: Seq[String]): Either[String, Vector[Regex]] = {
    val compiled = patterns.toVector.map { p =>
      try Right(new Regex(p))
      catch { case e: Exception => Left(s"invalid redact pattern '$p': ${e.getMessage}") }
    }
    compiled.collectFirst { case Left(e) => e } match {
      case Some(e) => Left(e)
      case None    => Right(compiled.collect { case Right(r) => r })
    }
  }

  def redact(text: String, patterns: Seq[Regex]): String =
    patterns.foldLeft(text)((acc, r) => r.replaceAllIn(acc, Regex.quoteReplacement(Redacted)))

  /** Keeps the diff under `limit` characters by dropping whole files, smallest first kept, so that as many files
    * as possible survive. Kept files stay in their original order. The stat always covers everything.
    */
  def build(files: Vector[FileDiff], stat: String, redactPatterns: Seq[Regex], limit: Int = DefaultDiffLimit): DiffState = {
    val texts = files.map(f => redact(f.text, redactPatterns))
    val total = texts.map(_.length).sum
    val changed = files.flatMap(_.paths).distinct
    val redactedStat = redact(stat, redactPatterns)
    if (total <= limit) DiffState(changed, redactedStat, texts.mkString, truncated = false, Vector.empty)
    else {
      val bySize = texts.indices.sortBy(i => (texts(i).length, i))
      var used = 0
      val keep = scala.collection.mutable.Set.empty[Int]
      bySize.foreach { i =>
        val len = texts(i).length
        if (used + len <= limit) {
          keep += i
          used += len
        }
      }
      val kept = texts.indices.filter(keep.contains).map(texts(_)).mkString
      val omitted = files.indices.filterNot(keep.contains).map(files(_).path).toVector
      DiffState(changed, redactedStat, kept, truncated = true, omitted)
    }
  }
}
