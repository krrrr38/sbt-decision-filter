package com.krrrr38.decisionfilter.select

import java.nio.file.{FileSystems, PathMatcher, Paths}

/** Changed files that make selection pointless: build definitions, CI config and the like (spec 6.2). */
final class RunAllTrigger private (patterns: Vector[(String, PathMatcher)]) {

  /** The first changed file that matches, with the pattern it matched. */
  def firstMatch(changedFiles: Iterable[String]): Option[(String, String)] =
    changedFiles.iterator.flatMap { f =>
      val p = Paths.get(f)
      patterns.collectFirst { case (glob, m) if m.matches(p) => (f, glob) }
    }.toSeq.headOption
}

object RunAllTrigger {

  val DefaultPatterns: Vector[String] = Vector("**.sbt", "project/**", ".jvmopts", ".sbtopts", ".gitlab-ci.yml")

  /** Fails on an invalid glob, which is a configuration error. */
  def apply(globs: Seq[String]): Either[String, RunAllTrigger] = {
    val fs = FileSystems.getDefault
    val compiled = globs.toVector.map { g =>
      try Right(g -> fs.getPathMatcher("glob:" + g))
      catch { case e: Exception => Left(s"invalid glob '$g': ${e.getMessage}") }
    }
    compiled.collectFirst { case Left(e) => e } match {
      case Some(e) => Left(e)
      case None    => Right(new RunAllTrigger(compiled.collect { case Right(m) => m }))
    }
  }
}
