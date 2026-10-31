package com.krrrr38.decisionfilter.select

import java.io.{ByteArrayOutputStream, File, InputStream}
import java.nio.charset.StandardCharsets

/** Thin wrapper over the git CLI. */
final class Git(workDir: File) {
  import Git.Result

  def run(args: String*): Result = {
    val cmd = new java.util.ArrayList[String]()
    cmd.add("git")
    // Keep paths unquoted so non-ASCII file names come through as they are.
    cmd.add("-c")
    cmd.add("core.quotePath=false")
    args.foreach(a => cmd.add(a))
    val pb = new ProcessBuilder(cmd).directory(workDir)
    pb.environment().put("GIT_PAGER", "cat")
    pb.environment().put("LC_ALL", "C")
    val p = pb.start()
    p.getOutputStream.close()
    val err = new StreamCollector(p.getErrorStream)
    err.start()
    val out = readAll(p.getInputStream)
    val code = p.waitFor()
    err.join()
    Result(code, out, err.result)
  }

  /** stdout, or None when git failed. */
  def output(args: String*): Option[String] = {
    val r = run(args: _*)
    if (r.ok) Some(r.stdout) else None
  }

  private def readAll(in: InputStream): String = {
    val buf = new ByteArrayOutputStream()
    val chunk = new Array[Byte](8192)
    var n = in.read(chunk)
    while (n >= 0) {
      buf.write(chunk, 0, n)
      n = in.read(chunk)
    }
    in.close()
    new String(buf.toByteArray, StandardCharsets.UTF_8)
  }

  private final class StreamCollector(in: InputStream) extends Thread {
    setDaemon(true)
    @volatile var result: String = ""
    override def run(): Unit = result = readAll(in)
  }
}

object Git {
  final case class Result(exitCode: Int, stdout: String, stderr: String) {
    def ok: Boolean = exitCode == 0
  }
}

/** What the selector needs from the repository. */
final case class RepoDiff(
    base: String,
    mergeBase: String,
    head: String,
    diff: String,
    stat: String,
    trackedFiles: Set[String]
)

object RepoDiff {

  /** Resolves the base (see spec 6.1) and takes the merge-base-to-working-tree diff.
    *
    * The diff covers committed, staged and unstaged changes to tracked files. Files that were never added to git
    * are ignored entirely.
    */
  def collect(git: Git, configuredBase: Option[String], env: Env): Either[String, RepoDiff] = {
    val base = resolveBase(git, configuredBase, env)
    for {
      mergeBase <- git
        .output("merge-base", base, "HEAD")
        .map(_.trim)
        .filter(_.nonEmpty)
        .toRight(s"cannot resolve merge-base with $base")
      head <- git.output("rev-parse", "HEAD").map(_.trim).toRight("cannot resolve HEAD")
      diff <- git
        .output("diff", "--no-color", "--no-ext-diff", "--no-textconv", "-U3", mergeBase)
        .toRight(s"git diff against $base failed")
      stat <- git
        .output("diff", "--no-color", "--no-ext-diff", "--no-textconv", "--stat", mergeBase)
        .toRight(s"git diff --stat against $base failed")
      tracked <- git.output("ls-files", "-z").toRight("git ls-files failed")
    } yield RepoDiff(base, mergeBase, head, diff, stat, tracked.split('\u0000').filter(_.nonEmpty).toSet)
  }

  def resolveBase(git: Git, configuredBase: Option[String], env: Env): String =
    configuredBase
      .map(_.trim)
      .filter(_.nonEmpty)
      .orElse(env.first(Seq("DECISION_FILTER_BASE")).map(_._2))
      .orElse(env.first(Seq("CI_MERGE_REQUEST_DIFF_BASE_SHA")).map(_._2))
      .orElse(
        git
          .output("symbolic-ref", "--quiet", "refs/remotes/origin/HEAD")
          .map(_.trim.stripPrefix("refs/remotes/"))
          .filter(_.nonEmpty)
      )
      .getOrElse("origin/main")

  def topLevel(dir: File): Option[File] =
    new Git(dir).output("rev-parse", "--show-toplevel").map(s => new File(s.trim).getCanonicalFile)
}
