package com.krrrr38.decisionfilter.select

/** One file's part of a unified diff.
  *
  * @param path the path after the change (the old path for a deleted file)
  * @param oldPath the path before the change when it differs (rename, delete)
  * @param text this file's part of the diff, verbatim
  * @param changedLines lines of the new file that were added, plus the lines either side of each deletion
  */
final case class FileDiff(
    path: String,
    oldPath: Option[String],
    text: String,
    changedLines: Set[Int],
    binary: Boolean,
    deleted: Boolean
) {
  def paths: Vector[String] = (path +: oldPath.toVector).distinct
}

object DiffParser {

  private val HunkHeader = """^@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@.*$""".r

  def parse(diff: String): Vector[FileDiff] =
    splitFiles(diff).map(parseFile)

  /** Splits on `diff --git` lines, keeping each file's text intact (including its trailing newline). */
  private def splitFiles(diff: String): Vector[Vector[String]] = {
    val lines = splitKeepingNewlines(diff)
    val out = Vector.newBuilder[Vector[String]]
    var current = Vector.newBuilder[String]
    var started = false
    // Lines left in the current hunk, so body lines starting with "diff --git" are never taken as headers.
    var oldLeft = 0
    var newLeft = 0
    lines.foreach { raw =>
      val line = stripNewline(raw)
      if (oldLeft <= 0 && newLeft <= 0 && line.startsWith("diff --git ")) {
        if (started) out += current.result()
        current = Vector.newBuilder[String]
        started = true
        current += raw
      } else if (started) {
        current += raw
        if (oldLeft > 0 || newLeft > 0) {
          if (line.startsWith("+")) newLeft -= 1
          else if (line.startsWith("-")) oldLeft -= 1
          else if (line.startsWith("\\")) ()
          else { oldLeft -= 1; newLeft -= 1 }
        } else
          line match {
            case HunkHeader(_, oldCount, _, newCount) =>
              oldLeft = Option(oldCount).map(_.toInt).getOrElse(1)
              newLeft = Option(newCount).map(_.toInt).getOrElse(1)
            case _ => ()
          }
      }
    }
    if (started) out += current.result()
    out.result()
  }

  private def parseFile(rawLines: Vector[String]): FileDiff = {
    val lines = rawLines.map(stripNewline)
    val text = rawLines.mkString

    var minusPath: Option[String] = None
    var plusPath: Option[String] = None
    var renameTo: Option[String] = None
    var renameFrom: Option[String] = None
    var binary = false
    var deleted = false
    val changed = scala.collection.mutable.Set.empty[Int]

    var i = 1 // skip "diff --git"
    var oldLeft = 0
    var newLeft = 0
    var newLine = 0
    while (i < lines.length) {
      val line = lines(i)
      if (oldLeft > 0 || newLeft > 0) {
        if (line.startsWith("+")) {
          changed += newLine
          newLine += 1
          newLeft -= 1
        } else if (line.startsWith("-")) {
          // A deletion sits between two lines of the new file; mark both neighbours.
          if (newLine > 1) changed += newLine - 1
          changed += newLine
          oldLeft -= 1
        } else if (line.startsWith("\\")) {
          ()
        } else {
          newLine += 1
          oldLeft -= 1
          newLeft -= 1
        }
      } else
        line match {
          case HunkHeader(_, oldCount, newStart, newCount) =>
            oldLeft = Option(oldCount).map(_.toInt).getOrElse(1)
            newLeft = Option(newCount).map(_.toInt).getOrElse(1)
            newLine = newStart.toInt
          case l if l.startsWith("--- ") =>
            minusPath = stripPrefix(l.substring(4), "a/")
          case l if l.startsWith("+++ ") =>
            plusPath = stripPrefix(l.substring(4), "b/")
          case l if l.startsWith("rename from ") => renameFrom = Some(l.substring("rename from ".length))
          case l if l.startsWith("rename to ")   => renameTo = Some(l.substring("rename to ".length))
          case l if l.startsWith("deleted file mode") => deleted = true
          case l if l.startsWith("Binary files ") || l == "GIT binary patch" => binary = true
          case _ => ()
        }
      i += 1
    }

    val header = headerPaths(lines.headOption.getOrElse(""))
    val newPath = plusPath.orElse(renameTo).orElse(header.map(_._2))
    val oldP = minusPath.orElse(renameFrom).orElse(header.map(_._1))
    val path = (if (deleted) oldP else newPath).orElse(oldP).getOrElse("")
    val old = oldP.filter(_ != path)
    FileDiff(path, old, text, changed.toSet, binary, deleted)
  }

  /** `--- a/x` / `+++ b/x`; `/dev/null` means the file is absent on that side. */
  private def stripPrefix(s: String, prefix: String): Option[String] = {
    val p = s.takeWhile(_ != '\t')
    if (p == "/dev/null") None
    else if (p.startsWith(prefix)) Some(p.substring(prefix.length))
    else Some(p)
  }

  /** `diff --git a/x b/y`. Ambiguous when paths contain " b/", so only used as a fallback (binary, mode change). */
  private def headerPaths(line: String): Option[(String, String)] = {
    val rest = line.stripPrefix("diff --git ")
    if (!rest.startsWith("a/")) None
    else {
      // For an unchanged path the two halves are equal, which disambiguates the split.
      val body = rest.substring(2)
      val candidates = indexesOf(body, " b/")
      val equal = candidates.find(i => body.substring(0, i) == body.substring(i + 3))
      equal.orElse(candidates.lastOption).map(i => (body.substring(0, i), body.substring(i + 3)))
    }
  }

  private def indexesOf(s: String, needle: String): Vector[Int] = {
    val b = Vector.newBuilder[Int]
    var i = s.indexOf(needle)
    while (i >= 0) {
      b += i
      i = s.indexOf(needle, i + 1)
    }
    b.result()
  }

  private def splitKeepingNewlines(s: String): Vector[String] = {
    val b = Vector.newBuilder[String]
    var start = 0
    var i = s.indexOf('\n')
    while (i >= 0) {
      b += s.substring(start, i + 1)
      start = i + 1
      i = s.indexOf('\n', start)
    }
    if (start < s.length) b += s.substring(start)
    b.result()
  }

  private def stripNewline(s: String): String =
    if (s.endsWith("\r\n")) s.substring(0, s.length - 2)
    else if (s.endsWith("\n")) s.substring(0, s.length - 1)
    else s
}
