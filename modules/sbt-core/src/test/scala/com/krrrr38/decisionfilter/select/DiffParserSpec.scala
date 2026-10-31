package com.krrrr38.decisionfilter.select

import org.scalatest.funsuite.AnyFunSuite

class DiffParserSpec extends AnyFunSuite {

  test("added, removed and context lines") {
    val diff =
      """diff --git a/src/A.scala b/src/A.scala
        |index 1111111..2222222 100644
        |--- a/src/A.scala
        |+++ b/src/A.scala
        |@@ -1,5 +1,5 @@
        | line1
        |-line2
        |+line2 changed
        | line3
        | line4
        |-line5
        |+line5 changed
        |@@ -20,3 +20,2 @@ def foo
        | a
        |-b
        | c
        |""".stripMargin
    val Vector(f) = DiffParser.parse(diff)
    assert(f.path == "src/A.scala")
    assert(f.oldPath.isEmpty)
    assert(f.text == diff)
    // 2 and 5 were replaced (deletion neighbours 1-2 and 4-5); in the second hunk "b" was removed between 20 and 21.
    assert(f.changedLines == Set(1, 2, 4, 5, 20, 21))
  }

  test("a body line starting with +++ or --- is content, not a header") {
    val diff =
      """diff --git a/notes.md b/notes.md
        |--- a/notes.md
        |+++ b/notes.md
        |@@ -1,2 +1,2 @@
        |---- old rule
        |+++++ new rule
        | keep
        |diff --git a/other.txt b/other.txt
        |new file mode 100644
        |--- /dev/null
        |+++ b/other.txt
        |@@ -0,0 +1 @@
        |+diff --git a/x b/x
        |""".stripMargin
    val files = DiffParser.parse(diff)
    assert(files.map(_.path) == Vector("notes.md", "other.txt"))
    assert(files(0).changedLines == Set(1))
    assert(files(1).changedLines == Set(1))
  }

  test("deleted, renamed and binary files") {
    val diff =
      """diff --git a/gone.scala b/gone.scala
        |deleted file mode 100644
        |--- a/gone.scala
        |+++ /dev/null
        |@@ -1 +0,0 @@
        |-x
        |diff --git a/old name.scala b/new name.scala
        |similarity index 90%
        |rename from old name.scala
        |rename to new name.scala
        |diff --git a/img.png b/img.png
        |index 1..2 100644
        |Binary files a/img.png and b/img.png differ
        |""".stripMargin
    val files = DiffParser.parse(diff)
    assert(files.map(f => (f.path, f.oldPath, f.deleted, f.binary)) == Vector(
      ("gone.scala", None, true, false),
      ("new name.scala", Some("old name.scala"), false, false),
      ("img.png", None, false, true)
    ))
    assert(files(1).paths == Vector("new name.scala", "old name.scala"))
  }

  test("an empty diff has no files") {
    assert(DiffParser.parse("").isEmpty)
  }
}
