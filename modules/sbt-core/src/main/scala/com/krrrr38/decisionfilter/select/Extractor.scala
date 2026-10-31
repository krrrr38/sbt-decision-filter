package com.krrrr38.decisionfilter.select

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scala.meta._
import scala.meta.inputs.Input
import scala.meta.parsers.{Parse, Parsed}

/** A module's test sources, as sbt reports them. */
final case class ModuleSources(module: String, testSourceDirs: Seq[File], scalaVersion: String)

final case class ExtractResult(suites: Vector[TestSuite], parseFailures: Vector[(String, String)])

/** Finds ScalaTest test classes by parsing sources. Nothing is compiled or loaded.
  *
  * A class that is not found here is unknown to the selection and therefore always runs.
  */
object Extractor {

  /** @param repoRoot paths in the result are relative to this directory
    * @param include which files to look at (relative path), e.g. only those git tracks
    */
  def extract(modules: Seq[ModuleSources], repoRoot: File, include: String => Boolean): ExtractResult = {
    val root = repoRoot.getCanonicalFile.toPath
    val parsed = modules.toVector.flatMap { m =>
      val dialect = dialectFor(m.scalaVersion)
      m.testSourceDirs.toVector.distinct.flatMap(d => scalaFiles(d)).distinct.flatMap { f =>
        val rel = root.relativize(f.getCanonicalFile.toPath).toString.replace(File.separatorChar, '/')
        if (!include(rel)) Vector.empty
        else Vector(parseFile(m.module, f, rel, dialect))
      }
    }
    ExtractResult(suitesOf(parsed.collect { case Right(u) => u }), parsed.collect { case Left(f) => f })
  }

  /** Extracts from sources given as text, for tests. Each entry is (module, relative path, source, scalaVersion). */
  def extractSources(sources: Seq[(String, String, String, String)]): ExtractResult = {
    val parsed = sources.toVector.map { case (module, rel, text, scalaVersion) =>
      parseText(module, rel, text, dialectFor(scalaVersion))
    }
    ExtractResult(suitesOf(parsed.collect { case Right(u) => u }), parsed.collect { case Left(f) => f })
  }

  def dialectFor(scalaVersion: String): Dialect =
    if (scalaVersion.startsWith("3")) dialects.Scala3
    else if (scalaVersion.startsWith("2.12")) dialects.Scala212Source3
    else dialects.Scala213Source3

  /** Test names listed per class in a question, at most. They are hints for the model, not identifiers. */
  val MaxTestNames: Int = 50

  // ---------------------------------------------------------------------------------------------------------------
  // Parsing

  private final case class TemplateInfo(
      module: String,
      file: String,
      simpleName: String,
      fqn: String,
      isConcreteClass: Boolean,
      parents: Vector[String],
      stats: List[Stat]
  )

  private def scalaFiles(dir: File): Vector[File] =
    if (!dir.isDirectory) Vector.empty
    else {
      val stream = Files.walk(dir.toPath)
      try {
        val b = Vector.newBuilder[File]
        val it = stream.iterator()
        while (it.hasNext) {
          val p = it.next()
          if (p.toString.endsWith(".scala") && Files.isRegularFile(p)) b += p.toFile
        }
        b.result().sortBy(_.getPath)
      } finally stream.close()
    }

  private def parseFile(module: String, f: File, rel: String, dialect: Dialect) = {
    val text =
      try Right(new String(Files.readAllBytes(f.toPath), StandardCharsets.UTF_8))
      catch { case e: java.io.IOException => Left((rel, e.toString)) }
    text.flatMap(t => parseText(module, rel, t, dialect))
  }

  private def parseText(
      module: String,
      rel: String,
      text: String,
      dialect: Dialect
  ): Either[(String, String), Vector[TemplateInfo]] =
    try
      Parse.parseSource(Input.VirtualFile(rel, text), dialect) match {
        case ok: Parsed.Success[_] =>
          ok.tree match {
            case source: Source => Right(collectTemplates(module, rel, source))
            case other          => Left((rel, s"unexpected parse result ${other.getClass.getName}"))
          }
        case e: Parsed.Error => Left((rel, e.message))
      }
    catch { case e: Exception => Left((rel, e.toString)) }

  /** Top-level classes, traits and objects. sbt does not run nested classes or objects as ScalaTest suites. */
  private def collectTemplates(module: String, file: String, source: Source): Vector[TemplateInfo] = {
    val b = Vector.newBuilder[TemplateInfo]
    def walk(stats: List[Stat], pkg: String): Unit = stats.foreach {
      case p: Pkg =>
        val name = refName(p.ref)
        walk(p.body.stats, if (pkg.isEmpty) name else s"$pkg.$name")
      case c: Defn.Class =>
        val isAbstract = c.mods.exists(_.isInstanceOf[Mod.Abstract])
        b += info(module, file, pkg, c.name.value, concrete = !isAbstract, c.templ)
      case t: Defn.Trait  => b += info(module, file, pkg, t.name.value, concrete = false, t.templ)
      case o: Defn.Object => b += info(module, file, pkg, o.name.value, concrete = false, o.templ)
      case _              => ()
    }
    walk(source.stats, "")
    b.result()
  }

  private def refName(r: Term.Ref): String = r match {
    case n: Term.Name => n.value
    case s: Term.Select =>
      s.qual match {
        case q: Term.Ref => s"${refName(q)}.${s.name.value}"
        case _           => s.name.value
      }
    case _ => ""
  }

  private def info(module: String, file: String, pkg: String, name: String, concrete: Boolean, templ: Template) =
    TemplateInfo(
      module,
      file,
      name,
      if (pkg.isEmpty) name else s"$pkg.$name",
      concrete,
      templ.inits.toVector.flatMap(i => typeName(i.tpe)),
      templ.body.stats
    )

  private def typeName(t: Type): Option[String] = t match {
    case n: Type.Name     => Some(n.value)
    case s: Type.Select   => Some(s.name.value)
    case a: Type.Apply    => typeName(a.tpe)
    case a: Type.Annotate => typeName(a.tpe)
    case _                => None
  }

  // ---------------------------------------------------------------------------------------------------------------
  // Suites

  /** A concrete class whose parents lead to a ScalaTest style, resolving in-project parents by simple name. */
  private def suitesOf(files: Vector[Vector[TemplateInfo]]): Vector[TestSuite] = {
    val all = files.flatten
    val index: Map[String, Vector[TemplateInfo]] = all.groupBy(_.simpleName)

    def resolve(t: TemplateInfo, seen: Set[String]): (Set[Style], Vector[TemplateInfo]) =
      t.parents.foldLeft((Set.empty[Style], Vector.empty[TemplateInfo])) { case ((styles, ancestors), parent) =>
        Style.byName.get(parent) match {
          case Some(style)                   => (styles + style, ancestors)
          case None if seen.contains(parent) => (styles, ancestors)
          case None =>
            index.getOrElse(parent, Vector.empty).foldLeft((styles, ancestors)) { case ((s, a), p) =>
              val (ps, pa) = resolve(p, seen + parent)
              (s ++ ps, (a :+ p) ++ pa)
            }
        }
      }

    all.filter(_.isConcreteClass).flatMap { t =>
      val (styles, ancestors) = resolve(t, Set(t.simpleName))
      if (styles.isEmpty) None
      else {
        val names = styles.toVector.flatMap(style => (ancestors :+ t).flatMap(a => TestNames.of(style, a.stats))).distinct
        Some(
          TestSuite(
            module = t.module,
            suiteId = t.fqn,
            file = t.file,
            baseFiles = ancestors.map(_.file).distinct.filter(_ != t.file),
            testNames = names.take(MaxTestNames)
          )
        )
      }
    }
  }

  // ---------------------------------------------------------------------------------------------------------------
  // Styles

  sealed abstract class Style(val name: String) extends Product with Serializable {

    /** Concrete style classes and their `*Like` traits, plain, async and fixture flavours. */
    def typeNames: Seq[String] =
      Seq(s"Any$name", s"Async$name", s"FixtureAny$name", s"FixtureAsync$name").flatMap(n => Seq(n, n + "Like"))
  }
  object Style {
    case object FunSuite extends Style("FunSuite")
    case object FunSpec extends Style("FunSpec")
    case object WordSpec extends Style("WordSpec")
    case object FlatSpec extends Style("FlatSpec")
    case object FreeSpec extends Style("FreeSpec")
    case object PropSpec extends Style("PropSpec")
    case object FeatureSpec extends Style("FeatureSpec")

    val all: Vector[Style] = Vector(FunSuite, FunSpec, WordSpec, FlatSpec, FreeSpec, PropSpec, FeatureSpec)
    val byName: Map[String, Style] = all.flatMap(s => s.typeNames.map(_ -> s)).toMap
  }

  /** Best-effort test names, following ScalaTest's naming per style. A name that is not a string literal shows as
    * `<dynamic>`; anything not understood is skipped. Only used to tell the model what a class tests.
    */
  private object TestNames {
    private val Dynamic = "<dynamic>"

    def of(style: Style, stats: List[Stat]): Vector[String] = {
      val b = Vector.newBuilder[String]
      style match {
        case Style.FlatSpec => flat(stats, b)
        case _              => nested(style, stats, Vector.empty, b)
      }
      b.result()
    }

    private def flattenApply(t: Term): (Term, List[List[Term]]) = t match {
      case a: Term.Apply =>
        val (f, argss) = flattenApply(a.fun)
        (f, argss :+ a.argClause.values)
      case other => (other, Nil)
    }

    private def applyName(t: Term): Option[(String, List[List[Term]])] = flattenApply(t) match {
      case (n: Term.Name, argss) if argss.size >= 2 && argss.head.nonEmpty => Some((n.value, argss))
      case _                                                               => None
    }

    private def text(t: Term): String = t match {
      case Lit.String(v) => v
      case _             => Dynamic
    }

    private def stripTagged(t: Term): Term = t match {
      case i: Term.ApplyInfix if i.op.value == "taggedAs" => stripTagged(i.lhs)
      case other                                          => other
    }

    private def statsOf(t: Term): List[Stat] = t match {
      case b: Term.Block => b.stats
      case other         => List(other)
    }

    private val TestWords: Map[Style, Set[String]] = Map(
      Style.FunSuite -> Set("test", "ignore"),
      Style.FunSpec -> Set("it", "they", "ignore"),
      Style.PropSpec -> Set("property", "ignore"),
      Style.FeatureSpec -> Set("Scenario", "scenario", "ignore")
    )

    private def nested(style: Style, stats: List[Stat], prefix: Vector[String], b: VectorBuilderOf): Unit = {
      def add(name: String): Unit = b += (prefix :+ name).mkString(" ")
      stats.foreach {
        case term: Term =>
          (style, term) match {
            case (Style.WordSpec | Style.FreeSpec, i: Term.ApplyInfix) =>
              val subject = stripTagged(i.lhs)
              (i.op.value, i.argClause.values) match {
                case ("-", List(blk: Term.Block)) if style == Style.FreeSpec =>
                  nested(style, blk.stats, prefix :+ text(subject), b)
                case (op @ ("should" | "must" | "can" | "when" | "which" | "that"), List(blk: Term.Block))
                    if style == Style.WordSpec =>
                  nested(style, blk.stats, prefix :+ s"${text(subject)} $op", b)
                case ("in" | "ignore" | "is", _) if !subject.isInstanceOf[Term.ApplyInfix] => add(text(subject))
                case _                                                                     => ()
              }
            case (_, a: Term.Apply) =>
              applyName(a) match {
                case Some(("describe", argss)) if style == Style.FunSpec =>
                  nested(style, argss.last.flatMap(statsOf), prefix :+ text(argss.head.head), b)
                case Some((("Feature" | "feature"), argss)) if style == Style.FeatureSpec =>
                  nested(style, argss.last.flatMap(statsOf), prefix :+ s"Feature: ${text(argss.head.head)}", b)
                case Some((word, argss)) if TestWords.get(style).exists(_.contains(word)) =>
                  add(if (style == Style.FeatureSpec) s"Scenario: ${text(argss.head.head)}" else text(argss.head.head))
                case _ => ()
              }
            case _ => ()
          }
        case _ => ()
      }
    }

    private def flat(stats: List[Stat], b: VectorBuilderOf): Unit = {
      var subject = Dynamic
      stats.foreach {
        case i: Term.ApplyInfix if i.op.value == "of" && (i.lhs match {
              case n: Term.Name => n.value == "behavior"; case _ => false
            }) =>
          i.argClause.values.headOption.foreach(a => subject = text(a))
        case i: Term.ApplyInfix if Set("in", "ignore", "is").contains(i.op.value) =>
          stripTagged(i.lhs) match {
            case v: Term.ApplyInfix if Set("should", "must", "can").contains(v.op.value) =>
              v.lhs match {
                case n: Term.Name if Set("it", "they", "ignore").contains(n.value) => ()
                case other                                                          => subject = text(other)
              }
              b += s"$subject ${v.op.value} ${v.argClause.values.headOption.map(text).getOrElse(Dynamic)}"
            case _ => ()
          }
        case _ => ()
      }
    }

    private type VectorBuilderOf = scala.collection.mutable.Builder[String, Vector[String]]
  }
}
