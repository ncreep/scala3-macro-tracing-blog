package ncreep.tracing

import scala.reflect.ClassTag

trait Tracer:
  def start(level: TraceLevel, id: String, fields: (String, String)*): Unit

  def finish(level: TraceLevel, id: String, fields: (String, String)*): Unit

  def isLevelEnabled(level: TraceLevel): Boolean

object Tracer:
  // creating a tracer for a specific class, the class name
  // is going to appear in the tracing messages
  def make[A](
      using classTag: ClassTag[A]): Tracer = new Tracer:

    private def makeMessage(
        stage: String,
        level: TraceLevel,
        id: String,
        fields: Seq[(String, String)]): String =
      val levelName = level.toString.toLowerCase
      val fieldsString = fields.map((name, value) => s"$name: $value").mkString(", ")
      val className = classTag.runtimeClass.getSimpleName

      s"[$levelName][$className]: *$stage* $id $fieldsString"

    def start(level: TraceLevel, id: String, fields: (String, String)*): Unit =
      println(makeMessage("start", level, id, fields))

    def finish(level: TraceLevel, id: String, fields: (String, String)*): Unit =
      println(makeMessage("finish", level, id, fields))

    // for demonstration purposes we want to always show tracing messages
    def isLevelEnabled(level: TraceLevel): Boolean = true

enum TraceLevel:
  case Debug, Info, Warn, Error

trait ToTrace[A]:
  def toTraceString(value: A): String

object ToTrace:

  inline def apply[A](
      using ToTrace[A]): ToTrace[A] = summon[ToTrace[A]]

  given ToTrace[String] with
    def toTraceString(value: String): String = value

  given ToTrace[Int] with
    def toTraceString(value: Int): String = value.toString

  given ToTrace[Throwable] with
    def toTraceString(value: Throwable): String = value.getMessage

  given [A](
      using ToTrace[A]): ToTrace[Option[A]] with
    def toTraceString(value: Option[A]): String =
      value.map(ToTrace[A].toTraceString).getOrElse("EmptyOption")

  given [A](
      using ToTrace[A]): ToTrace[Iterable[A]] with
    def toTraceString(value: Iterable[A]): String =
      value.map(ToTrace[A].toTraceString).mkString(",")
