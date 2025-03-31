package ncreep.tracing

case class SomeData(aNumber: Int, sensitiveDoNotLog: String)

object SomeData:
  given ToTrace[SomeData] with
    def toTraceString(value: SomeData): String = s"SomeData(${value.aNumber}, <redacted>)"

case class UntraceableData(aChar: Char, aString: String)

