package ncreep.tracing

import ncreep.util.inspectTree

@main def test(): Unit =
  inspectTree:
    trait TraitToBeTraced1:
      private val t: Tracer = Tracer.make[TraitToBeTraced1]

      def theMethodToBeTraced1(arg1: String, arg2: Int, arg3: SomeData, arg4: UntraceableData): String =
        if arg2 > 2 then "hello"
        else "bye"

      def methodNotTraced(arg1: String, arg2: Int): Boolean
    end TraitToBeTraced1
end test
