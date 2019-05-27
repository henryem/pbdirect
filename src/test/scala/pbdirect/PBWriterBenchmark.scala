package pbdirect

import org.scalameter.api._

object PBWriterBenchmark extends Bench.LocalTime {

  private def make5DeepTuple[V](rootValue: V): Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[V]]]]] = {
    Tuple1(Tuple1(Tuple1(Tuple1(Tuple1(rootValue)))))
  }

  private def make10DeepTuple[V](rootValue: V): Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[V]]]]]]]]]] = {
    make5DeepTuple(make5DeepTuple(rootValue))
  }

  private def make20DeepTuple[V](rootValue: V): Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[V]]]]]]]]]]]]]]]]]]]] = {
    make10DeepTuple(make10DeepTuple(rootValue))
  }

  private def make40DeepTuple[V](rootValue: V): Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[V]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]] = {
    make20DeepTuple(make20DeepTuple(rootValue))
  }

  private val sizes = Gen.range("size")(100000, 1000000, 100000)

  private val fiveDeep = for (size <- sizes) yield make5DeepTuple(Array.fill(size)(1.toByte))
  private val tenDeep = for (size <- sizes) yield make10DeepTuple(Array.fill(size)(1.toByte))
  private val twentyDeep = for (size <- sizes) yield make20DeepTuple(Array.fill(size)(1.toByte))
  private val fortyDeep = for (size <- sizes) yield make40DeepTuple(Array.fill(size)(1.toByte))

  performance of "5-Deep Tuple" in {
    measure method "toPB" in {
      using(fiveDeep) in {
        d => d.toPB
      }
    }
  }

  performance of "10-Deep Tuple" in {
    measure method "toPB" in {
      using(tenDeep) in {
        d => d.toPB
      }
    }
  }

  performance of "20-Deep Tuple" in {
    measure method "toPB" in {
      using(twentyDeep) in {
        d => d.toPB
      }
    }
  }

  performance of "40-Deep Tuple" in {
    measure method "toPB" in {
      using(fortyDeep) in {
        d => d.toPB
      }
    }
  }
}
