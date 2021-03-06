package pbdirect

import org.scalatest.{Matchers, WordSpecLike}

class PBReaderSpec extends WordSpecLike with Matchers {
  "PBReader" should {
    "read a Boolean from Protobuf" in {
      case class BooleanMessage(value: Option[Boolean])
      val bytes = Array[Byte](8, 1)
      bytes.pbTo[BooleanMessage] shouldBe BooleanMessage(Some(true))
    }
    "read a Byte from Protobuf" in {
      case class ByteMessage(value: Option[Byte])
      val bytes = Array[Byte](8, 32)
      bytes.pbTo[ByteMessage] shouldBe ByteMessage(Some(32))
    }
    "read a Short from Protobuf" in {
      case class ShortMessage(value: Option[Short])
      val bytes = Array[Byte](8, -1, 63)
      bytes.pbTo[ShortMessage] shouldBe ShortMessage(Some(8191))
    }
    "read an Int from Protobuf" in {
      case class IntMessage(value: Option[Int])
      val bytes = Array[Byte](8, 5)
      bytes.pbTo[IntMessage] shouldBe IntMessage(Some(5))
    }
    "read a Long from Protobuf" in {
      case class LongMessage(value: Option[Long])
      val bytes = Array[Byte](8, -128, -128, -128, -128, 8)
      bytes.pbTo[LongMessage] shouldBe LongMessage(Some(Int.MaxValue.toLong + 1))
    }
    "read a Float from Protobuf" in {
      case class FloatMessage(value: Option[Float])
      val bytes = Array[Byte](13, -51, -52, 76, 62)
      bytes.pbTo[FloatMessage] shouldBe FloatMessage(Some(0.2F))
    }
    "read a Double from Protobuf" in {
      case class DoubleMessage(value: Option[Double])
      val bytes = Array[Byte](9, -107, 100, 121, -31, 127, -3, -75, 61)
      bytes.pbTo[DoubleMessage] shouldBe DoubleMessage(Some(0.00000000002D))
    }
    "read a String from Protobuf" in {
      case class StringMessage(value: Option[String])
      val bytes = Array[Byte](10, 5, 72, 101, 108, 108, 111)
      bytes.pbTo[StringMessage] shouldBe StringMessage(Some("Hello"))
    }
    "read bytes from Protobuf" in {
      case class BytesMessage(value: Option[Array[Byte]])
      val bytes = Array[Byte](10, 5, 72, 101, 108, 108, 111)
      bytes.pbTo[BytesMessage].value.get shouldBe Array[Byte](72, 101, 108, 108, 111)
    }
    "read an enumeration from Protobuf" in {
      case object Grade extends Enumeration {
        val GradeA, GradeB = Value
      }
      val bytesA = Array[Byte](8, 0)
      val bytesB = Array[Byte](8, 1)
      case class GradeMessage(value: Option[Grade.Value])
      bytesA.pbTo[GradeMessage] shouldBe GradeMessage(Some(Grade.GradeA))
      bytesB.pbTo[GradeMessage] shouldBe GradeMessage(Some(Grade.GradeB))
    }
    "read an enum from Protobuf" in {
      sealed trait Grade extends Pos
      case object GradeA extends Grade with Pos._0
      case object GradeB extends Grade with Pos._1
      case class GradeMessage(value: Option[Grade])
      val bytesA = Array[Byte](8, 0)
      val bytesB = Array[Byte](8, 1)
      bytesA.pbTo[GradeMessage] shouldBe GradeMessage(Some(GradeA))
      bytesB.pbTo[GradeMessage] shouldBe GradeMessage(Some(GradeB))
    }
    "read a required field from Protobuf" in {
      case class RequiredMessage(value: Int)
      val bytes = Array[Byte](8, 5)
      bytes.pbTo[RequiredMessage] shouldBe RequiredMessage(5)
    }
    "read an empty message from Protobuf" in {
      case class EmptyMessage()
      val bytes = Array[Byte]()
      bytes.pbTo[EmptyMessage] shouldBe EmptyMessage()
    }
    "read a message with single missing field from Protobuf" in {
      case class MissingMessage(text: Option[String])
      val bytes = Array[Byte]()
      bytes.pbTo[MissingMessage] shouldBe MissingMessage(None)
    }
    "read a multi-field message from Protobuf" in {
      case class MultiMessage(text: Option[String], number: Option[Int])
      val bytes = Array[Byte](10, 5, 72, 101, 108, 108, 111, 16, 3)
      bytes.pbTo[MultiMessage] shouldBe MultiMessage(Some("Hello"), Some(3))
    }
    "read a message with missing field from Protobuf" in {
      case class MissingMessage(text: Option[String], number: Option[Int])
      val bytes = Array[Byte](10, 5, 72, 101, 108, 108, 111)
      bytes.pbTo[MissingMessage] shouldBe MissingMessage(Some("Hello"), None)
    }
    "read a message with repeated field from Protobuf" in {
      case class RepeatedMessage(values: List[Int])
      val bytes = Array[Byte](8, 1, 8, 2, 8, 3, 8, 4)
      bytes.pbTo[RepeatedMessage] shouldBe RepeatedMessage(1 :: 2 :: 3 :: 4 :: Nil)
    }
    "read a message with Seq from Protobuf" in {
      case class RepeatedMessage(values: Seq[Int])
      val bytes = Array[Byte](8, 1, 8, 2, 8, 3, 8, 4)
      bytes.pbTo[RepeatedMessage] shouldBe RepeatedMessage(Seq(1, 2, 3, 4))
    }
    "read a message with IndexedSeq from Protobuf" in {
      case class RepeatedMessage(values: IndexedSeq[Int])
      val bytes = Array[Byte](8, 1, 8, 2, 8, 3, 8, 4)
      bytes.pbTo[RepeatedMessage] shouldBe RepeatedMessage(IndexedSeq(1, 2, 3, 4))
    }
    "read a Map from Protobuf" in {
      case class MapMessage(values: Map[Int, String])
      val bytes = Array[Byte](10, 7, 8, 1, 18, 3, 111, 110, 101, 10, 7, 8, 2, 18, 3, 116, 119, 111)
      bytes.pbTo[MapMessage] shouldBe MapMessage(Map(1 -> "one", 2 -> "two"))
    }
    "read a scala.collection.Map from Protobuf" in {
      case class MapMessage(values: collection.Map[Int, String])
      val bytes = Array[Byte](10, 7, 8, 1, 18, 3, 111, 110, 101, 10, 7, 8, 2, 18, 3, 116, 119, 111)
      bytes.pbTo[MapMessage] shouldBe MapMessage(collection.Map(1 -> "one", 2 -> "two"))
    }
    "read a nested message from Protobuf" in {
      case class InnerMessage(value: Option[Int])
      case class OuterMessage(text: Option[String], inner: Option[InnerMessage])
      val bytes = Array[Byte](10, 5, 72, 101, 108, 108, 111, 18, 2, 8, 11)
      bytes.pbTo[OuterMessage] shouldBe OuterMessage(Some("Hello"), Some(InnerMessage(Some(11))))
    }
    "read a sealed trait from Protobuf" in {
      sealed trait Message
      case class IntMessage(value: Option[Int]) extends Message
      case class StringMessage(value: Option[String]) extends Message
      val intBytes = Array[Byte](8, 5)
      val stringBytes = Array[Byte](10, 5, 72, 101, 108, 108, 111)
      intBytes.pbTo[Message] shouldBe IntMessage(Some(5))
      stringBytes.pbTo[Message] shouldBe StringMessage(Some("Hello"))
    }
    "read a nested sealed trait from Protobuf" in {
      sealed trait Message
      case class IntMessage(value: Option[Int]) extends Message
      case class StringMessage(value: Option[String]) extends Message
      case class NestedMessage(value: Message)
      val intBytes = Array[Byte](10, 2, 8, 5)
      intBytes.pbTo[NestedMessage] shouldBe NestedMessage(IntMessage(Some(5)))
      val stringBytes = Array[Byte](10, 7, 10, 5, 72, 101, 108, 108, 111)
      stringBytes.pbTo[NestedMessage] shouldBe NestedMessage(StringMessage(Some("Hello")))
    }
    "read a sealed trait with same repr from Protobuf" in {
      sealed trait Message
      case class M1(@Index(1) value: Int) extends Message
      case class M2(@Index(2) value: Int) extends Message
      val m1Bytes = Array[Byte](8, 1)
      val m2Bytes = Array[Byte](16, 2)
      m1Bytes.pbTo[Message] shouldBe M1(1)
      m2Bytes.pbTo[Message] shouldBe M2(2)
    }
    "read a message with repeated nested message from Protobuf" in {
      case class Metric(name: String, service: String, node: String, value: Float, count: Int)
      case class Metrics(metrics: List[Metric])
      val message = Metrics(
        Metric("metric", "microservices", "node", 12F, 12345) :: Nil
      )
      val bytes = Array[Byte](10, 37, 10, 6, 109, 101, 116, 114, 105, 99, 18, 13, 109, 105, 99, 114, 111, 115, 101, 114,
        118, 105, 99, 101, 115, 26, 4, 110, 111, 100, 101, 37, 0, 0, 64, 65, 40, -71, 96)
      bytes.pbTo[Metrics] shouldBe message
    }
    "read a message with nested repeated message from Protobuf" in {
      case class ListOfLists(v: List[ListOfInt])
      case class ListOfInt(v: List[Int])
      val message = ListOfLists(List(ListOfInt(List(1, 2)), ListOfInt(List(3, 4))))
      val bytes = Array[Byte](10, 4, 8, 1, 8, 2, 10, 4, 8, 3, 8, 4)
      bytes.pbTo[ListOfLists] shouldBe message
    }
    "read a message with many fields from Protobuf" in {
      case class Message(
        _1: Int,
        _2: Int,
        _3: Int,
        _4: Int,
        _5: Int,
        _6: Int,
        _7: Int,
        _8: Int,
        _9: Int,
        _10: Int,
        _11: Int,
        _12: Int,
        _13: Int,
        _14: Int,
        _15: Int,
        _16: Int,
        _17: Int,
        _18: Int,
        _19: Int,
        _20: Int,
        _21: Int,
        _22: Int,
        _23: Int,
        _24: Int,
        _25: Int,
        _26: Int,
        _27: Int,
        _28: Int,
        _29: Int,
        _30: Int,
        _31: Int
      )
      val bytes = Array[Byte](8, 1, 16, 2, 24, 3, 32, 4, 40, 5, 48, 6, 56, 7, 64, 8, 72, 9, 80, 10, 88, 11, 96, 12, 104,
        13, 112, 14, 120, 15, -128, 1, 16, -120, 1, 17, -112, 1, 18, -104, 1, 19, -96, 1, 20, -88, 1, 21, -80, 1, 22,
        -72, 1, 23, -64, 1, 24, -56, 1, 25, -48, 1, 26, -40, 1, 27, -32, 1, 28, -24, 1, 29, -16, 1, 30, -8, 1, 31)
      val message = Message(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
        26, 27, 28, 29, 30, 31)
      bytes.pbTo[Message] shouldBe message
    }
    "derive new instance using map" in {
      import java.time.Instant
      import cats.syntax.functor._
      implicit val instantReader: PBReader[Instant] = PBReader[Long].map(Instant.ofEpochMilli)
      case class Message(instant: Instant)
      val instant = Instant.ofEpochMilli(1499411227777L)
      Array[Byte](8, -127, -55, -2, -34, -47, 43).pbTo[Message] shouldBe Message(instant)
    }
  }
}
