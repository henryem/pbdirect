import java.io.ByteArrayOutputStream

import cats.data.{NonEmptyList => NEL}
import com.google.protobuf.{CodedInputStream, CodedOutputStream}

package object pbdirect {
  implicit class PBWriterOps[A <: AnyRef](private val a: A) extends AnyVal {

    def toPB(implicit writer: PBWriter[A]): Array[Byte] = {
      val out = new ByteArrayOutputStream()
      val pbOut = CodedOutputStream.newInstance(out)
      val sizes = IdentityMaps.emptyJavaIdentityMap[Any, Int]
      writer.writeTo(NEL.one(1), a, pbOut, sizes)
      pbOut.flush()
      val bytes = out.toByteArray
      // remove the tag and return the content
      val input = CodedInputStream.newInstance(bytes)
      input.readTag()
      input.readByteArray()
    }
  }
  implicit class PBParserOps(private val bytes: Array[Byte]) extends AnyVal {
    def pbTo[A](implicit parser: PBParser[A]): A = {
      parser.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length))
    }
  }
}
