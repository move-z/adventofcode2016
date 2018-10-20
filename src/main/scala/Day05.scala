import java.security.MessageDigest

import scala.annotation.tailrec

object Day05 {
  def first(input: String): String = {
    val res = new StringBuilder
    var idx = 0

    while (res.length < 8) {
      val sum = md5(input + idx)
      if (sum.startsWith("00000")) {
        res ++= sum.charAt(5).toString
      }
      idx += 1
    }

    res.toString()
  }

  def second(input: String): String = {
    val res = Array[Option[Char]](None, None, None, None, None, None, None, None)
    @tailrec def fill(status: Array[Option[Char]], idx: Int = 0): Array[Option[Char]] = {
      val sum = md5(input + idx)
      if (sum.startsWith("00000")) {
        val posC = sum.charAt(5)
        if (posC >= '0' && posC <= '7') {
          val pos = posC.toString.toInt
          if (status(pos).isEmpty) {
            status(pos) = Some(sum.charAt(6))
            if (!status.contains(None)) {
              return status
            }
          }
        }
      }
      fill(status, idx + 1)
    }

    fill(res).map(_.get).mkString
  }

  lazy private val digest = MessageDigest.getInstance("MD5")
  def md5(s: String): String = {
    digest.digest(s.getBytes).map("%02x".format(_)).mkString
  }

  def main(args: Array[String]): Unit = {
    def test[T] = (fun: String => T, input: String, expected: T) => assert(fun(input) == expected, input)

    test(first, "abc", "18f47a30")

    println(first("cxdnnyjw"))

    test(second, "abc", "05ace8e3")

    println(second("cxdnnyjw"))
  }
}
