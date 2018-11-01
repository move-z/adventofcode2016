import java.security.MessageDigest

import scala.annotation.tailrec
import scala.collection.mutable

object Day14 {
  def first(input: String): Int = {
    Finder(md5).search(input)
  }

  def second(input: String): Int = {
    Finder(md52016).search(input)
  }

  lazy private val digest = MessageDigest.getInstance("MD5")
  private val md5Cache = mutable.Map[String, String]()
  def md5(s: String): String = {
    if (md5Cache.contains(s)) {
      md5Cache(s)
    } else {
      val m = digest.digest(s.getBytes).map("%02x".format(_)).mkString
      md5Cache(s) = m
      m
    }
  }

  private val md52016Cache = mutable.Map[String, String]()
  def md52016(s: String): String = {
    def dig(s: String) = digest.digest(s.getBytes).map("%02x".format(_)).mkString

    if (md52016Cache.contains(s)) {
      md52016Cache(s)
    } else {
      val m = (1 to 2016).foldLeft(dig(s))((s, _) => dig(s))
      md52016Cache(s) = m
      m
    }
  }

  case class Finder(keyer: String => String) {
    def search(input: String): Int = {
      nextNKeys(input, 64)
    }

    @tailrec private def nextNKeys(s: String, n: Int, startIndex: Int = 0): Int = {
      if (n == 0)
        startIndex - 1
      else {
        val k = nextKey(s, startIndex)
        nextNKeys(s, n - 1, k + 1)
      }
    }

    @tailrec private def nextKey(s: String, index: Int): Int = {
      if (key(s, index)) {
        index
      }
      else {
        nextKey(s, index + 1)
      }
    }

    private def key(input: String, index: Int): Boolean = {
      val k = keyer(input + index)

      (0 until k.length - 2)
        .find(i => k.slice(i, i + 3).distinct.length == 1)
        .map(i => k.substring(i, i + 1) * 5)
        .flatMap(rep => (1 to 1000).find(i => {
          keyer(input + (index + i)).contains(rep)
        }))
        .nonEmpty
    }
  }

  def main(args: Array[String]): Unit = {
    def test[T] = (fun: String => T, input: String, expected: T) => assert(fun(input) == expected, input)

    test(first, "abc", 22728)

    println(first("qzyelonm"))

    test(second, "abc", 22551)

    println(second("qzyelonm"))
  }
}
