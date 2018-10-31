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

  def md52016(s: String): String = {
    @tailrec def inner(s: String, r: Int = 2016): String = {
      val m = md5(s)
      if (r == 0)
        m
      else
        inner(m, r - 1)
    }
    inner(s)
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
        println(k)
        nextNKeys(s, n - 1, k + 1)
      }
    }

    @tailrec private def nextKey(s: String, index: Int): Int = {
      if (key(s, index))
        index
      else
        nextKey(s, index + 1)
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
    test(second, "qzyelonm", 20864)
  }
}


//Of course, in order to make this process even more secure, you've also implemented key stretching.
//
//  Key stretching forces attackers to spend more time generating hashes. Unfortunately, it forces everyone else to
// spend more time, too.
//
//  To implement key stretching, whenever you generate a hash, before you use it, you first find the MD5 hash of that
// hash, then the MD5 hash of that hash, and so on, a total of 2016 additional hashings. Always use lowercase
// hexadecimal representations of hashes.
//
//  For example, to find the stretched hash for index 0 and salt abc:
//
//  Find the MD5 hash of abc0: 577571be4de9dcce85a041ba0410f29f.
//Then, find the MD5 hash of that hash: eec80a0c92dc8a0777c619d9bb51e910.
//Then, find the MD5 hash of that hash: 16062ce768787384c81fe17a7a60c7e3.
//...repeat many times...
//Then, find the MD5 hash of that hash: a107ff634856bb300138cac6568c0f24.
//
//So, the stretched hash for index 0 in this situation is a107ff.... In the end, you find the original hash (one use of
// MD5), then find the hash-of-the-previous-hash 2016 times, for a total of 2017 uses of MD5.
//
//The rest of the process remains the same, but now the keys are entirely different. Again for salt abc:
//
//  The first triple (222, at index 5) has no matching 22222 in the next thousand hashes.
//The second triple (eee, at index 10) hash a matching eeeee at index 89, and so it is the first key.
//  Eventually, index 22551 produces the 64th key (triple fff with matching fffff at index 22859.
//
//Given the actual salt in your puzzle input and using 2016 extra MD5 calls of key stretching, what index now produces
// your 64th one-time pad key?
//
//Your puzzle answer was 20864.

// qzyelonm
