/*
*************************************************************************************
* Copyright 2019 Normation SAS
*************************************************************************************
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*
*************************************************************************************
*/


/*
 * This class provides common usage for Zio
 */

package com.normation

import java.io.FileInputStream
import java.net.URL
import java.util

import scalaz.zio._
import scalaz.zio.syntax._
import com.normation.zio.ZioRuntime
import net.liftweb.common._
import cats._
import cats.data._
import cats.implicits._
import com.normation.errors.IOResult
import com.normation.errors.RudderError

import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import specs2.run
import specs2.arguments._

import com.normation.errors._
import com.normation.zio._

@RunWith(classOf[JUnitRunner])
class ZioCommonsTest extends Specification {


  "When we use toIO, we should ensure that evaluation is only done at run" >> {
    def write(sb: StringBuffer, what: String): Unit = {
      sb.append(what)
    }

    def produceBox(sb: StringBuffer): Box[Int] = {
      write(sb, "In the method body\n")
      Full({write(sb, "in the full\n") ; 42})
    }

    val sb = new StringBuffer()

    write(sb, "before io\n")
    val io = produceBox(sb).toIO
    write(sb, "after io\n")

    write(sb, "*** run io\n")
    io.runNow
    write(sb, "*** done\n")

    sb.toString must beEqualTo(
      """before io
        |after io
        |*** run io
        |In the method body
        |in the full
        |*** done
        |""".stripMargin)
  }


}


object TestImplicits {
  import scalaz.zio._
  import scalaz.zio.syntax._

  object module1 {
    import com.normation.errors._
    sealed trait M_1_Error extends RudderError
    object M_1_Error {
      final case class Some(msg: String) extends M_1_Error
      final case class Chained[E <: RudderError](hint: String, cause: E) extends M_1_Error with BaseChainError[E]
    }

    object M_1_Result {
    }

    object service1 {
      def doStuff(param: String): IO[RudderError, String] = param.succeed
    }
  }

  object module2 {
    import com.normation.errors._
    sealed trait M_2_Error extends RudderError
    object M_2_Error {
      final case class Some(msg: String) extends M_2_Error
      final case class Chained[E <: RudderError](hint: String, cause: E) extends M_2_Error with BaseChainError[E]
    }
    object M_2_Result {
    }

    final case class TestError(msg: String) extends RudderError

    object service2 {
      def doStuff(param: Int): IO[RudderError, Int] = TestError("ah ah ah I'm failing").fail
    }
  }

  object testModule {
    import module1._
    import module2._

    import com.normation.errors._
    sealed trait M_3_Error extends RudderError
    object M_3_Error {
      final case class Some(msg: String) extends M_3_Error
      final case class Chained[E <: RudderError](hint: String, cause: E) extends M_3_Error with BaseChainError[E]
    }
    object M_3_Result {
      // implicits ?
    }

    import module1.M_1_Result._
    import module2.M_2_Result._
    import M_3_Result._

    /*
     * I would like all of that to be possible, with the minimum boilerplate,
     * and the maximum homogeneity between modules
     */
    object service {

      def trace(msg: => AnyRef): UIO[Unit] = ZIO.effect(println(msg)).run.void

      def test0(a: String): IO[RudderError, String] = service1.doStuff(a)

      def test1(a: Int): IO[RudderError, Int] = service2.doStuff(a)

      def test2(a: String, b: Int): IO[RudderError, (String, Int)] = {
        (for {
          x <- service1.doStuff(a)
          y <- service2.doStuff(b).chainError("Oups, I did it again")
        } yield {
          (x, y)
        })
      }

      def prog = test2("plop", 42) catchAll( err => trace(err.fullMsg) *> ("success", 0).succeed)
    }
  }


  import testModule.service.prog
  def main(args: Array[String]): Unit = {
    println(ZioRuntime.unsafeRun(prog))
  }
}


object TestSream {

  val log = NamedZioLogger("test-logger")


  def main(args: Array[String]): Unit = {
    val prog =
      log.error("wouhou") *>
      ZIO.bracket(Task.effect{
      val checkRelativePath = "file:///tmp/plop.txt"
      val url = new URL(checkRelativePath)
      url.openStream()
    })(is =>
      Task.effect(is.close).run // here, if I put `UIO.unit`, I can have the content
    )(is =>
      Task.effect(println(new String(is.readAllBytes(), "utf-8") ))
    ) <* log.warn("some plop plop")
    ZioRuntime.unsafeRun(prog)
    // A checked error was not handled:
    //  java.base/java.io.BufferedInputStream.getBufIfOpen(BufferedInputStream.java:176)
    // IOException("Stream closed");
  }

}

object TestLog {

  val log = NamedZioLogger("test-logger")

  def main(args: Array[String]): Unit = {


    def oups: IO[String, Int] = "oups error".fail

    val prog = oups.catchAll(e => log.error("I got an error!") *> e.fail) *> UIO.succeed(42)

    ZioRuntime.unsafeRun(prog)
  }
}




//
//object Test {
//  import zio._
//  import scalaz.zio._
//  import scalaz.zio.syntax._
//  import cats.implicits._
//
//  def main(args: Array[String]): Unit = {
//
//    val l = List("ok", "ok", "booo", "ok", "plop")
//
//    def f(s: String) = if(s == "ok") 1.succeed else s.fail
//
//    val res = IO.foreach(l) { s => f(s).either }
//
//    val res2 = for {
//      ll <- res
//    } yield {
//      ll.traverse( _.toValidatedNel)
//    }
//
//    println(ZioRuntime.unsafeRun(res))
//    println(ZioRuntime.unsafeRun(res2))
//    println(ZioRuntime.unsafeRun(l.accumulate(f)))
//
//  }
//
//}
//
//object Test2 {
//  import zio._
//  import scalaz.zio._
//  import scalaz.zio.syntax._
//  import cats.implicits._
//
//  def main(args: Array[String]): Unit = {
//
//    case class BusinessError(msg: String)
//
//    val prog1 = Task.effect {
//      val a = "plop"
//      val b = throw new RuntimeException("foo bar bar")
//      val c = "replop"
//      a + c
//    } mapError(e => BusinessError(e.getMessage))
//
//    val prog2 = Task.effect {
//      val a = "plop"
//      val b = throw new Error("I'm an java.lang.Error!")
//      val c = "replop"
//      a + c
//    } mapError(e => BusinessError(e.getMessage))
//
//    //println(ZioRuntime.unsafeRun(prog1))
//    println(ZioRuntime.unsafeRun(prog2))
//
//  }
//
//}
