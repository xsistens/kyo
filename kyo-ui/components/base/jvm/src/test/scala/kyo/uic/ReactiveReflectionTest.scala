package kyo.uic

import kyo.*

/** The classes `Reactive.scala` compiles to load under reflection, as a JVM tool that lists their methods (JMH's generator, a debugger)
  * does. An inline method that reaches the package-private macro object through a compiler-made accessor gives that accessor a
  * parameter typed by the package itself, a class no loader finds, and listing the methods then fails. JVM only: reflection is the
  * subject.
  */
class ReactiveReflectionTest extends kyo.test.Test[Any]:

    "the methods of the block's scope and of its package object can be listed" in {
        val listed = Seq(classOf[ReactiveScope], Class.forName("kyo.uic.Reactive$package"), Class.forName("kyo.uic.Reactive$package$"))
            .map { c =>
                // A missing class is a LinkageError, which `Try` does not catch.
                c.getName ->
                    (try Right(c.getDeclaredMethods.length)
                    catch case e: LinkageError => Left(e.toString))
            }
        assert(listed.forall(_._2.isRight), s"${listed.filter(_._2.isLeft)}")
    }
end ReactiveReflectionTest
