package kyo.internal

import scala.quoted.*

/** The `aria` and `data` string interpolators (see [[kyo.AriaKey]] and [[kyo.DataKey]]): the compile-time checks that make a key
  * compile, and the key they build.
  *
  * The literal includes the attribute prefix, `aria-` or `data-`, and the checks read the literal's static parts. Each static part must
  * hold only lowercase letters, digits and hyphens. The name must start with a lowercase letter, and a `data` name must not start with
  * `kyo-`, the prefix the framework reserves. A dynamic part (`aria"aria-item-$n"`) is not known until run time, so the whole key is
  * checked again then, by [[nameOf]], with the same rules.
  */
private[kyo] object KeyMacro:

    /** The name a whole key literal stands for: `full` with its `prefix` removed. Refuses a key the static checks would refuse. */
    def nameOf(full: String, prefix: String, data: Boolean): String =
        if !full.startsWith(prefix) then
            throw new IllegalArgumentException(s"the ${kindOf(data)} key must start with '$prefix', got '$full'")
        val name = full.substring(prefix.length)
        problem(Seq(name), complete = true, data) match
            case Some(reason) => throw new IllegalArgumentException(s"invalid ${kindOf(data)} key '$full': $reason")
            case None         => name
    end nameOf

    // The expansions name the key's run-time check here and cast its name to the opaque key. The cast is erased (a key is its name), and
    // it keeps the expansion off the key's companion, which holds the constants that are themselves built by this interpolator.

    /** The `aria"..."` expansion: the static checks, then the key the literal and its arguments make. */
    def aria(sc: Expr[StringContext], args: Expr[Seq[Any]])(using Quotes): Expr[kyo.AriaKey] =
        '{ kyo.internal.KeyMacro.nameOf(${ key(sc, args, "aria-", data = false) }, "aria-", data = false).asInstanceOf[kyo.AriaKey] }

    /** The `data"..."` expansion: the static checks, then the key the literal and its arguments make. */
    def data(sc: Expr[StringContext], args: Expr[Seq[Any]])(using Quotes): Expr[kyo.DataKey] =
        '{ kyo.internal.KeyMacro.nameOf(${ key(sc, args, "data-", data = true) }, "data-", data = true).asInstanceOf[kyo.DataKey] }

    private def key(sc: Expr[StringContext], args: Expr[Seq[Any]], prefix: String, data: Boolean)(using Quotes): Expr[String] =
        import quotes.reflect.*
        val parts = sc.valueOrAbort.parts.toList
        val kind  = kindOf(data)
        val head  = parts.head
        if !head.startsWith(prefix) then
            report.errorAndAbort(
                s"the $kind key must start with '$prefix': write $kind\"$prefix...\", got $kind\"${parts.mkString("{}")}\""
            )
        end if
        problem(head.drop(prefix.length) :: parts.tail, complete = parts.size == 1, data) match
            case Some(reason) => report.errorAndAbort(s"invalid $kind key \"${parts.mkString("{}")}\": $reason")
            case None         => '{ $sc.s($args*) }
    end key

    private def kindOf(data: Boolean): String = if data then "data" else "aria"

    private def lowerLetter(c: Char): Boolean = c >= 'a' && c <= 'z'

    private def nameChar(c: Char): Boolean = lowerLetter(c) || (c >= '0' && c <= '9') || c == '-'

    /** The reason `segments`, joined, is not a valid name, when it is not. `complete` says the segments are the whole name; a name whose
      * first segment is empty and not complete starts with a dynamic part, which is checked at run time.
      */
    private def problem(segments: Seq[String], complete: Boolean, data: Boolean): Option[String] =
        val head = segments.head
        if segments.exists(s => !s.forall(nameChar)) then Some("a key name is lowercase letters, digits and hyphens only")
        else if head.isEmpty && complete then Some("a key needs a name after its prefix")
        else if head.nonEmpty && !lowerLetter(head.head) then Some("a key name starts with a lowercase letter")
        else if data && head.startsWith("kyo-") then
            Some("a data name must not start with 'kyo-': the kyo- prefix is reserved by the framework")
        else None
        end if
    end problem

end KeyMacro
