package kyo.internal

/** The rule behind `filledClass`: a text field puts a class on itself, or on the closest ancestor a selector names,
  * while it holds text, and takes it off while it is empty.
  *
  * The browser keeps the text of a field nothing binds, so only the client knows when it changes: the class is toggled
  * there, on every `input` and `change` of the field, whenever a patch writes the field's value, and after every patch
  * (which may have written the target's `class` attribute from what the server last knew). The static render carries the
  * class already where the target is the field itself, so a page shown before the client attaches is right.
  *
  * There are two clients, the SPA backend ([[kyo.internal.DomBackend]], Scala) and the server-push script
  * ([[kyo.internal.HtmlRenderer]], JavaScript text). Both read the markers and the rule from here: the Scala client
  * calls [[filled]] with [[classAttr]], [[upAttr]] and [[targetAttr]], the script is [[js]], built from the same names, and
  * `FilledClassTest` holds the script against them.
  */
private[kyo] object FilledClass:

    /** The data key `filledClass(name)` sets: the class the field toggles on itself. */
    val classKey: String = "kyo-filled"

    /** The data key `filledClass(name, closest)` sets: the class the field toggles on an ancestor. */
    val upKey: String = "kyo-filled-up"

    /** The data key beside [[upKey]]: the selector `closest` finds that ancestor with. */
    val targetKey: String = "kyo-filled-on"

    /** The attribute for a class on the field itself, `data-` + [[classKey]]. */
    val classAttr: String = "data-" + classKey

    /** The attribute for a class on an ancestor, `data-` + [[upKey]]. */
    val upAttr: String = "data-" + upKey

    /** The attribute naming the ancestor, `data-` + [[targetKey]]. */
    val targetAttr: String = "data-" + targetKey

    /** Whether a field holding `value` is filled. */
    def filled(value: String): Boolean = value.nonEmpty

    /** The server-push client's half, one line: `__kyoFilled(field)` toggles the field's classes from its value,
      * `__kyoSweepFilled()` does it for every field that carries a marker.
      */
    val js: String =
        s"""function __kyoFilled(f){var v=typeof f.value==="string"&&f.value!=="";var n=f.getAttribute("$classAttr");""" +
            s"""if(n)f.classList.toggle(n,v);var u=f.getAttribute("$upAttr");""" +
            s"""if(u){var t=f.closest(f.getAttribute("$targetAttr"));if(t)t.classList.toggle(u,v);}}""" +
            s"""function __kyoSweepFilled(){var fs=document.querySelectorAll("[$classAttr],[$upAttr]");""" +
            s"""for(var i=0;i<fs.length;i++)__kyoFilled(fs[i]);}"""

end FilledClass
