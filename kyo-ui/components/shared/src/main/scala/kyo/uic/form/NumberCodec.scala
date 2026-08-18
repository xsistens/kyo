package kyo.uic.form

/** Bridges a typed numeric value `A` to and from the `Double` that `uic.InputNumber`
  * binds natively (its two-way ref is `SignalRef[Double]`). Mirrors [[DateCodec]]: the
  * `Double` stays the source of truth and `A` is a derived view, so there is no second
  * ref to keep in sync.
  *
  * `integer` reports whether the field is whole-valued (`Int`/`Long`); when it is, the
  * binding turns on `InputNumber.integer(true)`, whose client keystroke mask rejects the
  * decimal separator — so the bound `Double` is always integral and `fromDouble` is exact
  * and total. Out-of-range values are clamped to `A`'s bounds.
  *
  * Precision note: `Long` rides a `Double`, so it is exact only up to ±2^53 — fine for
  * counts/quantities, unsuitable for large ids.
  */
trait NumberCodec[A]:
    def toDouble(a: A): Double
    def fromDouble(d: Double): A
    def integer: Boolean

    /** Equality for `A`, carried by the codec so call sites never need `CanEqual[A, A]` in scope.
      *
      * A typed field projects its value through a `Signal`, which compares images to suppress
      * redundant emissions and therefore needs the instance. Requiring it at the factory would push
      * the obligation onto every caller, and java.time types carry no instance of their own; a codec
      * author, who already knows the type, answers it once here. Mirrors how kyo-ui's `Chart.ColorKey`
      * carries its own equality. The default covers the ordinary case, where representable-as-a-value
      * already implies comparable.
      */
    private[uic] def equality: CanEqual[A, A] = CanEqual.derived
end NumberCodec

object NumberCodec:

    given int: NumberCodec[Int] with
        def toDouble(a: Int): Double = a.toDouble
        def fromDouble(d: Double): Int =
            val r = math.rint(d)
            if r >= Int.MaxValue.toDouble then Int.MaxValue
            else if r <= Int.MinValue.toDouble then Int.MinValue
            else r.toInt
        end fromDouble
        def integer: Boolean = true
    end int

    given long: NumberCodec[Long] with
        def toDouble(a: Long): Double = a.toDouble
        def fromDouble(d: Double): Long =
            val r = math.rint(d)
            if r >= Long.MaxValue.toDouble then Long.MaxValue
            else if r <= Long.MinValue.toDouble then Long.MinValue
            else r.toLong
        end fromDouble
        def integer: Boolean = true
    end long

    given double: NumberCodec[Double] with
        def toDouble(a: Double): Double   = a
        def fromDouble(d: Double): Double = d
        def integer: Boolean              = false
    end double
end NumberCodec
