package kyo.uic

import kyo.*
import kyo.UI.*

/** SelectButton — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * SelectButton anatomy: `div.p-selectbutton.p-component[.p-invalid]
  * [role=group]` whose children are full [[ToggleButton]]s — the extracted
  * `@primeuix` selectbutton CSS styles `.p-selectbutton .p-togglebutton`
  * directly, fusing the buttons into one segmented control).
  *
  * Options are TYPED like Select: `options(items)(label)` projects any `A` to
  * its visible text; `optionKey` (defaults to the label projection) supplies
  * the stable key written into the bound ref(s). Single-select binds
  * `value(SignalRef[String])`; `multiple(true)` switches to a Set semantics
  * bound via `values(SignalRef[Set[String]])`. `allowEmpty(false)` blocks
  * clearing the last selection (Prime semantics).
  */
final case class SelectButton[A] private (
    items: List[A],
    labelF: A => String,
    keyF: Maybe[A => String] = Absent,
    itemTemplateF: Maybe[A => UI] = Absent,
    optionDisabledF: Maybe[A => Boolean] = Absent,
    valueRef: Maybe[SignalRef[String]] = Absent,
    valuesRef: Maybe[SignalRef[Set[String]]] = Absent,
    multipleFlag: Boolean = false,
    allowEmptyFlag: Boolean = true,
    sizeV: Size = Size.Normal,
    disabledFlag: Maybe[BoolValue] = Absent,
    invalidFlag: Boolean = false,
    accNameV: Maybe[TextValue] = Absent,
    accNameRefV: Maybe[String] = Absent,
    onChangeF: Maybe[String => Any < Async] = Absent
) extends Node:
    type Self = SelectButton[A]

    /** Appends typed options with their text projection. */
    def options(is: Seq[A])(label: A => String): SelectButton[A] =
        copy(items = items ++ is.toList, labelF = label)

    /** Convenience for plain string options (label = identity). */
    def options(is: Seq[String])(using ev: String =:= A): SelectButton[A] =
        copy(items = items ++ is.map(ev).toList, labelF = a => ev.flip(a))

    /** Stable per-option key — the value written into the bound ref(s); defaults
      * to the label projection.
      */
    def optionKey(f: A => String): SelectButton[A] = copy(keyF = Present(f))

    /** Custom option content (Prime's option slot): arbitrary UI rendered inside
      * each fused button instead of the text projection. The label projection
      * stays the accessibility name and — via [[optionKey]] — the selection key.
      */
    def itemTemplate(f: A => UI): SelectButton[A] = copy(itemTemplateF = Present(f))

    /** Renders matching options disabled. */
    def optionDisabled(f: A => Boolean): SelectButton[A] = copy(optionDisabledF = Present(f))

    /** Binds the SINGLE selection two-way to `ref` (empty string = no selection). */
    def value(ref: SignalRef[String]): SelectButton[A] = copy(valueRef = Present(ref))

    /** Switches to multi-select semantics (pair with [[values]]). */
    def multiple(v: Boolean): SelectButton[A] = copy(multipleFlag = v)

    /** Binds the MULTI selection two-way to `ref` (a set of option keys). */
    def values(ref: SignalRef[Set[String]]): SelectButton[A] = copy(valuesRef = Present(ref))

    /** Whether clicking the selected option may clear the selection entirely
      * (Prime default true).
      */
    def allowEmpty(v: Boolean): SelectButton[A] = copy(allowEmptyFlag = v)

    /** Size of the fused buttons: `.p-togglebutton-sm` / default / `-lg`. */
    def size(v: Size): SelectButton[A] = copy(sizeV = v)

    /** Disables the whole group; a `Signal[Boolean]` toggles it reactively (re-render). */
    def disabled(v: Boolean | Signal[Boolean]): SelectButton[A] = copy(disabledFlag = Present(ReactiveValue(v)))

    /** Marks the group invalid (`.p-invalid` outline). */
    def invalid(v: Boolean): SelectButton[A] = copy(invalidFlag = v)

    /** Accessible name → `aria-label` (a literal group label, e.g. "Language"). */
    def accessibleName(v: String): SelectButton[A] = copy(accNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): SelectButton[A] = copy(accNameV = Present(TextValue.Dyn(sig)))

    /** Accessible name reference → `aria-labelledby` (Prime's SelectButton a11y surface). */
    def accessibleNameRef(v: String): SelectButton[A] = copy(accNameRefV = Present(v))

    /** Fired with the clicked option's key after the selection write. */
    def onChange(f: String => Any < Async): SelectButton[A] = copy(onChangeF = Present(f))

    /** The stable option key: [[optionKey]] if set, else the label projection. */
    private def key(a: A): String = keyF.getOrElse(labelF)(a)

    private[uic] def render(using Frame): UI =
        BoolValue.reactive(disabledFlag): d =>
            copy(disabledFlag = d).renderResolved

    private def renderResolved(using Frame): UI =
        (valueRef, valuesRef) match
            case (Present(r), _) if !multipleFlag => r.render(v => body(if v.isEmpty then Set.empty else Set(v)))
            case (_, Present(r)) if multipleFlag  => r.render(body)
            case _                                => body(Set.empty)

    private def body(selected: Set[String])(using Frame): UI =
        var el = div.cssClass("p-selectbutton").cssClass("p-component").role("group")
        if invalidFlag then el = el.cssClass("p-invalid")
        accNameV match
            case Present(TextValue.Const(v)) => el = el.aria("label", v)
            case Present(TextValue.Dyn(s))   => el = el.aria("label", s)
            case Absent                      => ()
        end match
        accNameRefV.foreach(v => el = el.aria("labelledby", v))

        val buttons: List[UI] = items.map { a =>
            val k     = key(a)
            val label = labelF(a)
            var tb = ToggleButton()
                .checked(selected.contains(k))
                .onLabel(label)
                .offLabel(label)
                .size(sizeV)
            itemTemplateF.foreach(t => tb = tb.content(t(a)).accessibleName(label))
            if disabledFlag.constTrue || optionDisabledF.exists(_(a)) then tb = tb.disabled(true)
            else tb = tb.onChange(_ => activate(k))
            tb.render
        }
        el(buttons.map(toChild)*)
    end body

    /** Clicking an option updates the bound selection (single replaces, multiple
      * toggles; `allowEmpty` gates clearing the last pick), then fires `onChange`.
      */
    private def activate(k: String)(using Frame): Any < Async =
        val write: Any < Async =
            if multipleFlag then
                valuesRef match
                    case Present(ref) =>
                        ref.getAndUpdate { cur =>
                            if cur.contains(k) then if allowEmptyFlag || cur.size > 1 then cur - k else cur
                            else cur + k
                        }
                    case Absent => ()
            else
                valueRef match
                    case Present(ref) =>
                        ref.getAndUpdate(cur => if cur == k then (if allowEmptyFlag then "" else cur) else k)
                    case Absent => ()
        val fire: Any < Async = onChangeF match
            case Present(f) => f(k)
            case Absent     => ()
        for
            _ <- write
            r <- fire
        yield r
        end for
    end activate
end SelectButton

object SelectButton:
    def apply[A](): SelectButton[A] = new SelectButton[A](Nil, _.toString)
