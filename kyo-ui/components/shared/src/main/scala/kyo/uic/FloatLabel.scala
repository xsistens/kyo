package kyo.uic

import kyo.*
import kyo.UI.*

/** FloatLabel variants — where the label rests once the field is focused or
  * filled: `Over` (Prime default — above the field), `In` (inside the field's
  * top padding), `On` (on the border line).
  */
enum FloatLabelVariant derives CanEqual:
    case Over, In, On

    private[uic] def token: String = this match
        case FloatLabelVariant.Over => "over"
        case FloatLabelVariant.In   => "in"
        case FloatLabelVariant.On   => "on"
end FloatLabelVariant

/** FloatLabel — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * FloatLabel anatomy: `div.p-floatlabel[.p-floatlabel-over|-in|-on]` wrapping
  * the field plus a `<label>`), so the extracted `@primeuix` floatlabel CSS
  * applies verbatim.
  *
  * The float mechanics are pure CSS: `:has(input:focus)` floats the label on
  * focus with no JS, and the filled state is Prime's `.p-filled` class on the
  * field — stamped here at render time from the wrapped field's value binding
  * (a `SignalRef` value re-renders on change, so `.p-filled` tracks the bound
  * text; a `change` event commits on blur, exactly when the focus float ends).
  * NOTE: Prime floats the label permanently when the field has a `placeholder`
  * (they would overlap) — same behaviour here via `:has(input[placeholder])`.
  *
  * Hosts beyond [[Input]]: [[TextArea]] floats via the sheet's `textarea`
  * selectors; [[AutoComplete]] via `.p-filled` on its inner input; [[Select]]
  * via Prime's wrapper classes (`.p-inputwrapper-filled` while a value is
  * selected), with a `.p-uic-*` remainder mirroring the focus float for the
  * native `<select>`. A Select host without a placeholder gets an EMPTY
  * placeholder option stamped, so the closed field renders blank until a pick
  * — Prime's empty-label look; a non-empty placeholder floats the label
  * permanently, like `input[placeholder]`.
  */
final case class FloatLabel private (
    hostV: FloatLabel.Host,
    labelText: String,
    variantV: FloatLabelVariant = FloatLabelVariant.Over,
    forIdV: Maybe[String] = Absent
) extends Node:
    type Self = FloatLabel

    /** Float variant: `Over` (default), `In`, or `On`. */
    def variant(v: FloatLabelVariant): FloatLabel = copy(variantV = v)

    /** The label's native `for` binding (give the wrapped field the matching id). */
    def forId(id: String): FloatLabel = copy(forIdV = Present(id))

    private[uic] def render(using Frame): UI =
        FloatLabel.renderWrapper(
            base = div.cssClass("p-floatlabel").cssClass(s"p-floatlabel-${variantV.token}"),
            host = hostV,
            labelText = labelText,
            forIdV = forIdV
        )
end FloatLabel

object FloatLabel:
    /** The wrapped field (Input, TextArea, Select, or AutoComplete). */
    private[uic] enum Host:
        case In(i: Input)
        case Ta(t: TextArea)
        case Sel(s: Select[?])
        case Ac(a: AutoComplete[?])
    end Host

    /** Wraps `input` with a floating `label`. */
    def apply(input: Input, label: String): FloatLabel = new FloatLabel(Host.In(input), label)

    /** Wraps `textArea` with a floating `label`. */
    def apply(textArea: TextArea, label: String): FloatLabel = new FloatLabel(Host.Ta(textArea), label)

    /** Wraps `select` with a floating `label` (floats while a value is selected;
      * an empty placeholder option keeps the closed field blank until a pick).
      */
    def apply(select: Select[?], label: String): FloatLabel = new FloatLabel(Host.Sel(select), label)

    /** Wraps `autoComplete` with a floating `label` (floats via `.p-filled` on
      * the inner text input).
      */
    def apply(autoComplete: AutoComplete[?], label: String): FloatLabel =
        new FloatLabel(Host.Ac(autoComplete), label)

    /** Shared FloatLabel/IftaLabel body: renders through the field's value
      * binding so Prime's `.p-filled` / `.p-inputwrapper-filled` classes track
      * the current value.
      */
    private[uic] def renderWrapper(
        base: Ast.Div,
        host: Host,
        labelText: String,
        forIdV: Maybe[String]
    )(using Frame): UI =
        def labelled(field: UI): UI =
            var lbl = label
            forIdV.foreach(id => lbl = lbl.forId(id))
            base(toChild(field), toChild(lbl(labelText)))
        end labelled

        host match
            case Host.In(input) =>
                def body(filled: Boolean): UI =
                    labelled((if filled then input.extraClass("p-filled") else input).render)
                input.valueBinding match
                    case Present(Input.Value.Ref(ref)) => ref.render(v => body(v.nonEmpty))
                    case Present(Input.Value.Const(v)) => body(v.nonEmpty)
                    case Absent                        => body(false)
                end match

            case Host.Ta(textArea) =>
                def body(filled: Boolean): UI =
                    labelled((if filled then textArea.extraClass("p-filled") else textArea).render)
                textArea.valueBinding match
                    case Present(Input.Value.Ref(ref)) => ref.render(v => body(v.nonEmpty))
                    case Present(Input.Value.Const(v)) => body(v.nonEmpty)
                    case Absent                        => body(false)
                end match

            case Host.Ac(auto) =>
                def body(filled: Boolean): UI =
                    labelled((if filled then auto.fieldExtraClass("p-filled") else auto).render)
                auto.valueBinding match
                    case Present(Input.Value.Ref(ref)) => ref.render(v => body(v.nonEmpty))
                    case Present(Input.Value.Const(v)) => body(v.nonEmpty)
                    case Absent                        => body(false)
                end match

            case Host.Sel(select) =>
                // The Select stamps p-inputwrapper/-filled INSIDE its own subscription
                // (no nested subscription here); a missing placeholder gets an empty one
                // so the closed native select renders blank instead of the first option.
                var s: Select[?] = select.inputwrapper
                if select.placeholderV.isEmpty then s = s.placeholder("")
                labelled(s.render)
        end match
    end renderWrapper
end FloatLabel

/** IftaLabel — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * IftaLabel: `div.p-iftalabel` wrapping the field plus a permanently visible
  * small `<label>` inside the field's top padding — "infield top-aligned
  * label"), so the extracted `@primeuix` iftalabel CSS applies verbatim. No
  * variants and no float: the label always rests in the field.
  *
  * Same four hosts as [[FloatLabel]] — Input, TextArea, Select, AutoComplete —
  * because the two components are one idea with different label placement, and
  * Prime's own iftalabel sheet already carries the `textarea:focus` and
  * `.p-inputwrapper-focus` selectors the other three hosts need.
  */
final case class IftaLabel private (
    hostV: FloatLabel.Host,
    labelText: String,
    forIdV: Maybe[String] = Absent
) extends Node:
    type Self = IftaLabel

    /** The label's native `for` binding (give the wrapped field the matching id). */
    def forId(id: String): IftaLabel = copy(forIdV = Present(id))

    private[uic] def render(using Frame): UI =
        FloatLabel.renderWrapper(
            base = div.cssClass("p-iftalabel"),
            host = hostV,
            labelText = labelText,
            forIdV = forIdV
        )
end IftaLabel

object IftaLabel:
    /** Wraps `input` with the infield top-aligned `label`. */
    def apply(input: Input, label: String): IftaLabel = new IftaLabel(FloatLabel.Host.In(input), label)

    /** Wraps `textArea` with the infield top-aligned `label`. */
    def apply(textArea: TextArea, label: String): IftaLabel = new IftaLabel(FloatLabel.Host.Ta(textArea), label)

    /** Wraps `select` with the infield top-aligned `label` (an empty placeholder
      * option keeps the closed field blank until a pick, as in [[FloatLabel]]).
      */
    def apply(select: Select[?], label: String): IftaLabel = new IftaLabel(FloatLabel.Host.Sel(select), label)

    /** Wraps `autoComplete` with the infield top-aligned `label`. */
    def apply(autoComplete: AutoComplete[?], label: String): IftaLabel =
        new IftaLabel(FloatLabel.Host.Ac(autoComplete), label)
end IftaLabel
