# Feature parity: kyo-ui-components against PrimeReact

The reference API surface is **PrimeReact v10.9.x**, the richest stable documented surface, with v11 naming cleanups where they simplify. The pixel reference is **PrimeVue 4** under the same `@primeuix` presets the generated tokens and component CSS come from, so a rendered control can be compared against the reference computed style by style.

This file is the record of what is deliberately **absent**. Every entry is a feature the reference has and this library does not, with the reason. Nothing here degrades silently: a deferred feature is missing outright rather than half-present, so a caller who reaches for it fails to compile rather than getting a broken approximation.

Two conventions keep it honest:

- **A component's scaladoc wins.** It sits next to the code and moves with it; this file is the cross-component overview. Where the two disagree, the scaladoc is the fact and this file is the bug.
- **No counts are restated here.** The component count and the two glyph-set sizes are pinned by `NodeTest` and `IconsTest` and quoted from there; a number repeated in prose drifts from the thing it counts.

## Coverage

Every PrimeReact component is implemented except one, and every feature row of every implemented component is adopted except the rows listed below.

**The exception is `Editor`.** Prime's editor wraps Quill, a full rich-text editing engine with its own document model, toolbar and selection handling. Nothing in it survives the translation to a server-rendered value, so it is out of scope rather than approximated.

The interactive demo, one page per component and one section per feature, lives in the separate `kyo-ui-components` repository, which consumes this module as a published artifact. Deferred features appear there as "Not yet implemented".

## Deferred features, per component

| Component | Deferred | Notes |
|---|---|---|
| Overlay | portal | Flip and shift are implemented and re-measure continuously: the panel measures itself on open and on every scroll or resize, then repositions in place through an attribute patch instead of a re-render, so the vertical flip and the horizontal shift track the viewport live. `autoFlip(true)` is the default and lifts every overlay consumer. The host-gated menu-family submenus keep their declared anchor (one subscription per ref chain, unmeasured). Without a portal the panels stay in the anchor subtree, so no ancestor may carry a `transform`. Escape dismisses one level at a time. |
| Select | editable mode, per-option templates, virtual scrolling | The editable value model is not modelled yet. |
| AutoComplete | multiple selection (chips), `forceSelection`, virtual scrolling | Clicking back into the input closes the panel first, which is the backdrop's doing. |
| DatePicker | locale and `dateFormat` parsing, automatic today highlight, seconds, `numberOfMonths`, disabled dates | Render is pure and cannot read a clock, so `today(iso)` is an explicit parameter and the ISO-only value model is deliberate. Escape and backdrop dismissal of the panel bypass `onClose`. |
| Tooltip | `showDelay`, `hideDelay`, `autoHide(false)`, mouse tracking, viewport flip | Driven by CSS `:hover` and `:focus-within` rather than Prime's JS tooltip: no state, no round-trips. The box is not interactive. |
| Popover | `showCloseIcon` | The arrow sits at a fixed token offset; Prime tracks the pointer with JS. Overlay's limits apply. |
| Menu family | hover-open (root and nested rows open on click), arrow-row navigation outside the Menu popup and ContextMenu's root level, mobile and hamburger modes, per-item templates, Home/End, typeahead | A round-trip per hover is disproportionate for a server-rendered menu, so the hover styling stays in the sheet and the opening is a click. Escape and outside click each dismiss one level. |
| SpeedDial | circle, semi-circle and quarter-circle types; the mask variant; the fan transition; Escape and outside-click dismissal | The non-linear types need JS-measured per-item offsets, and the fan restarts on every re-render because kyo replaces the subtree. It is not an Overlay, so it closes by toggling its ref. |
| MultiSelect | virtual scrolling, per-option templates, `selectionLimit`, loading state | Select-all is binary because PrimeVue 4 renders no indeterminate select-all, and it counts across every group because Prime has no per-group select-all. Labels join in option order, since a `Set` carries no pick order. |
| CascadeSelect | arrow-key row navigation, mobile query mode, option templates, `showClear`, loading state | Rows open and pick by click; the panels seed focus so Escape works without a prior click. |
| TreeSelect | chip display mode, the header filter, `showClear`, arrow-key tree navigation | Checkbox mode inherits Tree's cascading tri-state. Nodes toggle and select by click; the panel seeds focus. |
| Drawer | non-modal (mask-less) mode | The slide animation is implemented per docking edge, and the panel slides back out on the mask's leave ghost. The mask sits on the Dialog z-layer, which is what a mask-less mode would have to leave behind. |
| Dialog | the user-driven maximize toggle button, positioned dialogs (top, bottom-left and the rest) | Draggable and resizable are implemented: a stable header drag surface plus a corner resize handle. A movable dialog drops the open scale animation, which would otherwise replay on every drag frame. Enter and leave animations are implemented. |
| Toast | swipe dismiss | Enter and leave animations are implemented for the single Toast and for the ToastService region, sliding per screen position. Swipe dismiss needs pointer-drag on the message itself. |
| ToastService | per-session stores over server push (one instance is shared across sessions), `Env.get` inside a handler over server push | Providing the layer at the root is the doctrine, and kyo-http session fibers do not inherit the `runHandlers` caller's context, so the service is resolved at build time. |
| ContextMenu | pointer-position anchoring, global and document mode, arrow-key navigation inside nested levels | The panel anchors to the target region rather than the pointer, because kyo's `MouseEvent` carries no coordinates. The native context menu is suppressed inside the region. |
| ConfirmDialog | the imperative service form (a confirmation service resolving the choice through `Env`), the ConfirmPopup variant | The declarative ref plus `onAccept` and `onReject` covers the anatomy and the interactions. Every dismissal, Escape included, counts as a reject. |
| DataTable | cell and row editing, row and column grouping, lazy mode, column resize, reorder and toggle, frozen columns, virtual scrolling, context menu, export, state persistence | Client-side behaviours, or later phases. The header and footer slots, per-column footers in a real `tfoot`, the loading mask, a scrollable height with pinned row groups, and the checkbox column's select-all header are implemented. Select-all is binary, as in Prime, and covers every row that survived the global filter rather than the visible page. Multi-sort is modifier-free and ranks the sorted columns with Prime's sort badge. |
| Tabs | scrollable prev and next buttons, closable tabs, the animated ink bar | JS measuring and transitions. |
| Tree | filter hook parity, drag and drop, lazy-loading flags | The cascading tri-state checkbox is implemented: a click carries over the subtree, and a partially-checked ancestor renders indeterminate with `aria-checked="mixed"`. |
| Listbox | striped rows | Groups carry the untyped `ListItem` through the same `OptionItem` rows the typed pickers take. |
| Rating | a default radio `name` when none is set, `.p-focus-visible` option styling | A pure server render has no per-instance uniqueness source to mint a name from, and focus-visible tracking is JS. |
| SelectButton | object `dataKey` and `optionValue` models | A typed label and key projection takes their place. |
| IconField | clickable InputIcons, which are inert in Prime too | The TextArea and Select padding are documented `.p-uic-*` extensions, because Prime's own sheet pads only `.p-inputtext`. |
| Timeline | nothing | Pure layout; rich content composes through the templates. |
| Paginator | the `template` slot-order prop, JumpToPageDropdown, start and end content slots, responsive breakpoints, a first-offset model | The slots render in Prime's default order. |
| DataView | lazy loading | `sortField` and `sortOrder` are intentionally not ported: the sorted-`Seq` idiom is documented and demonstrated instead. |
| Stepper | custom header content | The panel enter and leave animation is implemented: a step change crossfades the old panel out while the new one fades in. |
| MeterGroup | the `min` offset, the whole-list label slot | The per-row `labelTemplate` keeps the `ol`/`li` anatomy. |
| All pickers | keystroke-level behaviour runs server-side in push mode | One round-trip per keystroke: fine on a LAN, worth knowing about over a WAN. |

## Deferred features, the form and container controls

| Component | Deferred | Notes |
|---|---|---|
| InputNumber | live locale, currency and grouping formatting while typing | Keystroke-bound client JS in Prime. Prefix and suffix are static adornments here. |
| Password | the while-typing overlay meter | This one renders the meter inline below the field, and strength is Prime's own default rules expressed as pure predicates. |
| InputOtp | Backspace to the previous cell, paste to fill | Auto-advance is implemented: each cell stamps its own id and a digit drives focus to the next. `integerOnly` cannot visually undo a keystroke. |
| Slider | dual-handle range mode | A native range input drives the interaction, and two overlapping inputs would fight for the same pointer surface, so it is deferred rather than approximated. Drag commits on pointer release; the keyboard tracks live. |
| Knob | nothing | Pointer-drag rotation is implemented as atan2 dial geometry snapped to the step, and the keyboard is kept. |
| Accordion | header arrow keys, lazy panels | The collapse animation is implemented: a class toggles in place on the existing element, so the transition is not restarted by a re-render, and `aria-expanded` tracks. |
| OrderList, PickList | drag and drop, the filter header, responsive stacking, column headers | Needs a drag-and-drop runtime. |
| Carousel | `responsiveOptions`, the `circular` clone strip | Slide and built-in autoplay are implemented: every item renders in a stable strip translated in place, and `.autoplay(interval)` is a scope-bound fiber. Touch and pointer swipe are kept. |
| Galleria | fullscreen mode, thumbnail windowing, inset and positioned indicators | Needs a portal and JS measuring. |
| FileUpload | the advanced mode: drop zone, per-file list, progress, upload and cancel toolbar | Name, size, mimeType and content are delivered on select, the basic label shows the chosen name, and `multiple(true)` is supported. Binary content still arrives as a `readAsText` string. |
| TreeTable | pagination, checkbox cascade, column resize, frozen and scrollable columns, per-column filters | As DataTable and Tree. |
| OrganizationChart | per-node selectable and collapsible overrides | |
| Terminal | history recall keys, ANSI colours | Auto-scroll to the newest line is implemented. Recall and escape sequences need keystroke-level handling. |
| InputMask | the `slotChar` placeholder for the untyped tail, `autoClear` | The mask formats incrementally, inserting literals ahead of the caret as slots fill. Both deferrals are client-runtime behaviours the mask engine does not model, so they are documented rather than faked. `onComplete` is honest: it fires when the value fills every slot. |
| ScrollPanel | Prime's custom scrollbar elements | Prime drives `.p-scrollpanel-bar-x` and `-bar-y` from JS scroll measuring, a lifecycle with no server-side equivalent. This version scrolls natively: the theme re-enables the native scrollbars Prime's content CSS hides and styles them thin from the `--p-scrollpanel-bar-*` tokens. The JS-managed bars can layer on later without changing the anatomy. |
| VirtualScroller | lazy or on-demand data loading, auto-sizing the viewport from a measurement | The full item sequence is held and only its rendering is windowed. Row content is server-rendered, so a scroll round-trips to recompute the window: the native scrollbar is instant because the spacer defines the full height, and over a slow link the rows repaint a beat behind it. |
| ColorPicker | a true grayscale hue | Only the hex value is stored, so a fully achromatic colour reads back as hue 0. |

## Where this library goes beyond PrimeReact

- **Two-way bindings on every stateful prop.** `value`, `checked`, `selected`, `expanded`, `open`, `sort` and `page` each take a `SignalRef`, which is strictly more than a controlled prop: the component reads and writes the same value the caller owns.
- **The same behaviour over server push.** Everything above, including the pointer drags (ColorPicker, Knob, Dialog, Carousel), the measure round-trip behind the Overlay flip, and the imperative commands (InputOtp focus, Terminal scroll), round-trips over kyo-ui's WebSocket with no component JavaScript at all.
- **Self-addressing.** A reusable component mints a session-unique id, stamps it on its own element, and drives focus, scroll and measurement at it without ever learning its structural render path.
- **Typed options and rows.** `options(Seq[A])(label)` and `DataTable[A]` replace string-keyed `optionLabel` and `field` props, so a typo is a compile error rather than an empty column.
- **`invalidMessage` renders an inline message row.** Prime leaves that to the form layer.
- **Option groups reach the accessibility tree.** Prime renders a group header as a flat `li` beside the options it labels, which styles correctly and conveys nothing: the options are the header's siblings, so no relation survives into the tree. Here a group is a real `li[role=group]` carrying its label as its accessible name, holding header and options in a nested `ul[role=none]` that drops out again, which leaves every option a descendant of its group as the ARIA listbox pattern specifies. Two `.p-uic-*` rules restate the flex column and inherit its gap, so a grouped panel measures exactly like a flat one. Select, MultiSelect, AutoComplete and Listbox share the anatomy.
- **A second glyph set.** `FioriIcons` sits alongside PrimeIcons, and both emit one definition per glyph so dead-code elimination keeps unused paths out of a bundle.
