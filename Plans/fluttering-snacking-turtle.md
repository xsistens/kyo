# Ein Tastaturverhalten für die ganze Bibliothek

## Kontext

`c8087fa57` hat der Listenfamilie eine Tastatur gegeben und dabei eine Konvention
festgeschrieben: zwei Ebenen, die sich nie mischen (ein Ring um die Komponente, eine
Hervorhebung auf der Zeile), ein Tab-Stopp je Verbundwidget, und die Bewegung kommt aus
einer reinen Zustandsmaschine, die für sich getestet wird. Listbox, Tree, Menu, Menubar,
TieredMenu, ContextMenu, MegaMenu, CascadeSelect, TreeSelect und Accordion folgen ihr.

Der Rest der Bibliothek folgt ihr nicht, und an mehreren Stellen ist das Ergebnis nicht
bloß uneinheitlich, sondern unbedienbar: acht Elemente sind fokussierbar und anklickbar,
reagieren aber auf keine Taste; vier Komponenten nehmen ihren inaktiven Kindern per
`tabindex="-1"` den Tab-Stopp weg, ohne Pfeiltasten anzubieten, wodurch diese Kinder mit
der Tastatur gar nicht mehr erreichbar sind; drei Komponenten schreiben ihre Pfeilschleife
von Hand nach und verlieren dabei Home/End; und im Framework weicht die Aktivierungs-
Emulation an zwei Stellen von der Plattform ab, wovon eine dieselbe Doppelaktivierung
erzeugt, die `249024da1` gerade für Buttons behoben hat.

Ziel ist ein Verhalten, das der Leser einmal lernt und überall wiederfindet: was den Fokus
per Tab bekommt, bekommt ihn auch per Klick und umgekehrt; Enter und Space tun dasselbe,
außer wo das ARIA-Muster sie trennt, und dann aus dessen Grund, nicht aus dem der
Komponente; jedes Verbundwidget ist ein Tab-Stopp mit Pfeilen darin.

## Die vier Invarianten

Alles unten wird an diesen vier gemessen. Jede hat ein Vorbild im Baum.

1. **Fokussierbar heißt bedienbar.** Was `tabIndex(0)` und einen Klickeffekt trägt, trägt
   auch `onKeyDown` mit `Enter | Space` auf genau denselben Effekt. Vorbild
   `Card.scala:141`, `Avatar.scala:104`, `Password.scala:228`. Ausgenommen sind native
   `button`, `input` und `a[href]`, wo der Browser es tut und `doubleActivation` die zweite
   Auslösung unterdrückt; ein Anker nimmt nach ARIA nur Enter.
2. **Ein Verbundwidget ist ein Tab-Stopp mit Pfeilen darin.** Zwei zulässige Formen, beide
   schon im Baum, und die Wahl richtet sich danach, ob die Kinder echte Bedienelemente sind:
   - **Rovende Hervorhebung**, wenn nicht: der Container hält den Fokus, die Zeile trägt
     `.p-focus`, der Container `aria-activedescendant`, die Zeilen tragen gar keinen
     `tabindex` (`Listbox.scala:397-406` begründet, warum nicht `-1`).
   - **Rovender tabindex**, wenn doch: `tabIndex(0)` auf dem aktiven Kind, `-1` auf den
     übrigen, und die Pfeile bewegen echten Fokus per `cmds.focusId`
     (`Accordion.scala:197-204`).

   Rovender tabindex ohne Pfeile ist schlechter als gar kein roving und nie zulässig.
3. **Die Bewegung kommt aus einer reinen Maschine.** `ListNav`, `MenuNav`, `TreeNav`,
   `GridNav`, `MegaNav`. Keine Schleife im Renderpfad, keine zweite Kopie der Semantik.
4. **Der Zeiger sät, der Ring gehört der Tastatur.** Ein Klick setzt die Hervorhebung,
   damit der nächste Pfeil von dort weitergeht, zeichnet aber keinen Ring: der hängt an
   `:focus-visible` (`Theme.scala:118-133`, `:546-559`).

## Befunde

Alle Zeilennummern sind am Stand `452a95078` nachgeprüft.

### A. Fokussierbar, aber auf keine Taste bedienbar

| Stelle | Was dort steht |
|---|---|
| `Icon.scala:83`, `:99` | `span[role=button][tabindex=0]` mit `onClick`, kein Handler |
| `OrganizationChart.scala:120-126` | `a` ohne `href`, `tabIndex(0)`, `aria-expanded`, `onClick` |
| `Inplace.scala:74-77` | `div[role=button]`, nur `Enter`, kein `Space` |
| `DataTable.scala:3541`, `TreeTable.scala:289` | Sortierkopf `th tabIndex(0)` mit `onClick` |
| `DataTable.scala:3184` | Filterbedingung `li tabIndex(0)` mit `onClick` |
| `DataTable.scala:3951`, `TreeTable.scala:386` | auswählbare Zeile `tr tabIndex(0)` mit `onClick` |

### B. Rovender tabindex ohne Pfeiltasten

`Tabs.scala:97`, `Carousel.scala:371`, `Galleria.scala:190` und `:247`. In allen vier Fällen
ist nur das aktive Kind erreichbar, die übrigen sind aus der Tab-Reihenfolge genommen und
durch nichts ersetzt. `Stepper.scala:104` liegt daneben: `role="tab"` ohne `role="tablist"`
darüber, kein roving, keine Pfeile.

### C. Verbundrolle ohne Tastatur

- `TreeTable.scala:233` trägt `role="treegrid"` und hat in der ganzen Datei kein `onKeyDown`.
- `SpeedDial.scala:131` trägt `role="menu"` und hat keine Taste, keine Escape-Auflösung und
  keinen Fokusrücklauf. Schlimmer: der geschlossene Fächer bleibt tabbar, weil das Blatt ihn
  mit `transform: scale(0); opacity: 0` versteckt (`generated/ComponentCss.scala`, Block
  `.p-speeddial-item`) und die Buttons weder `disabled` noch `hidden` sind.
- `SelectButton.scala:142` trägt `role="group"` über eigenen ToggleButtons, ohne Pfeile.

### D. Drei handgeschriebene Pfeilschleifen

`Select.scala:468-472` und `MultiSelect.scala:469-473` tragen dieselbe `var`-Schleife, die
`ListNav.move` nachbaut; `AutoComplete.scala:369-380` eine dritte Fassung mit `min`/`max`.
Alle drei ohne Home/End, alle drei ohne Space im offenen Panel (der Auslöser nimmt Space,
das Panel nicht: `Select.scala:435` gegen `:545`), AutoComplete zusätzlich ohne Übersprung
deaktivierter Zeilen und mit ArrowUp aus dem Nichts auf die erste statt auf die letzte Zeile.

### E. Framework: die Emulation weicht von der Plattform ab

- `ReactiveUI.scala:737-742` behandelt `Anchor` wie `Button`, also aktiviert **Space jeden
  Link**. Und weil `doubleActivation` nur `BUTTON` abdeckt (`DomBackend.scala:1562`,
  `HtmlRenderer.scala:1924`), läuft **Enter auf einem Anker mit Klickhandler zweimal**:
  einmal über den Browser-Klick, einmal über die Emulation. Das ist derselbe Fehler, den
  `249024da1` für Buttons behoben hat, an der Elementart daneben.
- `Space` fehlt in der Scroll-Sperrliste (`DomBackend.scala:1585`,
  `HtmlRenderer.scala:1912`), also scrollt Space die Seite, während es eine Listenzeile
  aktiviert. Betrifft jede Komponente mit `preventScrollKeys`.
- Die Sperrliste steht zweimal da, in Scala und in JS, ohne Test, der beide vergleicht.
- `ReactiveUI.scala:744-753` lässt Enter eine Checkbox schalten. Das ist Absicht
  (TUI-Parität, in der Client-JS begründet) und bleibt, bekommt aber seinen Grund dort
  hingeschrieben, wo die Emulation steht.

### F. ARIA, das an der Tastatur hängt

- `aria-activedescendant` fehlt bei `Select.scala:498`, `MultiSelect.scala:515`,
  `AutoComplete.scala:476` und CascadeSelect, obwohl alle vier `.p-focus` bewegen.
- `role="menuitem"` kommt in der Bibliothek nirgends vor. Die Menüzeilen sind
  `role="presentation"`, und `aria-activedescendant` zeigt genau auf diese
  (`Menu.scala:144-146` plus die vier Geschwister): ein `role="menu"`, dessen Kinder
  ausschließlich präsentational sind.
- `role="combobox"` fehlt an allen vier Auswahlfeldern.

### G. Fokus in und um Overlays

- **`aria-activedescendant` steht bei jedem gesäten Panel auf dem falschen Element.** Nachgeprüft
  beim Commit zur Menüstruktur: `Overlay` gibt `focusAuto` und `tabIndex(-1)` dem Panel-`div` und
  hängt den Tastenhandler dort auf, der Fokus sitzt also auf dem Panel. `aria-activedescendant`
  steht aber auf der `ul` darin (Select, MultiSelect, CascadeSelect, TreeSelect, Menu im
  Popup-Modus, TieredMenu, ContextMenu), und ein Screenreader liest das Attribut des
  **fokussierten** Elements oder gar keines. Das Ziel ist seit dem Rollen-Commit korrekt, der
  Ansatzpunkt nicht. Die richtige Auflösung ist das Combobox-Muster: der Fokus bleibt auf dem
  Auslöser (bei `filterable` auf dem Filterfeld im Panel), das Panel sät nichts, und
  `role="combobox"` plus `aria-controls` plus `aria-activedescendant` sitzen zusammen auf dem
  einen fokussierten Element. Das geht nicht ohne den Schalter-Split dieses Commits, deshalb hier
  und nicht dort. AutoComplete ist bereits fertig: sein Panel sät nichts, sein
  `aria-activedescendant` steht seit dem Rollen-Commit auf dem Eingabefeld.
- ~~`Overlay.scala:370` hängt `focusAuto`, `focusRestore`, `tabIndex(-1)` und `focusTrap` an
  einen Schalter. Die vier Panels mit `seedFocus(false)` verlieren deshalb auch die
  Tab-Falle.~~ **Prämisse falsch, beim Bearbeiten von Commit 9 nachgeprüft.** Die Falle im
  Client greift über `e.target.closest('[data-kyo-focus-trap="1"]')`, wirkt also nur, wenn Tab
  INNERHALB des Panels gedrückt wird. Genau die vier `seedFocus(false)`-Panels halten den Fokus
  absichtlich außerhalb (AutoComplete im Eingabefeld, MegaMenu und Menubar-Untermenüs auf der
  rovenden Wurzelliste, DatePicker im Feld), also kann dort nie ein Tab aus dem Panel heraus
  gedrückt werden und es geht keine Falle verloren. DatePickers Panel enthält als einziges echte
  Knöpfe, und seine Scaladoc sagt ausdrücklich, dass die per Tab erreichbar bleiben sollen: eine
  Falle wäre dort eine Verschlechterung. Der Schalter-Split entfällt ersatzlos.
- `ColorPicker` setzt `focusTrap`, hat aber nichts Fokussierbares im Panel (`:185-186` sind
  Divs), die Falle fängt also nichts. `:274` öffnet außerdem nur, statt zu schalten.
- `DatePicker`s Panel behält `dismissOnEscape`, obwohl der Fokus nie hineinkommt: tote
  Konfiguration.
- `Popover.scala:100-106` rendert seinen Auslöser als nacktes `span` ohne `tabIndex`.

### H. Ganz fehlende Muster

- **DatePicker-Kalender**: `span.p-datepicker-day` mit `onClick` (`:637`), ohne `tabIndex`,
  ohne `role="gridcell"`, ohne Pfeile, in einem `table role="grid"` (`:657`). Das Feld
  öffnet nicht auf ArrowDown/Enter/Space, anders als die vier Geschwister-Auslöser.
- **ColorPicker**: Sättigungsfläche und Hue-Leiste sind reine Zeigerflächen.
- **Typeahead**: nirgends.
- ~~**OrderList/PickList**: keine Taste fürs Verschieben oder Übertragen.~~ **Prämisse zur Hälfte
  falsch, in zwei Schritten geklärt.** Die Move-Buttons sind echte `<button>`, Enter und Space
  aktivieren sie. Der erste Defekt lag daneben: die Buttons waren nativ `disabled`, solange ihre
  Seite nichts ausgewählt hatte, und ein PickList-Transfer leert genau diese Auswahl, der eben
  gedrückte Button ging also unter der Hand des Lesers aus und verlor den Fokus ans Dokument.
  Behoben über `ariaDisabled`. Die zweite Hälfte der Prämisse stimmte doch: die EINGEBETTETE
  Listbox (`resolved`) bekam nie einen Highlight-Ref, also war dort jede Zeile ein eigener
  Tab-Stopp ohne Pfeile dazwischen, anders als die freistehende Listbox seit `c8087fa57`.
  Nachgeholt in `3e5232fb4`, zusammen mit den Akkorden fürs Verschieben und Übertragen.
- **Rating**: `nameV.foreach` (`:170`). Ohne `.name(...)` haben die versteckten Radios keinen
  Gruppennamen, also keine Pfeile, obwohl die Scaladoc bei `:161` sie zusagt.

### I. Kleinigkeiten mit klarer Ursache

- `GridNav.scala:123` fängt `Space` vor dem Zeichenzweig `:125` ab. `Space.charValue` ist
  `Present(" ")`, also ist das Leerzeichen das einzige druckbare Zeichen, das keinen Editor
  öffnen kann.
- Readonly bedeutet dreierlei: Select öffnet weiter per Tastatur (`:433`),
  CheckBox/ToggleSwitch/Rating schalten das Input ab, ToggleButton behält den Fokus und
  lässt nur den Handler weg.

### Was geprüft wurde und bewusst so bleibt

`MenuModel.scala:195` nimmt für Links nur Enter, was nach Behebung von E richtig ist.
Accordion verzichtet absichtlich auf Enter/Space und stützt sich auf die native Aktivierung
plus die Unterdrückung. `Chip.scala:87` nimmt nur Backspace, weil der native Button den Rest
tut. `Listbox.scala:265` überspringt nichts, weil `ListItem` kein Deaktiviert-Merkmal trägt.
Slider und Rating tragen echte native Inputs. Paginator macht jeden Seitenknopf zum
Tab-Stopp, wie Prime. `Toolbar.scala:35` bleibt ohne roving: sein Inhalt gehört ihm nicht,
und rovender tabindex über fremde Kinder wäre eine Anmaßung, keine Verbesserung.

## Reihenfolge

Ein Commit je Zeile, in dieser Abhängigkeitsfolge.

| # | Commit | Inhalt |
|---|---|---|
| 1 | Framework-Aktivierung | Anker nimmt nur Enter; `doubleActivation` deckt `a[href]` mit Klickhandler mit ab; `Space` kommt in die Sperrliste, ausgenommen editierbare und nativ aktivierende Ziele; die Sperrliste bekommt eine Quelle und einen Test, der beide Transporte vergleicht; die Checkbox-Enter-Parität bekommt ihre Begründung |
| 2 | Reine Maschinen | `ListNav` bekommt eine `Orientation` (Default `Vertical`, damit die vorhandenen Hosts unberührt bleiben); `GridNav.Space` hört auf, den Zeichenzweig zu verschlucken; neuer reiner `Typeahead` mit eigenem Test |
| 3 | Fokussierbar heißt bedienbar | die acht Stellen aus A |
| 4 | Roving mit Pfeilen | Tabs, Stepper, Carousel, Galleria, SelectButton auf Accordions Form: Ids geprägt, `ListNav` horizontal, `focusId`, Home/End |
| 5 | SpeedDial wird ein Menü | geschlossener Fächer aus der Tab-Reihenfolge, Pfeile, Escape, Fokus zurück auf den Auslöser |
| 6 | TreeTable bekommt das Treegrid | `GridNav` für die Zellen, `TreeNav` fürs Auf- und Zuklappen, nach dem Vorbild von DataTable |
| 7 | Die Auswahlfelder auf `ListNav` | Select, MultiSelect, AutoComplete verlieren ihre Schleifen und gewinnen Home/End, Space im Panel, Typeahead und `aria-activedescendant`; CascadeSelect bekommt letzteres nach |
| 8 | Menüstruktur | `role="menuitem"` auf der Zeile, `aria-activedescendant` zeigt darauf statt auf das präsentationale `li`; `role="combobox"` an den vier Auslösern |
| 9 | Overlay trennt Saat und Falle | `seedFocus` und `focusTrap` werden zwei Schalter, damit die vier `seedFocus(false)`-Panels Tab wieder halten; dazu der Befund unten: bei jedem Panel, das den Fokus sät, steht `aria-activedescendant` auf der Liste im Panel und nicht auf dem fokussierten Element |
| 10 | DatePicker-Gitter | `role="gridcell"`, ein Tab-Stopp, `GridNav` mit PageUp/PageDown, Feld öffnet auf ArrowDown/Enter/Space, tote Escape-Konfiguration weg |
| 11 | ColorPicker | Fläche und Leiste werden `role="slider"` mit `tabIndex(0)`, `aria-valuemin/max/now/text` und Knobs Tastensatz; der Auslöser schaltet statt nur zu öffnen |
| 12 | Rating | prägt einen Gruppennamen, wenn keiner gesetzt ist, damit die zugesagten Pfeile ohne Zutun laufen |
| 13 | OrderList und PickList | Verschieben und Übertragen per Taste |
| 14 | Readonly heißt eines | fokussierbar, `aria-readonly`, und jede verändernde Taste wie jeder Klick läuft ins Leere; für Select, CheckBox, ToggleSwitch, Rating, ToggleButton |
| 15 | Dokumente | `PARITY.md` entstauben (vier Zeilen behaupten fehlende Tastatur, die es seit `c8087fa57` gibt), Tastaturabschnitt in `components/CLAUDE.md` |

Danach, im Demo-Repository und als eigene Commits: ein Tastaturabschnitt je Komponentenseite,
der die Tasten dieser Komponente auflistet und vorführt, plus die Prosa, die durch 1 bis 14
falsch geworden ist. `CheckDocs` bekommt die Regel, dass eine Seite ohne Tastaturabschnitt
ein Befund ist.

## Dateien

**kyo-ui (Framework):** `shared/src/main/scala/kyo/internal/ReactiveUI.scala`,
`shared/src/main/scala/kyo/internal/HtmlRenderer.scala`,
`js-wasm/src/main/scala/kyo/internal/DomBackend.scala`, `shared/src/main/scala/kyo/UI.scala`.

**Reine Maschinen:** `ListNav.scala`, `GridNav.scala`, neu `Typeahead.scala`.

**Komponenten:** `Icon`, `OrganizationChart`, `Inplace`, `DataTable`, `TreeTable`, `Tabs`,
`Stepper`, `Carousel`, `Galleria`, `SelectButton`, `SpeedDial`, `Select`, `MultiSelect`,
`AutoComplete`, `CascadeSelect`, `Menu`, `Menubar`, `TieredMenu`, `ContextMenu`, `MegaMenu`,
`MenuModel`, `Overlay`, `DatePicker`, `ColorPicker`, `Rating`, `OrderList`, `PickList`,
`CheckBox`, `ToggleSwitch`, `ToggleButton`, `Popover`, `Theme.scala`.

**Tests:** `ListNavTest`, `GridNavTest`, neu `TypeaheadTest`; je geänderter Komponente ein
`*Test.scala` mit passendem Namenspräfix nach dem Muster von `DataTableTest:46-51` (Handler
vom AST ziehen, mit `UI.KeyboardEvent` rufen); `GoldenRenderTest` für das Markup;
`FocusableTest` und `KeyboardTest` in kyo-ui für Commit 1.

**Dokumente:** `kyo-ui/components/PARITY.md`, `kyo-ui/components/CLAUDE.md`.

## Verifikation

1. `bloop test kyo-uiJVM-test -c .bloop` und die Komponenten-Suiten grün; vor dem Abschluss
   einmal über sbt auf JVM, JS, Native und Wasm, weil die Golden-Renders alle vier fahren.
   Der Testcompile braucht `BLOOP_JAVA_OPTS="-Xmx9G ..."`, siehe die Notiz dazu.
2. Ein Test, der die beiden Fassungen der Scroll-Sperrliste gegeneinander hält, statt sie
   weiter von Hand gleich zu halten.
3. `FocusableTest` fährt im echten Browser: Space auf einer rovenden Liste aktiviert und
   scrollt nicht; Space in einem Eingabefeld tippt weiter; Enter auf einem Anker mit
   Klickhandler löst genau einmal aus; Space auf einem Anker löst nicht aus.
4. Pro geänderter Komponente ein Handler-Test, der die Taste hineingibt und den Effekt oder
   die neue Hervorhebung prüft, plus ein Golden-Render für `tabindex`, `role` und
   `aria-activedescendant`.
5. CDP-Durchlauf über beide Transporte (`:5179` SPA, `:8080` Server-Push) am Ende, je
   Komponente: mit Tab hinein, mit den Pfeilen durch, Enter und Space, Escape, und der Ring
   erscheint erst nach einer Taste, nicht nach einem Klick. Die Skripte aus
   `scratchpad/probe2/` sind die Grundlage.
6. `CheckSnippets`, `CheckDocs` und `DemoPagesGen` grün, nachdem die Demo nachgezogen ist.

## Risiken und getroffene Entscheidungen

- **Space in die Sperrliste zu nehmen ist die riskanteste Einzeländerung.** Die Regel muss
  editierbare Ziele und nativ aktivierende Elemente (`button`, `a[href]`, `input`, `select`,
  `textarea`, `summary`, `contenteditable`) ausnehmen, sonst tippt in einem
  `preventScrollKeys`-Bereich niemand mehr ein Leerzeichen. Der Test dafür kommt vor der
  Änderung.
- **Die Anker-Doppelaktivierung zu beheben ändert Verhalten für jeden Link mit
  Klickhandler**, und darauf steht die halbe Menüfamilie. Erst reproduzieren, dann beheben.
- **`ListNav` eine Orientierung zu geben berührt fünf bestehende Hosts.** Additiv mit Default
  `Vertical`, damit `ListNavTest` unverändert grün bleibt und die Änderung an den Hosts
  sichtbar wird statt still zu passieren.
- **TreeTable auf `GridNav` zu heben ist die größte Einzelposition** und der wahrscheinlichste
  Kandidat dafür, dass der Umfang größer ausfällt als hier geschätzt. DataTable ist die
  Vorlage, aber TreeTable hat Zeilen mit Tiefe, also greifen `GridNav` und `TreeNav`
  ineinander, was es bei DataTable nicht gibt.
- **`Toolbar` bleibt bewusst ohne roving.** Ich halte das für richtig und nicht für eine
  Auslassung: seine Kinder gehören ihm nicht, Prime macht es genauso, und alle Kinder sind
  einzeln erreichbar.
- **Der Tastaturabschnitt je Demo-Seite ist rund 84 Seiten Text.** Der Umfang liegt im Text,
  nicht in der Technik; die Seiten selbst sind ein wiederholtes Muster.

## Weitergereichte Befunde: beide eingelöst (kyo `c5a008f7c1` und `cf8b4943da`)

Beides beim Arbeiten aufgefallen, beides geprüft statt vermutet, beides jetzt behoben.

**`BrowserLauncherCleanupJvmTest`, 4 von 5 Blättern rot**, `captured no Chrome PID for our tag
(is --user-agent reaching argv?)`. Nicht der Launcher, sondern die Suche: sie las
`ProcessHandle.info().arguments()`. Chrome schreibt sein argv beim Start zu EINEM
nullterminierten String zusammen, die gewöhnliche Unix-Prozesstitel-Umschrift, an der seine
Kinder als `--type=renderer` erkennbar werden. `/proc/<pid>/cmdline` trägt danach ein einziges
`argv[0]`, die JVM meldet also völlig zu Recht ein LEERES Argumentfeld. Gegen
chrome-headless-shell 152 gemessen: `arguments()` leer, `commandLine()` nur der Binärpfad,
`ps -o args=` die ganze Zeile samt `--user-data-dir` und `--user-agent`. Vier Tests fanden
nichts, worüber sie hätten urteilen können; der fünfte war grün, weil er eine Abwesenheit prüft.
Die Suche liest jetzt die Prozessliste, ein Weg für Unix und der WMI-Weg, den Windows schon
hatte, beide auf dieselben `(pid, Kommandozeile)`-Paare.

Beim Nachlaufen fiel ein zweiter roter Test derselben Familie auf, `killOrphans kills processes
matching the kyo-browser user-data-dir pattern`. Sein Wachposten war `sh -c 'true; sleep 30 #
<tag>'`, laut eigenem Kommentar so gewählt, dass die Shell nicht in den Sleep hinein
exec-optimiert und dabei das argv mit dem Tag verliert. Eine Anweisung VOR dem Sleep verhindert
das nicht: bash 5 als `/bin/sh` exect trotzdem das letzte Kommando, übrig bleibt ein nacktes
`sleep 30`, das `pgrep -f` nicht findet. Die Anweisung muss dahinter stehen, also
`sleep 30; true # <tag>`. Der Launcher selbst blieb unverändert; sein eigener Waisen-Kehraus
läuft über `pgrep -f` und liest denselben Puffer, den `ps` druckt.

**`DataTableTest`, "a windowed source is asked for the range the scroll is about to draw"** war
kein Flake unter Last, sondern ein Fehler, der fast immer zuschlägt und nur an dieser Stelle fast
nie sichtbar wurde. `RowSource` forkt den Beobachter, der aus einer Seitenzahl eine Range macht;
seine erste Zustellung schreibt die Range von Seite 0, sobald der Scheduler sie laufen lässt. Wer
dazwischen selbst eine Range schreibt, verliert sie, und der Leser steht wieder oben in einer
Liste, durch die er schon gescrollt war.

Der frühere Eintrag hier sagte, 300 Durchläufe verlören nichts. Das lag an der Form des
Versuchs: mit dem Schreiben als ERSTEM Schritt nach `init` verlieren 49 von 50 Läufen die Range.
Genau deshalb läuft der Regressionstest das Szenario 50-mal, und genau deshalb sah ein
Einzellauf jahrelang grün aus.

Behoben, indem der Beobachter mit der Seite gesetzt wird, auf der die Quelle aufmacht, er also
nur schreibt, was ein Aufrufer GEÄNDERT hat. `observeChanges` wäre der naheliegende Griff und
tauscht den veralteten Schreibvorgang gegen einen verlorenen: eine vor dem Fork gesetzte Seite
würde zur Grundlinie und nie bedient.

---

# Vorheriger Plan: die Demo als vollständige Komponenten-Referenz

> **Abgearbeitet.** Schritte 0 bis 5 waren erledigt; der Rollout (6 bis 11) ist es auch, nur
> stand es hier nicht: 84 Seiten, 98 Tabellen, 976 beschriebene Einstellungen, `CheckDocs`
> ohne Befund. Schritt 12 und die Verifikationsliste stehen in Nachtrag 11.
> Bleibt hier stehen, weil die Datei nicht versioniert ist und der Stand sonst verloren wäre.

## Kontext

Die Demo (`/home/crz/Programming/kyo-ui-components`) hat heute 81 Seiten, 504 Beispiele
und eine gute Erfassungs-Mechanik: `ExampleMacro` schneidet den Quelltext jedes Beispiels
zur Compile-Zeit aus der Datei, der Playground kompiliert jeden Schnipsel gegen denselben
Klassenpfad, und der Editor ist im SPA live editierbar. Was fehlt, ist alles, was aus einer
Sammlung von Beispielen eine Referenz macht:

1. **Keine Routen.** Kein `pushState`, kein Hash, kein `<a href>`. Alle vier Runner starten
   auf `button`; es gibt keinen teilbaren Link auf eine Komponente und keinen Zurück-Button.
2. **Kein Untermenü.** Eine Seite ist eine flache Liste von Abschnitten; die DataTable-Seite
   hat 44 davon und keine Übersicht.
3. **Keine Einstellungs-Tabelle.** Die Bibliothek hat rund 1300 öffentliche Setter. Die Demo
   dokumentiert sie in Prosa: ein Einleitungssatz nach der Formel
   `"uic.X mirrors PrimeReact's X: <Aufzählung>"`. Welche Einstellung wo vorgeführt wird,
   weiß niemand, und ob überhaupt, auch nicht.
4. **Die Einleitung sagt nicht, wofür man die Komponente nimmt**, nur was sie kann.
5. **Der Editor zeigt immer das ganze Beispiel** und nie die Imports oder die Fixtures, also
   weder das Wesentliche noch das Vollständige.
6. Drei Komponenten haben keine eigene Seite: `Text` teilt sich eine mit `Title`,
   `OverlayBadge` liegt auf `badge`, `IftaLabel` auf `floatlabel`.

Ziel ist eine Referenz, in der jede Einstellung jeder Komponente eine Zeile in einer Tabelle,
mindestens ein laufendes Beispiel und einen teilbaren Link hat, und in der der Leser den
Ausschnitt sieht, um den es geht, den vollständigen kompilierbaren Code aber auf Knopfdruck
bekommt.

**Entschieden (in diesem Gespräch):** Pfad für die Seite plus `#hash` für den Abschnitt bei
einer langen Scroll-Seite; Setter-Liste und Signaturen automatisch aus dem Artefakt, die
Kurzbeschreibungen von Hand; die Vollansicht enthält Imports **und** die Fixture-Deklarationen
(`products` und Konsorten); Gerüst zuerst, dann zwei Pilotseiten, dann der Rest.

**Sprache:** Die Demo-Prosa bleibt Englisch (Status quo). Die deutschen `groupHeading`-Texte
auf `FormValidationPage` werden dabei auf Englisch vereinheitlicht. Wenn stattdessen alles
auf Deutsch soll, ist das eine Ansage, die den Textumfang verdoppelt.

**Keine Änderung an kyo nötig.** Alles unten kommt mit dem publizierten Artefakt aus:
`UILocation` (History-API, JS) existiert, `MenuItem.url` rendert echte Anker,
`Listbox.optionGroups` liefert Gruppen-Überschriften und `Listbox.itemTemplate` lässt eine
Option als Anker rendern. Verifiziert: die Scaladoc-Kommentare stecken im publizierten TASTy
(`strings kyo/uic/DataTable.tasty | grep "Appends data rows"`), das Makro kann sie also lesen.

## Das Zielbild einer Seite

```
/c/datatable#zeilen-umsortieren

┌──────────┬────────────────────────────────┬──────────────┐
│ Nav      │ Home / Structure / DataTable   │ Auf dieser   │
│ (Listbox │                                │ Seite        │
│  mit     │ DataTable                      │ · Einstell.  │
│  Gruppen,│ Was ist das (1 Absatz)         │ · Spalten    │
│  Anker)  │ Wofür nimmt man es (1 Absatz)  │ · Sortieren  │
│          │                                │ ▸ Umsortieren│
│ Button   │ ┌ Einstellungen (uic.DataTable)│ · Filtern    │
│ DataTable│ │ Signatur    Wofür   Beispiele│ · Export     │
│ Tree     │ │ rows(…)     …       Grundl.  │              │
│ …        │ │ flexScroll  …       Scrollen │              │
│          │ └──────────────────────────────│              │
│          │ ## Sortieren                   │              │
│          │ ## Zeilen umsortieren  ← #hash │              │
│          │   Beschreibung                 │              │
│          │   ┌ live ────────────────────┐ │              │
│          │   └──────────────────────────┘ │              │
│          │   ┌ Code · [Vollständig] ────┐ │              │
│          │   └──────────────────────────┘ │              │
└──────────┴────────────────────────────────┴──────────────┘
```

## Die sechs Bausteine

### 1. Routing: `/c/<id>` plus `#<slug>`

`demo/src/main/scala/uicdemo/Route.scala` (neu, shared): `Route.path(id): String`,
`Route.parse(pathAndSearch): Maybe[String]`, `Route.slug(title): String` (kebab-case,
diakritikfrei) und ein `slugs(page)`-Helfer, der Eindeutigkeit prüft.

- **Nav-Einträge werden echte Anker.** Die Rail bleibt `uic.Listbox` (Gruppen über
  `optionGroups`, Auswahlzustand für die Hervorhebung), aber `itemTemplate` rendert den Text
  als `a.href(UI.Href.Path(Route.path(id)))`. Damit gilt **eine Markup-Form für beide
  Transporte**: im SPA fängt `UILocation`s Capture-Phase-Interceptor den Klick ab und macht
  `pushState` daraus, im Server-Push ist es eine echte Navigation.
- **SPA** (`DemoMain.scala`): `UILocation.current` wird zu `Signal[String]`, daraus die
  Seiten-Id, und die Shell rendert `route.render(id => …)`.
- **Server-Push** (`DemoServer.scala`): eine `UI.runHandlers`-Registrierung pro Seite
  (`/c/<id>`) plus `/` für die Übersicht. `UI.runHandlers` nimmt genau einen Basispfad, also
  sind es 82 Aufrufe, 164 Handler. Nebeneffekt und Gewinn: jede Session baut nur noch **eine**
  Seite statt aller 81 (heute `Kyo.foreach(pages)(…)` vor dem ersten Paint).
- **Hash.** `UILocation.current` lässt den Hash bewusst weg. Gleich-Dokument-Anker werden
  nicht abgefangen, der Browser scrollt also nativ; zu scrollen ist nur nach einem
  Deep-Link-Start im SPA und nach einem Seitenwechsel mit Hash. Dafür ein
  Plattform-Zwilling `CurrentHash` (js: `dom.window.location.hash`, jvm: `""`, weil der
  Server den Hash nicht kennt und der Browser dort beim ersten Paint selbst scrollt) und
  ein kleines `UI.mounted` am Seitenkopf, das `cmds.scrollIntoViewId(slug)` schickt.
- **Vite** liefert für unbekannte Pfade per Default `index.html` aus (`appType: 'spa'`); das
  ist zu verifizieren, nicht anzunehmen.

### 2. Das Seitenmodell in `Doc.scala`

```scala
trait ComponentPage[C]:                       // C = die dokumentierte Komponente
  def id: String
  def title: String
  def group: String
  def api: Seq[Doc.Setting]                   // aus Doc.settings[C](…), siehe 3.
  def content(using Frame): Doc.Page < Doc.Row

final case class Page(
    title: String,
    what: String,                             // Was ist das für eine Komponente
    whenToUse: String,                        // Wofür kann und sollte man sie nehmen
    api: Seq[Setting],
    sections: Seq[Section]
)

final case class Section(
    title: String,
    shows: Seq[String],                       // die Setter-Namen, um die es hier geht
    render: UI < Row,
    code: Maybe[Code],                        // Ausschnitt + Vollansicht, siehe 4.
    owner: Maybe[String]
):
  def slug: String = Route.slug(title)
```

`shows` ist die eine Deklaration, die drei Dinge trägt: die Beispiel-Links der Tabelle, die
Faltung des Code-Ausschnitts und den Abdeckungs-Gate. Ohne sie würde die Tabellenspalte
"Beispiele" für `rows` auf alle 44 DataTable-Abschnitte zeigen, was keine Auskunft ist.

Seiten mit mehreren Trägern (`DataTable` plus `Column`, `RowGroup`, `ColumnFilter`, `CellType`;
`Accordion` plus `AccordionPanel`; `Tabs` plus `Tab`; die `uic.form`-Seite) bekommen **mehrere
Tabellen** untereinander, eine pro Träger. Die Regel: eine eigene Seite für jede Komponente,
die man allein konstruieren kann; ein Unterbauteil, das ohne seinen Wirt sinnlos ist, bekommt
eine eigene Tabelle und eigene Abschnitte auf der Seite des Wirts. Neue eigenständige Seiten:
`text` (aus `title-text` herausgelöst), `overlaybadge`, `iftalabel`.

### 3. Die Einstellungs-Tabelle

`demo/src/main/scala/uicdemo/ApiMacro.scala` (neu). `Doc.settings[C]("name" -> "Text", …)`
reflektiert über `TypeRepr.of[C].typeSymbol.declaredMethods`, filtert Synthetisches,
`apply`/`copy`/Produkt-Methoden und alles nicht-Öffentliche weg und liefert pro **Methodenname**
eine `Setting(name, signatures: Seq[String], description)`. Überladungen teilen sich eine Zeile
und eine Beschreibung (`invalid(v: Boolean)` und `invalid(sig: Signal[Boolean])` sind eine
Einstellung), alle Signaturen stehen untereinander in der Zelle.

Zwei Fehler zur Compile-Zeit, beide mit Namensnennung: ein Setter ohne Beschreibung, und eine
Beschreibung, die keinen Setter trifft (Tippfehler, umbenannter Setter). Damit kann die Tabelle
nach einem Versions-Bump nicht stillschweigend veralten.

**Bootstrap für die rund 900 Beschreibungen:** ein Runner `uicdemo.SeedDescriptions`, der pro
Komponente den ersten Satz des Scaladoc als fertigen `"name" -> "…"`-Block ausgibt. Das ist ein
Entwurf zum Überarbeiten, keine Quelle: Scaladoc beantwortet "was tut der Setter", die Tabelle
soll "wofür nimmt man ihn" beantworten. Der Pilot legt den Ton fest.

Gerendert wird die Tabelle als `uic.DataTable` mit `globalFilter`, damit der Leser in 62
DataTable-Settern suchen kann. Die Bibliothek dokumentiert sich mit sich selbst.

### 4. Ausschnitt und Vollansicht

Eine Markierungs-Konvention, zwei Verwendungen, beide vom Makro gelesen und aus dem
angezeigten Text wieder entfernt:

```scala
      // demo:fixtures
      final case class Product(id: String, name: String, category: String, price: Double, stock: Int)
      val products = List(Product("p1", "Bamboo Watch", "Accessories", 65.0, 24), …)
      // demo:end

      Doc.example("A viewport sized by its parent", "flexScroll(true) …", shows = Seq("flexScroll"))(
        div.style(_.height(220.px))(
          uic.DataTable[Product]()
            .rows(products)
            .rowKey(_.id)
            .columns(uic.column("Name")(_.name))
            // demo:show
            .flexScroll(true)
            // demo:end
        )
      )
```

- **Ausschnitt**, was der Editor zuerst zeigt: die `demo:show`-Bereiche plus die Konstruktor-
  Zeile und die schließenden Klammern. Ohne Marker wird aus `shows` abgeleitet (die Zeilen, die
  einen deklarierten Namen nennen, plus Gerüst); ohne beides ist der Ausschnitt das ganze
  Beispiel. Weggefaltete Läufe werden als `… n Zeilen` angezeigt.
- **Vollansicht**: Import-Block, dann der `demo:fixtures`-Ausschnitt der Seite, dann das
  vollständige Beispiel. Der Fixture-Block wird mit demselben Marker-Trick aus der Seitendatei
  geschnitten, ohne `-Yretain-trees`, und deckt Typen (`case class Product`) und Werte
  (`val products`) gleich ab.
- **Der Umschalter läuft auf beiden Transporten.** Er ist ein `SignalRef[Boolean]` pro
  Abschnitt und ein `uic.Button` in der `.demoCodeBar`: im SPA tauscht `DocEditor` das
  CodeMirror-Dokument über `CM.setDoc`, im Server-Push rendert der JVM-Zwilling das andere
  `<pre>`. Unter SSG steht der Ausschnitt.

### 5. Untermenü und Kopf

Rechte Spalte, klebend: `uic.Listbox` mit `itemTemplate`, das jeden Eintrag als
`a.href("#" + slug)` rendert, erster Eintrag "Einstellungen". Der aktive Eintrag folgt dem
sichtbaren Abschnitt über `cmds.observeViewportById` (existiert bereits, wird von
`VirtualScroller` benutzt). Seitenkopf: `uic.Breadcrumb` (Home / Gruppe / Komponente),
`uic.Title`, die beiden Absätze, dann die Tabelle. Kopfleiste der Shell als `uic.Toolbar` mit
dem vorhandenen Theme-`Select` und dem Dark-Schalter.

### 6. Der Gate: `uicdemo.CheckDocs`

Ein JVM-Runner neben `CheckSnippets`, der über `DemoPage.pages` läuft und **fehlschlägt**, mit
vollständiger Liste statt erstem Treffer:

1. jede Einstellung jedes Trägers hat mindestens einen Abschnitt, der sie in `shows` nennt;
2. jeder in `shows` genannte Name kommt im erfassten Quelltext des Abschnitts auch vor
   (sonst zeigt die Tabelle auf ein Beispiel, das die Einstellung nicht benutzt);
3. jeder Abschnitts-Slug ist auf seiner Seite eindeutig;
4. jede Seite hat `what` und `whenToUse`;
5. jede Komponente aus `kyo.uic` hat eine Seite oder eine Tabelle auf der Seite ihres Wirts.

Dazu ein `CheckFullSources` in der Art von `CheckSnippets`, der jede **Vollansicht** für sich
allein kompiliert, ohne Import des Seitenobjekts. Das ist der Beweis, dass die Vollansicht
wirklich vollständig ist, statt es zu behaupten.

## Reihenfolge

Vorweg, weil der Arbeitsbaum der Demo seit `mounted42` uncommitted ist (14 geänderte, 9 neue
Dateien, +3949/-5703): **den Stand als eigenen Commit sichern**, bevor irgendetwas anfängt.
Ohne Basislinie ist kein Vorher/Nachher-Vergleich über `DemoPagesGen` möglich.

| # | Commit | Inhalt |
|---|---|---|
| 0 | Basislinie | Der vorhandene Stand (mounted65, die elf DataTable-Features) als Commit |
| 1 | Routing | `Route`, `CurrentHash`, Nav als Anker, Route pro Seite im Server-Push, Route-Signal im SPA, Hash-Scroll. Inhalte unverändert |
| 2 | Seitenmodell | `ComponentPage[C]`, `what`/`whenToUse`, `shows`, Slugs, `ApiMacro`, `SeedDescriptions`, Tabellen-Rendering, `CheckDocs` **als Report** |
| 3 | Code-Ansichten | Marker im Makro, Ausschnitt-Faltung, Vollansicht mit Imports und Fixtures, Umschalter in beiden `DocEditor`-Zwillingen, `CheckFullSources` |
| 4 | Pilot | `datatable` (44 Abschnitte, 62 Setter, alle Träger) und `button` (12 Abschnitte, 33 Setter) vollständig im neuen Format. Hier wird der Ton festgelegt |
| 5 | Untermenü und Kopf | TOC-Rail, Breadcrumb, Toolbar, Viewport-Hervorhebung, gegen den Piloten gebaut |
| 6-11 | Rollout | Eine Gruppe pro Commit: Actions, Menu, Display, Feedback, Structure, Form. Neue Seiten `text`, `overlaybadge`, `iftalabel` in ihrer Gruppe |
| 12 | Gate scharf | `CheckDocs` von Report auf Fehlschlag, in den Build gehängt |

Nach Schritt 2 sagt der Report exakt, wie viele Einstellungen noch ein Beispiel brauchen. Das
ist die Zahl, die den Rollout bemisst; heute ist sie unbekannt. Grobe Erwartung: rund 900
Setter-Namen mit Beschreibung, 504 vorhandene Beispiele, also einige hundert neue Abschnitte.

## Dateien

**Neu:** `Route.scala`, `ApiMacro.scala`, `CurrentHash.scala` (js/jvm-Zwillinge),
`scalajvm/CheckDocs.scala`, `scalajvm/SeedDescriptions.scala`, `playground/…/CheckFullSources.scala`.

**Geändert:** `Doc.scala` (Seitenmodell, TOC, Tabelle, `chromeCss`), `ExampleMacro.scala`
(Marker, `shows`, Vollansicht), `DemoPage.scala` (Shell, Nav, Route-Auflösung),
`scalajs/DemoMain.scala`, `scalajvm/DemoServer.scala` (Route pro Seite),
beide `DocEditor.scala`, `demo/editor.js` (`setDoc`-Umschaltung), alle sieben Seitendateien
unter `pages/`, `demo/vite.config.ts` (History-Fallback prüfen), `build.sbt`.

**Nicht geändert:** kyo und kyo-ui-components. Wenn im Verlauf doch eine Lücke auftaucht,
ist das ein eigener kyo-Commit plus `mounted<N+1>`-Publish plus Bump, nicht ein Behelf in der
Demo.

## Verifikation

1. `sbt "playground/runMain uicdemo.play.CheckSnippets"` grün (heute 479 Schnipsel).
2. `sbt "playground/runMain uicdemo.play.CheckFullSources"` grün: jede Vollansicht kompiliert
   für sich allein.
3. `sbt "demo/runMain uicdemo.CheckDocs"` grün: kein Setter ohne Beschreibung, keiner ohne
   Beispiel, kein doppelter Slug, keine Seite ohne Beschreibung, keine Komponente ohne Tabelle.
4. `sbt "demo/runMain uicdemo.DemoPagesGen /tmp/pages-neu.txt"` läuft über alle Seiten durch;
   Diff gegen die in Schritt 0 gesicherte Basislinie zeigt nur Gewolltes.
5. `demo/compile` auf JVM und JS grün.
6. Im Browser, **beide Transporte** (`:8080` Server-Push, `:5179` SPA), über den Interceptor:
   - `/c/datatable#zeilen-umsortieren` direkt aufgerufen landet auf der DataTable-Seite,
     gescrollt zum Abschnitt, mit dem TOC-Eintrag hervorgehoben;
   - Klick in der Nav ändert die URL; im SPA ohne Reload, im Server-Push mit;
   - Zurück-Button und Vorwärts-Button gehen durch die zuletzt besuchten Komponenten;
   - ein Klick auf einen Beispiel-Link in der Einstellungs-Tabelle springt zum Abschnitt und
     setzt den Hash;
   - der Code-Bereich zeigt zuerst den Ausschnitt, der Knopf zeigt Imports, `Product`,
     `products` und das ganze Beispiel, der Knopf schaltet zurück;
   - im SPA kompiliert und läuft ein per Run geändertes Beispiel weiterhin.

## Risiken

- **`route.render(id => UI.mounted(…))` im SPA.** Der Inhalt pro Route effektvoll zu bauen
  braucht ein `mounted` innerhalb einer reaktiven Region. kyos Komponenten-CLAUDE.md warnt vor
  einem Mount in einer bereits abonnierten Region (doppeltes Abonnement beim Overlay).
  Hier ist der Abbau bei Routenwechsel gewollt, aber das ist zu verifizieren, bevor Commit 1
  als fertig gilt. Rückfallebene: im SPA weiter alle Seiten eifrig bauen, im Server-Push
  ohnehin nur eine.
- **164 Handler im Server-Push.** Ein `runHandlers` pro Seite ist Absicht, kostet aber
  Startzeit; zu messen, bevor der Rollout darauf aufsetzt.
- **Rund 900 handgeschriebene Beschreibungen** sind der Löwenanteil der Arbeit und die
  Stelle, an der Qualität wegrutscht. Der Pilot legt den Ton fest, `SeedDescriptions` liefert
  nur den Entwurf, und `CheckDocs` erzwingt Vollständigkeit, nicht Güte.
- **Der Ausschnitt kann schlecht falten.** Die Ableitung aus `shows` ist eine Heuristik; die
  Marker sind die Korrektur. Wenn im Piloten mehr als eine Handvoll Abschnitte Marker
  brauchen, ist die Heuristik falsch geraten und wird ersetzt, nicht geflickt.
- **`title-text` aufzuteilen ändert Ids**, also alte Links. Es gibt heute keine Links, das ist
  der günstigste Moment dafür.

---

## Nachtrag: was der Browser gefunden hat (nach mounted80)

Vier Befunde aus einem echten Tastendurchlauf, alle vier reproduziert, bevor etwas geändert wurde.
`interceptor`s Tastenversand ist synthetisch und taugt dafür NICHT: native Radio-Navigation und
jede andere Browser-Aktivierung laufen nur auf `Input.dispatchKeyEvent` über CDP
(`scratchpad/keys.mjs` gegen einen Brave mit `--remote-debugging-port`).

1. **Rating** (`77adb2999`): Pfeiltaste setzte den Wert und löschte ihn sofort wieder. Eine
   Tastaturauswahl an einem Radio ist eine Aktivierung, der Browser feuert `change` UND einen
   `click`, der aus dem Input herausblubbert; beide waren verdrahtet. Der zweite Handler kam aus
   dem bereits neu gerenderten Baum und las den Klick als zweite Wahl desselben Sterns, also
   Prime's Cancel-on-same-value. Eine Aktivierung, ein Handler. Dazu der fehlende Fokusring.
2. **ColorPicker** (`4de174ef7`): die Fläche war zwei Slider, einer je Achse. Für die Ansage
   richtig, für die Bedienung falsch, weil die Fläche EINEN Griff hat. Jetzt ein Tab-Stopp mit
   allen vier Pfeilen; dazu Enter/Space als Bestätigung und Escape als Rücknahme.
3. **PickList** (`bc5a3ac5d`): ein fertiger Transfer wurde vom nächsten Zeilenklick zurückgesetzt.
   Vier verschachtelte Regionen, drei Ref-Schreibvorgänge pro Transfer, und die innere Region
   blieb gegen die Werte abonniert, die sie beim Anlegen geschlossen hatte. Jetzt eine Region über
   `combineLatest`. `UicTest.regionsAbove` prüft die Regel.
4. **OrderList/PickList** (`3e5232fb4`): siehe oben, plus die Akkorde. Ctrl/Cmd + Pfeil trägt die
   Auswahl, mit Shift ans Ende, waagerechter Pfeil überträgt in die andere Spalte, mit Ctrl/Cmd
   die ganze. Die einfachen Pfeile bleiben bewusst dem Highlight: sonst gäbe es keinen Weg zu der
   Zeile, die man verschieben will. Das weicht von der Ansage des Principals ab (dort sollten die
   einfachen Pfeile verschieben) und ist der einzige Punkt, an dem ich das getan habe.


## Nachtrag 2: der DatePicker, mit derselben Methode gegangen (mounted82 bis mounted84)

Sechs Befunde, alle im Browser mit echten Tasten gefunden und je einem Test festgehalten, bevor
etwas geändert wurde.

1. **Ein Pick warf den Leser an den Seitenanfang** (`76126ffc5`). Der Pick schließt das Panel, das
   Panel nimmt das Gitter mit, auf dem der Leser steht, und der Fokus fiel auf `body`. Escape gab
   ihn immer schon zurück; jetzt tut es jeder Abschluss im Panel, Tagespick, fertige Range,
   Monats- oder Jahrespick und die Button-Leiste.
2. **Der eingebettete Kalender schloss sich selbst** (`76126ffc5`). Derselbe Close lief für
   `inline(true)`, wo es kein Panel über der Seite gibt: ein Klick auf einen Tag nahm den ganzen
   Kalender vom Bildschirm. Inline bleibt jetzt unangetastet, vom Pick wie von Escape.
3. **Monats- und Jahresgitter waren überhaupt nicht erreichbar** (`76126ffc5`). Die Titelknöpfe
   rendern ohne `currentView`-Ref deaktiviert und ein Drill-down hatte ohne `month`-Ref nichts zu
   schreiben. Der Mount prägt die View-Ref jetzt neben der Monats-Ref, und der Tages-Cursor
   verlangt ein vollständiges Datum, weil er sich die Ref mit den groben Gittern teilt.
4. **Ein Klick säte die Hervorhebung nicht** (`76126ffc5`), also fing der nächste Pfeil dort an,
   wo das Gitter zuletzt selbst einen Cursor ableitete.
5. **Ein View-Wechsel verlor den Fokus** (`010ea567e`, vollständig erst `bdc3ef29b`). Der Kopf, den
   der Wechsel zeichnet, enthält den gedrückten Knopf nicht mehr. Der erste Versuch war ein
   Fokus-Kommando und griff nur dort, wo der Patch den Knoten wiederverwendet: vom Monats- ins
   Tagesgitter wird aus einem `div` eine `table`, das Kommando fokussierte den Knoten, den der
   Patch gerade wegwarf. Jetzt sagt das Gitter selbst, dass es den Fokus nimmt: ein Flag steht auf
   "die Tastatur kommt herein", das Gitter rendert `focusAuto`, und der Client fokussiert es in
   demselben Patch, der es einfügt. Das löst auch die zwei ArrowDown-Drücke auf einen auf.
6. **`role="gridcell"` ohne `role="row"`** (`010ea567e`) in beiden groben Gittern. Die Zellen
   stehen jetzt in Zeilen der Breite, die die senkrechten Pfeile ohnehin schreiten, und
   `display: contents` gibt sie Primes Flexbox zurück. Im echten Browser nachgemessen: der
   AX-Baum zeigt `grid` → 4 `row` → 12 `gridcell`, und die Zellen liegen auf dieselbe Pixelzeile
   wie vorher.

Verifiziert über beide Transporte (`:5179` SPA und `:8080` Server-Push) mit demselben Skript:
Feld → ArrowDown → Tagesgitter → Shift+Tab auf den Jahrestitel → Enter → Jahresgitter → Pfeile →
Enter → Monatsgitter → Enter → Tagesgitter → Enter → Wert geschrieben, Fokus zurück im Feld.
Mit echter Maus geprüft, dass ein Klick den Ring nicht zeichnet und den Leser im Feld lässt.


## Nachtrag 3: die vier Ansagen des Principals zum DatePicker (mounted85)

1. **Escape ist ein Cancel.** Die Werte, die das Panel hielt, werden beim ERSTEN Schreibvorgang
   darin gemerkt, nicht beim Öffnen: die Open-Ref gehört dem Aufrufer, ein von der Anwendung
   geöffnetes Panel läuft nie durch den Open-Pfad der Komponente. Mehrfachauswahl, halbe Range und
   eine hochgedrehte Stunde gehen zurück; ein Pick, der schließt, ist ein Commit und vergisst.
   `onChange` schweigt beim Zurücknehmen, weil über das ganze Öffnen nichts passiert ist.
2. **Öffnen setzt den Fokus ins Gitter**, egal wodurch. Vorher hing es daran, wie geöffnet wurde.
   Preis: das Textfeld hält den Cursor nicht mehr, solange das Panel offen ist, Tippen also erst
   nach Escape oder Shift+Tab.
3. **Ctrl oder Cmd macht aus einem Pfeil eine Seite**: waagerecht ein Monat, senkrecht ein Jahr,
   mit Shift mal vier bzw. mal zehn. Oben ist bei der getragenen Variante vorwärts in der Zeit,
   beim einfachen Pfeil rückwärts durch das Gitter, weil das zwei verschiedene Gesten sind.
4. **Enter und Space wählen und schließen**, wie zuvor. Offen bleibt das Panel nur, wo der Modus
   noch etwas zu holen hat: ein zweites Datum, das andere Ende einer Range, eine Uhrzeit.

Über beide Transporte mit echten Tasten geprüft (`:5179` und `:8080`), Akkorde und Cancel Schritt
für Schritt identisch.


## Nachtrag 4: die Auswahlfelder, mit derselben Methode gegangen (mounted86)

Vier Befunde, alle im Browser mit echten Tasten gefunden, jeder mit einem Test festgehalten, der
auf dem alten Stand aus dem richtigen Grund rot wird.

1. **Der Öffnen-Taster landete auf nichts.** Ein ArrowDown auf dem geschlossenen Feld öffnete das
   Panel und setzte keine Hervorhebung: der Leser musste ein zweites Mal drücken, um die erste
   Zeile zu erreichen. In Select, MultiSelect, TreeSelect und CascadeSelect gleichermaßen; nur
   AutoComplete machte es richtig und hatte die Regel sogar im eigenen Kommentar stehen. Jetzt
   landet jedes Öffnen auf einer Zeile: der gewählten, sonst der ersten, auf der eine Hervorhebung
   sitzen darf. Bei CascadeSelect ist es die erste Wurzelzeile, weil die Kette geschlossen aufgeht
   und ein drei Ebenen tiefes Blatt gar keine Zeile auf dem Bildschirm hat.
2. **Tab ließ ein offenes Panel zurück.** Der Fokus ging zum nächsten Bedienelement, und über der
   Seite blieb eine hervorgehobene Liste stehen, die keine Taste mehr beantwortete. Jetzt schließt
   Tab in beide Richtungen, in allen fünf, weil die Tastatur des Panels genau auf dem Element
   sitzt, von dem der Tab den Leser wegträgt.
3. **MultiSelects Select-all wurde doppelt gelesen.** Die Kopf-Checkbox war ein Tab-Stopp INNERHALB
   des Panels; ein Space darauf schaltete alle Optionen an und erreichte über das Bubbling
   zusätzlich den Trigger, der ihn als Aktivierung der hervorgehobenen Option las und diese wieder
   abwählte (gemessen: aus "alle vier" wurde "drei von vier"). Die Box behält Rolle und Zeiger,
   verliert den Tab-Stopp (`CheckBox.tabbable(false)`, paketintern) und trägt eine gestempelte Id,
   an der der Trigger eine Taste aus ihr erkennt und liegen lässt. Ihr Tastenweg ist jetzt der
   ARIA-Akkord Ctrl bzw. Cmd + A auf dem Element, das den Fokus wirklich hält.
4. **`focusRestore` holte den Leser zurück, den es gar nicht verloren hatte** (kyo-ui, eigener
   Commit). Beim Entfernen eines gesäten Panels stellte der Sweep den Fokus bedingungslos auf das
   Rückkehrziel. Mit Befund 2 zusammen wäre das eine Verschlechterung geworden: der Leser tabbt
   heraus, der Browser setzt den Fokus weiter, das Panel schließt, und der Sweep zog ihn auf den
   Trigger zurück. Ein Restore ist für den Fokus da, den das Entfernen mitgenommen hat, und den
   erkennt man daran, dass der Browser `activeElement` auf `body` fallen lässt. Beide Clients
   (`DomBackend`, `HtmlRenderer.clientJs`) prüfen das jetzt, `FocusableTest` hält sie daran fest.

**Weitergereicht, nicht Teil dieses Durchgangs:** keine Komponente der Bibliothek scrollt ihre
Hervorhebung in den sichtbaren Bereich (`grep scrollIntoView` findet in `kyo/uic` nur Terminal).
Bei einer Liste, die an Primes 14rem-Deckel scrollt, wandert die Hervorhebung damit aus dem Bild.
In der Demo ist keine Liste lang genug, um es vorzuführen, also ist es aus dem Quelltext
geschlossen und nicht gemessen. Die Behebung braucht vermutlich ein deklaratives Attribut in
kyo-ui (ein Kommando adressiert beim Öffnen eine Zeile, die es noch nicht gibt: die
DatePicker-Lehre), betrifft die ganze Listenfamilie und ist damit ein eigener Kampagnenpunkt.


## Nachtrag 5: die Roving-Gruppe, mit echten Tasten nachgemessen (mounted87)

Commit 4 der Kampagne ("Roving mit Pfeilen") hat vier von fünf aufgelisteten Komponenten
angefasst. Der Browserdurchlauf bestätigt die vier und findet die fünfte.

**Was hält:** Tabs, Stepper, Carousel und Galleria beantworten die Pfeile, bewegen den Fokus ohne
zu aktivieren, aktivieren auf Enter und Space, ziehen Panel bzw. Seite nach, und der Tab-Stopp
sitzt auf dem GEWÄHLTEN Kind (nicht auf dem fokussierten) — das ist die APG-Manual-Activation-Form
und Primes eigene. Home und End erreichen die Enden, Tab verlässt die Gruppe. Beim senkrechten
Carousel greifen ArrowUp/Down und ArrowRight bleibt der Seite. Nichts davon war zu ändern.

**Was fehlte: SelectButton.** Im Plan unter Befund C und in der Reihenfolge unter Commit 4
aufgeführt, im Commit selbst nicht enthalten. Gemessen: `role="group"`, zwei bis fünf
`aria-pressed`-Toggle-Buttons, jeder ein eigener Tab-Stopp, keine Pfeiltaste tut etwas.

Die Entscheidung war nicht "Pfeile nachrüsten", sondern eine Rollenfrage: eine Auswahl von EINEM
aus mehreren ist eine Radiogruppe, egal woraus sie gebaut ist. Also sagt sie das jetzt
(`role="radiogroup"` über `role="radio"` mit `aria-checked` statt `aria-pressed`; beides zugleich
zu tragen wäre für einen Screenreader eine zweite, andere Geschichte über dasselbe Bedienelement),
und damit kommt die Tastatur des Musters: ein Tab-Stopp auf der gewählten Option (sonst auf der
ersten wählbaren), alle vier Pfeile bewegen UND wählen, mit Umlauf, Home und End, deaktivierte
Optionen übersprungen. Bewegen leert nie — nur ein Klick oder Space auf der bereits gewählten
Option tut das, und nur solange `allowEmpty` an ist. `multiple(true)` bleibt unangetastet: mehrere
unabhängige Entscheidungen SIND mehrere Toggle-Buttons, jeder mit dem Tab-Stopp, den ein Button
von sich aus hat.

`ListNav` hat dafür eine dritte Orientierung bekommen, `Both`: Down und Right sind eine Bewegung,
Up und Left die andere. Das ist genau das, was ein Leser von nativen Radios geschenkt bekommt
(Rating fährt darauf), also muss eine aus Buttons gebaute Gruppe dieselben Tasten beantworten.

Über beide Transporte geprüft, Schritt für Schritt identisch, samt Umlauf, übersprungener
deaktivierter Option, itemTemplate-Gruppe (Icons behalten ihre `aria-label`) und der
Mehrfachgruppe, die weiterhin drei Tab-Stopps hat.


## Nachtrag 6: SpeedDial, mit echten Tasten nachgemessen (mounted88)

Commit 5 der Kampagne ("SpeedDial wird ein Menü") hält dem Durchlauf bis auf einen Punkt stand.

**Was hält:** Der geschlossene Fächer rendert gar keine Aktionen, statt sie mit `scale(0)` zu
verstecken, ein Leser tabbt also nicht mehr durch einen Wählscheiben-Inhalt, den er nie geöffnet
hat. Der Pfeil, der vom Auslöser in den Fächer zeigt, öffnet ihn und trägt den Fokus hinein, der
Gegenpfeil bleibt der Seite. Die Pfeile entlang der eigenen Achse laufen durch und kommen um, Home
und End erreichen die Enden, Enter führt die Aktion genau einmal aus und schließt, Escape schließt
ohne auszuführen, und der Fokus geht in beiden Fällen an den Auslöser zurück. Von außen geöffnet
(die zweite Demo bindet `open`) sät es den Fokus genauso, und Escape gibt ihn dem Knopf zurück, von
dem er kam.

**Was fehlte: das Loslassen.** Tab trug den Leser zum nächsten Bedienelement und ließ den Fächer
offen über der Seite stehen, `aria-expanded="true"` auf einem Menü, in dem niemand mehr war, mit
Aktionen, die keine Taste mehr erreichte. Das ist derselbe Befund wie bei den Auswahlfeldern in
mounted86, an einer anderen Bauform.

Die Behebung ist mehr als eine Taste. Damit Tab in beide Richtungen aus dem Widget herausführt,
darf keine Aktion ein Tab-Stopp sein: mit `tabIndex(0)` auf der ersten landete ein Shift+Tab von
der dritten Aktion wieder INNERHALB des Fächers, und ein Schließen hätte den Fokus ins Nichts
fallen lassen. Jetzt trägt jede Aktion `-1`, die Wählscheibe ist der eine Tab-Stopp, der ein
Menüknopf ist, der Fokus kommt durchs Öffnen hinein (das ist auch der Rückweg in einen Fächer, den
der Leser mit der Maus verlassen hat) und geht durch ein Tab hinaus, das schließt. Der Auslöser
schließt auf Tab mit, denn seine Aktionen sind nicht mehr das, wohin er tabbt.

Über beide Transporte Schritt für Schritt identisch geprüft, samt Shift+Tab, Tab vom Auslöser aus,
Wiedereintritt per Pfeil und dem von außen geöffneten Fächer.

**Werkzeugbefund, wichtig für alle weiteren Durchläufe:** `Input.dispatchKeyEvent` mit
`type: "rawKeyDown"` erzeugt für Enter keinen nativen Klick. Ein `<button>` wirkt damit auf Enter
tot, und eine Doppelaktivierung bliebe unsichtbar, weil nur die Emulation des Frameworks feuert.
Enter braucht `type: "keyDown"` mit `text: "\r"`. `scratchpad/cdp.mjs` setzt es jetzt; mit dem
richtigen Enter läuft SpeedDials Aktion genau einmal, die Unterdrückung greift also.


## Nachtrag 7: TreeTables Treegrid, mit echten Tasten nachgemessen (mounted89)

Commit 6 der Kampagne ("TreeTable bekommt das Treegrid") hält bis auf einen Punkt.

**Was hält:** Die Pfeile laufen über die Zeilen, die auf dem Bildschirm sind, ein eingeklappter
Teilbaum ist also schlicht nicht da. Rechts öffnet einen geschlossenen Elternknoten, ohne ihn zu
verlassen, ein zweiter Druck steigt in die Kinder ein, die er gerade gezeigt hat; links schließt
einen offenen und klettert sonst zum Elternknoten. Home und End erreichen die Enden. Enter und
Space wählen die Zeile und klappen sie mit demselben Druck auf oder zu. Der Fokus überlebt das
Re-Rendern, das ein Auf- oder Zuklappen auslöst. Die sortierbaren Kopfzellen sind eigene
Tab-Stopps und sortieren auf Enter und Space samt Modifikatoren. Beide Transporte identisch.

**Was fehlte: der Tab-Stopp blieb stehen.** Er hing fest auf der ersten Zeile. Wer zwanzig Zeilen
tief läuft, zum nächsten Bedienelement tabbt und zurückkommt, stand wieder ganz oben, mit allen
Pfeildrücken noch einmal vor sich. Genau davor schützt ein rovender tabindex, und genau das sagt
auch Invariante 2 dieses Plans: `tabIndex(0)` gehört auf das AKTIVE Kind.

Die Behebung braucht Zustand. Das DOM weiß, wo der Fokus ist, ein Render nicht, also prägt der
Mount eine Cursor-Ref neben den Ids, geschlüsselt über den Zeilenschlüssel statt über den Index
(ein Aufklappen oberhalb würde einen Index verschieben). Der Tab-Stopp leitet sich daraus ab: der
Cursor, sonst die gewählte Zeile, sonst die erste. Ein Klick sät ihn mit, damit Zeiger und Tastatur
die Tabelle im selben Zustand hinterlassen; dafür bekommt auch eine Tabelle ohne Auswahl einen
Klick-Handler auf der Zeile. Preis ist ein Re-Rendern der Region je Pfeiltaste, was die
Listenfamilie für ihre Hervorhebung ohnehin zahlt und was DataTable bewusst nicht tut: dort IST die
Tab-Reihenfolge die editierbaren Zellen, es gibt keinen Cursor aufzuheben.

Dazu eine Kleinigkeit aus derselben Ecke: das Treegrid sagt jetzt `aria-multiselectable`, wo
mehrere Zeilen gleichzeitig gehalten werden dürfen. DataTable braucht das nicht, weil seine
`<table>` gar keine Grid-Rolle trägt; dort wäre das Attribut ohne Bedeutung.

**Werkzeugnotiz:** Das Scratchpad unter `/tmp` wurde zwischen zwei Läufen dieser Sitzung
aufgeräumt, samt CDP-Helfer, Publish-Skript und Argumentliste. Beides liegt jetzt unter
`Plans/probe/` (nicht versioniert, aber dauerhaft), die Argumentliste lässt sich aus
`~/.ivy2/local/io.getkyo/*/<version>` neu erzeugen. Und die alte Falle noch einmal: vite liefert
nach einem `fastLinkJS` das alte Bundle aus, bis man es mit gelöschtem `node_modules/.vite` neu
startet; im ersten SPA-Durchlauf sah der neue Tab-Stopp deshalb aus wie der alte.


## Nachtrag 8: die Menüfamilie, mit echten Tasten nachgemessen (mounted91)

Commit 8 der Kampagne ("Menüstruktur") hält in den Bewegungen und hatte zwei Lücken.

**Was hält:** In allen fünf (Menu, Menubar, TieredMenu, ContextMenu, MegaMenu) laufen die Pfeile
über die Ebene, auf der der Leser steht, und kommen um; rechts öffnet ein Untermenü auf seiner
ersten Zeile, links schließt es und kehrt auf die Elternzeile zurück; die Menüleiste wechselt
seitwärts zwischen den Wurzeln und öffnet nach unten, und aus einem Blatt heraus trägt ein
Seitwärtspfeil zur nächsten Wurzel und öffnet sie. Home und End erreichen die Enden der Ebene.
Enter führt ein Blatt aus und schließt alles. Escape verlässt genau eine Ebene pro Druck, die
Hervorhebung landet dabei auf der Elternzeile. Das inline stehende Menu setzt beim Fokussieren die
Hervorhebung auf die erste bedienbare Zeile und nimmt sie beim Verlassen zurück.

**Erste Lücke: Tab ließ jedes offene Panel stehen.** Der Fokus ging zum nächsten Bedienelement,
und über der Seite blieb ein Menü mit gesetzter Hervorhebung, das keine Taste mehr beantwortete;
bei der Menüleiste standen dabei bis zu drei Ebenen offen. Jetzt gilt: Escape verlässt eine Ebene,
Tab verlässt das Widget, also schließt es alle auf einmal. Das steht in den Zustandsmaschinen
(`MenuNav`, und `MegaNav` für die eine, die nicht über MenuNav läuft), nicht in fünf Handlern.

**Zweite Lücke: die Ansage hing an einer Id, die niemand setzt.** `aria-activedescendant` wurde
nur gerendert, wenn der Aufrufer `id(...)` gerufen hatte. Kaum eine Seite tut das, weil von außen
nichts auf die Zeilen eines Menüs zeigt: die Hervorhebung wanderte also sichtbar und wurde nicht
angesagt. Der Mount prägt die Basis-Id jetzt selbst, neben den Ids, die er ohnehin prägt; die
eigene Id des Aufrufers gewinnt weiterhin. Der `wired`-Schnitt nimmt die Basis als Parameter, damit
die Golden-Renders die Form zeigen, der der Leser begegnet, und nicht die stumme. SplitButtons
eingebettetes Menu leitet seine Id aus der des Knopfes ab.

Verifiziert über beide Transporte, samt Escape-Leiter, Enter auf einem Blatt und dem SplitButton-
Menü. Vier Demo-Seiten trugen außerdem noch die Behauptung, es gebe keine Pfeiltastennavigation;
das war seit dem Tastatur-Commit falsch und ist jetzt korrigiert, alle fünf haben einen
Tastaturabschnitt.

**Werkzeugnotiz:** Zweimal in diesem Durchgang zeigte der Browser den alten Stand. Einmal, weil
`kill` auf den sbt-Launcher den geforkten DemoServer nicht mitnimmt (er lief mit dem alten
Klassenpfad weiter; `ps -o etimes` auf den `uicdemo.DemoServer`-Prozess ist der schnellste Test),
einmal wegen vites `.vite`-Cache. Und: `pgrep -f <muster>` trifft die eigene Shell mit, deren
Kommandozeile das Muster enthält; ein `kill` darüber beendet den eigenen Lauf (Exitcode 144). Erst
über `/proc/<pid>/comm` auf `java`/`bun` filtern.


## Nachtrag 9: der weitergereichte Befund, eingelöst (mounted92)

Aus Nachtrag 4 stand offen: keine Komponente scrollt ihre Hervorhebung in den sichtbaren Bereich.
Das war aus dem Quelltext geschlossen und nicht gemessen, weil in der Demo keine Liste lang genug
war, um es vorzuführen. Beides ist jetzt erledigt.

**Gemessen, als Gegenprobe im Browser:** `Element.prototype.scrollIntoView` auf einen No-op gesetzt
(das ist die Welt vor diesem Commit), dann in einer 60-Zeilen-Listbox in einem 200px-Kasten den
Fokus gesetzt und die Pfeile gedrückt. Die Hervorhebung stand auf Zeile 30, dann 42, dann 60, der
Kasten blieb auf `scrollTop: 1`, `visible: false` — der Leser steuert etwas, das er nicht sieht.
Danach wiederhergestellt: `End` → `scrollTop: 2176`, sichtbar.

**Warum ein Kommando dafür nicht reicht.** `cmds.scrollIntoViewId` funktioniert, solange die Zeile
schon auf der Seite steht. Der wichtigere Moment ist der andere: ein Panel, das auf die gewählte
Option aufgeht, muss zu einer Zeile scrollen, die es beim Lauf des Handlers noch gar nicht gibt (die
DatePicker-Lehre). Also ein deklaratives Attribut in kyo-ui, wie vermutet.

**`scrollAuto(Boolean)`** setzt `data-kyo-scroll-auto`. Bleibt das Flag nach einem Patch auf einem
ANDEREN Element als vorher, scrollt der Client dieses Element mit `block: "nearest"` ins Bild: das
bewegt den nächsten scrollenden Vorfahren so wenig wie möglich und gar nicht, wenn die Zeile schon
sichtbar ist. Der Unterschied zu `focusAuto` ist genau dieser Wiederholungsfall: focusAuto feuert
einmal beim Einfügen (sonst stiehlt ein Echo-Render den Fokus), scrollAuto folgt der Bewegung, denn
die Bewegung IST der Anlass. Beide Clients (`DomBackend`, `HtmlRenderer.clientJs`) führen dieselbe
Trägermenge; der erste Anstrich merkt sich nur, was schon trägt, ohne zu scrollen.

Die Hosts markieren die hervorgehobene Zeile: 16 Stellen in Listbox, Tree, Menu, Menubar,
MegaMenu, Select, MultiSelect, AutoComplete, CascadeSelect und DatePicker. Die Roving-tabindex-
Familie braucht es nicht, dort bewegt sich echter Fokus, und den scrollt der Browser selbst.

**Im Browser über beide Transporte gemessen, Zahl für Zahl gleich:** ein Select mit 57 Jahren geht
auf `1994` auf und der Panel steht auf `scrollTop: 767` von 2261, die Zeile sichtbar; zehnmal
ArrowDown → 1163; `End` → 2033; `Home` → 4. Die Listbox im 200px-Kasten: Fokus → 1091 (die gewählte
Zeile 30), zwölfmal Down → 1464, `End` → 2176, `Home` → 5, jedes Mal sichtbar.

**Tests:** vier neue in `FocusableTest` (echtes Chrome, Server-Push): das Attribut wird geschrieben,
eine wandernde Markierung zieht ihren Kasten nach, sie zieht ihn auch wieder zurück, und ein
Repaint, der das Flag stehen lässt, scrollt nichts — die beiden mittleren auf dem Vorzustand
nachweislich rot. Dazu eine Invariante in `GoldenRenderTest`: jedes Element, das `p-focus` rendert,
trägt das Flag, für die ganze Familie auf einmal.

**Zwei Demo-Beispiele**, weil bisher keine Liste lang genug war: ein Select mit 57 Optionen (Panel
bei 14rem gedeckelt) und eine Listbox mit 60 Zeilen in einem 200px-Scroller. Nebenbei aufgefallen
und in PARITY nachgetragen: kyos Listbox hat kein `scrollHeight` wie Primes (Standard 14rem), sie
richtet sich nach ihren Optionen und überlässt den Deckel der Seite.


## Nachtrag 10: der Tastaturabschnitt auf allen 84 Seiten (mounted93)

Der letzte offene Punkt der Reihenfolge (15, zweite Hälfte). Alle 84 Komponentenseiten haben jetzt
einen Tastaturabschnitt, 68 hier geschrieben, 16 aus den Durchgängen, die sie erzeugt haben. Dazu
die `CheckDocs`-Regel, dass eine Seite ohne einen ein Befund ist; nachgewiesen, dass sie greift
(einen Abschnitt umbenannt, Lauf endet mit `page with no keyboard section (1)` und Exitcode 1).

**Was ein Abschnitt sagt, hängt davon ab, was die Komponente ist.** Ein Verbundwidget zählt seine
Tasten auf und sagt, wo der Tab-Stopp sitzt, weil das der Teil ist, den man keinem Screenshot
ansieht. Ein natives Bedienelement sagt, welches Element es wirklich ist, denn daraus folgen die
Tasten: eine versteckte Checkbox hinter dem Schalter, namensgleiche Radios hinter dem Rating, ein
Range-Input hinter dem Slider. Eine Komponente ohne Tastatur sagt genau das und wohin man statt
ihrer greift. Schweigen wäre derselbe Befund wie ein fehlender Abschnitt: der Leser muss es
ausprobieren, um es zu erfahren.

**Jede Behauptung, die nicht schon aus einem Durchgang gemessen war, wurde im Browser gegen diesen
Build geprüft:** Enter und Space auf einem klickbaren Tag, Space und Enter auf einer Checkbox, die
Pfeile, die in einer Radiogruppe bewegen UND wählen, die Range-Tasten am Slider, die Paginator-
Knöpfe als je eigener Tab-Stopp, die Spin-Knöpfe als eben keiner, Enter und Space auf der
Passwort-Enthüllung und auf einem Panel-Kopf, Backspace auf dem Entfernen-Knopf eines Chips, und die
Scrolltasten auf ScrollPanel und VirtualScroller, sobald der Scroller den Fokus hat.

**Drei Abschnitte wurden nach dem Blick in die Quelle umgeschrieben:** Stepper rovt seine Köpfe auch
in der Panel-Form (senkrechte Pfeile), DataViews `layout` ist eine statische Einstellung und kein
Schalter, und die Formularschicht scrollt das erste fehlerhafte Feld zusätzlich ins Bild.

**Ein Befund fiel dabei ab und ist behoben** (`ece737c23`): `uic.Tag` nimmt ein `onClick` (kyo-
Erweiterung über Primes inertes Tag) und tat damit nichts außer den Zeigercursor zu setzen. Die
Aktion war für die Maus da und für sonst niemanden, weil der Chip weder erreichbar noch bedienbar
war. Jetzt trägt er dieselben drei Zeilen wie Card, Avatar und Icon: `role="button"`, ein
Tab-Stopp, Enter und Space. Ein Tag ohne Handler bleibt ein Etikett ohne Tab-Stopp, und der Golden
hält beide Richtungen fest.


## Nachtrag 11: der Gate scharf, und die Verifikationsliste abgelaufen (Demo-Commit 26c7208)

Schritt 12 des Demo-Plans. Der Rollout selbst war schon fertig, nur hatte es niemand
aufgeschrieben: `CheckDocs` läuft über 84 Seiten, 98 Tabellen und 976 beschriebene
Einstellungen und findet nichts, und es scheitert seit dem Tastaturabschnitt-Commit auf jeden
Befund. Was fehlte, war die andere Hälfte des Schritts: dass irgendetwas die drei Prüfungen
auch ausführt.

**Zwei der drei waren gar keine Gates.** `CheckSnippets` und `CheckFullSources` haben ihre
Fehlschläge gedruckt und mit Exitcode 0 geendet, und `CheckSnippets` hat seine
Owner-Mismatch-Befunde nach dem Druck fallen lassen, statt sie zu zählen: ein Abschnitt, der
der falschen Seite zugeschrieben ist, meldete sich in ein Scrollback, das niemand liest.

**Jetzt sind alle drei Test UND Hauptklasse.** Die eigentliche Prüfung ist je eine Funktion,
die Befunde zurückgibt; die Hauptklasse druckt sie und endet mit 1, die munit-Suite lässt die
Behauptung platzen und hängt den Bericht an. `sbt test` ist damit das eine Kommando, das
beantwortet, ob die Demo noch stimmt. sbt 2 führt dabei aus, was fehlschlug, was nie lief und
worunter sich eine transitive Abhängigkeit geändert hat: eine geänderte Seite zieht alle drei
nach (jede Seite erreicht sie über `DemoPage`), ein Lauf ohne Änderung kostet nichts.
`testOnly *` erzwingt den vollen Durchgang. Zusammen 41 s.

**Drei Brüche, einer nach dem anderen, jeder auf seine Prüfung isoliert:**

| Bruch | Was rot wird | Was grün bleibt |
|---|---|---|
| Ein Tastaturabschnitt in „Keys" umbenannt | `demo/test`: *page with no keyboard section* | die beiden anderen |
| Eine Seiten-Fixture auf `private` gesetzt | `CheckSnippetsTest`: 5× *Not found: events* | die Vollansichten |
| Dieselbe Fixture unter `def content` geschoben | `CheckFullSourcesTest`: dieselben 5 | die Snippets |

Der mittlere und der letzte Bruch sind kein Kunstgriff, sondern genau die Drift, gegen die die
beiden Prüfungen stehen: `private[uicdemo]` ist, wie eine Seite ihre Daten dem Playground
öffnet, und der Fixture-Bereich endet bei `def content`, also ist ein Helfer weiter unten in
der Datei für die Vollansicht nicht vorhanden.

**Die JS-Reihe bekommt eine leere Framework-Liste.** Sie trägt keine Tests und existiert, um
das SPA-Bündel zu linken; unangetastet startet `test` trotzdem Scala.js' Node-Umgebung, nur um
sie nach ihrer (leeren) Liste zu fragen. Node ist auf dieser Maschine kaputt (`libada.so.3`
fehlt, `.so.4` liegt da), der Aufruf endet mit 127, und der ganze Gate wäre damit unbenutzbar
gewesen. Nebenbei die Erklärung dafür, dass die Demo über `bun --bun` läuft und nicht über
`bun run dev`, das den node-Shebang von vite nimmt.

**Die Verifikationsliste des Demo-Plans, Punkt für Punkt abgelaufen:** `CheckSnippets`,
`CheckFullSources` und `CheckDocs` grün, `DemoPagesGen` schreibt alle 84 Seiten,
`demo/compile` auf JVM und JS grün, und der Browserdurchlauf über beide Transporte
(`:8080` Server-Push, `:5179` SPA), mit echten Tasten und echten Klicks:

- `/c/datatable#lazily-loaded-rows` direkt aufgerufen landet auf der Seite, der Abschnitt steht
  bei `top: 64` im Bild, die Brotkrume sagt Structure / DataTable, die Nav-Zeile ist
  hervorgehoben und der TOC-Eintrag heißt „Lazily loaded rows". Auf beiden Transporten gleich.
- Ein Klick in der Nav ändert die URL. Im SPA überlebt eine vorher gesetzte Marke am `window`
  den Wechsel, im Server-Push nicht: genau der Unterschied, den die beiden haben sollen.
- Zurück und vorwärts laufen durch die zuletzt besuchten Komponenten, samt mitgeführtem Hash.
- Ein Beispiel-Link aus der Einstellungstabelle setzt den Hash und stellt den Abschnitt ins
  Bild (`top: 63`), der TOC-Eintrag zieht mit.
- Der Code-Bereich zeigt zuerst den Ausschnitt. Im Server-Push tauscht der Knopf das `<pre>`
  gegen die Vollansicht (252 → 1313 Zeichen, mit Imports und `products`) und wieder zurück; im
  SPA klappt er die zwei versteckten Läufe im Editor auf (238 → 287 Zeichen, 2 → 0
  Faltmarken) und setzt die Präambel darüber. Zwei Bauformen derselben Zusage, beide erfüllt.
- Ein im SPA geändertes Beispiel kompiliert und läuft: ` uic.Tag("Edited"),` in den Editor
  getippt, Run gedrückt, und der Ergebnisrahmen zeigt „Primary Edited Success Info …", ohne
  Compilerfehler in der Leiste.

**Zwei Messungen, die erst wie Befunde aussahen und keine waren.** Der SPA-Knopf schien den
Editorinhalt nicht zu ändern: gemessen an einem Beispiel, das gar nichts zu falten hat, und
gelesen an `.cm-content`, wo die Präambel als eigenes `<pre>` daneben steht. Beides
nachgemessen statt behoben.

Die Skripte liegen bei den anderen in `Plans/probe/` (`route12*.mjs`).
