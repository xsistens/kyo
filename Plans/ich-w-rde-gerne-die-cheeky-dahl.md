# Interaktive Beispiele für die kyo-ui-components Demo

## Context

Die Demo unter `~/Programming/kyo-ui-components` dokumentiert 76 Komponenten auf 81 Seiten
mit 557 Beispiel-Sektionen. Jede Sektion zeigt ein Live-Beispiel und darunter dessen
Quellcode. Besucher sollen den Quellcode direkt in der Seite ändern und das Ergebnis
sofort sehen können.

Das eigentliche Hindernis ist nicht der Editor, sondern die heutige Datenlage: der
gezeigte Quellcode ist ein **handgetipptes Duplikat** des Live-Beispiels, kein
abgeleiteter Text.

- `Doc.section(title, text, code)(examples*)` (`demo/src/main/scala/uicdemo/Doc.scala:44`)
  nimmt Code und Live-Beispiel als zwei unabhängige Argumente. 557 Aufrufe, davon 479 mit
  Code-String, alle positional als Triple-Quoted-Literal.
- 31 der 479 driften bereits messbar vom Live-Beispiel weg. `StructurePages.scala:334` hat
  einen Code-String, aber ein leeres `examples`; `FeedbackPages.scala:269` zeigt ein
  `svc <- Env.get[uic.ToastService]` ausserhalb jedes `for`.
- Rund 40 Prozent der Strings sind gar keine Ausdrücke, sondern kommaseparierte Fragmente
  einer varargs-Liste. Der umschliessende `FlexBox` wird konsequent weggelassen.
- 260 Sektionen referenzieren einen `SignalRef`, der in der `content`-For-Comprehension der
  Seite gebunden ist. `DataTablePage` hoistet allein 21 Refs (`StructurePages.scala:232-254`).
- 87 Sektionen referenzieren ein Page-Level-`val`/`def`/`case class` (`Product`, `products`,
  `cities`, `files`, `stockTag`, `translator`, ...), das im Snippet nirgends definiert ist.

Ein Snippet, das kompiliert werden soll, muss vollständig sein. Deshalb ist die Umstellung
auf **makro-gefangene, self-contained Snippets** nicht Beiwerk, sondern der Kern der Arbeit:
danach ist gezeigter Code per Konstruktion gleich laufendem Code, Drift ist strukturell
unmöglich, und jede Sektion wird ohne Zusatzarbeit editierbar.

Für die Ausführung gibt es einen Weg, der Scala.js komplett umgeht: `DemoServer`
(`demo/src/main/scalajvm/uicdemo/DemoServer.scala`) fährt die gesamte Demo bereits
serverseitig über `UI.runHandlers` als SSR plus WebSocket-Patches, und alle `kyo.uic`
Komponenten sind plattformneutraler shared source. Ein Snippet muss also nur zu
**JVM-Bytecode** kompiliert werden. Kein Scala.js-Linking, kein Multi-MB-Bundle pro Lauf.

Externe Playgrounds scheiden aus: **scribble.ninja** kompiliert nur gegen kuratierte
Templates mit fester Library-Liste (`laminar_3`, Scala 3.3.1, Scala.js 1.17.0), nimmt also
keine beliebigen Maven-Koordinaten. **Scastie** könnte es prinzipiell (beliebige public
Libraries, `targetType: 'js'`, `embedded.js` mit Code-Prefill), scheitert aber an zwei
externen Vorbedingungen: `kyo-ui-components` liegt noch nicht auf Maven Central (die Demo
konsumiert `1.0.0-RC5-mounted41-SNAPSHOT` aus ivy-local), und kyo braucht Scala 3.8.4 auf
JDK 25 (RC5-Makros sind class-file major 69). Der Link-out bleibt als spätere Option
dokumentiert, ist aber kein Fallback, auf den man heute bauen kann.

**Entscheidungen des Principals:** alles im Demo-Repo, alle 479 Sektionen in einem Zug,
CodeMirror 6 direkt, lokal zuerst mit späterem Deploy als Option (GitHub Pages wünschenswert,
keine harte Bedingung).

## Zielbild

```
Vite-SPA (:5179, UI.runMount)             Playground-Backend (JVM, :8090)
┌──────────────────────────────┐          ┌────────────────────────────────┐
│ DemoPage (unverändert)       │          │ SnippetCompiler                │
│  Doc.example                 │          │  warmer dotty Driver           │
│   ├ Live-UI (kompiliert)     │  POST    │  auf einem gepinnten Thread    │
│   ├ CodeMirror 6 (Scala)     │ ───────► │  ↓ Klassen in Child-Loader     │
│   └ <iframe src=/play/slotK> │          │ 16 feste UI.runHandlers-Slots  │
│                          ▲   │          │                                │
└──────────────────────────┼───┘          └────────────────────────────────┘
                           └── SSR + WebSocket, via vite proxy (ws: true)

DemoServer (:8080, runHandlers) und DemoPageGen (SSG): statisches pre, kein Editor.
```

Der Editor lebt ausschliesslich im Scala.js-Runner. Der Grund steht in 3.2 und ist eine
Eigenschaft von kyo-ui, keine Bequemlichkeit. Ohne erreichbares Backend degradiert die
SPA auf einen read-only Editor mit "Copy", womit ein statischer Deploy als GitHub Page
möglich bleibt, nur eben ohne Ausführung.

## Phase 0: Spikes (ABGESCHLOSSEN, beide grün)

Durchgeführt am 2026-08-27 in einem Wegwerf-Build. Beide Annahmen tragen, und ein dritter
Befund kam dazu, der Phase 1.1 verändert.

### Spike A: Makro-Quellcodefang (grün)

`body.asTerm.pos` liefert bei einem `inline`-Parameter die **exakte Call-Site-Spanne**.
Verifiziert an vier Formen: Einzeiler, `{ ... }`-Block mit lokaler Bindung, mehrzeilige
Builder-Kette, For-Comprehension. Jede wurde zeichengenau zurückgewonnen.

Zwei Befunde für die Implementierung:

- **Dedent braucht `pos.startColumn`.** Die Spanne beginnt hinter der Einrückung der ersten
  Zeile, diese Einrückung steht also nicht im gefangenen Text. Wer den gemeinsamen Präfix
  nur aus den Folgezeilen berechnet, zieht zu viel ab und die Fortsetzungszeilen einer
  Builder-Kette kollabieren auf Spalte 0. Korrekt ist
  `toDrop = min(pos.startColumn, min(Einrückung der nicht-leeren Folgezeilen))`.
- **Die Makro-Beschränkung ist datei-, nicht projektweit.** Der Compiler meldet "Cannot call
  macro method defined in the same source **file**". Ein Aufruf aus einer anderen Datei
  desselben sbt-Projekts kompiliert und läuft. `ExampleMacro.scala` darf also wie geplant
  neben den Pages im `demo`-Projekt liegen; ein eigenes Teilprojekt ist nicht nötig.

### Spike B: Compile-Latenz (grün, rund 30-fach unter dem Ziel)

Gemessen gegen den echten Demo-Classpath (22 Einträge, `kyo-ui-components` aus ivy-local),
mit warmem `dotty.tools.dotc.Driver` nach dem Muster von kyo-doctest, JDK 25.

| Messung | Wert |
|---|---|
| Driver-Setup, einmalig | 176 bis 188 ms |
| Erster Compile (JIT kalt) | rund 1,4 s |
| Warmer Dauerzustand, 76 Compiles | min 17 ms, Median **34 ms**, p90 65 ms, max 97 ms |
| Fresh-Driver-Fallback | 230 bis 350 ms |

Das Ziel im Plan waren 2 Sekunden. Der Median liegt bei 34 ms.

Drei weitere Befunde:

- **`sourcesRequired` muss auf `false` überschrieben werden.** Ohne das druckt
  `Driver.setup` die scalac-Usage und liefert `None`, weil `CompilerCommand.checkUsage`
  mindestens eine Quelldatei auf der Kommandozeile verlangt. Das Snippet kommt aber als
  virtuelle `SourceFile`. kyo-doctest macht das in `Driver.scala:219`.
- **Der warme Driver hält lange, aber nicht unbegrenzt.** Über 80 aufeinanderfolgende
  Compiles trat die "denotation invalid in run N"-Assertion nicht auf und die Latenz sank
  sogar (JIT). **Diese Schlussfolgerung war zu früh**: im echten Lauf über alle 479 Snippets
  schlug sie bei **Run 254** zu (`denotation module class Kyo$ invalid in run 254`). 80
  Iterationen reichten als Stichprobe nicht. Die Antwort ist nicht der pauschal langsame
  `freshDriver`, sondern Selbstheilung: der Compiler fängt genau diese Assertion, baut den
  Driver neu auf und wiederholt den Compile einmal (Phase 2.1).
- **Es entstehen echte Classfiles** (200 im Ausgabeverzeichnis), nicht nur Typer-Ergebnisse.
  Der Classloader-Schritt aus 2.3 ist damit tragfähig.

### Zusatzbefund: der Rückgabetyp des Snippets

Eine blanke `uic`-Komponente konvertiert **nicht** nach `UI < (Async & Env[uic.ToastService])`,
auch ohne jeden Effekt. Grund: `uic`-Komponenten sind kein `UI`, sondern `uic.Node`
(`kyo-ui/components/shared/src/main/scala/kyo/uic/Node.scala:34`) mit einem
`implicit def nodeToUI` in der Companion. Scala führt aber keine zwei Hops aus, und die
Konvertierung feuert nicht gegen den Kyo-Typ `UI < S`. Gemessen:

| Rumpfform | gegen `UI < ExRow` |
|---|---|
| `uic.Button("Save")` | **Fehler** |
| `for ... yield uic.Button(...)` | **Fehler** |
| `uic.FlexBox().gap(12)(...)` | **Fehler** |
| `div(uic.Button("Save"))` | ok |
| `for ... yield div(...)` | ok |
| `for ... yield (uic.Button(...): UI)` | ok |

Da die allermeisten der 479 Beispiele auf einer blanken Komponente enden, würde die naive
Signatur ein `.toUI` in fast jedem Snippet erzwingen: sichtbares Rauschen in genau dem Text,
der als vorbildlicher Anwendungscode gelesen werden soll.

Die Lösung ist verifiziert: ein geschichtetes Given, das den Rumpf hebt.

```scala
trait AsExample[A]:
    def lift(a: A)(using Frame): UI < ExRow

trait AsExampleLow:                              // pure Rümpfe, niedrige Priorität
    given pureUI[A <: UI]: AsExample[A]          // subtyp-generisch: div(...) ist Ast.Div, nicht UI
    given pureNode[A <: uic.Node]: AsExample[A]  // hebt über .toUI

object AsExample extends AsExampleLow:           // effektbehaftete Rümpfe gewinnen
    given effUI[A <: UI, S]: AsExample[A < S]
    given effNode[A <: uic.Node, S]: AsExample[A < S]
```

Beide UI-Givens **müssen** subtyp-generisch sein: `div(...)` hat den Typ `Ast.Div`, und ein
invariantes `AsExample[UI]` wird dafür nicht gefunden. Mit dieser Fassung kompilieren alle
sechs Rumpfformen fehlerfrei, ohne ein einziges `.toUI`.

## Phase 1: Snippet ist Quelle der Wahrheit (ABGESCHLOSSEN)

Stand: **alle 479 Code-Sektionen migriert**, `Doc.section` mit Code existiert nirgends mehr.
Der Nachweis über alle 81 Seiten: 76 Seiten geändert, davon **genau eine** mit einer
Abweichung ausserhalb der Code-Blöcke, und die ist beabsichtigt (siehe 1.5).

### Was gebaut wurde

**1.1 `Doc.example` (fertig).** `demo/src/main/scala/uicdemo/ExampleMacro.scala` plus
`AsExample.scala`. Das Makro ist **variadisch**: 40 Sektionen zeigen mehrere
Geschwister-Beispiele, und die müssen Geschwister bleiben. Ein `fragment` als Sammelknoten
hätte eine Ebene in die `data-kyo-path`-Adressierung geschoben, und `VirtualScrollerPage`
adressiert einen Pfad wörtlich (`requestMeasure`). Die gefangene Spanne läuft vom ersten
bis zum letzten Beispiel, was exakt der bisherigen Anzeige entspricht.

**1.2 `Doc` (fertig).** `page[S]` sequenziert effektbehaftete Sektionen und ist im Row
polymorph, damit die Migration seitenweise grün bleibt. `spliceTopFragment` löst ein
`fragment` auf oberster Ebene wieder in Geschwister auf, sodass Sektionen, deren Beispiele
sich einen Ref teilen, pfadstabil bleiben. Die Sektionen werden per Element nach
`HtmlChildVal` konvertiert statt in ein `fragment` gepackt: gemessen, ein `fragment` dort
verschiebt jeden Pfad darunter.

**1.4 Migration (fertig).** Skript `migrate.py` (im Scratchpad) über sechs Dateien, plus
`migrate_fv.py` für `FormValidationPage`, dessen Aufbau abweicht: dort füllt je ein lokaler,
genau einmal benutzter `def xForm` eine Sektion, und der Code-String war eine abgetippte
Kurzfassung davon. Die 26 Rümpfe sind jetzt in ihre Sektion inlined.

Ergebnis pro Datei: ActionPages 27, DisplayPages 74, FeedbackPages 37, FormPages 182,
FormValidationPage 26, MenuPages 21, StructurePages 112 = **479**.

**1.5 Drift-Fälle (fertig).**
- "Unsorted keeps its slot" (DataTable) hatte einen Code-Block, aber gar kein Beispiel, und
  der Block nannte den Ref der Tabelle aus der Sektion darüber. Jetzt reine Prosa; die
  Aussage "removableSort(true) is the default" steht im Text. **Das ist die einzige
  Abweichung ausserhalb der Code-Blöcke im gesamten Vergleich.**
- `MeasurePath`, `bigList` und `handle` waren Seiten-Level-Definitionen mit genau einem
  Nutzer. Sie sind in ihre Sektion inlined; die Qualifikation
  `VirtualScrollerPage.MeasurePath` entfällt damit von selbst. Der Pfad des Messkastens ist
  vor und nach der Migration `1.1.1.0.0`, also unverändert.
- Toast-Interleave und das nicht kompilierbare `svc <- Env.get`-Fragment lösen sich durch
  den Fang von selbst.

**Weitere Befunde aus der Durchführung.**
- **Geteilte Refs werden pro Sektion dupliziert.** Zwölf Refs (`lastRef`, `svc`, `scoreRef`,
  `mutedRef`, `unitRef`, `strongRef`, `resultRef`) wurden von mehreren Sektionen benutzt.
  Das war Hoisting-Artefakt, die Beispiele sind unabhängig. Gleicher Startwert, also
  unverändertes Rendering.
- `TooltipPage`s `Kyo.lift` entfällt, weil `Doc.page` jetzt selbst effektbehaftet ist.
- **`DemoPageGen` taugt nicht als Regressions-Baseline.** Die Navigation ist ein reaktiver
  Knoten, dessen statische Projektion sein aktueller Wert ist, also enthält der Snapshot
  genau eine Seite. Der neue `DemoPagesGen` (`demo/src/main/scalajvm/`) rendert jede Seite
  einzeln hinter einem id-Marker; nur damit ist der Vergleich aussagekräftig.
- Beide Zeilen verifiziert: `demo/compile`, `demoJS/compile` und `demoJS/fastLinkJS` grün.

### 1.3 Geteilte Fixtures: seitenweiter Import (entschieden, umgesetzt)

Das geplante gemeinsame `SampleData` scheidet aus, weil die Fixtures inhaltlich kollidieren:
`City(name, code)` gegen `City(code, name)` (vertauschte Feldreihenfolge), `Product` mit 5
gegen 3 Feldern, `countries` mit 3 gegen 4 Einträgen. Ein gemeinsames Objekt ginge nur über
Umbenennen (verändert den Text von 58 Snippets) oder Datenangleich (verändert das Rendering).

Stattdessen bleiben die Fixtures, wo sie sind; ihre Sichtbarkeit ist auf `private[uicdemo]`
geweitet (30 Deklarationen), und der Playground-Preamble importiert das Seiten-Objekt der
Sektion. Kein Snippet ändert sich, keine Kollision entsteht.

Vier Fixtures waren lokal in `content` statt Seiten-Member und damit auch für den Import
unerreichbar: `events` und `products`/`listRow` (DisplayPages) sind gehoben. Drei mit genau
einem Nutzer (`MeasurePath`, `bigList`, `handle`) sind stattdessen in ihre Sektion inlined.

### 1.7 Sektionen sind jetzt Werte, nicht nur Markup

Aus der Playground-Arbeit kam eine Korrektur an `Doc` zurück. Ursprünglich sollte der
Playground die Snippets aus dem gerenderten `pre.demoCode` zurücklesen. Das ist falsch:
`FormValidationPage` schreibt über `rawHtml` eigene `pre.demoCode`-Blöcke für eine
Referenztabelle, die Prosa sind und keine Beispiele; der Extraktor zog 486 statt 479 Blöcke.

`Doc.example` liefert deshalb jetzt ein `Doc.Section(render, code)` statt eines nackten `UI`,
und `Doc.page` ein `Doc.Page(title, intro, sections)` mit `ui` als Projektion. Damit ist die
Snippet-Liste einer Seite ein typisierter Wert (`Page.snippets`), den der Playground direkt
liest, ohne zu rendern und ohne HTML zu parsen. `ComponentPage.content` liefert
entsprechend `Doc.Page < Doc.Row`.

## Phase 2: Playground-Backend (ABGESCHLOSSEN)

Neues sbt-Projekt `playground` in `~/Programming/kyo-ui-components/build.sbt`, JVM-only,
`publish / skip := true`, abhängig von `demo.jvm(Scala3)` und
`"org.scala-lang" %% "scala3-compiler" % "3.8.4"`.

Build-Randbedingungen des Repos: sbt **2.0.0** (`project/build.properties`), `projectMatrix`
ist dort eingebaut, `sbt-scalajs` 1.22.0 ist das einzige Plugin. `playground` ist ein
gewöhnliches `project.in(file("playground"))` mit explizitem `scalaVersion := Scala3`, keine
Matrix. Aufrufe laufen mit `sbt`, nicht `sbtn`: der Thin Client der sbt-1-Distribution
spricht ein anderes Protokoll und ist gegen sbt 2 ungetestet.

**2.1 `SnippetCompiler`.** Dünner warmer Wrapper über `dotty.tools.dotc.Driver`.
`kyo-doctest/jvm/src/main/scala/kyo/doctest/internal/Driver.scala` ist die Blaupause, kann
aber nicht direkt benutzt werden: die Klasse ist `private[kyo]` (`Driver.scala:30`). Die
beiden Details, die man von dort übernehmen muss, stehen im Scaladoc `Driver.scala:17-28`:
`Driver.init` einmal aufrufen und den `Context` cachen, und **alle** Compile-Aufrufe auf
einen einzigen dedizierten Thread pinnen, weil dottys `ContextBase` Thread-Ownership
assertiert und kyo-Fibers auf beliebigen OS-Threads laufen. Dazu die dritte Zutat aus
Spike B: `override def sourcesRequired: Boolean = false` auf dem Driver-Subtyp
(`Driver.scala:219`), sonst liefert `setup` nur die Usage und `None`.

Gemessen in Phase 0: Setup 180 ms einmalig, danach Median 34 ms pro Compile. `freshDriver`
wird nicht gebraucht.

Der Classpath kommt aus einem `Compile / resourceGenerators`, der
`(demo.jvm(Scala3) / Compile / fullClasspath).value` in `playground-classpath.txt` schreibt.

**2.2 Wrapper.** Der Editor-Puffer ist der Body. Das Backend baut daraus:

```scala
package uicdemo.play
import kyo.*, kyo.UI.*, kyo.uic, uicdemo.SampleData.*
import scala.language.implicitConversions
object Snippet extends uicdemo.play.Snippet:
  def ui(using Frame): UI < (Async & Env[uic.ToastService]) = <BODY>
```

Zeilenversatz merken, damit Compiler-Diagnostics auf die Editor-Zeilen zurückgerechnet
werden.

**2.3 Ausführung.** Pro Lauf ein `URLClassLoader` über dem Ausgabeverzeichnis, mit dem
Playground-Classloader als Parent, sodass `UI` und `kyo.uic` typidentisch sind. Objekt
laden, `ui` aufrufen, Ergebnis an `UI.runHandlers(s"/play/$runId")` hängen. LRU über N
Läufen, Eviction schliesst Classloader und Scope.

Das ist bewusst In-Process, passend zur Entscheidung "erstmal nur lokal". Für ein
öffentliches Deployment ist das **arbitrary code execution** und muss vorher auf eine
Fork-JVM pro Lauf mit Timeout, Heap-Cap und ohne Netzwerk umgestellt werden. Dieser Punkt
wird im README des Playground-Moduls festgehalten, nicht stillschweigend übergangen.

**Stand.** Das `playground`-Projekt existiert (JVM, `publish / skip`, hängt an
`demo.jvm` und `scala3-compiler`), der Classpath kommt als generierte Ressource mit 21
Einträgen, `SnippetCompiler` läuft, und **`CheckSnippets` kompiliert alle 479 Snippets grün
in 17 Sekunden**. Damit ist "der angezeigte Code ist lauffähig" eine geprüfte Eigenschaft.

Zwei Dinge kamen dabei ans Licht:

- **Der warme Driver braucht Selbstheilung.** Bei Run 254 schlug
  `denotation module class Kyo$ invalid in run 254` zu. Der Compiler fängt genau diese
  Assertion, baut Context und Compiler neu und wiederholt den Compile einmal. Das erhält den
  34-ms-Median, statt pauschal auf die ~300 ms des `freshDriver` zu gehen.
- **Eine Seite braucht deklariertes Setup.** `FormValidationPage` teilt echten
  Session-Zustand über alle Sektionen (ein Sprachumschalter, der jedes Formular
  re-übersetzt). Der kann weder Sektions-Ref noch Seiten-`val` sein. `ComponentPage`
  bekommt deshalb `snippetSetup: Maybe[String]`, eine Scala-Vorlage mit einer
  `$BODY$`-Zeile, in die der Wrapper das Snippet setzt. Das ist eine Wiederholung dessen,
  was `content` baut, aber eine **geprüfte**: driftet sie, kompilieren die 26 Snippets der
  Seite nicht mehr.

**2.4/2.5 stehen ebenfalls.** `SlotPool` vergibt 16 beim Start registrierte
`UI.runHandlers`-Slots reihum; die Routen sind `/play/health`, `/play/compile`,
`/play/theme` und `/play/slot0` bis `/play/slot15`. Der Vite-Proxy steht in
`demo/vite.config.ts` mit `ws: true`. Das Theme liegt serverseitig statt im Query-String,
weil der Builder von `runHandlers` den Request ohnehin nicht sieht; `POST /play/theme`
schaltet alle offenen Ergebnisse gleichzeitig um.

**Am laufenden Server verifiziert** (`playground/runMain uicdemo.play.PlaygroundServer 8090`):

- `GET /play/health` → `{"ok":true,"slots":16}`
- `POST /play/compile` mit gültigem Snippet → `{"ok":true,"slot":0,"version":1}`
- `GET /play/slot0` liefert die gerenderte Seite samt Beispiel-Canvas, Theme-Attribut und
  Resize-Script
- `POST /play/compile` mit einem Fehler in Editor-Zeile 3, Spalte 33 → genau
  `{"line":3,"col":33}`. Beide Achsen werden auf den Editor-Puffer zurückgerechnet, und
  dottys Doppelmeldungen sind dedupliziert.
- `POST /play/theme` → die Slot-Seite trägt `data-theme="material"` und `data-scheme="dark"`

**Interaktivität in echtem Chrome bestätigt** (Interceptor): Klick auf den kompilierten
Button ändert die Anzeige von "0 mal geklickt" auf "1", nach drei Klicks auf "3". Der Klick
läuft über den WebSocket zur JVM, der Effekt läuft dort, und nur der DOM-Patch kommt zurück.
Ein editiertes Beispiel ist damit genauso interaktiv wie das, das es ersetzt.

Sicherheitslage und Betriebsgrenzen stehen in `playground/README.md`.

**2.4 Routen und der Slot-Pool.** Eine Route pro Lauf ist nicht möglich:
`UI.runHandlers(basePath)` registriert über `UIServer.handlers` eine **feste** Route
(`kyo-ui/shared/src/main/scala/kyo/internal/UIServer.scala:10-18`, `HttpRoute.getText(pagePath)`),
der by-name `ui` sieht den Request nicht, und `HttpServer.init` nimmt seine Handler
einmalig beim Bind entgegen (`kyo-http/shared/src/main/scala/kyo/HttpServer.scala:75-113`,
kein `addHandler`). Die WebSocket-Hälfte selbst nachzubauen scheidet aus, weil
`UIServer.serveSession` `private[kyo]` ist.

Also ein **Pool fester Slots**, beim Start registriert:

- `GET /play/health` liefert `{ok: true}`. Das Feature-Gate des Clients.
- `POST /play/compile` nimmt `{source}`, kompiliert, legt den fertigen Builder in einen
  freien Slot und liefert `{slot, version}` oder `{diagnostics: [{line, col, severity, message}]}`.
- `/play/slot0` bis `/play/slot15`, jeweils einmalig über
  `UI.runHandlers(s"/play/slot$k")(currentOf(k))` registriert. `currentOf(k)` liest bei jedem
  Request und jeder WS-Verbindung ein `AtomicRef` mit dem aktuell zugewiesenen Builder. Der
  Client lädt `/play/slotK?v=<version>`; der Query-Parameter ist der Cache-Buster, der den
  iframe-Reload erzwingt. Slots recyceln per LRU, Eviction schliesst Classloader und Scope.
- Theme über `?theme=aura&dark=false`; die Snippet-Seite injiziert `uic.Theme.css` selbst,
  genau wie `DemoPage.scala:158-159`.
- Die Snippet-Seite enthält ein kleines `UI.rawHtml("<script>...</script>")`, das per
  `ResizeObserver` ihre `scrollHeight` via `postMessage` nach aussen meldet.

16 Slots reichen für den lokalen Einzelnutzer-Fall. Für ein öffentliches Deployment sind
Slots ohnehin pro Session nötig, was zusammen mit dem Fork-JVM-Sandboxing aus 2.3 in einem
Zug kommt.

**2.5 Vite-Proxy.** In `demo/vite.config.ts` unter `server.proxy` einen Eintrag
`'/play': { target: 'http://localhost:8090', ws: true }`. Ohne `ws: true` kommt der
WebSocket der Snippet-Seite nicht durch.

**2.6 Der Test, der alles zusammenhält.** `PlaygroundSnippetsTest` läuft über
`DemoPage.pages`, zieht jedes gefangene Snippet und kompiliert alle 479 gegen den
Playground-Classpath, mit Content-Hash-Cache für Inkrementalität. Das ist das Äquivalent zu
`sbt <module>/doctest` und liefert die eigentliche Garantie: jeder "Run"-Button funktioniert,
und kein Snippet kann still unkompilierbar werden.

## Phase 3: Editor in der Seite (ABGESCHLOSSEN)

**Der Editor ist SPA-only, und das ist eine Eigenschaft von kyo-ui.** `UI.mounted` führt
seinen Effekt auf der Seite des Runners aus: unter `UI.runMount` als Scala.js im Browser,
unter `UI.runHandlers` als JVM-Bytecode ohne DOM. Das Server-Push-Protokoll kennt kein
"führe JS aus", ein `<script>` in einem WS-Patch ist inert, und unter SSG läuft ein
Mount-Effekt gar nicht. Der Plattform-Seam liegt deshalb in `DocEditor`, einmal je Zeile.

**Gebaut:**

- `demo/editor.js` — CodeMirror-6-Verdrahtung (mount/getDoc/setDoc/setDiagnostics/destroy/
  attachResult). Bewusst JavaScript: eine Scala.js-Fassade dafür wäre mehr Code als die
  Datei und müsste bei jeder CodeMirror-Änderung nachgezogen werden. Scala-Highlighting über
  `StreamLanguage.define(scala)` aus `@codemirror/legacy-modes`; die Token-Farben sind
  Prime-Design-Tokens, damit der Editor dem Theme und dem Hell/Dunkel-Schalter ohne eigene
  Umschaltung folgt. Als `uicdemo-editor` in `vite.config.ts` aliasiert, weil die
  Scala.js-Ausgabe unter `target/` liegt und ein `publicDir`-File seine eigenen Imports
  nicht aufgelöst bekäme.
- `demo/src/main/scalajvm/uicdemo/DocEditor.scala` — die statische Quellansicht.
- `demo/src/main/scalajs/uicdemo/DocEditor.scala` — ein gekeyter Mount pro Sektion, mit
  verschachteltem Mount für den Attach (beim Lauf des Mount-Effekts ist der eigene Inhalt
  noch nicht im DOM) und `Scope.acquireRelease` für Teardown von Editor und
  Message-Listener.
- `Doc.playgroundAvailable: Local[Boolean]` — `Local` statt `Env`, weil nur der
  Scala.js-Runner überhaupt ein Backend hat und die Effektzeile sonst auch die JVM-Runner
  belasten würde. `DemoMain` probt einmal und setzt den Wert um `runMount`.

**Was unterwegs auffiel und geändert wurde:**

- **`VirtualScrollerPage` maß über einen fest verdrahteten `data-kyo-path`.** Jede
  Strukturänderung im SPA hätte ihn verschoben. Das Element hat aber eine id, also misst das
  Beispiel jetzt über `requestMeasureById`. Die Fragilität ist damit weg statt umgangen.
- **Die iframe-Größenmeldung maß `documentElement.scrollHeight`** und bekam damit die
  iframe-Höhe zurückgespiegelt, sodass die Größe nie konvergierte. Jetzt wird das gerenderte
  Wurzelelement gemessen, das seinen Inhalt umschließt.
- **Diagnostics-Spalten** waren um die Einrückung des Wrappers verschoben, und dotty meldet
  denselben Fehler mehrfach. Beide Achsen werden jetzt zurückgerechnet, Duplikate entfernt.
- Der Compile-Request nennt das **Seiten-Objekt** statt einer Seiten-ID, gefangen vom Makro.
  Der Server gleicht es gegen die bekannten Seiten ab, damit ein Request keine beliebige
  Klasse laden kann.

**In echtem Chrome verifiziert** (Vite :5179 mit Backend :8090):

- Button-Seite: 12 CodeMirror-Editoren, 12 Run-Buttons, 0 statische `pre`-Blöcke — der
  Health-Probe hat also gegriffen und jede Sektion ist editierbar.
- Run auf dem unveränderten Schnipsel: iframe erscheint auf `/play/slot0?v=1` über den
  Vite-Proxy, Höhe 73 px statt des 160-px-Defaults, Inhalt sind die acht Severity-Buttons.
- **Schnipsel geändert und ausgeführt**: `uic.Button("GEAENDERT").severity(Danger)` landet
  in einem neuen Slot (`slot1?v=2`), und die Seite dort trägt `p-button-danger` und das
  Label `GEAENDERT`. Damit ist die ganze Kette Editor → Compile → Slot → iframe belegt.

**Nicht im Browser verifiziert**, weil der Browser während der Session geschlossen wurde:
die Lint-Marker im Gutter und der read-only-Zustand ohne Backend. Beide Pfade sind auf
HTTP-Ebene bzw. im Code belegt (`/play/compile` liefert `{"line":3,"col":33}`, und ohne
`live` rendert `editorArea` weder Run noch Reset und `CM.mount` bekommt `readOnly = true`),
aber ein Klick-Nachweis fehlt.

**Regression:** JVM-Rendering über alle 81 Seiten unverändert bis auf `virtualscroller`,
und dort nur die beiden bewusst geänderten Prosa-Sätze. `CheckSnippets` weiterhin 479/479
grün, null Owner-Fehlzuordnungen. `demoJS/fastLinkJS` grün.

## Phase 4: Code-Completion (ABGESCHLOSSEN)

Die Annahme des Plans, dafür brauche es `kyo-compiler` und damit eine Erweiterung des
36-Projekt-publishLocal-Schlusses, war falsch. Der Presentation Compiler, den Metals fährt,
liegt seit Scala 3.4 als eigenes Artefakt `org.scala-lang:scala3-presentation-compiler_3` auf
Maven Central, in derselben Version wie `scala3-compiler`, das im `playground` ohnehin hängt.
Eine Zeile `libraryDependencies`, keine Fork-Änderung.

**Gebaut:** `SnippetCompletion` (warme pc-Instanz auf dem Demo-Classpath, Anfragen über einen
`Meter`-Mutex serialisiert, `CancelToken` per `Sync.ensure`), Route `POST /play/complete`,
`Snippet.Wrapped.toSourceOffset`/`toEditorOffset` für die Positionen in beide Richtungen,
`@codemirror/autocomplete` mit einer Completion-Source, die den Cursor schickt und die
Ersetzungsspanne in Editor-Koordinaten zurückbekommt.

**Gemessen:** erste Anfrage 1,3 s (beim Serverstart bezahlt), warm 10 bis 30 ms.

**Im Browser verifiziert:** Popup nach `.` mit 104 Membern, Tippen filtert auf 11, Enter
ersetzt genau `.sev` durch `.severity`, Enum-Werte nach `uic.Severity.`, und ein vollständig
über die Completion geschriebenes `.severity(uic.Severity.Danger)` läuft über Run in ein Slot,
dessen Button `p-button-danger` trägt. Hell und dunkel, Aura und Material.

**Drei Dinge kamen dabei ans Licht und sind behoben:**

- **Der Theme-Umschalter warf den Editor-Inhalt weg.** `shell` rahmte die gesamte Seite in
  `themeRef.render { darkRef.render { ... } }`, also lag jeder Editor in der Region, die diese
  beiden Signale neu rendern. `.keyed` hilft dort nicht: es hält eine Instanz nur über
  Re-Renders seiner *unmittelbar umgebenden* Region, und hier wurde die Region selbst neu
  gebaut. Der Verifikationspunkt 4 dieses Plans wäre also nie grün geworden. Die Attribute
  `data-theme` und `data-scheme` werden jetzt über `UI.commands.bindAttrById` am Wurzelelement
  gesetzt, ohne irgendetwas neu zu rendern; `uic.Theme.css` trägt ohnehin alle Themes und
  selektiert über genau diese beiden Attribute. Damit überlebt nicht nur der Editor, sondern
  jeder Beispielzustand einen Theme-Wechsel.
- **`/play/theme` wurde clientseitig nie aufgerufen.** Die Route war auf HTTP-Ebene geprüft,
  aber nichts rief sie, also blieb ein Ergebnis-Frame hell auf dunkler Seite. `DocEditor`
  (Scala.js) beobachtet jetzt `themeRef.zip(darkRef)` und lädt nach dem POST die offenen
  Frames neu, weil ein bereits geladener Frame seine Theme-Entscheidung schon getroffen hat.
- **Diagnostics trugen ANSI-Escapes.** dotty färbt Teile einer Meldung, sobald es ein Terminal
  vermutet, und eine Overload-Meldung ist so eine. Die Escapes reisten durch das JSON in den
  Editor. `SnippetCompiler` streift sie ab.

**Regression:** `CheckSnippets` 479/479 grün, `DemoPagesGen` über alle 81 Seiten byte-identisch
zur Phase-3-Baseline, Server-Push-Runner auf :8080 zeigt weiterhin 0 Editoren und 12 statische
`pre`-Blöcke und schaltet das Theme über den WebSocket-Patch.

**Ohne Backend nachgeprüft** (die Lücke aus Phase 3): der Editor rendert mit Highlighting,
ist read-only (eine erzwungene DOM-Eingabe wird zurückgerollt), Run und Reset fehlen, und die
Completion ist gar nicht erst installiert. Das ist der statische Deploy-Fall.

**Hover und Signature-Help** liegen auf derselben pc-Instanz und derselben serialisierten
Anfragestrecke: `query` in `SnippetCompletion` ist über die Operation parametrisiert, die drei
Routen unterscheiden sich nur in Adapter und Antworttyp. Im Client ist Hover ein
`hoverTooltip`, Signature-Help ein `StateField` plus `showTooltip`, gefüttert von einem
`updateListener` mit 120 ms Entprellung. Der Listener fragt nur, wenn der Cursor in einer
offenen Argumentliste steht (Klammertiefe vorwärts gezählt, Stringliterale übersprungen), und
das Popup der Completion unterdrückt die Signatur, weil zwei Tooltips an derselben Stelle
unlesbar übereinanderliegen.

Verifiziert: Hover auf `Button` liefert `object Button: kyo.uic`, auf `severity`
`def severity(v: Severity): Button`; Signature-Help in `severity(` zeigt beide Overloads mit
dem aktiven fett, und `scala.math.max(1, ` markiert die Int-Überladung als aktiv und `y: Int`
als aktuellen Parameter. Hell und dunkel.

Eine CSS-Falle dabei: CodeMirror hängt `cm-tooltip` bei einem Signatur-Tooltip **an das
übergebene Element**, bei einem Hover-Tooltip dagegen an einen **Wrapper**, in dem das eigene
Element zur Section wird. Ein Selektor trifft deshalb nur eine der beiden Formen; die Regel
nennt beide.

**Ohne Dokumentation:** der pc bekommt `EmptySymbolSearch`, löst also keine Docstrings auf.
Hover zeigt die Signatur, nicht den Scaladoc-Text.

## Phase 5: Optional, später

- **"Open in Scastie"** pro Sektion. Freigeschaltet, sobald ein kyo-Release
  `kyo-ui-components` auf Maven Central bringt (das Modul hängt in den Aggregaten
  `build.sbt:390-392,472-473`, wird also automatisch mitveröffentlicht) und Scastie Scala
  3.8.x auf JDK 25 anbietet. Beide Bedingungen sind extern; ohne sie ist der Button tot.

## Zu ändernde Dateien

Im Demo-Repo `~/Programming/kyo-ui-components`:

| Datei | Änderung |
|---|---|
| `demo/src/main/scala/uicdemo/ExampleMacro.scala` | neu, Quellcodefang |
| `demo/src/main/scala/uicdemo/SampleData.scala` | neu, geteilte Beispieldaten |
| `demo/src/main/scala/uicdemo/Doc.scala` | `page`/`example`/`section` Signaturen, Editor-Slot, CSS |
| `demo/src/main/scala/uicdemo/DemoPage.scala` | `build` sequenziert effektbehaftete Seiten |
| `demo/src/main/scala/uicdemo/pages/*.scala` | 7 Dateien, 479 Sektionen migriert |
| `demo/src/main/scalajvm/uicdemo/DocEditor.scala` | neu, statisches `pre` für Server-Push und SSG |
| `demo/src/main/scalajs/uicdemo/DocEditor.scala` | neu, CodeMirror-Mount |
| `playground/src/main/scala/uicdemo/play/*.scala` | neu, Compiler, Runner, Routen |
| `build.sbt` | Projekt `playground`, Classpath-Resource-Generator |
| `demo/vite.config.ts` | Proxy `/play` mit `ws: true` |
| `demo/package.json` | CodeMirror-Dependencies |

Im kyo-Repo: keine Änderung.

## Verifikation

1. **Spikes.** ERLEDIGT, siehe Phase 0. Makro-Fang an vier Formen verifiziert, warme
   Compile-Latenz Median 34 ms, Rumpf-Hebung an sechs Formen verifiziert.
2. **Phase 1.** `sbt demo/compile` grün. SSG-Diff `before.html` gegen `after.html` zeigt
   Unterschiede ausschliesslich in den Code-Blöcken. Interceptor-Screenshots hell und
   dunkel über alle sechs Nav-Gruppen ohne Layout-Regression.
3. **Phase 2.** `PlaygroundSnippetsTest` kompiliert alle 479 Snippets grün. Backend
   manuell: `POST /play/compile` mit gutem Snippet liefert `runId`, mit kaputtem liefert
   Diagnostics mit korrekt zurückgerechneten Zeilennummern. `GET /play/<runId>` rendert und
   die WebSocket-Reaktivität funktioniert (Klick im iframe ändert den Zustand).
4. **Phase 3.** Über Interceptor am laufenden Vite (`:5179` mit Backend auf `:8090`):
   Snippet ändern, "Run", iframe zeigt das geänderte Ergebnis, "Reset" stellt her.
   Syntaxfehler zeigt Lint im Gutter. Backend stoppen, Seite neu laden, Editor ist
   read-only und "Run" ist weg.
   Der Keyed-Mount wird gezielt geprüft: Theme mehrfach umschalten und Dark ein- und
   ausschalten, während ein Editor bearbeiteten Inhalt hält. Der Inhalt und die
   Cursor-Position müssen überleben. Das ist der Test, der einen fehlenden `.keyed`
   sofort auffliegen lässt.
5. **Cross-Runner.** Dieselben Stichproben gegen `sbt "demo/run 8080"` (Server-Push) und
   gegen `DemoPageGen` (SSG). Erwartung dort: statisches `pre`, kein Editor, kein
   JS-Fehler in der Konsole. Das ist der Nachweis, dass der Plattform-Seam sauber trennt.
