# Migrationsplan: Morph-Schicht auf upstreams ReactiveRegion

## Kontext

Der Rebase von `uic/migration` auf `origin/main` hat das Fundament der reaktiven Regionen
ausgetauscht. Unsere Fassung (`RegionMarker`, `<!--kyo:PATH-->`) ist durch upstreams
`ReactiveRegion` (`<!--kyo-rs:$id-->` … `<!--kyo-re:$id-->`, `DomReactiveRegions`) ersetzt worden.
Fünf Commits, die auf dem alten Fundament saßen, sind dabei zurückgestellt worden, und mit ihnen
sechs Browser-Zusicherungen sowie drei ganze Testdateien.

Der eigentliche Verlust ist nicht kosmetisch. Upstreams `tryMorphRange` ist **kein Morph**, sondern
ein Sonderfall: er greift nur, wenn die Region genau ein altes und genau ein neues Element hat,
dieses Element gerade den Fokus hält, ein `INPUT` oder `TEXTAREA` ist, und die eingehende HTML keine
verschachtelte Region enthält. In jedem anderen Fall gibt er `false` zurück, `replaceWith` löscht den
Bereich und baut ihn neu. Damit stirbt bei jedem Re-Render einer Region jede Knotenidentität: Fokus,
Caret, Scrollposition, imperativ gesetzte Attribute und Klassen, Expandos, und die lebende DOM eines
keyed Mounts. Der Besitz-Schutz für imperative Attribute existiert bereits, aber nur *innerhalb*
dieses Sonderfalls, wo ihn nichts erreicht.

Ziel dieses Plans ist die volle Parität: der allgemeine Morph zurück auf dem neuen Fundament,
einschließlich des zwei-endigen keyed Diffs und des Listen-Patches auf der Leitung.

## Ausgangslage (gemessen am 2026-09-02)

`DomBackendTest` trägt die fünf geparkten Zusicherungen wieder: **26 grün, 5 rot**, in 65 s
(`SBT_OPTS="-Xmx4G …" sbt 'kyo-uiJVM/testOnly kyo.DomBackendTest'`; mit 8 GB Driver killt der Kernel
den Lauf, weil die geforkte Test-JVM ihrerseits `-Xmx12G` nimmt).

| Zusicherung | Bruch | Braucht Schritt |
|---|---|---|
| keyed mount überlebt Parent-Re-Render | `after == 7`, Expando weg | 3 |
| keyless mount ghostet nicht | `g == 0` verletzt | 3 |
| gebundene Klasse überlebt Morph | `owned-cls` fehlt | 1 |
| gebundenes Attribut überlebt Morph | `data-owned` fehlt | 1 |
| Portal-Twin in place | Expando-Probe negativ | 1 oder 2 |

Von den 17 im Scratchpad geparkten Morph- und Kanal-Testtiteln sind 16 bereits auf dem Branch. Der
einzige fehlende ist `morph reuses an inner element (preserving its DOM-local state) across a
re-render of its region`, der Grundbeweis des Morphs. Er kommt mit Schritt 1 zurück.

Vom Listen-Paket ist die **Protokollhälfte gelandet**: `UIExchange.onListPatch`, `ListRow(key, ui,
changed)`, die Emission in `ReactiveUI` und `ListPatchProtocolTest`. Es fehlen `HtmlOp.PatchList`,
das DomBackend-Override und die drei zugehörigen Testdateien.

## Die Naht

`DomReactiveRegions.replaceWith(regionId, html)(tryMorph)(before)(after)` parst die eingehende HTML
zum Fragment, scannt verschachtelte Bereiche, bildet `oldElements`/`newElements` und ruft
`tryMorph(oldElements, newElements, incoming.isEmpty)`. Bei `true` passiert nichts weiter, bei
`false` folgt der komplette Neuaufbau mit den `before`/`after`-Haken (Enter/Leave-Ghosts, Fokus,
focusAuto, Portal- und Scroll-Sweep).

**Entscheidung:** der Callback wird erweitert. Ein Morph, der einen Knotenbereich rekonziliert,
braucht dessen Grenzen; Elemente allein reichen nicht (Textknoten zwischen ihnen wären unsichtbar).
`replaceWith` reicht Endpunkte, Parent und das geparste Fragment durch. Das ist eine bewusste
Abweichung in upstreams Datei, die bei jedem künftigen Merge sichtbar bleibt; sie wird im
Scaladoc der Methode begründet, damit der nächste Merge sie nicht wegräumt.

Alles unten geschieht **zweimal, in Gleichschritt**: in Scala (`DomBackend`, `DomReactiveRegions`)
und im clientJs-Zwilling (`HtmlRenderer.reactiveRangesJs`, aktuell 12 KB; die 64-KB-Grenze für ein
Klassendatei-Konstantenliteral ist noch weit). Die bestehenden Paare heißen `tryMorphRange` /
`kyoRangeMorph` und `replaceWith` / `kyoRangeReplace`; jede neue Funktion bekommt ihren Zwilling und
den Querverweis im Kommentar, wie im Rest der Datei üblich.

## Schritt 1: Morph-Kern

Der rekursive Element-Morph, portiert aus `scratchpad/dropped/DomBackend-morph-block.scala`
(Zeilen 311 bis 375): `morphNode`, `morphEl`, `morphAttrs`, `morphChildren`.

- `morphAttrs` bekommt den Besitz-Schutz, den die geparkte Fassung noch nicht hatte: ein Name aus dem
  `__kyoOwn`-Expando wird nie rekonziliert. Der Code dafür steht bereits im heutigen
  `tryMorphRange` (`ownedAttrs`) und im Zwilling (`kyoRangeMorph`) und wird dorthin gezogen.
- `morphAttrs` behält die Aktiv-Feld-Erhaltung (`.value` nur bei echter Fremdänderung schreiben) und
  `morphEl` den contenteditable-Schutz.
- `tryMorphRange` wird allgemein: Knotenbereich gegen Fragmentkinder, Element gegen Element über
  `morphNode`. Fällt zurück (`false`) bei verschachtelten eingehenden Bereichen und bei
  inkompatiblen Wurzeln, damit der Neuaufbau die Fälle übernimmt, die Schritt 2 erst öffnet.
- `applyJsPropsSync` läuft über die gemorphten Wurzeln, wie es der Neuaufbau-Pfad tut.

**Dateien:** `kyo-ui/js-wasm/src/main/scala/kyo/internal/DomBackend.scala`,
`kyo-ui/js-wasm/src/main/scala/kyo/internal/DomReactiveRegions.scala` (Naht),
`kyo-ui/shared/src/main/scala/kyo/internal/HtmlRenderer.scala` (Zwilling).

**Fertig, wenn:** `morph reuses an inner element …` wieder in `DomBackendTest` steht und grün ist,
die Zusicherungen zu gebundener Klasse und gebundenem Attribut grün sind, und die 26 bestehenden
Blätter grün bleiben.

## Schritt 2: Logische Knoten über die kyo-rs-Marker

Damit der Morph auch läuft, wenn der Bereich verschachtelte Regionen enthält. Ein verschachtelter
Bereich ist für den Morph **ein logischer Knoten**, adressiert über seine Region-ID.

- Portierung von `logicalKey`, `logicalNext`, `spanClose`, `patchLogical`, `eachSpanNode`,
  `moveLogicalBefore`, `removeLogical`, `insertLogicalClone` aus dem geparkten Block, mit
  `kyo-rs:`/`kyo-re:` statt `<!--kyo:PATH-->` als Klammer.
- Registerpflege: nach einem erfolgreichen Morph wird der Bereich über
  `DomReactiveRegions.scan` neu erfasst und `ranges` abgeglichen (neue eintragen, verschwundene
  entfernen). Das ersetzt `rescanRange`/`rebuildRegions` aus dem alten Fundament, ohne eine zweite
  Registry zu bauen.
- `tryMorphRange` verliert damit die Bedingung `incomingRangesEmpty`.

**Fertig, wenn:** die Portal-Zusicherung grün ist (Twin wird durch den Platzhalter hindurch in place
rekonziliert, kein Inline-Duplikat) und `TableRegionTest`-Fälle, die bereits als
`a reactive row group inside a table survives its first patch` in `DomBackendTest` liegen, grün
bleiben.

## Schritt 3: Mount-Flags auf den Markern

Der Inhalt von `3ab5e7b024`, auf upstreams Marker portiert. Ein keyed Mount überlebt heute als
*Instanz*, aber nicht als DOM: der Parent projiziert ihn auf seinen Platzhalter und morpht den
lebenden Teilbaum weg.

- Die Marker-Nutzlast bekommt einen Flag-Abschnitt hinter der ID: `m` (Mount-Wurzel), `s` (Slot der
  eingehenden Nutzlast), `k=<escaped>` (Mount-Key). Die **ID selbst bleibt unverändert**, damit
  `ReactiveRegion.htmlId`, `pathOf` und `isValidHtmlId` unangetastet bleiben; nur das Parsen der
  Marker lernt die Flags. Betroffen: `DomReactiveRegions.scan` und `validatedParent` (die heute
  `start.data != s"kyo-rs:$regionId"` exakt vergleichen) und die Zwillinge `kyoRangeScan` und
  `kyoRangeReplace`.
- `patchLogical` überspringt eine Spanne nur, wenn lebendes `m` und eingehendes `s` im `k`
  übereinstimmen; ein abweichender Key fällt in den Morph durch (der Slot wird zurückgesetzt, genau
  wie heute) und der lebende Marker übernimmt den eingehenden Key.
- Server-gerendertes HTML trägt kein `k`. Der erste Durchlauf adoptiert den Key, danach ist die
  Spanne opak. Die goldene HTML bleibt damit byte-identisch, was
  `HtmlRendererReactiveRangesTest` prüft.
- Ghost-Schutz aus `aeeb8331f1`, Teil 2: die eigene Republikation eines Mounts bereitet keine Ghosts
  vor.

**Fertig, wenn:** die Zusicherungen zu adoptiertem keyed Mount und zum Ghost bei keyless
Republikation grün sind, also alle 32 Blätter in `DomBackendTest`.

## Schritt 4: Zwei-endiger keyed Diff und Text-Fast-Path

Der Inhalt von `1d492b43d3`. Bis hierher ist der Morph korrekt, aber der Cursor-Lauf löst einen
hinter ihm gefundenen Key durch Vorziehen auf, was jede Zwischenzeile verschiebt.

- Zwei-endiger Vorlauf vor dem Cursor-Lauf (`morphRange`, `morphRangeCursor`, `collectLogical` aus
  dem geparkten Block).
- Text-Fast-Path: eine reine Textnutzlast in eine reine Textregion weist den Textknoten zu, statt
  durch `<template>`-Parse und Morph zu gehen. `DomReactiveRegions.setTextAt` existiert bereits und
  ist die Grundlage.
- Zielvorgabe aus der alten Commit-Message, als Messung nachzuvollziehen: swap1k 997 Mutationen auf
  2, remove-one-1k 801 auf 271 ms, update10th 100 Parses auf 0.

**Fertig, wenn:** die Messwerte in derselben Größenordnung reproduziert sind und alle
`DomBackendTest`-Blätter grün bleiben.

**Stand 2026-09-02: der zwei-endige Pass ist drin, der Text-Fast-Path nicht.** Er ist zweimal
gebaut und beide Male verworfen worden, weil er auf upstreams Fundament Suiten wegbricht, die ohne
ihn grün sind: mit ihm lief `ReactiveScenarioItTest` in 48 s statt 12 s und `CrossComponentItTest`
in eine Kaskade, ohne ihn sind beide grün (12/12 und 17/17). Der Grund liegt vermutlich in der
Nachbereitung, die die Region auf dem neuen Fundament fährt (Portal-Sweep, focusAuto, scrollAuto):
die Variante ohne Sweeps ließ Portal-Zusicherungen fallen, die Variante mit Sweeps kostete den
Durchsatz. Der Nutzen ist eine eingesparte `<template>`-Parse pro Textregion und damit deutlich
kleiner als das Risiko; er gehört mit einer eigenen Messung wiederaufgenommen, nicht nebenbei.

## Schritt 5: Listen-Patch auf der Leitung

Die zweite Hälfte von `a73faace2` plus `e3d3ff573`. Die Protokollhälfte liegt bereits.

- `DomBackend` überschreibt `onListPatch`: nur geänderte Zeilen rendern, als eine Nutzlast, und über
  den zwei-endigen keyed Pass aus Schritt 4 rekonzilieren, mit der Zeilenordnung an Stelle eines
  geparsten Dokuments. Vorlage: `scratchpad/dropped/DomBackend-listpatch.scala`.
- `HtmlOp.PatchList` plus clientJs-Handler für den Server-Push-Transport. Vorlage:
  `scratchpad/dropped/e3d3ff573-list-wire.patch` (vollständig, inklusive Tests).
- `HtmlRenderer.paintsAsKeyedRoot` als totale Übereinstimmung ohne `case _`, damit ein neuer
  UI-Knoten hier wie in `renderTo` beantwortet werden muss. Das Schutzgatter zieht nach `ReactiveUI`,
  weil eine Leitung nicht zurückfallen kann: sind die unberührten Zeilen einmal nicht im Frame, hat
  der Client nichts, woraus er sie rekonstruieren könnte.
- Zurück in den Baum: `ListPatchDomTest` (9 Blätter), `ListPatchWireTest`, `KeyedRootParityTest`.

**Fertig, wenn:** die drei Testdateien grün sind und `kyo-uiJS/compile` durchläuft.

## Verifikation

Pro Schritt, nicht erst am Ende:

```sh
SBT_OPTS="-Xmx4G -Xss10M -XX:MaxMetaspaceSize=768m -XX:+UseG1GC" \
  sbt -batch 'kyo-uiJVM/testOnly kyo.DomBackendTest'      # ~65 s, das Kernsignal
sbt -batch 'kyo-uiJS/compile'                              # Pflicht bei jeder DomBackend-Änderung
```

`kyo-uiJS/compile` ist nicht optional: `DomBackend.scala` liegt unter `js-wasm/` und wird vom
JVM-Compile nicht gesehen. Beim Rebase hat genau dieser Check drei echte Defekte gefunden.

Am Ende jeder Phase zusätzlich:

```sh
SBT_OPTS="-Xmx4G …" sbt -batch 'kyo-uiJVM/test'            # ~32 min, 136 Suiten
```

Und vor dem Abschluss die Consumer, wie beim Rebase: mounted-Version publishen (36-Projekt-Schluss
plus Apollo-Module), dann `kyo-ui-components` und `kyo-apollo-examples/showcase`.

## Bewusste Entscheidungen und Risiken

- **Abweichung von upstream.** Schritt 1 ändert eine Signatur in `DomReactiveRegions.scala`,
  Schritt 3 das Marker-Parsing. Beides sind Dateien, die upstream aktiv weiterentwickelt. Die
  Begründung gehört ins Scaladoc der jeweiligen Stelle, nicht nur in die Commit-Message.
- **Zwei Zwillinge.** Jede Änderung am Morph existiert doppelt (Scala und clientJs). Der Rebase hat
  gezeigt, dass der JVM-Compile die zweite Hälfte nicht sieht. Kein Commit ohne
  `kyo-uiJS/compile`.
- **Reihenfolge ist Abhängigkeit, nicht Wichtigkeit.** Schritt 3 setzt Schritt 2 voraus (Flags sind
  Flags *auf logischen Knoten*), Schritt 5 setzt Schritt 4 voraus (der Listen-Patch *ist* der
  zwei-endige Pass mit der Zeilenordnung).
- **Halt nach Schritt 3 ist möglich.** Nach Schritt 3 sind alle geparkten Zusicherungen grün und die
  Korrektheit ist wiederhergestellt; 4 und 5 sind Leistung und Leitungsvolumen.
- **Speicher.** sbt-Driver auf 4 GB halten und keinen zweiten Build parallel laufen lassen: die
  geforkte Test-JVM nimmt `-Xmx12G`, und der Kernel hat den Lauf bei 8 GB Driver bereits einmal
  gekillt.
