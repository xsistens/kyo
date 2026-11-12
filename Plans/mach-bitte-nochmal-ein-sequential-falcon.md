# Plan: Jeder Live-Watcher steht in der ActiveQueryRegistry (LIBRARY-IDEAS #16)

## Kontext

`client.refetchQueries(names*)` und `client.resetStore` (`kyo-apollo-ui/src/main/scala/kyo/apollo/ApolloRefetch.scala`) erreichen nur die Abfragen, die in der `ActiveQueryRegistry` stehen (`kyo-apollo/shared/src/main/scala/kyo/apollo/ActiveQueryRegistry.scala`). Eingetragen wird bisher nur in `buildQueryHandle` (`ApolloQuery.scala:330-346`), also nur über die beiden Überladungen von `Apollo.query`.

**Wer heute fehlt:**

- `watchSignal` in allen drei Formen;
- `pollingSignal`;
- `paginatedQuery` in allen Formen;
- `preload`;
- `lazyQuery`.

**Folgen:**

- `refetchQueries("IssueComments")` tut in einer App aus `dataSignal`-Komponenten stillschweigend nichts. Deshalb führt die Tour die Abfrage von Hand aus, und dabei werden Fehler verschluckt (APP-REVIEW F1, in `Conflict.reload` noch offen).
- `resetStore` leert den Cache, ohne die Watcher zu benachrichtigen (`ApolloStore.clearAll`, `ApolloStore.scala:891`, ruft kein `publish` auf), und lädt dann nur registrierte Abfragen neu. Jede `watchSignal`-Komponente bleibt danach auf altem Stand stehen.

**Wofür ein Refetch eines Watchers überhaupt nötig ist.** `watchSignal` ist keine Server-Subscription, sondern ein Cache-Watcher (`ApolloCall.watch()`, `Watcher.scala:73-84`). Er holt einmal nach seiner `FetchPolicy` und meldet danach jede Änderung, die **in den lokalen Cache** geschrieben wird: von anderen Abfragen, Mutationen, Subscriptions oder direkten Store-Writes. Was sich nur auf dem Server ändert, sieht er nie.

Ein Refetch bleibt also für den Fall, dass der Server etwas weiß, was der Cache nicht wissen kann:

- **Änderungen anderer**, ohne Feed: der Kollegenkommentar in Schritt 9.
- **Serverseitig berechnete Ergebnisse:** die Zugehörigkeit zu einer Liste nach einem Create, Sortierung, Zähler wie `openCount` (APP-REVIEW F4), Paginierung.
- **Ein Konflikt (409):** Der Server hat eine neuere Version.
- **Ein Wechsel der Identität:** `resetStore` nach dem Anmelden oder Abmelden.

Für Änderungen, die der eigene Client macht, ist kein Refetch nötig, wenn der Payload der Mutation die geänderten Felder mitbringt. Die Normalisierung aktualisiert dann jeden Watcher. Das sagt die Scaladoc von `ApolloRefetch` auch so.

**Ein Grund für die Lücke ist nicht festgehalten.** Registry, `refetchQueries`, `resetStore` und `watchSignal` kamen alle im Import-Commit `62c4515da`. Die Scaladoc beruft sich ausdrücklich auf React (`react-apollo`). Dort steht jede beobachtete Abfrage (ObservableQuery) in der Liste, pausierte (standby) nehmen nicht teil.

**Entscheidungen des Principals (2026-10-20):**

1. Pausierte Abfragen werden wie in React ausgenommen: `skip = true` und Live-Variablen bei `Absent`. Das ändert auch das Verhalten von `query(call, skip)`, die heute pausiert neu lädt.
2. Watcher derselben Abfrage werden entdoppelt. Gleicher Name, gleiches Dokument und gleiche Variablen bedeuten eine Netzanfrage, und der Cache verteilt die Antwort.

Das ist eine Library-Änderung (kyo-apollo, kyo-apollo-ui), vom Principal angefragt. Sie landet auf dem Snapshot-Branch in `kyo/.worktrees/snap-v2` als eigener Commit `[kyo-apollo] …`.

## Entwurf

### 1. Die Registry kennt pro Eintrag ein veränderliches Ziel

**`ActiveQueryRegistry.register(target: Maybe[Target] < Sync)`:**

- Ein Watcher registriert sich **einmal** für seinen `Scope`, wie heute über `Scope.acquireRelease`.
- Der Eintrag liefert bei jeder Abfrage sein **aktuelles** Ziel: `Absent` heißt pausiert oder noch nicht geladen. Damit decken
  - `skip`,
  - Live-Variablen
  - und `lazyQuery` vor dem ersten `load`
  
  dieselbe Form ab, ohne Einträge an- und abzumelden.

**`Target`:**

- **`key: QueryKey`:** `QueryKey(name: String, document: String, variables: Json)`, gebildet aus `call.requestBuilder.operation`. `Json` hat strukturelle Gleichheit. Das Dokument gehört dazu, weil `SelectionBuilder` zwei verschiedenen Selektionen denselben Namen geben kann.
- **`fetch: Unit < (Async & Abort[ApolloExecuteFailure])`:** das `NetworkOnly`-`.data` des Calls, über `ApolloEffect.asExecuteFailure` wie heute.
- **`during: Maybe[SignalRef[Maybe[NetworkStatus]]]`:** Für ein Handle aus `Apollo.query` ist das seine `activity`-Zelle, damit `networkStatus` während eines Refetches über die Registry `Refetch` zeigt. Sonst `Absent`.
  - `NetworkStatus` liegt in kyo-apollo-ui und die Registry in kyo-apollo. Die Registry bekommt darum nur zwei generische Effekte, `begin: Unit < Sync` und `end: Unit < Sync`. kyo-apollo-ui füllt sie mit `activity.set(Present(Refetch))` und `activity.set(Absent)`.

**`selected(names)` wird `refetchPlan(names)`:**

- Liest von jedem Eintrag das aktuelle Ziel und filtert nach Name. Eine leere Namensliste heißt alle.
- Gruppiert nach `key` in der Reihenfolge der Registrierung.
- Liefert pro Gruppe einen Effekt: `begin` aller Einträge der Gruppe, dann einmal `fetch` des ersten, dann `end` aller, auch bei einem Fehler (`Sync.ensure`).
- `refetchQueries` und `resetStore` arbeiten diese Liste weiter **nacheinander** ab, und der erste Fehler bricht ab. Das ist das heutige Verhalten, es bleibt unverändert und wird in der Scaladoc genannt.

`registerResetHook` bleibt, wie es ist.

### 2. Genau eine Stelle registriert: der Watcher selbst

In kyo-apollo-ui kommt ein privater Helfer `ApolloSignal.registerWatcher(target: Maybe[Target] < Sync)`, der `client.activeQueries.register` aufruft. Die Einstiege bekommen ihr Ziel so:

| Einstieg | Aktuelles Ziel |
|---|---|
| `watchSignal(call)` (`Apollo.scala:626`) | immer `Present(target(call))` |
| `watchSignal(call, skip, mode)` (`:642`) | `skip.current`: bei `true` `Absent`, sonst das Ziel; gilt für `Freeze` und `Unsubscribe` |
| `watchSignal(values)(call)` (`:668`) | `values.current`: `Absent` bleibt `Absent`, `Present(v)` wird `target(call(v))` |
| `pollingSignal` beide (`:687`, `:701`) | erbt von `watchSignal` |
| `paginatedQuery(initial)(page)` und mit `skip` (`:427`, `:464`) | erbt von `watchSignal(page(initial))`; der Refetch der ersten Seite führt in die Connection zusammen wie `pagedOver.refetch`, die Scaladoc sagt das |
| `paginatedQuery(values)(initial)(page)` (`:529`) | eigene Registrierung mit `values.current` und `page(v, initial)`, weil diese Form `driveSwitching` direkt aufruft und nicht über `watchSignal(values)` geht |
| `preload` (`:611`) | erbt von `watchSignal` |
| `lazyQuery` (`:62`) | `Absent` bis zum ersten `load`, danach das Ziel mit `load` als `fetch`; `load` schreibt `state` selbst, weil kein Cache-Watcher dahintersteht |
| `query(call)` und `query(call, skip, mode)` (`:41`, `:52`) | über den privaten Kern mit `during = Present(activity)`; `buildQueryHandle` registriert **nicht mehr selbst**, so gibt es keinen doppelten Eintrag |

**Dafür nötig:**

- Die drei `watchSignal`-Formen bekommen einen privaten Kern mit dem Parameter `during: Maybe[SignalRef[Maybe[NetworkStatus]]]`. Die öffentlichen Signaturen bleiben gleich.
- `buildQueryHandle(call, stateEff)` wird `buildQueryHandle(call, activity => stateEff(activity))`: Es legt `activity` zuerst an und gibt die Zelle an den Kern.
- `QueryHandle.refetch` bleibt `withActivity(Refetch)(networkFetch)`.

**Abonnements und Fragmente bleiben draußen.** Abonnements (`subscribe`) und Fragment-Signale (`fragment`, `fragmentRows`, …) haben keine Operation zum Neuladen. Bei React nehmen sie ebenso wenig teil. Sie sehen den neuen Stand, sobald der Refetch ihre Schlüssel schreibt.

### 3. Doku

- **Scaladoc anpassen**, die heute nur „`useQuery` watchers“ nennt:
  - `ActiveQueryRegistry.scala:7-10`;
  - `ApolloClient.scala:71-74`;
  - `ApolloRefetch.scala:7-9, 20-32` (jede beobachtete Abfrage außer pausierten, entdoppelt, nacheinander, erster Fehler bricht ab);
  - der Kommentar in `ApolloQuery.scala:338-341`.
- **Scaladoc der Einstiege:** Jeder sagt einen Satz dazu, dass er an `refetchQueries` und `resetStore` teilnimmt, solange er nicht pausiert ist.
- **`query(call, skip, mode)`:** Der Satz „`refetch` is unaffected“ bleibt für das Handle selbst richtig. Neu dazu: Die Registry übergeht ein pausiertes Handle.
- **Kommentar in `build.sbt:4237`:** „JS only“ für kyo-apollo-ui ist veraltet und wird korrigiert, weil der Plan sich auf die Plattformen stützt.

### 4. Danach in der App (kyo-apollo-examples)

- Snapshot `1.0.0-RC7-v2fail-20261020b-SNAPSHOT` lokal veröffentlichen, mit dem Skript `~/.cache/kyo-runs/publish-20261020a.sh` und neuer Version. Vorher prüfen, dass die Version frei ist. Dann den Pin `SpotifyKyoVersion` anheben.
- **`Conflict.reload` (`StatusControl.scala:83-87`)** wird `client.refetchQueries("IssueDetail")` statt `Scope.run(...execute).unit`. Damit ist der Rest von F1 behoben: Ein Fehler erreicht die Regeln. Zuerst kommt ein roter Test in `DecisionsInAppSpec`, ein fehlschlagendes Neuladen nach einem 409 führt zu einer Notice, nach dem Muster des W17-Tests.
- **Texte nachziehen:** `LIBRARY-IDEAS.md` #16 als erledigt markieren, mit Commit und Snapshot. Außerdem `APP-REVIEW.md` (F1, W2 im Agentenbericht), den Kommentar zum Pin in `build.sbt`, die `uses`-Texte in `Decisions` und die Code-Ziele in `Tours`, falls Zeilen von Schritt 9 betroffen sind.
- **W17 bleibt, wie es ist.** Das Handle ist für Refresh und Polling weiter der klarere Weg.

## Tests (zuerst rot)

Neue Datei `kyo-apollo-ui/src/test/scala/kyo/apollo/ApolloRefetchSpec.scala`, Basis `kyo.test.Test[Any]`, geteilt auf allen Plattformen.

**Hilfsmittel:** aus `KyoTestSupport.scala`

- `cached(engine)`;
- `StaticEngine` mit dem Zähler `calls`;
- `GatedEngine` mit `nextRequest`/`release`;
- `awaitSignal`;
- ein neues Fixture `IssueQuery(id)` mit Variablen, als Case-Class nach dem Muster von `CurrentUserQuery`.

Keine `sleep`s, nur Barrieren und `awaitSignal`.

**Rot vor dem Fix:**

1. `refetchQueries("CurrentUser")` erreicht `watchSignal`: Die Engine zählt eine Anfrage mehr, und das Signal zeigt die neue Antwort.
2. Dasselbe für `pollingSignal`, `paginatedQuery`, `preload` und `watchSignal(values)` mit `Present`.
3. `lazyQuery`: vor dem ersten `load` keine Anfrage, danach eine.
4. `resetStore`: Ein `watchSignal` zeigt danach die frische Antwort statt der alten.
5. **Entdopplung:** zwei `watchSignal` und ein `query` auf dieselbe Abfrage ergeben genau eine Anfrage. Das Handle zeigt währenddessen `NetworkStatus.Refetch`, geprüft mit `GatedEngine` vor `release`.
6. **Verschiedene Variablen** (`IssueQuery("a")`, `IssueQuery("b")`) ergeben zwei Anfragen.
7. **Standby:**
   - `watchSignal(call, skip)` mit `skip = true` ergibt keine Anfrage, nach `skip = false` eine.
   - Dasselbe für `query(call, skip)`. Das ist die Verhaltensänderung, und der Test war vorher rot, weil `query` pausiert neu lud.
   - `watchSignal(values)` bei `Absent`: keine Anfrage.
8. **Namensfilter:** `refetchQueries("Other")` lässt `CurrentUser` unberührt.

**Grün halten:**

9. Wenn der `Scope` schließt, ist der Eintrag weg (`client.activeQueries.entries`, sichtbar aus `package kyo.apollo`).
10. Ein Fehler beim Neuladen kommt als `Abort[ApolloException]` bei `refetchQueries` an.

**Weitere Tests:**

- `ActiveQueryRegistrySpec` (kyo-apollo) an die neue Form von `register` und `refetchPlan` anpassen.
- Neu dort: das Gruppieren nach Schlüssel, `begin`/`end` auch bei einem Fehler, und `Absent` wird übergangen.
- Der `typeCheckFailure`-Fall für die Fehler-Reihe bleibt.

## Verifikation

- **Library, in `kyo/.worktrees/snap-v2`:**
  - `scripts/sbt.sh test-jvm 'kyo-apolloJVM/testOnly kyo.apollo.ActiveQueryRegistrySpec' 'kyo-apollo-uiJVM/test' 'kyo-apollo-uiJS/test' 'kyo-apollo-uiNative/testOnly kyo.apollo.ApolloRefetchSpec' 'kyo-apollo-uiWasm/testOnly kyo.apollo.ApolloRefetchSpec'`, alles grün.
  - Davor den roten Lauf der neuen Spec festhalten, mit Log unter `~/.cache/kyo-runs/registry/`.
- **App:** `sbt --client 'scalafmtCheckAll; issuesClient/testFull; issuesClient/fastLinkJS'` grün, danach esbuild nach `client/bundle.js`.
- **Chrome mit Interceptor**, im eigenen Tab 466250851:
  - Schritt 10 mit „optimistic + refetch on 409“: im Server-Panel einen 409 und danach einen Netzwerkfehler für die nächste Anfrage auslösen, den Status wechseln. Das Log muss „unheard …“ bzw. die Notice zeigen statt nichts.
  - Pixel-Screenshot ansehen.
  - Danach zurück zu Tab 466250776.
- **Aufräumen:** sbt-Server beenden (über `readlink /proc/<pid>/cwd` geprüft), keine `pkill -f`.
- **Commits:** isoliert, mit expliziten Pfaden, nicht gepusht:
  - kyo: `[kyo-apollo] every live watcher is in the registry, once per query, and none while it is paused`;
  - App: eigener Commit für Pin und `Conflict.reload`.
