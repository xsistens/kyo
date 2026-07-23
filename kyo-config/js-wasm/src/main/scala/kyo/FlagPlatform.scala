package kyo

import scala.jdk.CollectionConverters.*
import scala.scalajs.js

private[kyo] object FlagPlatform {

    def property(name: String): String =
        java.lang.System.getProperty(name)

    def properties: Iterable[String] =
        java.lang.System.getProperties.propertyNames().asScala.map(_.toString).toList

    // The `process` global read MUST stay inlined under `js.typeOf`: the compiler special-cases
    // `js.typeOf(<global member>)` to emit a bare `typeof process`, which is safe when the global is
    // undeclared. Binding `js.Dynamic.global.process` to a `val` first (as upstream's shape does) emits a
    // direct global `process` reference that throws `ReferenceError: process is not defined` in ESM browser
    // bundles (strict mode) BEFORE the typeof guard ever runs. The nested `js.typeOf(...process.env)`
    // compiles to `typeof process.env`, which would also throw on an undeclared `process`, but the
    // short-circuit `||` guarantees it only evaluates once `process` is known to exist.
    // On absence we fall back to `java.lang.System.getenv` (adopted from upstream #1766's env refactor):
    // null under Scala.js-Node, but a Wasm host may resolve it through its own environment binding, so the
    // fallback gives the caller its best answer instead of a hardcoded null — while keeping the inlined guard.
    def env(name: String): String = {
        if (
            js.typeOf(js.Dynamic.global.process) == "undefined" ||
            js.typeOf(js.Dynamic.global.process.env) == "undefined"
        ) java.lang.System.getenv(name)
        else {
            val value = js.Dynamic.global.process.env.selectDynamic(name)
            if (js.isUndefined(value) || value == null) java.lang.System.getenv(name)
            else value.asInstanceOf[String]
        }
    }

    def envNames: Iterable[String] = {
        if (
            js.typeOf(js.Dynamic.global.process) == "undefined" ||
            js.typeOf(js.Dynamic.global.process.env) == "undefined"
        ) Iterable.empty
        else js.Object.keys(js.Dynamic.global.process.env.asInstanceOf[js.Object]).toSeq
    }

}
