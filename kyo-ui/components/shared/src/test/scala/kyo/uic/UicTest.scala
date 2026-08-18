package kyo.uic

/** Base class for the kyo-ui-components suites, the component-library counterpart of kyo-ui's `UITest`.
  *
  * These suites are pure: they assert on rendered HTML strings, on the menu state machines, and on what does or does
  * not type-check. Nothing drives a browser, so none of `UITest`'s Chrome accommodations apply and the kyo-test
  * defaults are exactly right.
  */
abstract class UicTest extends kyo.test.Test[Any]
