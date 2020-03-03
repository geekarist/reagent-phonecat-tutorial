(ns reagent-phonecat.core)

(defmacro <?
  "Version of <! which throws Error coming out of the channel"
  [c]
  `(reagent-phonecat.core/accept-or-throw (cljs.core.async/<! ~c)))

(defmacro error-or
  "If body throws an exception, catch and return it"
  [& body]
  `(try ~@body
        (catch js/Error e# e#)))

(defmacro go-safe "Same as `go` but catch and return exceptions" [& body]
  `(cljs.core.async.macros/go
     (reagent-phonecat.core/error-or ~@body)))