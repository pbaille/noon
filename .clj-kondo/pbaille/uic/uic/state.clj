(ns uic.state)

(defmacro signal [[deps event] & body]
  `(fn [~'_ ~event]
     (let ~(vec (interleave (keys deps) (repeat nil)))
       ~@body)))
