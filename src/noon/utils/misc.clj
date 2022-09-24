(ns noon.utils.misc
  (:require [clojure.string :as str]
            [me.raynes.fs :as fs]))

(do :numbers

    (defn abs [x] (if (neg? x) (* x -1) x))

    (defn divmod [mod x]
      (let [r (rem x mod)
            n (int (/ x mod))]
        (if (neg? r)
          [(- n 1) (+ r mod)]
          [n r])))

    (defn round [x]
      (let [r (rem x 1)]
        (if (> r 0.5)
          (Math/ceil x)
          (Math/floor x))))

    (defn rounded-div
      [n d]
      (+ (quot n d)
         (if (> (* 2 (rem n d)) d) 1 0)))

    (defn dist [a b]
      (abs (- a b)))

    (def negate (partial * -1))

    ;; from overtone
    (defn scale-range
      "
     Scales a given input value within the specified input range to a
     corresponding value in the specified output range using the formula:

              (out-max - out-min) (x - in-min)
      f (x) = --------------------------------  + out-min
                      in-max - in-min
    "
      ([in [in-min in-max] [out-min out-max]]
       (scale-range in in-min in-max out-min out-max))
      ([x in-min in-max out-min out-max]
       (+ (/ (* (- out-max out-min) (- x in-min))
             (- in-max in-min))
          out-min)))

    (defn linear-interpolation
      "not sure about the name"
      [from to steps]
      (let [step (/ (- to from)
                    (dec steps))]
        (take steps (iterate (partial + step) from))))

    #_(linear-interpolation 4 6 10)

    (defn sums

      ([total size steps]
       (sums total size steps (apply min steps) (apply max steps)))
      ([total size steps floor ceil]
       #_(println "sums3" total size steps floor ceil)
       (if (>= (* size ceil) total (* size floor))
         (cond
           (= 1 size) (if ((set steps) total)
                        (list (list total)))
           :else (mapcat (fn [step]
                           (map (partial cons step)
                                (sums (- total step) (dec size)
                                      (filter (fn [s] (>= s step)) steps)
                                      step
                                      ceil)))
                         steps)))))

    (defn lazy-primes
      ([] (lazy-primes 2 []))
      ([current known-primes]
       (let [factors (take-while #(<= (* % %) current) known-primes)
             remainders (map #(mod current %) factors)]
         (if (not-any? zero? remainders)
           (lazy-seq (cons
                      current
                      (lazy-primes (inc current) (conj known-primes current))))
           (recur (inc current) known-primes)))))

    (defn factorize [num]
      (loop [num num, acc [1], primes (lazy-primes)]
        (if (= num 1)
          acc
          (let [factor (first primes)]
            (if (= 0 (mod num factor))
              (recur (quot num factor) (conj acc factor) primes)
              (recur num acc (rest primes)))))))

    (defn rand-int-between
      "return an integer between a and b (inclusive)."
      [a b]
      (let [[from to] (sort (map int [a b]))]
        (rand-nth (range from (inc to))))))

(do :error&logs

    (defn throw* [& xs]
      (throw (Exception. (apply str xs))))

    (defmacro prob [& xs]
      `(let [ret# ~(last xs)]
         (mapv println ["\n---" ~@(butlast xs) ret# "---\n"])
         ret#)))

(do :strings-and-names

    (def random-kw? #{:random :rand})

    (defn str->keyword [name]
      (-> (str/lower-case name)
          (str/replace #" " "-")
          (keyword))))

(do :metadata

    (defn t
      "artity 2: assign type sym to e
   arity 1: get the type tag of e"
      ([e] (:type (meta e)))
      ([sym e] (vary-meta e assoc :type sym)))

    (defn t?
      "check if e is of type (type tag) sym"
      ([sym] (partial t? sym))
      ([sym e] (= sym (t e))))

    (defn t=
      "check if all given args are of same type (tag)"
      [x & xs]
      (every? (partial t? (t x)) xs))

    (defn flagged
      "add some flags into metadata"
      ([flag value]
       (vary-meta value assoc flag true))
      ([flag1 flag2 value]
       (vary-meta value assoc flag1 true flag2 true))
      ([flag1 flag2 flag3 value]
       (vary-meta value assoc flag1 true flag2 true flag3 true))
      ([flag1 flag2 flag3 flag4 & rest]
       (vary-meta (last rest)
                  merge
                  (zipmap (concat (list flag1 flag2 flag3 flag4)
                                  (butlast rest))
                          (repeat true)))))

    (defn flagged?
      ([flag] (partial flagged? flag))
      ([flag value] (get (meta value) flag))))

(do :macros

    (do :utils

        (defn parse-defn [[name x & xs]]
          (let [[doc [x & xs]] (if (string? x) [x xs] [nil (cons x xs)])
                [attrs body] (if (map? x) [x xs] [nil (cons x xs)])
                arities (if (vector? (first body)) (list body) body)]
            {:name name
             :doc doc
             :attrs attrs
             :arities arities})))

    (defmacro >_
      "shorthand for (as-> x _ ...)"
      [seed & forms]
      `(as-> ~seed ~'_ ~@forms))

    (defmacro f_
      "Unary lambda with threading body.
   arity 1: simple unary lambda with arg bound to _
   arity 2+: shorthand for: (fn [x] (as-> x _ ...))."
      ([ret]
       `(fn [~'_] ~ret))
      ([x & xs]
       `(fn [x#] (>_ x# ~x ~@xs)))))

(do :colls

    (defn snoc [l x]
      (concat l (list x)))

    (defn $ [x f]
      (cond (seq? x) (map f x)
            (vector? x) (mapv f x)
            (map? x) (into {} (map f x))
            (set? x) (into #{} (map f x))))

    (defn deep-check
      "Deeply checks if all values in a map are truthy."
      [m]
      (if (map? m)
        (every? deep-check (vals m))
        (boolean m)))

    (defn deep-merge [x y]
      (cond
        (and (map? x) (map? y)) (merge-with deep-merge x y)
        (nil? y) x
        :else y))

    (defn map-vals [f m]
      (apply merge (map (fn [[k v]] {k (f v)}) m)))

    ;; TODO this do not seems to be correct, should be removed ?
    (defn all-paths
      "return all the possible paths of a map"
      [m & [ret]]
      (let [ret (or ret [[]])
            next-ret
            (mapcat
             (fn [path]
               (map
                (fn [[k _]]
                  (conj path k))
                (get-in m path)))
             ret)]
        (if (seq next-ret)
          (concat ret (all-paths m next-ret))
          ret)))

    (defn hm-nodes
      ([] (sorted-map))
      ([x] (hm-nodes x [] (sorted-map)))
      ([x from acc]
       (let [acc (assoc acc from x)]
         (if (map? x)
           (reduce (fn [acc [i v]] (hm-nodes v (conj from i) acc))
                   acc
                   x)
           acc))))

    (defn hm-leaves [m]
      (->> (hm-nodes m)
           (remove (fn [[_ v]] (map? v)))
           (into {}))))

(do :form

    (defn form
      [x]
      (or (some-> x meta :form form)
          (if (coll? x) ($ x form) x)))

    (defn with-form [x f]
      (vary-meta x assoc :form f)))

(do :eq

    (defn eq
      ([x]
       (with-form
         (fn [y] (eq x y))
         `(eq ~x)))
      ([x y]
       (= (form x)
          (form y)))))

(do :closures

    (defn- qualify-closure-name [x]
      (symbol (str *ns*) (name x)))

    (defn- closure-form-expression [nam argv]
      (let [variadic? (some #{'&} argv)
            name (qualify-closure-name nam)]
        (if variadic?
          `(list* '~name ~@(drop-last 2 argv) ~(last argv))
          `(list '~name ~@argv))))

    (defmacro defclosure
      "utility to define functions that returns functions.
       the point is to mark the returned function so it can be compared."
      ([name return]
       `(def ~name
          (with-form ~return
            '~(qualify-closure-name name))))
      ([name x & xs]
       (let [{:keys [doc attrs arities]}
             (parse-defn (list* name x xs))]

         `(defn ~name
            ~@(if doc [doc])
            ~@(if attrs [attrs])
            ~@(map (fn [[argv & body]]
                     (assert (every? symbol? argv)
                             "no destructuration allowed here")
                     `(~argv (with-form (do ~@body)
                               ~(closure-form-expression name argv))))
                   arities))))))

(do :files&paths

    (defn parse-file-path [n]
      (if n
        (let [xs (str/split n #"/")
              filename (last xs)
              directory (str/join "/" (butlast xs))
              [file-barename & extensions] (str/split filename #"\.")]
          (->> {:fullname n
                :directory directory
                :filename filename
                :extension (str/join "." extensions)
                :file-barename file-barename}
               (filter (comp seq val))
               (into {})))))

    #_(parse-file-path "yo/gro/tonk.op")
    #_(parse-file-path nil)

    (defn ensure-directory [x]
      (when-not (fs/exists? x)
        (fs/mkdirs x)))

    (comment
      (ensure-directory "generated/history")
      (ensure-file "generated/history/one.bob"))

    (defn ensure-file [name]
      (when-not (fs/exists? name)
        (let [{:keys [directory]} (parse-file-path name)]
          (fs/mkdirs directory)
          (fs/create (fs/file name)))))

    (defn copy-file [file name]
      #_(if (fs/exists? name)
          (fs/delete name))
      (fs/copy file name)))

(do :more-macros

    (defmacro definvokable
      [type argv body]
      (let [invoke-sym (gensym)
            args (repeatedly 20 gensym)
            arity (fn [n]
                    (let [args (take n args)]
                      `(invoke [this# ~@args] (~invoke-sym this# ~@args))))
            vararg `(invoke [this# ~@args more#]
                            (apply ~invoke-sym ~@args more#))
            apply-to `(applyTo [this# args#] (apply ~invoke-sym this# args#))]
        `(do
           (def ~invoke-sym (fn ~argv ~body))
           (defrecord ~type []
             clojure.lang.IFn
             ~@(map arity (range (inc 20)))
             ~vararg
             ~apply-to))))

    (defn hm->defs
      "takes an hashmap of type (named x) -> any, and def all in the given ns"
      [ns hm]
      (doseq [[sym val] hm]
        (intern ns (symbol (name sym)) val)))

    (defn reduction
      "Turn a binary fn 'f into a variadic function that use 'f and reduce to produce a result,
       shortcircuiting on first nil intermediate result"
      [f]
      (fn [this & xs]
        (reduce (fn [this x]
                  (if this
                    (f this x)
                    (reduced nil)))
                this xs)))

    (defmacro defreduction
      [name x & xs]
      (let [[doc [argv & body]]
            (if (string? x) [x xs] [nil (cons x xs)])]
        `(def ~name
           ~@(if doc [doc])
           (reduction (fn ~argv ~@body))))))
