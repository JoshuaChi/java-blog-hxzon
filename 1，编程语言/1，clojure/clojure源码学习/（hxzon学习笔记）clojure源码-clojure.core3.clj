;（hxzon学习笔记）clojure源码-clojure.core3.clj

;====
;定义私有函数：

(defmacro defn-
  "same as defn, yielding non-public def"
  {:added "1.0"}
  [name & decls]
    (list* `defn (with-meta name (assoc (meta name) :private true)) decls))

;====
;检查是否是特殊形式：

(defn special-symbol?
  "Returns true if s names a special form"
  {:added "1.0"
   :static true}
  [s]
    (contains? (. clojure.lang.Compiler specials) s))


;=====
;条件分支：

(defmacro cond
  "Takes a set of test/expr pairs. It evaluates each test one at a
  time.  If a test returns logical true, cond evaluates and returns
  the value of the corresponding expr and doesn't evaluate any of the
  other tests or exprs. (cond) returns nil."
  {:added "1.0"}
  [& clauses]
    (when clauses
      (list 'if (first clauses)
            (if (next clauses)
                (second clauses)
                (throw (IllegalArgumentException.
                         "cond requires an even number of forms")))
            (cons 'clojure.core/cond (next (next clauses))))))


;======
;
(defn spread
  {:private true
   :static true}
  [arglist]
  (cond
   (nil? arglist) nil
   (nil? (next arglist)) (seq (first arglist))
   :else (cons (first arglist) (spread (next arglist)))))

;创建列表：

(defn list*
  "Creates a new list containing the items prepended to the rest, the
  last of which will be treated as a sequence."
  {:added "1.0"
   :static true}
  ([args] (seq args))
  ([a args] (cons a args))
  ([a b args] (cons a (cons b args)))
  ([a b c args] (cons a (cons b (cons c args))))
  ([a b c d & more]
     (cons a (cons b (cons c (cons d (spread more)))))))

;=====
;函数应用：

(defn apply
  "Applies fn f to the argument list formed by prepending intervening arguments to args."
  {:added "1.0"
   :static true}
  ([^clojure.lang.IFn f args]
     (. f (applyTo (seq args))))
  ([^clojure.lang.IFn f x args]
     (. f (applyTo (list* x args))))
  ([^clojure.lang.IFn f x y args]
     (. f (applyTo (list* x y args))))
  ([^clojure.lang.IFn f x y z args]
     (. f (applyTo (list* x y z args))))
  ([^clojure.lang.IFn f a b c d & args]
     (. f (applyTo (cons a (cons b (cons c (cons d (spread args)))))))))

;====
;创建延迟序列：

(defmacro lazy-seq
  "Takes a body of expressions that returns an ISeq or nil, and yields
  a Seqable object that will invoke the body only the first time seq
  is called, and will cache the result and return it on all subsequent
  seq calls. See also - realized?"
  {:added "1.0"}
  [& body]
  (list 'new 'clojure.lang.LazySeq (list* '^{:once true} fn* [] body)))    

;====
;序列拼接：

(defn concat
  "Returns a lazy seq representing the concatenation of the elements in the supplied colls."
  {:added "1.0"
   :static true}
  ([] (lazy-seq nil))
  ([x] (lazy-seq x))
  ([x y]
    (lazy-seq
      (let [s (seq x)]
        (if s
          (if (chunked-seq? s)
            (chunk-cons (chunk-first s) (concat (chunk-rest s) y))
            (cons (first s) (concat (rest s) y)))   ;创建新惰性序列，以x的头元素为新头部，以x的余部和y为新余部。
          y))))
  ([x y & zs]
     (let [cat (fn cat [xys zs]
                 (lazy-seq
                   (let [xys (seq xys)]
                     (if xys
                       (if (chunked-seq? xys)
                         (chunk-cons (chunk-first xys)
                                     (cat (chunk-rest xys) zs))
                         (cons (first xys) (cat (rest xys) zs)))
                       (when zs
                         (cat (first zs) (next zs)))))))]
       (cat (concat x y) zs))))

;=====
;与：

(defmacro and
  "Evaluates exprs one at a time, from left to right. If a form
  returns logical false (nil or false), and returns that value and
  doesn't evaluate any of the other expressions, otherwise it returns
  the value of the last expr. (and) returns true."
  {:added "1.0"}
  ([] true)
  ([x] x)
  ([x & next]
   `(let [and# ~x]
      (if and# (and ~@next) and#))))

;或：

(defmacro or
  "Evaluates exprs one at a time, from left to right. If a form
  returns a logical true value, or returns that value and doesn't
  evaluate any of the other expressions, otherwise it returns the
  value of the last expression. (or) returns nil."
  {:added "1.0"}
  ([] nil)
  ([x] x)
  ([x & next]
      `(let [or# ~x]
         (if or# or# (or ~@next)))))

;====
;数字增一，自动类型提升：

(defn inc'
  "Returns a number one greater than num. Supports arbitrary precision.
  See also: inc"
  {:inline (fn [x] `(. clojure.lang.Numbers (incP ~x)))
   :added "1.0"}
  [x] (. clojure.lang.Numbers (incP x)))

;数字增一，可能抛出溢出异常：

(defn inc
  "Returns a number one greater than num. Does not auto-promote
  longs, will throw on overflow. See also: inc'"
  {:inline (fn [x] `(. clojure.lang.Numbers (~(if *unchecked-math* 'unchecked_inc 'inc) ~x)))
   :added "1.2"}
  [x] (. clojure.lang.Numbers (inc x)))

;加法，自动类型提升：

(defn +'
  "Returns the sum of nums. (+) returns 0. Supports arbitrary precision.
  See also: +"
  {:inline (nary-inline 'addP)
   :inline-arities >1?
   :added "1.0"}
  ([] 0)
  ([x] (cast Number x))
  ([x y] (. clojure.lang.Numbers (addP x y)))
  ([x y & more]
   (reduce1 +' (+' x y) more)))

;加法，可能抛出溢出异常：

(defn +
  "Returns the sum of nums. (+) returns 0. Does not auto-promote
  longs, will throw on overflow. See also: +'"
  {:inline (nary-inline 'add 'unchecked_add)
   :inline-arities >1?
   :added "1.2"}
  ([] 0)
  ([x] (cast Number x))
  ([x y] (. clojure.lang.Numbers (add x y)))
  ([x y & more]
     (reduce1 + (+ x y) more)))

;====
;查找键值对：

(defn find
  "Returns the map entry for key, or nil if key not present."
  {:added "1.0"
   :static true}
  [map key] (. clojure.lang.RT (find map key)))     ;clojure.lang.RT.find(map,key)

;====
;串行宏，前一表达式的值作为点表达式的第2个元素：

(defmacro ..
  "form => fieldName-symbol or (instanceMethodName-symbol args*)

  Expands into a member access (.) of the first member on the first
  argument, followed by the next member on the result, etc. For
  instance:

  (.. System (getProperties) (get \"os.name\"))

  expands to:

  (. (. System (getProperties)) (get \"os.name\"))

  but is easier to write, read, and understand."

  {:added "1.0"}
  ([x form] `(. ~x ~form))
  ([x form & more] `(.. (. ~x ~form) ~@more)))

;串行宏，前一表达式的值作为后一个表达式的第2个元素：

(defmacro ->
  "Threads the expr through the forms. Inserts x as the
  second item in the first form, making a list of it if it is not a
  list already. If there are more forms, inserts the first form as the
  second item in second form, etc."
  {:added "1.0"}
  ([x] x)
  ([x form] (if (seq? form)
              (with-meta `(~(first form) ~x ~@(next form)) (meta form)) ;将xU 放入 formU 的第2个位置中，求值formU，带上原来的元数据。
              (list form x)))
  ([x form & more] `(-> (-> ~x ~form) ~@more)))

;串行宏，前一表达式的值作为后一个表达式的最后一个元素：

(defmacro ->>
  "Threads the expr through the forms. Inserts x as the
  last item in the first form, making a list of it if it is not a
  list already. If there are more forms, inserts the first form as the
  last item in second form, etc."
  {:added "1.1"} 
  ([x form] (if (seq? form)
              (with-meta `(~(first form) ~@(next form)  ~x) (meta form))    ;为什么需要将form解开？
              (list form x)))
  ([x form & more] `(->> (->> ~x ~form) ~@more)))

;====
;创建循环无限序列：

(defn cycle
  "Returns a lazy (infinite!) sequence of repetitions of the items in coll."
  {:added "1.0"
   :static true}
  [coll] (lazy-seq 
          (when-let [s (seq coll)] 
              (concat s (cycle s)))))

;====
;声明：

(defmacro declare
  "defs the supplied var names with no bindings, useful for making forward declarations."
  {:added "1.0"}
  [& names] `(do ~@(map #(list 'def (vary-meta % assoc :declared true)) names)))

;====
;强制求值：

(defn dorun
  "When lazy sequences are produced via functions that have side
  effects, any effects other than those needed to produce the first
  element in the seq do not occur until the seq is consumed. dorun can
  be used to force any effects. Walks through the successive nexts of
  the seq, does not retain the head and returns nil."
  {:added "1.0"
   :static true}
  ([coll]
   (when (seq coll)
     (recur (next coll))))
  ([n coll]
   (when (and (seq coll) (pos? n))
     (recur (dec n) (next coll)))))

;强制求值：

(defn doall
  "When lazy sequences are produced via functions that have side
  effects, any effects other than those needed to produce the first
  element in the seq do not occur until the seq is consumed. doall can
  be used to force any effects. Walks through the successive nexts of
  the seq, retains the head and returns it, thus causing the entire
  seq to reside in memory at one time."
  {:added "1.0"
   :static true}
  ([coll]
   (dorun coll)
   coll)
  ([n coll]
   (dorun n coll)
   coll))

;类型转型：

(definline chars
  "Casts to chars[]"
  {:added "1.1"}
  [xs] `(. clojure.lang.Numbers chars ~xs))     ;clojure.lang.Numbers.chars(xs)

;====
;一次性定义：

(defmacro defonce
  "defs name to have the root value of the expr iff the named var has no root value,
  else expr is unevaluated"
  {:added "1.0"}
  [name expr]
  `(let [v# (def ~name)]    ;(def ~name) 返回什么？
     (when-not (.hasRoot v#)    ;如果v没有根值，求值expr，并绑定到name。
       (def ~name ~expr))))

;====
;;;;;;;;;;;;; nested associative ops ;;;;;;;;;;;
;内嵌结构操作：

(defn get-in
  "Returns the value in a nested associative structure,
  where ks is a sequence of keys. Returns nil if the key
  is not present, or the not-found value if supplied."
  {:added "1.2"
   :static true}
  ([m ks]
     (reduce1 get m ks))
  ([m ks not-found]
     (loop [sentinel (Object.)
            m m
            ks (seq ks)]
       (if ks
         (let [m (get m (first ks) sentinel)]
           (if (identical? sentinel m)
             not-found
             (recur sentinel m (next ks))))
         m))))

(defn assoc-in
  "Associates a value in a nested associative structure, where ks is a
  sequence of keys and v is the new value and returns a new nested structure.
  If any levels do not exist, hash-maps will be created."
  {:added "1.0"
   :static true}
  [m [k & ks] v]
  (if ks
    (assoc m k (assoc-in (get m k) ks v))
    (assoc m k v)))

(defn update-in
  "'Updates' a value in a nested associative structure, where ks is a
  sequence of keys and f is a function that will take the old value
  and any supplied args and return the new value, and returns a new
  nested structure.  If any levels do not exist, hash-maps will be
  created."
  {:added "1.0"
   :static true}
  ([m [k & ks] f & args]
   (if ks
     (assoc m k (apply update-in (get m k) ks f args))
     (assoc m k (apply f (get m k) args)))))

;====
;插入：

(defn into
  "Returns a new coll consisting of to-coll with all of the items of
  from-coll conjoined."
  {:added "1.0"
   :static true}
  [to from]
  (if (instance? clojure.lang.IEditableCollection to)
    (with-meta (persistent! (reduce conj! (transient to) from)) (meta to))
    (reduce conj to from)))

;====
;并发处理

(defn pmap
  "Like map, except f is applied in parallel. Semi-lazy in that the
  parallel computation stays ahead of the consumption, but doesn't
  realize the entire result unless required. Only useful for
  computationally intensive functions where the time of f dominates
  the coordination overhead."
  {:added "1.0"
   :static true}
  ([f coll]
   (let [n (+ 2 (.. Runtime getRuntime availableProcessors))
         rets (map #(future (f %)) coll)
         step (fn step [[x & xs :as vs] fs]
                (lazy-seq
                 (if-let [s (seq fs)]
                   (cons (deref x) (step xs (rest s)))
                   (map deref vs))))]
     (step rets (drop n rets))))
  ([f coll & colls]
   (let [step (fn step [cs]
                (lazy-seq
                 (let [ss (map seq cs)]
                   (when (every? identity ss)
                     (cons (map first ss) (step (map rest ss)))))))]
     (pmap #(apply f %) (step (cons coll colls))))))

(defn pcalls
  "Executes the no-arg fns in parallel, returning a lazy sequence of
  their values"
  {:added "1.0"
   :static true}
  [& fns] (pmap #(%) fns))

(defmacro pvalues
  "Returns a lazy sequence of the values of the exprs, which are
  evaluated in parallel"
  {:added "1.0"
   :static true}
  [& exprs]
  `(pcalls ~@(map #(list `fn [] %) exprs)))

;; ====
(defmacro for
  "List comprehension. Takes a vector of one or more
   binding-form/collection-expr pairs, each followed by zero or more
   modifiers, and yields a lazy sequence of evaluations of expr.
   Collections are iterated in a nested fashion, rightmost fastest,
   and nested coll-exprs can refer to bindings created in prior
   binding-forms.  Supported modifiers are: :let [binding-form expr ...],
   :while test, :when test.

  (take 100 (for [x (range 100000000) y (range 1000000) :while (< y x)] [x y]))"
  {:added "1.0"}
  [seq-exprs body-expr]
  (assert-args
     (vector? seq-exprs) "a vector for its binding"
     (even? (count seq-exprs)) "an even number of forms in binding vector")

  (let [to-groups (fn [seq-exprs]
                    ;; seq-exprs : for 的绑定向量
                    (reduce1 (fn [groups [k v]]
                              (if (keyword? k)
                                (conj (pop groups) (conj (peek groups) [k v]))
                                ;;  遇到关键字，[a av b bv] -> [a av [b bv :k kv]]
                                (conj groups [k v])))
                            [] (partition 2 seq-exprs)))
                            ;;  绑定向量中的元素两两成一组
                            ;; 输出： [ [a av ] [b bv :k kv :k2 k2v] [c cv] ] ，每个元素是一层

        err (fn [& msg] (throw (IllegalArgumentException. ^String (apply str msg))))

        ;; emit-bind 生成一个迭代函数 iter# 
        ;; 迭代函数的输入是本层的值（序列），它处理第一个元素，然后递归调用，处理剩余的元素
        emit-bind (fn emit-bind [[[bind expr & mod-pairs]
                                  & [[_ next-expr] :as next-groups]]]
                    ;; 输入即 to-groups 的输出
                    ;; bind = a , expr = av , mod-pairs = [:k kv :k2 k2v]
                    ;; next-expr = bv , next-groups =[ [b bv] [c cv] ]

                    (let [giter (gensym "iter__")
                          ;; emit-bind 生成一个函数，giter 是这个函数的名字 （gen_iter）
                          gxs (gensym "s__")
                          ;; gxs ： giter 的参数名

                          do-mod (fn do-mod [[[k v :as pair] & etc]]
                          ;; do-mod 递归的生成本层的身体（非chunked-seq版本）
                          ;; 输入 ： [ :k v :k2 k2v ] ，即下一层的修饰器
                 (cond
                                     (= k :let) `(let ~v ~(do-mod etc))
                                     (= k :while) `(when ~v ~(do-mod etc))
                                     (= k :when) `(if ~v
                                                    ~(do-mod etc)
                                                    (recur (rest ~gxs)))
                                     (keyword? k) (err "Invalid 'for' keyword " k)
                   ;; 有下一层时（闭包，捕获了 next-groups）
                   next-groups
                                      `(let [iterys# ~(emit-bind next-groups)
                                             fs# (seq (iterys# ~next-expr))]
                                         ;; iterys# ：下一层的迭代函数
                                         ;; fs# ：本层第一个元素的收集结果
                                         ;; 收集本层第一个元素的结果，并递归处理本层剩余的元素
                                         (if fs#
                                           (concat fs# (~giter (rest ~gxs)))
                                           (recur (rest ~gxs))))
                    ;; 最后一层时（插入 for 的 body-expr），递归处理本层剩余的元素
                   :else `(cons ~body-expr
                                                  (~giter (rest ~gxs)))))]

                      (if next-groups

                        #_"not the inner-most loop"
                        `(fn ~giter [~gxs]
                           ;; 迭代函数，输入是本层的值
                           (lazy-seq
                             (loop [~gxs ~gxs]
                               (when-first [~bind ~gxs]
                                 ~(do-mod mod-pairs)))))

                        #_"inner-most loop"
                        ;; 已经是最后一层
                        (let [gi (gensym "i__")
                              ;; gi ：迭代本段的元素时的位置索引
                              gb (gensym "b__")
                              ;; gb ：本段的结果缓存的名字

                              do-cmod (fn do-cmod [[[k v :as pair] & etc]]
                              ;; do-cmod 递归的生成本层的身体（chunked-seq版本）
                                        (cond
                                          (= k :let) `(let ~v ~(do-cmod etc))
                                          (= k :while) `(when ~v ~(do-cmod etc))
                                          (= k :when) `(if ~v
                                                         ~(do-cmod etc)
                                                         (recur
                                                           (unchecked-inc ~gi)))
                                          (keyword? k)
                                            (err "Invalid 'for' keyword " k)
                                          ;; 收集当前元素的结果，迭代本段下一个元素
                                          :else
                                            `(do (chunk-append ~gb ~body-expr)
                                                 (recur (unchecked-inc ~gi)))))]

                          `(fn ~giter [~gxs]
                             (lazy-seq
                               (loop [~gxs ~gxs]
                                 (when-let [~gxs (seq ~gxs)]
                                   (if (chunked-seq? ~gxs)
                                     ;; chunked版本，一段一段批量处理本层元素
                                     (let [c# (chunk-first ~gxs)
                                           ;; c# ：本层的第一段
                                           size# (int (count c#))
                                           ;; size# ：第一段的大小
                                           ~gb (chunk-buffer size#)]
                                           ;; gb ：本段的结果缓存，放置第一段的结果
                                       ;; 迭代第一段的元素
                                       (if (loop [~gi (int 0)]
                                             (if (< ~gi size#)
                                               (let [~bind (.nth c# ~gi)]
                                                 ~(do-cmod mod-pairs))
                                               true))
                                         ;; 收集第一段的结果，并迭代剩余的段
                                         (chunk-cons
                                           (chunk ~gb)
                                           (~giter (chunk-rest ~gxs)))
                                         (chunk-cons (chunk ~gb) nil)))
                                     ;; 非chunked版本，逐个处理本层的元素
                                     (let [~bind (first ~gxs)]
                                       ~(do-mod mod-pairs)))))))))))]

    `(let [iter# ~(emit-bind (to-groups seq-exprs))]
       ;; seq-exprs : for 的绑定向量
       ;; emit-bind 生成第一层的迭代函数 iter# ，然后这个迭代函数处理第一层的值
        (iter# ~(second seq-exprs)))))

;; (for [a av] (fff a))
#_ (let [iter__1062__auto__ 
	(fn iter__6232 [s__6233] 
	(lazy-seq 
		(loop [s__6233 s__6233] 
			(when-let [s__6233 (seq s__6233)] 
				(if (chunked-seq? s__6233) 
					;; 分段批量处理
					(let [c__1060__auto__ (chunk-first s__6233) ;; a 的第一段
						size__1061__auto__ (int (count c__1060__auto__)) ;; 第一段大小 
						b__6235 (chunk-buffer size__1061__auto__)] ;; 缓存，放置结果
						(if 
							(loop [i__6234 (int 0)] ;; 迭代段中的元素
								(if (< i__6234 size__1061__auto__) 
									(let [a (.nth c__1060__auto__ i__6234)] ;; 段中的元素 a
										(do 
											(chunk-append b__6235 ^{:line 1, :column 29} (fff a)) ;; 放置结果
											(recur (unchecked-inc i__6234)))) ;; 段中的下一个元素
									true)) ;; end loop
								(chunk-cons (chunk b__6235) 
									(iter__6232 (chunk-rest s__6233))) ;; 迭代剩余的段（递归 iter__6232）
								(chunk-cons (chunk b__6235) nil)
							)) ;; end if end let
						;; not chunked-seq ，逐个元素处理
						(let [a (first s__6233)] 
							(cons ^{:line 1, :column 29} (fff a) 
								(iter__6232 (rest s__6233))))
	)))))] 
	(iter__1062__auto__ av))

;; (macroexpand-1 '(for [a av :let [bv (:k a)] :while (> bv 0) b bv] (fff b)))
#_(let [iter__1062__auto__ 
        (fn iter__6234 [s__6235] 
          (lazy-seq (loop [s__6235 s__6235] 
                      (when-first [a s__6235] ;; 第一层的第一个元素，a 
                        (let [bv ^{:line 1, :column 37} (:k a)] ;; 第二层开始，let
                          (when ^{:line 1, :column 52} (> bv 0) ;; 第二层，when
                            (let [iterys__1058__auto__ 
                                  (fn iter__6236 [s__6237] ;; 第二层迭代
                                    (lazy-seq (loop [s__6237 s__6237] 
                                                (when-let [s__6237 (seq s__6237)] 
                                                  (if (chunked-seq? s__6237) 
                                                    (let [c__1060__auto__ (chunk-first s__6237) 
                                                          size__1061__auto__ (int (count c__1060__auto__)) 
                                                          b__6239 (chunk-buffer size__1061__auto__)] 
                                                      (if (loop [i__6238 (int 0)] 
                                                            (if (< i__6238 size__1061__auto__) 
                                                              (let [b (.nth c__1060__auto__ i__6238)] 
                                                                (do 
                                                                  (chunk-append b__6239 ^{:line 1, :column 67} (fff b)) ;; 收集结果
                                                                  (recur (unchecked-inc i__6238)))) 
                                                              true)) 
                                                        (chunk-cons (chunk b__6239) 
                                                                    (iter__6236 (chunk-rest s__6237))) ;; 第二层，迭代剩余的段
                                                        (chunk-cons (chunk b__6239) nil))) 
                                                    (let [b (first s__6237)] 
                                                      (cons ^{:line 1, :column 67} (fff b) 
                                                            (iter__6236 (rest s__6237))))))))) 
                                  ;; 计算第一层的第一个元素的结果
                                  fs__1059__auto__ (seq (iterys__1058__auto__ bv))] 
                              ;; 收集第一层的第一个元素的结果，迭代第一层剩余的元素
                              (if fs__1059__auto__ 
                                (concat fs__1059__auto__ (iter__6234 (rest s__6235))) 
                                (recur (rest s__6235))))))
                        ))))] 
    (iter__1062__auto__ av))

