;（hxzon学习笔记）clojure源码-clojure.core.clj

;by hxzon

;https://github.com/clojure/clojure/blob/c6756a8bab137128c8119add29a25b0a88509900/src/clj/clojure/core.clj

;fn* 见 clojure/lang/Compiler.java 。
;let*

;=====
;反引述和编接反引述，只是声明，没有实现。见 SyntaxQuoteReader，UnquoteReader 。

(def unquote)
(def unquote-splicing)


;======
;----
;let：

;during bootstrap we don't have destructuring let, loop or fn, will redefine later
(def
  ^{:macro true
    :added "1.0"}
  let (fn* let [&form &env & decl] (cons 'let* decl)))
; let是一个宏（宏底层也是函数），生成 (let* decl)


;----
;loop：

(def
 ^{:macro true
   :added "1.0"}
 loop (fn* loop [&form &env & decl] (cons 'loop* decl)))

;----
;fn：

(def
 ^{:macro true
   :added "1.0"}
 fn (fn* fn [&form &env & decl] 
         (.withMeta ^clojure.lang.IObj (cons 'fn* decl) 
                    (.meta ^clojure.lang.IMeta &form))))
;将 &form 上的元数据加到 (fn* decl) 上。

;========
;除掉集合的尾部元素。
;=> (butlast [1 2 3])
;(1 2)
;=> (next [1 2])
;(2)
;=> (next [1])
;nil

(def 
 ^{:arglists '([coll])
   :doc "Return a seq of all but the last item in coll, in linear time"
   :added "1.0"
   :static true}
 butlast (fn ^:static butlast [s]
           (loop [ret [] s s]
             (if (next s)   ;如果s还有尾部（即s至少还有两个元素），取出s头部加入ret，否则返回ret（转成序列）。
               (recur (conj ret (first s)) (next s))
               (seq ret)))))

;==========
;定义函数（defn）：

(def 
^{:doc "Same as (def name (fn [params* ] exprs*)) or (def
    name (fn ([params* ] exprs*)+)) with any doc-string or attrs added
    to the var metadata. prepost-map defines a map with optional keys
    :pre and :post that contain collections of pre or post conditions."
   :arglists '([name doc-string? attr-map? [params*] prepost-map? body]
                [name doc-string? attr-map? ([params*] prepost-map? body)+ attr-map?])
   :added "1.0"}

defn (fn defn [&form &env name & fdecl]    ;; name，函数名，必须是一个符号
        ;; Note: Cannot delegate this check to def because of the call to (with-meta name ..)
       (if (instance? clojure.lang.Symbol name)        ;函数名name必须是一个符号
         nil
          (throw (IllegalArgumentException. "First argument to defn must be a symbol")))
        (let [m (if (string? (first fdecl))     ;如果fdecl第一个元素是字符串，视为文档参数，放入m（元数据），并设为:doc元数据。
                 {:doc (first fdecl)}
                  {})
              fdecl (if (string? (first fdecl))     ;除掉fdecl开头的文档参数，如果有。
                     (next fdecl)
                      fdecl)
              m (if (map? (first fdecl))    ;如果fdecl第一个元素是map（元数据），加入到m。
                 (conj m (first fdecl))
                  m)
              fdecl (if (map? (first fdecl))    ;除掉fdecl开头的map（元数据），如果有。
                     (next fdecl)
                      fdecl)
              fdecl (if (vector? (first fdecl)) ;如果fdecl第一个元素是向量（参数列表），将fdecl转成列表（即统一成有重载的形式）。
                     (list fdecl)
                      fdecl)
              m (if (map? (last fdecl))     ;如果fdecl最后一个元素是map（元数据），加入到m。
                 (conj m (last fdecl))
                  m)
              fdecl (if (map? (last fdecl)) ;除掉fdecl最后一个元素，如果最后一个元素是map（元数据）。
                     (butlast fdecl)
                      fdecl)
              m (conj {:arglists (list 'quote (sigs fdecl))} m)     ;添加arglists到m中。

              m (let [inline (:inline m)    ;更新m的inline信息。inline示例见下。
                      ifn (first inline)    ;即“fn”这个关键字。
                      iname (second inline)]    ;第二部分，可能是内联函数名，或者参数列表。
                  ;; same as: (if (and (= 'fn ifn) (not (symbol? iname))) ...)
                  (if (if (clojure.lang.Util/equiv 'fn ifn)    ;; 如果没有指定内联函数名，自动根据函数名生成一个。
                         (if (instance? clojure.lang.Symbol iname) false true))
                    ;; inserts the same fn name to the inline fn if it does not have one
                    (assoc m :inline (cons ifn (cons (clojure.lang.Symbol/intern (.concat (.getName ^clojure.lang.Symbol name) "__inliner"))
                                                     (next inline))))
                    m))

              m (conj (if (meta name) (meta name) {}) m)]   ;将m加入到name（函数名）的元数据中。
;hxzon深入理解：这个宏的返回值：
          (list 'def (with-meta name m)     ;定义Var（指向函数），带有元数据。
                ;;todo - restore propagation of fn name
                ;;must figure out how to convey primitive hints to self calls first
                (cons `fn fdecl) ))))    ;; 到这里，各类元数据，从 fdecl 转移到 name 上了

(. (var defn) (setMacro))   ;将defn标记为宏。（宏的底层实现其实也是函数。）

;inline信息示例：
;{  :inline (fn [x] `(. clojure.lang.Numbers (incP ~x)))
;   :added "1.0"}

(defn nil?
  "Returns true if x is nil, false otherwise."
  {:tag Boolean
   :added "1.0"
   :static true
   :inline (fn [x] (list 'clojure.lang.Util/identical x nil))}
  [x] (clojure.lang.Util/identical x nil))

;======
;定义宏（defmacro）：

(def
 ^{:doc "Like defn, but the resulting function name is declared as a
  macro and will be used as a macro by the compiler when it is
  called."
   :arglists '([name doc-string? attr-map? [params*] body]
                 [name doc-string? attr-map? ([params*] body)+ attr-map?])
   :added "1.0"}

defmacro (fn [&form &env 
                name & args]
             (let [prefix (loop [p (list name) args args]
                            (let [f (first args)]
                              (if (string? f)    ;如果args第一个元素是字符串，视为“文档字符串”
                                (recur (cons f p) (next args));将文档字符串加入到p，并从args中移除
                                (if (map? f)    ;如果args第一个元素是map（元数据），加入到p
                                  (recur (cons f p) (next args))
                                  p))))
                   fdecl (loop [fd args]    ;从args移除开头的“文档字符串”和map（元数据）
                           (if (string? (first fd))
                             (recur (next fd))
                             (if (map? (first fd))
                               (recur (next fd))
                               fd)))
                   fdecl (if (vector? (first fdecl))    ;如果开头是向量（参数向量），将fdecl转成列表，即统一成有重载的形式
                           (list fdecl)
                           fdecl)    ;如果开头不是向量，即有重载

                   add-implicit-args (fn [fd]    ;; 在每个参数向量的开头，加入 &form 和 &evn 这两个隐式参数
                             (let [args (first fd)]
                               (cons (vec (cons '&form (cons '&env args))) (next fd))))

                   add-args (fn [acc ds]    ;; ds = ( {可选的元数据} ([] body) ([] body))
                              (if (nil? ds)
                                acc
                                (let [d (first ds)]
                                  (if (map? d)
                                    (conj acc d)
                                    (recur (conj acc (add-implicit-args d)) (next ds))))))

                   fdecl (seq (add-args [] fdecl))    ;; 给每个重载形式的参数向量，添加两个隐式参数
                   decl (loop [p prefix d fdecl]    ;; 把开头的文档字符串，和元数据，重新加回去
                          (if p
                            (recur (next p) (cons (first p) d))
                            d))]
;hxzon深入理解：
;返回代码： (do  (defn decl_)    (. (var name_) (setMacro))    (var name_)  )
               (list 'do
                     (cons `defn decl)
                     (list '. (list 'var name) '(setMacro))
                     (list 'var name)))))


(. (var defmacro) (setMacro))


;====
;动态绑定：

(defmacro binding
  "binding => var-symbol init-expr

  Creates new bindings for the (already-existing) vars, with the
  supplied initial values, executes the exprs in an implicit do, then
  re-establishes the bindings that existed before.  The new bindings
  are made in parallel (unlike let); all init-exprs are evaluated
  before the vars are bound to their new values."
  {:added "1.0"}

  [bindings & body]
  (assert-args
    (vector? bindings) "a vector for its binding"   ;绑定表达式必须是一个向量。
    (even? (count bindings)) "an even number of forms in binding vector")   ;绑定表达式的向量必须是偶数个元素。

  (let [var-ize (fn [var-vals]
                  (loop [ret [] vvs (seq var-vals)]
                    (if vvs
                      (recur  (conj (conj ret `(var ~(first vvs))) (second vvs))
                             (next (next vvs)))
                      (seq ret))))];; [sym1 v1 sym2 v2] -> [(var sym1) v1 (var sym2) v2]
    `(let []
       (push-thread-bindings (hash-map ~@(var-ize bindings)))
       (try
         ~@body
         (finally
           (pop-thread-bindings))))))


;====
;宏展开：

(defn macroexpand-1
  "If form represents a macro form, returns its expansion,
  else returns form."
  {:added "1.0"
   :static true}

  [form]
    (. clojure.lang.Compiler (macroexpand1 form)))

;==
(defn macroexpand
  "Repeatedly calls macroexpand-1 on form until it no longer
  represents a macro form, then returns it.  Note neither
  macroexpand-1 nor macroexpand expand macros in subforms."
  {:added "1.0"
   :static true}

  [form]
    (let [ex (macroexpand-1 form)]      ;递归，直到macroexpand-1后与macroexpand-1前相同
      (if (identical? ex form)
        form
        (macroexpand ex))))

;=====
;redefine let and loop  with destructuring

;解构：
(defn destructure [bindings]
  (let [bents (partition 2 bindings)    ;将绑定表达式两两切分
        pb (fn pb [bvec b v]
               (let [pvec
                     (fn [bvec b val]
                       (let [gvec (gensym "vec__")]
                         (loop [ret (-> bvec (conj gvec) (conj val))
                                n 0
                                bs b
                                seen-rest? false]
                           (if (seq bs)
                             (let [firstb (first bs)]
                               (cond
                                (= firstb '&) (recur (pb ret (second bs) (list `nthnext gvec n))
                                                     n
                                                     (nnext bs)
                                                     true)
                                (= firstb :as) (pb ret (second bs) gvec)
                                :else (if seen-rest?
                                        (throw (new Exception "Unsupported binding form, only :as can follow & parameter"))
                                        (recur (pb ret firstb  (list `nth gvec n nil))
                                               (inc n)
                                               (next bs)
                                               seen-rest?))))
                             ret))))
                     pmap
                     (fn [bvec b v]
                       (let [gmap (gensym "map__")
                             gmapseq (with-meta gmap {:tag 'clojure.lang.ISeq})
                             defaults (:or b)]
                         (loop [ret (-> bvec (conj gmap) (conj v)
                                        (conj gmap) (conj `(if (seq? ~gmap) (clojure.lang.PersistentHashMap/create (seq ~gmapseq)) ~gmap))
                                        ((fn [ret]
                                           (if (:as b)
                                             (conj ret (:as b) gmap)
                                             ret))))
                                bes (reduce1
                                     (fn [bes entry]
                                       (reduce1 #(assoc %1 %2 ((val entry) %2))
                                               (dissoc bes (key entry))
                                               ((key entry) bes)))
                                     (dissoc b :as :or)
                                     {:keys #(keyword (str %)), :strs str, :syms #(list `quote %)})]
                           (if (seq bes)
                             (let [bb (key (first bes))
                                   bk (val (first bes))
                                   has-default (contains? defaults bb)]
                               (recur (pb ret bb (if has-default
                                                   (list `get gmap bk (defaults bb))
                                                   (list `get gmap bk)))
                                      (next bes)))
                             ret))))]
                 (cond
                  (symbol? b) (-> bvec (conj b) (conj v))
                  (vector? b) (pvec bvec b v)
                  (map? b) (pmap bvec b v)
                  :else (throw (new Exception (str "Unsupported binding form: " b))))))
        process-entry (fn [bvec b] (pb bvec (first b) (second b)))]

    (if (every? symbol? (map first bents))
      bindings
      (reduce1 process-entry [] bents))))


;重新定义let宏：

(defmacro let
  "binding => binding-form init-expr

  Evaluates the exprs in a lexical context in which the symbols in
  the binding-forms are bound to their respective init-exprs or parts
  therein."

  {:added "1.0", :special-form true, :forms '[(let [bindings*] exprs*)]}

  [bindings & body]
  (assert-args
     (vector? bindings) "a vector for its binding"
     (even? (count bindings)) "an even number of forms in binding vector")
  `(let* ~(destructure bindings) ~@body))

; 生成代码 (let* destruBindings body1 body2)

;; ====
(defn ^{:private true}
  maybe-destructured
  [params body]
  (if (every? symbol? params)
    (cons params body)
    (loop [params params
           new-params []
           lets []]
      (if params
        (if (symbol? (first params))
          (recur (next params) (conj new-params (first params)) lets)
          (let [gparam (gensym "p__")]
            (recur (next params) (conj new-params gparam)
                   (-> lets (conj (first params)) (conj gparam)))))
        `(~new-params
          (let ~lets
            ~@body))))))

;重新定义fn宏：

;redefine fn with destructuring and pre/post conditions
(defmacro fn
  "params => positional-params* , or positional-params* & next-param
  positional-param => binding-form
  next-param => binding-form
  name => symbol

  Defines a function"

  {:added "1.0", :special-form true,
   :forms '[(fn name? [params* ] exprs*) (fn name? ([params* ] exprs*)+)]}

  [& sigs]
    (let [name (if (symbol? (first sigs)) (first sigs) nil)  ;第一个元素是否是符号（可选的内部函数名）
          sigs (if name (next sigs) sigs)  ;移除内部函数名，如果有
          sigs (if (vector? (first sigs))  ;第一个元素是否是向量（参数列表） ，如果是，sigs转成列表，如果不是，是否为序列，不是，抛出异常
                     (list sigs) 
                     (if (seq? (first sigs))
                          sigs
                          ;; Assume single arity syntax
                         (throw (IllegalArgumentException. 
                            (if (seq sigs)
                              (str "Parameter declaration " 
                                   (first sigs)
                                   " should be a vector")
                              (str "Parameter declaration missing"))))))
          psig (fn* [sig]
                 ;; Ensure correct type before destructuring sig
                 (when (not (seq? sig))
                   (throw (IllegalArgumentException.
                            (str "Invalid signature " sig
                                 " should be a list"))))
                 (let [[params & body] sig  ;sig解构成参数列表和方法体
                       _ (when (not (vector? params))  ;如果不是向量，抛出异常
                           (throw (IllegalArgumentException. 
                                    (if (seq? (first sigs))
                                      (str "Parameter declaration " params
                                           " should be a vector")
                                      (str "Invalid signature " sig
                                           " should be a list")))))
                       conds (when (and (next body) (map? (first body)))  ;body的第1个元素是否为map（元数据） 
                                           (first body))
                       body (if conds (next body) body)
                       conds (or conds (meta params))
                       pre (:pre conds)
                       post (:post conds)                       
                       body (if post
                              `((let [~'% ~(if (< 1 (count body)) 
                                            `(do ~@body) 
                                            (first body))]
                                 ~@(map (fn* [c] `(assert ~c)) post)
                                 ~'%))
                              body)
                       body (if pre
                              (concat (map (fn* [c] `(assert ~c)) pre) 
                                      body)
                              body)]
                   (maybe-destructured params body)))

          new-sigs (map psig sigs)]  ;通过psig函数，转换sigs的每个元素

      (with-meta  ;给整体加上&form的元数据
        (if name
          (list* 'fn* name new-sigs)
          (cons 'fn* new-sigs))
        (meta &form))))

;; ====
;重新定义loop：

(defmacro loop
  "Evaluates the exprs in a lexical context in which the symbols in
  the binding-forms are bound to their respective init-exprs or parts
  therein. Acts as a recur target."
  {:added "1.0", :special-form true, :forms '[(loop [bindings*] exprs*)]}
  [bindings & body]
    (assert-args
      (vector? bindings) "a vector for its binding"
      (even? (count bindings)) "an even number of forms in binding vector")
    (let [db (destructure bindings)]
      (if (= db bindings)
        `(loop* ~bindings ~@body)
        (let [vs (take-nth 2 (drop 1 bindings))
              bs (take-nth 2 bindings)
              gs (map (fn [b] (if (symbol? b) b (gensym))) bs)
              bfs (reduce1 (fn [ret [b v g]]
                            (if (symbol? b)
                              (conj ret g v)
                              (conj ret g v b g)))
                          [] (map vector bs vs gs))]
          `(let ~bfs
             (loop* ~(vec (interleave gs gs))
               (let ~(vec (interleave bs gs))
                 ~@body)))))))



;====
;定义内联：？

(defmacro definline
  "Experimental - like defmacro, except defines a named function whose
实验特性。类似宏，定义一个函数，它的身体是展开式。
不能使用不定参数。
  body is the expansion, calls to which may be expanded inline as if
  it were a macro. Cannot be used with variadic (&) args."
  {:added "1.0"}
  [name & decl]
  (let [[pre-args [args expr]]    ;; 切分成三部分：形参向量，实参向量，身体
        (split-with (comp not vector?) decl)]
    `(do
       (defn ~name ~@pre-args ~args ~(apply (eval (list `fn args expr)) args))
       (alter-meta! (var ~name) assoc :inline (fn ~name ~args ~expr))
       (var ~name))))



;; (defn bad-sqr [x] (* x x))
user=> (definline bad-sqr [x] `(* ~x ~x))
; #'user/bad-sqr
user=> (bad-sqr (do (println "x") 5))
; x
; x
; 25

;====
(defmacro letfn 
  "fnspec ==> (fname [params*] exprs) or (fname ([params*] exprs)+)

  Takes a vector of function specs and a body, and generates a set of
  bindings of functions to their names. All of the names are available
  in all of the definitions of the functions, as well as the body."

  {:added "1.0", :forms '[(letfn [fnspecs*] exprs*)],
   :special-form true, :url nil}

  [fnspecs & body] 
  `(letfn* ~(vec (interleave (map first fnspecs) 
                             (map #(cons `fn %) fnspecs)))
           ~@body))

;; (letfn [(f1 [p1 p2] f1body) (f2 [p1 p2] f2body)] body)
;; => (letfn* [f1 (fn f1 [p1 p2] f1body) , f2 (fn f2 [p1 p2] f2body)] body)

