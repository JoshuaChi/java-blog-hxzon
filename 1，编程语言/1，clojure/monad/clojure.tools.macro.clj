;; clojure.tools.macro.clj
;; 0.1.2 注释by hxzon
;; https://github.com/clojure/tools.macro

;; Macrolet, symbol-macrolet, and related tools

;; Copyright (c) Konrad Hinsen 2011. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns
  ^{:author "Konrad Hinsen"
     :doc "Local macros and symbol macros
           Local macros are defined by a macrolet form. They are usable only
用 macrolet 来定义本地宏。
           inside its body. Symbol macros can be defined globally
           (defsymbolmacro) or locally (symbol-macrolet). A symbol
用 defsymbolmacro 和 symbol-macrolet 来定义”全局符号宏“ 和 ”本地符号宏“ 。
           macro defines a form that replaces a symbol during macro
           expansion. Function arguments and symbols bound in let
           forms are not subject to symbol macro expansion.
符号宏在宏展开时，会被替换成它们所代表的形式。
函数参数，和let的绑定符号，不会被替换。
           Local macros are most useful in the definition of the expansion
           of another macro, they may be used anywhere. Global symbol
因为不是clojure原生支持的，所以符号宏只能在 with-symbol-macros 中使用。
           macros can be used only inside a with-symbol-macros form."}
  clojure.tools.macro
  [:require clojure.string])

; A set of all special forms. Special forms are not macro-expanded, making
; it impossible to shadow them by macro definitions. For most special
; forms, all the arguments are simply macro-expanded, but some forms
; get special treatment.
(def ^{:private true} special-forms
  (into #{} (keys clojure.lang.Compiler/specials)))
; Value in Clojure 1.2 and 1.3:
; #{deftype* new quote & var set! monitor-enter recur . case* clojure.core/import* reify* do fn* throw monitor-exit letfn* finally let* loop* try catch if def}

; The following three vars are constantly redefined using the binding
; form, imitating dynamic scoping.
;
; Local macros.
(def ^{:dynamic true :private true} macro-fns {})
; Local symbol macros.
(def ^{:dynamic true :private true} macro-symbols {})
; Symbols defined inside let forms or function arguments.
(def ^{:dynamic true :private true} protected-symbols #{})

(defn- protected?
  [symbol]
  ;; 符号是否是受保护的，即它是点号开头或结尾，或是let中的绑定符号，或是函数的参数
  "Return true if symbol is a reserved symbol (starting or ending with a dot)
   or a symbol bound in a surrounding let form or as a function argument."
  (or (contains? protected-symbols symbol)
      (let [s (str symbol)]
        (or (= "." (subs s 0 1))
            (= "." (subs s (dec (count s))))))))

(defn- expand-symbol
  ;; 展开符号，返回该符号对应的形式
  "Expand symbol macros"
  [symbol]
  (cond (protected? symbol)                   symbol    ;; 如果是受保护符号，原样返回
        (contains? macro-symbols symbol)     (get macro-symbols symbol)    ;; 取出该符号所对应的形式
        :else (let [v (try (resolve symbol)
                           (catch java.lang.ClassNotFoundException e nil))
                    m (meta v)]
                (if (:symbol-macro m)
                  (var-get v)
                  symbol))))

(defn- expand-1
  "Perform a single non-recursive macro expansion of form."
  [form]
  (cond
    (seq? form)
      (let [f (first form)]        ;; 如果是列表，取出第一个元素作为判断
        (cond (contains? special-forms f)   form        ;; 如果是特殊形式，原样返回
              (and (not (protected? f))
                   (contains? macro-fns f)) 
              (apply (get macro-fns f) (rest form))    ;; 如果不是受保护的，且是macro-fns，调用该macro-fn  （hxzon注意）
              (symbol? f)  (cond
                            (protected? f)  form    ;; 受保护的符号，原样返回
                            ; macroexpand-1 fails if f names a class
                            (class? (ns-resolve *ns* f)) form        ;; 如果是类名，原样返回
                            :else    (let [exp (expand-symbol f)]
                                       (if (= exp f)
                                         (clojure.core/macroexpand-1 form)
                                         (cons exp (rest form)))))        ;; 对第一个元素展开
              ; handle defmacro macros and Java method special forms
              :else (clojure.core/macroexpand-1 form)))
    (symbol? form)    ;; 如果是符号，展开该符号
      (expand-symbol form)
     :else
       form))

(defn- expand
  "Perform repeated non-recursive macro expansion of form, until it no
   longer changes."
  [form]
  (let [ex (expand-1 form)]
    (if (identical? ex form)
      form
      (recur ex))))

(declare expand-all)

;; 展开形式的参数，即“第n位以后的元素”
(defn- expand-args
  "Recursively expand the arguments of form, leaving its first
   n elements unchanged."
  ([form]
   (expand-args form 1))
  ([form n]
   (doall (concat (take n form) (map expand-all (drop n form))))))

;; 展开本地绑定表达式
(defn- expand-bindings
  [bindings exprs]
  (if (empty? bindings)
    (list (doall (map expand-all exprs)))    ;;  如果没有绑定列表，对body进行展开
    (let [[[s b] & bindings] bindings]        ;; 取出绑定列表的第1对绑定
      (let [b (expand-all b)]    ;; 对绑定列表的”初始值表达式“进行展开
        (binding [protected-symbols (conj protected-symbols s)]        ;; 绑定符号加入到受保护列表中
          (doall (cons [s b] (expand-bindings bindings exprs))))))))     ;; 已完成第1对绑定的展开，递归处理绑定向量的下一对

(defn- expand-with-bindings
  "Handle let*, letfn* and loop* forms. The symbols defined in them are
   protected from symbol macro expansion, the definitions and the body
   expressions are expanded recursively."
  [form]
  (let [f        (first form)        ;; 操作符
        bindings (partition 2 (second form))    ;; 绑定列表
        exprs    (rest (rest form))        ;; body
        expanded (expand-bindings bindings exprs)        ;; 展开绑定列表和body
        bindings (vec (apply concat (butlast expanded)))
        exprs    (last expanded)]
    (cons f (cons bindings exprs))))

;; 展开函数的身体（参数列表不处理）
(defn- expand-fn-body
  [[args & exprs]]
  (binding [protected-symbols (reduce conj protected-symbols
                                     (filter #(not (= % '&)) args))]
    (cons args (doall (map expand-all exprs)))))

;; 对函数进行展开（即 fn*）
(defn- expand-fn
  "Handle fn* forms. The arguments are protected from symbol macro
   expansion, the bodies are expanded recursively."
  [form]
  (let [[f & bodies] form
        name         (when (symbol? (first bodies)) (first bodies))
        bodies       (if (symbol? (first bodies)) (rest bodies) bodies)
        bodies       (if (vector? (first bodies)) (list bodies) bodies)    ;; 如果没有重载，转成重载形式
        bodies       (doall (map expand-fn-body bodies))]
    (if (nil? name)
      (cons f bodies)
      (cons f (cons name bodies)))))

(defn- expand-deftype
  "Handle deftype* forms."
  [[symbol typename classname fields implements interfaces & methods]]
  (assert (= implements :implements))
  (let [expanded-methods (doall (map #(expand-args % 2) methods))]
    (concat
     (list symbol typename classname fields implements interfaces)
     expanded-methods)))

(defn- expand-reify
  "Handle reify* forms."
  [[symbol interfaces & methods]]
  (let [expanded-methods (map #(expand-args % 2) methods)]
    (cons symbol (cons interfaces expanded-methods))))

;; 不同的特殊形式，有不同的展开行为
; Handlers for special forms that require special treatment. The default
; is expand-args.
(def ^{:private true} special-form-handlers
  {'quote         identity
   'var           identity
   'def           #(expand-args % 2)
   'new           #(expand-args % 2)
   'let*          expand-with-bindings
   'letfn*        expand-with-bindings
   'loop*         expand-with-bindings
   'fn*           expand-fn
   'deftype*      expand-deftype
   'reify*        expand-reify})

(defn- expand-list
  "Recursively expand a form that is a list or a cons."
  [form]
  (let [f (first form)]
    (if (symbol? f)
      (if (contains? special-forms f)
        ((get special-form-handlers f expand-args) form)
        (expand-args form))
      (doall (map expand-all form)))))

(defn- expand-all
  "Expand a form recursively."
  [form]
  (let [exp (expand form)]
    (cond (symbol? exp) exp
          (seq? exp) (expand-list exp)
          (vector? exp) (into [] (map expand-all exp))
          (map? exp) (into {} (map expand-all (seq exp)))
          :else exp)))

;; 检查本地宏的符号，不允许带有命名空间限定
(defn- check-not-qualified
  "Verify that none of the supplied symbols are namespace-qualified"
  [symbols]
  (when (not-every? nil? (map namespace symbols))
    (throw (Exception.
            (str "Can't macrolet qualified symbol(s): "
                 (clojure.string/join ", "
                                      (map str (filter namespace symbols)))))))
  symbols)

;; =============
;; 定义本地宏
(defmacro macrolet
  "Define local macros that are used in the expansion of exprs. The
   syntax is the same as for letfn forms."
  [fn-bindings & exprs]
  ;; fn-bindings ”本地宏“绑定列表
  (let [names      (check-not-qualified (map first fn-bindings))
        name-map   (into {} (map (fn [n] [(list 'quote n) n]) names))
        macro-map  (eval `(letfn ~fn-bindings ~name-map))]
    (binding [macro-fns     (merge macro-fns macro-map)
              macro-symbols (apply dissoc macro-symbols names)]
      `(do ~@(doall (map expand-all exprs))))))

;; 定义本地符号宏
(defmacro symbol-macrolet
  "Define local symbol macros that are used in the expansion of exprs.
   The syntax is the same as for let forms."
  [symbol-bindings & exprs]
  (let [symbol-map (into {} (map vec (partition 2 symbol-bindings)))
        names      (check-not-qualified (keys symbol-map))]
    (binding [macro-fns     (apply dissoc macro-fns names)
              macro-symbols (merge macro-symbols symbol-map)]
      `(do ~@(doall (map expand-all exprs))))))

;; 定义符号宏
(defmacro defsymbolmacro
  "Define a symbol macro. Because symbol macros are not part of
   Clojure's built-in macro expansion system, they can be used only
   inside a with-symbol-macros form."
  [symbol expansion]
  (let [meta-map (if (meta symbol) (meta symbol) {})
        meta-map (assoc meta-map :symbol-macro true)]        ;; 在元数据里标明是”符号宏“
  `(def ~(with-meta symbol meta-map) (quote ~expansion))))          ;; 符号宏，对应到第二个形式

;; 因为符号宏不是clojure原生支持，所以符号宏必须在with-symbol-macros 中使用
(defmacro with-symbol-macros
  "Fully expand exprs, including symbol macros."
  [& exprs]
  `(do ~@(doall (map expand-all exprs))))

;; ============
(defmacro deftemplate
  "Define a macro that expands into forms after replacing the
   symbols in params (a vector) by the corresponding parameters
   given in the macro call."
  [name params & forms]
  (let [param-map (for [p params] (list (list 'quote p) (gensym)))
        template-params (vec (map second param-map))
        param-map (vec (apply concat param-map))
        expansion (list 'list (list 'quote `symbol-macrolet) param-map
                        (list 'quote (cons 'do forms)))]
    `(defmacro ~name ~template-params ~expansion)))

(defn mexpand-1
  "Like clojure.core/macroexpand-1, but takes into account symbol macros."
  [form]
  (binding [macro-fns {}
            macro-symbols {}
            protected-symbols #{}]
    (expand-1 form)))

(defn mexpand
  "Like clojure.core/macroexpand, but takes into account symbol macros."
  [form]
  (binding [macro-fns {}
            macro-symbols {}
            protected-symbols #{}]
    (expand form)))

(defn mexpand-all
  "Perform a full recursive macro expansion of a form."
  [form]
  (binding [macro-fns {}
            macro-symbols {}
            protected-symbols #{}]
    (expand-all form)))

(defn name-with-attributes
  "To be used in macro definitions.
   Handles optional docstrings and attribute maps for a name to be defined
   in a list of macro arguments. If the first macro argument is a string,
如果参数列表第一个元素是字符串，视为该名字的文档字符串。
   it is added as a docstring to name and removed from the macro argument
   list. If afterwards the first macro argument is a map, its entries are
   added to the name's metadata map and the map is removed from the
如果参数列表接下来的元素是一个映射，视为该名字的元数据。
   macro argument list. The return value is a vector containing the name
   with its extended metadata map and the list of unprocessed macro
返回该名字，以及剩下的参数。
   arguments."
  [name macro-args]
  (let [[docstring macro-args] (if (string? (first macro-args))
                                 [(first macro-args) (next macro-args)]
                                 [nil macro-args])
    [attr macro-args]          (if (map? (first macro-args))
                                 [(first macro-args) (next macro-args)]
                                 [{} macro-args])
    attr                       (if docstring
                                 (assoc attr :doc docstring)
                                 attr)
    attr                       (if (meta name)
                                 (conj (meta name) attr)
                                 attr)]
    [(with-meta name attr) macro-args]))

