;（hxzon学习笔记）clojure源码-clojure.core4-for.clj


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

