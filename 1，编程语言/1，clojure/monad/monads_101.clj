;; monads_101.clj
;; http://www.intensivesystems.net/tutorials/monads_101.html

;; Copyright (c) Jim Duey, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; By using this software in any fashion, you are agreeing to be bound
;; by the terms of this license.  You must not remove this notice,
;; or any other, from this software.

(use 'clojure.contrib.monads)
(import 'java.net.URLDecoder)

;; ==========
(with-monad sequence-m
         ; a monadic function under the sequence-m monad
         (defn f2 [n]
              (list (inc n)))

         (assert (= [2 6 8]
                  
                  (mapcat f2 [1 5 7])
                  
                  (m-bind [1 5 7] f2)))

         (assert (= (m-result 6)
                  [6]))

         (defn m-comp [f1 f2 f3]
              (fn [x]
                 ; the monad name is not needed if the 'domonad' is enclosed in a 'with-monad'
                 (domonad
                  [a (f3 x)
                   b (f2 a)
                   c (f1 b)]
                  c)))

         (assert (= '([a 1] [a 2] [a 3] [b 1] [b 2] [b 3] [c 1] [c 2] [c 3])
                  
                  (domonad 
                   [letters ['a 'b 'c]
                    numbers [1 2 3]]
                   [letters numbers])
                  
                  (for
                   [letters ['a 'b 'c]
                    numbers [1 2 3]]
                   [letters numbers])))
         )

;; =============
(with-monad state-m
         (defn g1 [state-int]
              [:g1 (inc state-int)])

         (defn g2 [state-int]
              [:g2 (inc state-int)])

         (defn g3 [state-int]
              [:g3 (inc state-int)])

         (def gs (domonad 
                 [a g1
                  b g2
                  c g3]
                 [a b c]))

         (assert (= '([:g1 :g2 :g3] 8)
                  (gs 5)))

         (def gs1
             (domonad
               [a g1
               x (fetch-state)
               b g2]
               [a x b]))

         (assert (= '([:g1 4 :g2] 5)
                  (gs1 3)))

         (def gs2
             (domonad state-m
                    [a g1
                     x (set-state 50)
                     b g2]
                    [a x b]))

         (assert (= '([:g1 4 :g2] 51)
                  (gs2 3)))
         )

(assert (= '([:g1 :g2 :g3] 8)
         (gs 5)))
(assert (= '([:g1 4 :g2] 5)
         (gs1 3)))
(assert (= '([:g1 4 :g2] 51)
         (gs2 3)))

;; =============
(defmonad parser-m
        [m-result (fn [x]
					  (fn [strn]
						  (list x strn)))

         m-bind (fn [parser func]
					(fn [strn]
						(let [result (parser strn)]
						  (when (not= nil result)
							((func (first result)) (second result))))))        ;; 给func传入的是解析结果

         m-zero (fn [strn]
					nil)

         m-plus (fn [& parsers]        ;; 输入是一系列解析器，然后依次用这些解析器解析字符串，直到有成功的解析结果
					(fn [strn]        ;; m-plus 返回的是一个解析器
						(first 
						  (drop-while nil?
									  (map #(% strn) parsers)))))])

(with-monad parser-m
         (defn any-char [strn]        ;; 解析器，判断是否为空字符串，非空时取出第一个字符
              (if (= "" strn)
               nil
               (list (first strn) (. strn (substring 1)))))

         (defn char-test [pred]        ;; 返回一个解析器
              (domonad
               [c any-char
                :when (pred c)]
               (str c)))

         (defn is-char [c]
              (char-test (partial = c)))

         ; just renaming is-char to be consistent later
         (def match-char is-char)

         (defn match-string [target-strn]
              (if (= "" target-strn)
               (m-result "")
               (domonad
                 [c (is-char (first target-strn))
                  cs (match-string (. target-strn (substring 1)))]
                 (str c cs))))
;; 解析器组合子，返回一个新的解析器
         (defn optional [parser]
              (m-plus parser (m-result nil)))

         (def match-one m-plus)

         (defn match-all [& parsers]
               (m-fmap (partial apply str)
                       (m-seq parsers))) 

         (def one-or-more)


         (defn none-or-more [parser]
              (optional (one-or-more parser)))


         (defn one-or-more [parser]
              (domonad
               [a parser
                  as (none-or-more parser)]
               (str a as)))
;; 常用解析器
         (defn one-of [target-strn]
              (let [str-chars (into #{} target-strn)]
               (char-test #(contains? str-chars %))))

         (def alpha (one-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
         (def whitespace (one-of " \t\n\r"))
         (def digit (one-of "0123456789"))
         (def hexdigit (one-of "0123456789abcdefghABCDEFGH"))
;; 测试
         (def is-n (is-char \n))

         (assert (= ["n" "bc"]
                  (is-n "nbc")))

         (assert (= nil
                  (is-n "xbc")))

         (def this-string (match-string "this"))

         (def is-space (is-char \space))

         (def test-string (match-string "test"))

         (assert (= ["this" " is a test"]        ;; 匹配出 “this”
                  (this-string "this is a test")))

         (def test-phrase        ;; 依次解析出“this”，空格，“is a”，空格，“test”
             (domonad
               [this-var this-string
               _ is-space
               middle (match-string "is a")
               _ is-space
               test-var test-string]
               (print-str this-var "resembles a" test-var)))

         (assert (= ["this resembles a test" ""]
                  (test-phrase "this is a test")))

         (assert (= nil        ;; "is not"不匹配"is a"，整体返回nil
                  (test-phrase "this is not a test")))
         )

;; ==============
(with-monad parser-m
         (def #^{:private true} method        ;; 解析器，用来匹配 http方法
             (match-one (match-string "PUT")
                     (match-string "HEAD")
                     (match-string "TRACE")
                     (match-string "OPTIONS")
                     (match-string "POST")
                     (match-string "DELETE")
                     (match-string "GET")))

         (def #^{:private true} pct-encoded        ;; 匹配百分号，和两个十六进制字符
             (match-all 
               (match-char \%)
               hexdigit
               hexdigit))

         (def #^{:private true} path-char
             (match-one
               alpha
               digit
               pct-encoded
               (one-of "-._~!$&'()*+,;=:@")))

         (def #^{:private true} path
             (match-one 
               (match-char \*)
               (match-all 
                (match-char \/)
                (none-or-more path-char)
                (none-or-more 
                  (match-all
                   (match-char \/)
                   (none-or-more path-char))))))

         (def #^{:private true} query-char
             (match-one
               alpha
               digit
               pct-encoded
               (one-of "!$'()*+,;:@/?&=")))

         (def #^{:private true} query
             (one-or-more query-char))

         (def #^{:private true} version
             (match-all
               (match-string "HTTP/1.")
               (match-one
                (match-string "0")
                (match-string "1"))))

         (def #^{:private true} token
             (one-or-more
               (match-one
                alpha
                digit
                (one-of "~`!#$%^&*-_+=|'."))))

         (def #^{:private true} crlf
             (match-all
               (optional (match-char \return))       ;; hxzon注意
               (match-char \newline)))

         (def #^{:private true} spaces
             (domonad
               [ws (one-or-more
                   (match-one
                     (match-char \space)
                     (match-char \tab)))]
               " "))

         (def #^{:private true} field-value
             (one-or-more
               (one-of
                (map char (range 32 127)))))

         (def #^{:private true} value-continuation
             (domonad
               [linefeed crlf
               ws spaces
               value field-value]
               (str ws value)))

         (def #^{:private true} header-value
             (match-all
               field-value
               (none-or-more value-continuation)))

         (def #^{:private true} header
             (match-all
               token
               (match-all (match-char \:) (none-or-more spaces))
               (optional header-value)
               crlf))

         (defn- build-map [map-pairs]
               (reduce #(assoc %1
                           (keyword (. (first %2) toLowerCase))
                           (second %2))
                      {}
                      map-pairs))

         (defn- query-map [query-strn]
               (let [query-strn (.substring query-strn 1)
                   queries (seq (.split query-strn "&"))
                   name-vals (map #(seq (.split % "=")) queries)]
                (build-map name-vals)))

         (defn- split-header [header-strn]
               (let [split-index (.indexOf header-strn ":")]
                [(.substring header-strn 0 split-index)
                 (.trim (.substring header-strn (inc split-index)))]))

         (defn- build-header-map [headers-strn]
               (when headers-strn
                (let [headers (seq (.split headers-strn "\r\n"))]
                  (build-map
                   (map split-header headers)))))

         (def http-request
             (domonad
               [method-str method
                _ spaces
               path-str path
               query-str (optional query)
               _ spaces
               version-str version
               _ crlf
               header-str (none-or-more header)
               _ crlf]
               (let [header-map (build-header-map header-str)]
                {:method (keyword (. method-str toLowerCase))
                :path (. URLDecoder (decode path-str))
                :query (when (not= "" query-str)
                       (query-map query-str))
                :version version-str
                :headers header-map
                :host (get header-map :host)})))
         )

; unit tests
(defn parse [parser strn]
     (first (parser strn)))

(assert (= "GET"
         (parse method "GET")))

(assert (= "PUT"
         (parse method "PUT")))

(assert (= "/"
         (parse path "/")))

(assert (= "*"
         (parse path "*")))

(assert (= "/bogus/path"
         (parse path "/bogus/path")))

(assert (= "bogus=value"
         (parse query "bogus=value")))

(assert (= "another"
         (parse query "another")))

(assert (= "bogus=value&another"
         (parse query "bogus=value&another")))

(assert (= "HTTP/1.0"
         (parse version "HTTP/1.0")))

(assert (= "HTTP/1.1"
         (parse version "HTTP/1.1")))

(assert (= nil
         (parse version "HTTP/1.2")))

(assert (= "Host: \r\n"
         (parse header "Host:   \r\n")))

(assert (= "Host: text\r\n"
         (parse header "Host:    text\r\n")))

(assert (= "Host: text more text\r\n"
         (parse header "Host:   text\r\n    more text\r\n")))

(assert (= (str "Host: localhost:8000\r\n"
         "User-Agent: Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.8.1.12) Gecko/20080207 Ubuntu/7.10 (gutsy) Firefox/2.0.0.12\r\n")
         (parse (none-or-more header) "Host: localhost:8000\r
User-Agent: Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.8.1.12)\r
                                        Gecko/20080207 Ubuntu/7.10 (gutsy)\r
                                        Firefox/2.0.0.12\r\n")))

(assert (= nil
         (parse (none-or-more header) "\r\n")))

(def small-http (str
"GET /bogus?q1=a&q2=b HTTP/1.1\r\n"
"First: one\r\n"
"Only:\r\n"
"Host: l\r\n"
"User-Agent: More\r\n"
" Mozilla\r\n"
"Another: one\r\n"
"\r\n"
))

(assert (= {:version "HTTP/1.1"
         :method :get
         :query {:query "99"}
         :path "/bogus/path"
         :headers nil
         :host nil}
         (parse http-request "GET /bogus/path?query=99 HTTP/1.1\r\n\r\n")))

(assert (= {:version "HTTP/1.1"
         :method :get
         :query {:query "99"}
         :path "/"
         :headers nil
         :host nil}
         (parse http-request "GET /?query=99 HTTP/1.1\r\n\r\n")))

(def test-http (str "GET /bogus/path?query=99 HTTP/1.1\r\n"
"Host: localhost:8000\r\n"
"User-Agent: Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.8.1.12) Gecko/20080207 Ubuntu/7.10 (gutsy) Firefox/2.0.0.12\r\n"
"Accept: text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5\r\n"
"Accept-Language: en-us,en;q=0.5\r\n"
"Accept-Encoding: gzip,deflate\r\n"
"Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7\r\n"
"Keep-Alive: 300\r\n"
"Connection: keep-alive\r\n"
"\r\n"
"this is the payload\r\n"
"\r\n"
))

(assert (= {:method :get
         :path "/bogus/path"
         :headers {:user-agent "Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.8.1.12) Gecko/20080207 Ubuntu/7.10 (gutsy) Firefox/2.0.0.12"
                  :keep-alive "300"
                  :accept-charset "ISO-8859-1,utf-8;q=0.7,*;q=0.7"
                  :accept "text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5"
                  :host "localhost:8000"
                  :accept-encoding "gzip,deflate"
                  :accept-language "en-us,en;q=0.5"
                  :connection "keep-alive"}
         :query {:query "99"}
         :version "HTTP/1.1"
         :host "localhost:8000"}
        (parse http-request test-http)))

(def more-http (str
"BOGUS / HTTP/1.1\r\n"
"Host: localhost:8085\r\n"
"User-Agent: Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.9.0.5) Gecko/2008121623 Ubuntu/8.10 (intrepid) Firefox/3.0.5\r\n"
"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n"
"Accept-Language: en-us,en;q=0.5\r\n"
"Accept-Encoding: gzip,deflate\r\n"
"Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7\r\n"
"Keep-Alive: 300\r\n"
"Connection: keep-alive\r\n"
"\r\n"
))

(assert (= nil
         (parse http-request more-http)))


