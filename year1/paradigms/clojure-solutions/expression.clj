(load-file "parser.clj")

; Functional
(defn constant [val] (fn [m] val))
(defn variable [sign] (fn [m] (m sign)))

(defn binary [f] (fn [x y] (fn [m] (f (x m) (y m)))))
(def add (binary +))
(def subtract (binary -))
(def multiply (binary *))
(def divide (binary #(/ (double %1) (double %2))))
(def pow (binary #(Math/pow %1 %2)))
(def log (binary #(/ (Math/log (Math/abs %2)) (Math/log (Math/abs %1)))))

(defn unary [f] (fn [x] (fn [m] (f (x m)))))
(def negate (unary -))

(def binary-func {'+ add '- subtract '* multiply '/ divide 'pow pow 'log log})
(def unary-func {'constant constant 'variable variable 'negate negate})
(defn parseExpression [unary binary]
  (fn [string]
    (letfn [(parse [expr]
              (cond (number? expr) ((unary 'constant) expr)
                    (symbol? expr) ((unary 'variable) (name expr))
                    (not (nil? (unary (first expr)))) ((unary (first expr))
                                                       (parse (second expr)))
                    :else ((binary (first expr))
                           (parse (second expr))
                           (parse (last expr)))))]
      (parse (read-string string)))))
(def parseFunction (parseExpression unary-func binary-func))

; Object
(definterface Expression
  (^Number evaluate [m])
  (^Object diff [variable])
  (^String toStringSuffix []))

(deftype JConstant [value]
  Expression
  (evaluate [this m] value)
  (diff [this variable] (JConstant. 0))
  (toStringSuffix [this] (str value))
  Object
  (toString [this] (str value)))
(defn Constant [value] (JConstant. value))

(deftype JVariable [sign name]
  Expression
  (evaluate [this m] (m sign))
  (diff [this variable] (if (= variable sign) (JConstant. 1) (JConstant. 0)))
  (toStringSuffix [this] name)
  Object
  (toString [this] name))
(defn Variable [name] (JVariable. (clojure.string/lower-case (str (first name))) name))

(deftype BinaryOperation [first second operation operator diff-rule]
  Expression
  (evaluate [this m] (operation (.evaluate first m) (.evaluate second m)))
  (diff [this variable] (diff-rule variable first second))
  (toStringSuffix [this] (str "(" (.toStringSuffix first) " " (.toStringSuffix second) " " operator ")"))
  Object
  (toString [this] (str "(" operator " " (.toString first) " " (.toString second) ")")))
(defn Add [first second] (BinaryOperation. first second + "+" (fn [v x y] (Add (.diff x v) (.diff y v)))))
(defn Subtract [first second] (BinaryOperation. first second - "-" (fn [v x y] (Subtract (.diff x v) (.diff y v)))))
(defn Multiply [first second] (BinaryOperation. first second * "*"
                                                (fn [v x y] (Add (Multiply (.diff x v) y)
                                                                 (Multiply x (.diff y v))))))
(defn Divide [first second] (BinaryOperation. first second (fn [x y] (/ (double x) (double y))) "/"
                                              (fn [v x y] (Divide (Subtract (Multiply (.diff x v) y)
                                                                            (Multiply x (.diff y v)))
                                                                  (Multiply y y)))))
(defn bit-op [op] (fn [x y] (Double/longBitsToDouble (op (Double/doubleToLongBits x) (Double/doubleToLongBits y)))))
(defn BitAnd [first second] (BinaryOperation. first second (bit-op bit-and) "&" nil))
(defn BitOr [first second] (BinaryOperation. first second (bit-op bit-or) "|" nil))
(defn BitXor [first second] (BinaryOperation. first second (bit-op bit-xor) "^" nil))

(deftype UnaryOperation [expression operation operator diff-rule]
  Expression
  (evaluate [this m] (operation (.evaluate expression m)))
  (diff [this variable] (diff-rule variable expression))
  (toStringSuffix [this] (str "(" (.toStringSuffix expression) " " operator ")"))
  Object
  (toString [this] (str "(" operator " " (.toString expression) ")")))
(defn Negate [expression] (UnaryOperation. expression - "negate" (fn [v x] (Negate (.diff x v)))))
(defn Ln [first] (UnaryOperation. first (fn [x] (Math/log (Math/abs x))) "ln" (fn [v x] (Divide (.diff x v) x))))
(defn Log [first second] (BinaryOperation. first second (fn [x y] (/ (Math/log (Math/abs y)) (Math/log (Math/abs x)))) "log"
                                           (fn [v x y] (.diff (Divide (Ln y) (Ln x)) v))))
(defn Pow [first second] (BinaryOperation. first second (fn [x y] (Math/pow x y)) "pow"
                                           (fn [v x y] (Add (Multiply (Multiply y (Pow x (Subtract y (Constant 1))))
                                                                      (.diff x v))
                                                            (Multiply (Multiply (Pow x y)
                                                                                (.diff y v))
                                                                      (Log (Constant Math/E) x))))))
(defn evaluate [expr m] (.evaluate expr m))
(defn toString [expr] (.toString expr))
(defn diff [expr v] (.diff expr v))

(def binary-obj {'+ Add '- Subtract '* Multiply '/ Divide 'pow Pow 'log Log})
(def unary-obj {'constant Constant 'variable Variable 'negate Negate})
(def parseObject (parseExpression unary-obj binary-obj))

;Parsing
(defn toStringSuffix [expr] (.toStringSuffix expr))
(def *digit (+char "0123456789"))
(def *letter (_char #(Character/isLetter %)))
(def *space (+char " \t\n\r"))
(def *ws (+ignore (+star *space)))

(def *number (+str (+seq (+str (+seq (+opt (+char "-+")))) (+str (+plus *digit)) (+str (+opt (+seqf cons (+char ".") (+star *digit)))))))
(def *constant (+map (comp Constant read-string) *number))
(def *sign (+str (+or (+plus (+char "xyzXYZ")) (+seq *letter))))
(def *variable (+map Variable *sign))
(def *operator (+str (+or (+seq (+char "+-*/&|^"))
                          (+seq (+char "n") (+char "e") (+char "g") (+char "a") (+char "t") (+char "e")))))
(def function-str {"+" Add "-" Subtract "*" Multiply "/" Divide "negate" Negate "&" BitAnd "|" BitOr "^" BitXor})
(def *operation (+map function-str *operator))
(defn *parse [] (+or (+seq *ws (+ignore (+char "(")) *ws
                           (+or *constant *variable (delay (*parse))) *ws
                           (+or *constant *variable (delay (*parse))) *ws
                           *operation *ws
                           (+ignore (+char ")")) *ws)
                     (+seq *ws (+ignore (+char "(")) *ws
                           (+or *constant *variable (delay (*parse))) *ws
                           *operation *ws
                           (+ignore (+char ")")) *ws)
                     (+seqn 0 *ws *constant *ws)
                     (+seqn 0 *ws *variable *ws)))

(defn parseObjectSuffix [string]
  (letfn [(f [expr] (cond (and (vector? expr) (== (count expr) 3)) ((last expr) (f (first expr)) (f (second expr)))
                          (and (vector? expr) (== (count expr) 2)) ((last expr) (f (first expr)))
                          :else expr))]
    (f (-value ((*parse) string)))))