(ns reitit.pretty
  (:refer-clojure :exclude [format])
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [arrangement.core]
    ;; expound
            [expound.ansi]
            [expound.alpha]
    ;; fipp
            [fipp.visit]
            [fipp.edn]
            [fipp.ednize]
            [fipp.engine]))

;;
;; colors
;;

(def colors
  {:white 255
   :text 253
   :grey 245
   :title-dark 32
   :title 45

   :string 180
   :comment 243
   :doc 223
   :core-form 39
   :function-name 178
   :variable-name 85
   :constant 149
   :type 123
   :foreign 220
   :builtin 167
   :half-contrast 243
   :half-contrast-inverse 243
   :eldoc-varname 178
   :eldoc-separator 243
   :arglists 243
   :anchor 39
   :light-anchor 39
   :apropos-highlight 45
   :apropos-namespace 243
   :error 196})

(defn -color [color & text]
  (str "\033[38;5;" (colors color color) "m" (apply str text) "\u001B[0m"))

(comment
  (doseq [c (range 0 255)]
    (println (-color c "kikka") "->" c))

  (doseq [[n c] colors]
    (println (-color c "kikka") "->" c n)))

(defn -start [x] (str "\033[38;5;" x "m"))
(defn -end [] "\u001B[0m")

(defn color [color & text]
  [:span
   [:pass (-start (colors color))]
   (apply str text)
   [:pass (-end)]])

;;
;; EDN
;;

(defrecord EdnPrinter [symbols print-meta print-length print-level]

  fipp.visit/IVisitor

  (visit-unknown [this x]
    (fipp.visit/visit this (fipp.ednize/edn x)))

  (visit-nil [this]
    (color :text "nil"))

  (visit-boolean [this x]
    (color :text (str x)))

  (visit-string [this x]
    (color :string (pr-str x)))

  (visit-character [this x]
    (color :text (pr-str x)))

  (visit-symbol [this x]
    (color :text (str x)))

  (visit-keyword [this x]
    (color :constant (pr-str x)))

  (visit-number [this x]
    (color :text (pr-str x)))

  (visit-seq [this x]
    (if-let [pretty (symbols (first x))]
      (pretty this x)
      (fipp.edn/pretty-coll this (color :text "(") x :line (color :text ")") fipp.visit/visit)))

  (visit-vector [this x]
    (fipp.edn/pretty-coll this (color :text "[") x :line (color :text "]") fipp.visit/visit))

  (visit-map [this x]
    (let [xs (sort-by identity (fn [a b] (arrangement.core/rank (first a) (first b))) x)]
      (fipp.edn/pretty-coll this (color :text "{") xs [:span (color :text ",") :line] (color :text "}")
                            (fn [printer [k v]]
                              [:span (fipp.visit/visit printer k) " " (fipp.visit/visit printer v)]))))

  (visit-set [this x]
    (let [xs (sort-by identity (fn [a b] (arrangement.core/rank a b)) x)]
      (fipp.edn/pretty-coll this "#{" xs :line "}" fipp.visit/visit)))

  (visit-tagged [this {:keys [tag form]}]
    (let [object? (= 'object tag)
          tag-f (if (map? form) (partial color :type) identity)]
      [:group "#" (tag-f (pr-str tag))
       (when (or (and print-meta (meta form)) (not (coll? form)))
         " ")
       (if object?
         [:group "["
          [:align
           (color :type (first form)) :line
           (color :text (second form)) :line
           (fipp.visit/visit this (last form))] "]"]
         (fipp.visit/visit this form))]))

  (visit-meta [this m x]
    (if print-meta
      [:align [:span "^" (fipp.visit/visit this m)] :line (fipp.visit/visit* this x)]
      (fipp.visit/visit* this x)))

  (visit-var [this x]
    [:text (str x)])

  (visit-pattern [this x]
    [:text (pr-str x)])

  (visit-record [this x]
    (fipp.visit/visit this (fipp.ednize/record->tagged x))))

(defn ->printer
  ([]
   (->printer nil))
  ([options]
   (map->EdnPrinter
     (merge
       {:width 80
        :symbols {}
        :print-length *print-length*
        :print-level *print-level*
        :print-meta *print-meta*}
       options))))

(defn pprint
  ([x] (pprint x {}))
  ([x options]
   (let [printer (->printer (dissoc options :margin))
         margin (apply str (take (:margin options 0) (repeat " ")))]
     (binding [*print-meta* false]
       (fipp.engine/pprint-document [:group margin [:group (fipp.visit/visit printer x)]] options)))))

(defn print-doc [doc printer]
  (fipp.engine/pprint-document doc {:width (:width printer)}))

(defn repeat-str [s n]
  (apply str (take n (repeat s))))

(defn source [[target _ file line]]
  (let [ns (-> target name (str/replace #"\..[^.]*$" ""))]
    [(str ns "." file) line]))

(defn title [message [file line] {:keys [width]}]
  (let [source-str (str file " " line)
        between (- width (count message) 8 (count source-str))]
    [:group
     (color :title-dark "-- ")
     (color :title message " ")
     (color :title-dark (repeat-str "-" between) " ")
     (color :title source-str) " "
     (color :title-dark (str "--"))]))

(defn footer [{:keys [width]}]
  (color :title-dark (repeat-str "-" width)))

(defn text [& text]
  (apply color :text text))

(defn edn
  ([x] (edn x {}))
  ([x options]
   (with-out-str (pprint x options))))

(defn exception-str [message source printer]
  (with-out-str
    (print-doc
      [:group
       (title "Router creation failed" source printer)
       [:break] [:break]
       message
       [:break]
       (footer printer)]
      printer)))

(defmulti format-type (fn [type _ _] type))

(defn format [e]
  (let [data (-> e ex-data :data)
        message (format-type (-> e ex-data :type) (ex-message e) data)
        source (->> e Throwable->map :trace
                    (drop-while #(not= (name (first %)) "reitit.core$router"))
                    next first source)]
    (ex-info (exception-str message source (->printer)) (or data {}))))

(defn de-expound-colors [^String s mappings]
  (-> (reduce
        (fn [s [from to]]
          (.replace ^String s (expound.ansi/esc [from]) (-start (colors to))))
        s mappings)
      (.replace (expound.ansi/esc [:none]) (str (expound.ansi/esc [:none]) (-start (colors :text))))))


(defn fippify [s]
  [:align
   (-> s
       (de-expound-colors {:cyan :grey
                           :green :constant})
       (str/split #"\n") (interleave (repeat [:break])))])

(defn indent [x n]
  [:group (repeat-str " " n) [:align x]])

(def expound-printer
  (expound.alpha/custom-printer
    {:theme :figwheel-theme
     :show-valid-values? false
     :print-specs? false}))

;;
;; Formatters
;;

(defmethod format-type :default [_ message data]
  [:group
   (text message)
   [:break] [:break]
   (edn data)])

(defmethod format-type :path-conflicts [_ _ conflicts]
  [:group
   (text "Router contains conflicting route paths:")
   [:break] [:break]
   (into
     [:group]
     (mapv
       (fn [[[path] vals]]
         [:group
          [:span "   " (text path)]
          [:break]
          (into
            [:group]
            (map
              (fn [p] [:span (color :grey "-> " p) [:break]])
              (mapv first vals)))
          [:break]])
       conflicts))
   [:span (text "Either fix the conflicting paths or disable the conflict resolution")
    [:break] (text "by setting a router option: ") [:break] [:break]
    (edn {:conflicts nil} {:margin 3})]
   [:break]
   (color :white "https://cljdoc.org/d/metosin/reitit/CURRENT/doc/basics/route-conflicts")
   [:break]])

(defmethod format-type :name-conflicts [_ _ conflicts]
  [:group
   (text "Router contains conflicting route names:")
   [:break] [:break]
   (into
     [:group]
     (mapv
       (fn [[name vals]]
         [:group
          [:span (text name)]
          [:break]
          (into
            [:group]
            (map
              (fn [p] [:span (color :grey "-> " p) [:break]])
              (mapv first vals)))
          [:break]])
       conflicts))
   (color :white "https://cljdoc.org/d/metosin/reitit/CURRENT/doc/basics/route-conflicts")
   [:break]])

(defmethod format-type :reitit.spec/invalid-route-data [_ _ {:keys [problems]}]
  [:group
   (text "Route data validation failed:")
   [:break] [:break]
   (into
     [:group]
     (map
       (fn [{:keys [data path spec]}]
         [:group
          [:span (color :grey "-- On route -----------------------")]
          [:break]
          [:break]
          (text path)
          [:break]
          [:break]
          (-> (s/explain-data spec data)
              (expound-printer)
              (with-out-str)
              (fippify))
          [:break]])
       problems))
   (color :white "https://cljdoc.org/d/metosin/reitit/CURRENT/doc/basics/route-data-validation")
   [:break]])

;;
;; spike
;;

(defrecord Kikka [a])

(pprint
  {:kikka (->Kikka 1)
   :inst (java.util.Date.)
   :local-date (java.time.LocalDate/now)
   :qualified #:user{:a 1, :b 2}
   :info {:page 1, :seed "a366e3543970d332", :version "1.2", :results 1},
   :results [{:email "laly.charles@example.com",
              :phone "05-76-83-11-69",
              :name {:title "ms", :first "laly", :last "charles"},
              :nat "FR",
              :dob {:date "1951-08-06T10:35:58Z", :age 67},
              :id {:name "INSEE", :value "2NNaN03568987 00"},
              :gender "female",
              :registered {:date "2003-07-26T12:00:53Z", :age 15},
              :cell "06-22-73-80-01",
              :location {:timezone {:description "Brazil, Buenos Aires, Georgetown", :offset "-3:00"},
                         :coordinates {:longitude "153.8848", :latitude "-3.7704"},
                         :city "grenoble",
                         :postcode 34826,
                         :state "puy-de-dôme",
                         :street "2777 rue des chartreux"}}]})
