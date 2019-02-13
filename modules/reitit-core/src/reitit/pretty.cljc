(ns reitit.pretty
  (:refer-clojure :exclude [format])
  (:require [puget.color :as color]
            [reitit.exception :as exception]
            [puget.printer :as puget]))

(def puget-printer
  (puget/pretty-printer
    {:width (or *print-length* 80)
     :print-color true
     :color-scheme {:delimiter [:red]
                    :tag #_[:red] [:magenta]
                    ;:nil [:cyan]
                    :boolean #_[:cyan] [:bold :cyan]
                    :number [:cyan]
                    :string #_[:cyan] [:yellow]
                    :character [:cyan]
                    :keyword [:green]
                    :symbol nil

                    :function-symbol [:blue]
                    :class-delimiter [:blue]
                    :class-name nil

                    :exception [:red]

                    ::link [:white :bold]
                    ::error [:red]
                    ::other [:yellow]
                    ::title [:cyan]
                    ::body [:white]}}))

(defn format-doc [expr printer]
  (puget/format-doc printer expr))

(defn print-doc [doc printer]
  (fipp.engine/pprint-document doc {:width (:width printer)}))

(defn repeat-str [s n]
  (apply str (take n (repeat s))))

(defn title [message {:keys [width] :as printer}]
  (color/document
    printer
    ::title
    (str "-- " message " " (repeat-str "-" (- width (count message) 4)))))

(defn body [message printer]
  (color/document
    printer
    ::body
    message))

(defn edn-str [printer x]
  (puget/cprint-str x printer))

(defn footer [{:keys [width] :as printer}]
  (color/document
    printer
    ::title
    (repeat-str "-" width)))

(defn ansi [color text]
  (str "\033[38;5;" color "m" text "\u001B[38;5;0m"))

(defn format-str [data printer]
  (with-out-str
    (print-doc
      [:group
       (title "Router creation failed" printer)
       [:break] [:break]
       (body data printer)
       (footer printer)]
      printer)))

(defmulti format-type (fn [type _ _] type))

(defmethod format-type :default [_ message _ _]
  [:group
   message
   [:break] [:break]])

(defn format [e]
  (let [data (-> e ex-data :data)
        message (format-type (-> e ex-data :type) (.getMessage e) data)]
    (ex-info (format-str message puget-printer) data)))

(def colors
  {:white 254
   :light 253
   :grey 245
   :yellow 229
   :red 174})

(defn color [color & text]
  (str "\033[38;5;" (colors color color) "m" (apply str text) "\u001B[0m"))

(comment

  (doseq [[c] colors]
    (println (color c c)))

  (doseq [n (range 255)]
    (println n " -> " (ansi n "kikka"))
    (println "...")))

(def text (partial color :light))
(def grey (partial color :grey))
(def white (partial color :white))

;;
;; Formatters
;;

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
              (fn [p] [:span (grey "-> " p) [:break]])
              (mapv first vals)))
          [:break]])
       conflicts))
   [:span (text "Either fix the conflicting paths or disable the conflict resolution")
    [:break] (text "by setting a router option: ") [:break] [:break]
    "   " (text "{") (color 193 :conflicts) (text " nil}")]
   [:break] [:break]
   (white "https://cljdoc.org/d/metosin/reitit/CURRENT/doc/basics/route-conflicts")
   [:break]
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
              (fn [p] [:span (grey "-> " p) [:break]])
              (mapv first vals)))
          [:break]])
       conflicts))
   (white "https://cljdoc.org/d/metosin/reitit/CURRENT/doc/basics/route-conflicts")
   [:break] [:break]])
