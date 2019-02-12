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

(defn edn-str [x printer]
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

(defmulti format-type (fn [type _ _ _] type))

(defmethod format-type :default [_ message _ _]
  [:group
   message
   [:break] [:break]])

(defn format [e]
  (let [data (-> e ex-data :data)
        message (format-type (-> e ex-data :type) (.getMessage e) data puget-printer)]
    (ex-info (format-str message puget-printer) data)))

;;
;; Formatters
;;

(defmethod format-type ::exception/path-conflicts [_ _ conflicts printer]
  (let [txt #(body % printer)
        edn #(edn-str % printer)
        color #(color/document printer %1 %2)]
    [:group
     (txt "Router contains conflicting route paths:")
     [:break] [:break]
     (into
       [:group]
       (mapv
         (fn [[[path] vals]]
           [:group
            [:span "   " (color :string path)]
            [:break]
            (into
              [:group]
              (map
                #(color ::error [:span "-> " % [:break]])
                (mapv first vals)))
            [:break]])
         conflicts))
     (txt
       [:span "Either fix the conflicting paths or disable the conflict resolution"
        [:break] "by setting a router option: " [:break] [:break] "   "
        (edn {:conflicts nil})])
     [:break]
     [:break]]))

(defmethod format-type ::exception/name-conflicts [_ _ conflicts printer]
  (let [txt #(body % printer)
        edn #(edn-str % printer)
        color #(color/document printer %1 %2)]
    [:group
     (txt "Router contains conflicting route names:")
     [:break] [:break]
     (into
       [:group]
       (mapv
         (fn [[name vals]]
           [:group
            [:span (color :keyword (str name))]
            [:break]
            (into
              [:group]
              (map
                #(color ::error [:span "-> " % [:break]])
                (mapv first vals)))
            [:break]])
         conflicts))
     (txt
       [:span "Fix the conflicting paths."])
     [:break]
     [:break]]))
