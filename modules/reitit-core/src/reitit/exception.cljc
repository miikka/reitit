(ns reitit.exception
  (:refer-clojure :exclude [format])
  (:require [clojure.string :as str]))

(defn fail!
  ([type]
   (fail! type nil))
  ([type data]
   (throw (ex-info (str type) {:type type, :data data}))))

(defmulti format-type (fn [type _ _] type))

(defn format [e]
  (let [data (ex-data e)
        message (format-type (:type data) (.getMessage e) (:data data))]
    (ex-info message data)))

;;
;; Formatters
;;

(defmethod format-type :default [_ message data]
  (str message "\n\n" (pr-str data)))

(defmethod format-type :path-conflicts [_ _ conflicts]
  (apply str "Router contains conflicting route paths:\n\n"
         (mapv
           (fn [[[path] vals]]
             (str "   " path "\n-> " (str/join "\n-> " (mapv first vals)) "\n\n"))
           conflicts)))

(defmethod format-type :name-conflicts [_ _ conflicts]
  (apply str "Router contains conflicting route names:\n\n"
         (mapv
           (fn [[name vals]]
             (str name "\n-> " (str/join "\n-> " (mapv first vals)) "\n"))
           conflicts)))


