(ns silabificador.core
  (:require [instaparse.core :as insta]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(def nl "\n")

(def vocales-abiertas ["a" "e" "o"])
(def vocales-cerradas ["i" "u"])
(def consonantes-simples ["b" "c" "d" "f" "g" "h" "j" "k" "l" "m" "n" "ñ" "p" "q" "r" "s" "t" "v" "w" "x" "y" "z"])
(def consonantes-compuestas ["ch" "ll" "rr" "qu" "gu"])
(def consonantes (concat consonantes-simples consonantes-compuestas))

(defn a-str [vector]
  (str/join " | " (map #(str/join "" (str "'" % "'")) vector)))

(a-str vocales-abiertas)

(def grammar
  (str "palabra = silaba | silaba silaba | silaba silaba silaba" nl
       "silaba = consonante vocal" nl
       "consonante = " (a-str consonantes) nl
       "vocal = vocal-abierta | vocal-cerrada" nl
       "vocal-abierta = " (a-str vocales-abiertas) nl
       "vocal-cerrada = " (a-str vocales-cerradas) nl))


(print grammar)

(def parser (insta/parser grammar :output-format :hiccup))

(defn parse [text]
  (insta/parse parser text))

(defn extraer-cadenas [data]
  (cond
    (vector? data) (mapcat extraer-cadenas data)
    (string? data) [data]
    :else []))

(defn unir-cadenas [datos]
  (str/join "-" (map #(str/join "" %) datos)))

(defn unir-silabas [ast]
  (unir-cadenas (map extraer-cadenas (rest ast))))

(def ast (parse  "casa"))

(unir-silabas ast)

(defn silabificar [palabra]
  (unir-silabas (parse palabra)))

(silabificar "casa")











