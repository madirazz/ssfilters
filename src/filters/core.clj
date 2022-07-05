(ns filters.core
  (:require [clojure.string :as st])
  (:gen-class))

;; number values : "equal", "not-equal", "greater", "less", "between"
;; text values: "equal", "not-equal", "contains", "not-contains"
;; date values: "equal", "not-equal", "greater", "less", "between"

(def param [{:field-name "importo" :comparator "not-equal" :input-value "10000" :input-type "number"}
             {:field-name "nome_cliente" :comparator "contains" :input-value "NOV" :input-type "text"}
             {:field-name "data_pagamento" :comparator "between" :input-value "2022-01-01" :max-input-value "2022-01-01" :input-type "date"}])

(def comparator-mapping {"greater" "> ?"
                         "less" "< ?"
                         "equal" "= ?"
                         "not-equal" "!= ?"
                         "between" "BETWEEN ? AND ?"
                         "contains" "LIKE %?%"
                         "not-contains" "NOT LIKE %?%"})

(def field-name-to-columns {"importo" "fatture.importo"
                            "fee_best" "bids.fee_richiesta"
                            "numero" "fatture.numero"
                            "nome_cliente" "clienti.ragione_sociale"
                            "nome_buyer" "buyers.nome"
                            "nome_seller" "cedenti.ragione_sociale"
                            "data_pagamento" "fatture.data_pagamento" 
                            "data_scadenza_" "data_scadenza_" 
                            "data_interessi_" "data_interessi_"})

(defn map-to-raw-sql [params]
  (->> (for [{:keys [field-name comparator input-type]} params]
         (->> (condp = input-type
                "number" [(field-name-to-columns field-name)
                          (comparator-mapping comparator)]
                "text" [(field-name-to-columns field-name)
                        (comparator-mapping comparator)]
                "date" [(field-name-to-columns field-name)
                        (comparator-mapping comparator)])
              (st/join " ")))
       (st/join " AND ")))


(map-to-raw-sql param)


(defn flatten [x]
  (if (coll? x)
    (mapcat flatten x)
    [x]))
;;"equal" input-value "not-equal" input-value "greater" input-value "less" input-value

(defn map-to-query-params [params]
  (->> (for [{:keys [comparator input-value max-input-value input-type]} params]
                  (->> (condp = input-type
                         "number" [({"between" [input-value max-input-value] "equal" input-value "not-equal" input-value "greater" input-value "less" input-value} comparator)]
                         "text" [input-value]
                         "date" [({"between" [input-value max-input-value] "equal" input-value "not-equal" input-value "greater" input-value "less" input-value} comparator)])))
       (apply concat)
       (filterv (complement nil?))
       (flatten)
       (into [])
       ))

(map-to-query-params param)
