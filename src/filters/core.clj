(ns filters.core
  (:require [clojure.string :as st])
  (:gen-class))

;; number values : "equal", "not-equal", "greater", "less", "between"
;; text values: "equal", "not-equal", "contains", "not-contains"
;; date values: "equal", "not-equal", "greater", "less", "between"

(def param [{:field-name "importo" :comparator "greater" :input-value "10000" :input-type "number"}
            {:field-name "fee_best" :comparator "greater" :input-value 70 :input-type "number"}
            {:field-name "data_scadenza_" :comparator "less" :input-value "2022-01-01" :input-type "date"}
            {:field-name "nome_seller" :comparator "not-contains" :input-value "NOV" :input-type "text"}
            {:field-name "nome_cliente" :comparator "not-equal" :input-value "NOV" :input-type "text"}
            {:field-name "data_pagamento" :comparator "less" :input-value "2024-01-01" :input-type "date"}])

(def comparator-mapping {"greater" ">"
                         "less" "<"
                         "equal" "= "
                         "not-equal" "!= "
                         "between" "BETWEEN "
                         "contains" "LIKE "
                         "not-contains" "NOT LIKE"})

(def field-name-to-columns {"importo" "fatture.importo"
                            "fee_best" "bids.fee_richiesta"
                            "numero" "fatture.numero"
                            "nome_cliente" "clienti.ragione_sociale"
                            "nome_buyer" "buyers.nome"
                            "nome_seller" "cedenti.ragione_sociale"
                            "data_pagamento" "fatture.data_pagamento"
                            "data_scadenza_" "fatture.data_scadenza"
                            "data_interessi_" "operazioni.data"})

(defn number-values [comparator input-value max-input-value] 
  (if (= "between" comparator)
    (str input-value " AND " max-input-value)
    input-value))
                                            
(defn text-values [comparator input-value]
  (if (= #{"equal" "not-equal"} comparator)
       (str "'" input-value "'")
       (str "'%" input-value "%'")))
       
       
(defn date-values [comparator input-value max-input-value]
  (if (= "between" comparator)
    (str "'" input-value "'" " AND " "'" max-input-value "'")
    (str "'" input-value "'")))

(defn map-to-raw-sql [params]
  (->> (for [{:keys [field-name comparator input-value max-input-value input-type]} params]
         (->> (condp = input-type
                "number" [(field-name-to-columns field-name)
                          (comparator-mapping comparator) (number-values comparator input-value max-input-value)] 
                "text" [(field-name-to-columns field-name)
                        (comparator-mapping comparator)(text-values comparator input-value)]
                "date" [(field-name-to-columns field-name)
                        (comparator-mapping comparator) (date-values comparator input-value max-input-value)])
              (st/join " ")))
       (st/join " AND ")))

(map-to-raw-sql param)

