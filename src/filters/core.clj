(ns filters.core
  (:require [clojure.string :as st])
  (:gen-class))

;; number values : "equal", "not-equal", "greater", "less", "between"
;; text values: "equal", "not-equal", "contains", "not-contains"
;; date values: "equal", "not-equal", "greater", "less", "between"


(def param [{:field-name "importo" :comparator "greater" :input-value "10000" :input-type "number"}
             {:field-name "nome_cliente" :comparator "contains" :input-value "NOV" :input-type "text"}
             {:field-name "data_pagamento" :comparator "greater" :input-value "2022-01-01" :input-type "date"}])

(defn map-to-raw-sql [params]
  (->> (for [{:keys [field-name comparator input-type]} params]
         (->> (condp = input-type
                "number" [({"importo" "fatture.importo" "fee_best" "bids.fee_richiesta" "fatture.numero" "numero"}field-name) 
                          ({"greater" "> ?" "less" "< ?" "equal" "= ?" "not-equal" "!= ?" "between" "BETWEEN ? AND ?"} comparator) ]
                "text" [({"nome_cliente" "clienti.ragione_sociale" "buyers.nome" "nome_buyer" "cedenti.ragione_sociale" "nome_seller"}field-name) 
                        ({"equal" "= ?" "not-equal" "!= ?" "contains" "LIKE %?%" "not-contains" "NOT LIKE %?%"} comparator)]
                "date" [({"data_pagamento" "fatture.data_pagamento" "data_scadenza_" "data_scadenza_" "data_interessi_" "data_interessi_"}field-name)
                        ({"equal" "= ?" "not-equal" "!= ?" "greater" "> ?" "less" "< ?" } comparator)])
              (st/join " ")))
       (st/join " AND ")))


(map-to-raw-sql param)

(defn map-to-query-params [params]
  (->> (for [{:keys [input-value max-input-value input-type]} params]
                  (->> (condp = input-type
                         "number" [input-value max-input-value]
                         "text" [input-value max-input-value]
                         "date" [input-value max-input-value])))
       (apply concat)
       (into [] )
       (filterv (complement nil?))))

(map-to-query-params param)