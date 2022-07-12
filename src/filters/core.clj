(ns filters.core
  (:require [clojure.string :as st])
  (:gen-class))

;; number values : "equal", "not-equal", "greater", "less", "between"
;; text values: "equal", "not-equal", "contains", "not-contains"
;; date values: "equal", "not-equal", "greater", "less", "between"

(def params [{:field-name "importo" :comparator "equal" :input-value 23204800 :input-type "number"}
             ;;{:field-name "importo" :comparator "not-equal" :input-value 579500 :input-type "number"}
             ;;{:field-name "importo" :comparator "greater" :input-value 579500 :input-type "number"}
             ;;{:field-name "importo" :comparator "less" :input-value 579500 :input-type "number"}
             ;;{:field-name "importo" :comparator "between" :input-value 579500 :max-input-value 1000000 :input-type "number"}
             ;;{:field-name "data_scadenza_" :comparator "equal" :input-value "2022-07-31" :input-type "date"}
             ;;{:field-name "data_interessi_" :comparator "not-equal" :input-value "2022-01-01" :input-type "date"}
             ;;{:field-name "data_pagamento" :comparator "greater" :input-value "2022-01-01" :input-type "date"}
             ;;{:field-name "data_pagamento" :comparator "between" :input-value "2022-01-01" :max-input-value "2022-07-01" :input-type "date"}
             ;;{:field-name "data_scadenza_" :comparator "less" :input-value "2022-07-01" :input-type "date"}
             ;;{:field-name "nome_seller" :comparator "equal" :input-value "GH CATANIA SRL" :input-type "text"}
             ;;{:field-name "nome_buyer" :comparator "not-equal" :input-value "Fasanara Investments II S.A., SICAV-RAIF" :input-type "text"}
             ;;{:field-name "nome_buyer" :comparator "contains" :input-value "Fas" :input-type "text"}
             ;;{:field-name "nome_cliente" :comparator "not-contains" :input-value "NOV" :input-type "text"}
             ])

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
  (if ( #{"equal" "not-equal"} comparator)
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

(map-to-raw-sql params)

