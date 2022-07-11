(ns filters.multi
  (:require [clojure.string :as st])
  (:gen-class))

;; number values : "equal", "not-equal", "greater", "less", "between"
;; date values: "equal", "not-equal", "greater", "less", "between"
;; text values: "equal", "not-equal", "contains", "not-contains"

(def params [{:field-name "importo" :comparator "between" :input-value 20 :max-input-value 100 :input-type "number"}
             {:field-name "data_pagamento" :comparator "between" :input-value "2024-01-01" :max-input-value "2024-01-01" :input-type "date"}
            {:field-name "fee_best" :comparator "greater" :input-value 70 :input-type "number"}
            {:field-name "data_scadenza_" :comparator "less" :input-value "2022-01-01" :input-type "date"}
            ;;{:field-name "nome_seller" :comparator "not-contains" :input-value "NOV" :input-type "text"}
            ;;{:field-name "nome_cliente" :comparator "not-equal" :input-value "NOV" :input-type "text"}
            {:field-name "data_pagamento" :comparator "greater" :input-value "2024-01-01" :input-type "date"}
             ])

(def field-name-to-columns {"importo" "fatture.importo"
                            "fee_best" "bids.fee_richiesta"
                            "numero" "fatture.numero"
                            "nome_cliente" "clienti.ragione_sociale"
                            "nome_buyer" "buyers.nome"
                            "nome_seller" "cedenti.ragione_sociale"
                            "data_pagamento" "fatture.data_pagamento"
                            "data_scadenza_" "fatture.data_scadenza"
                            "data_interessi_" "operazioni.data"})

(def sql-hierarchy
  (-> (make-hierarchy)
      (derive :bounded :any)
      (derive :equality :any)
      (derive :text :any)
      (derive :number :bounded)
      (derive :date :bounded)))

(defmulti to-sql
  (fn [{:keys [input-type comparator]}]
    (mapv keyword [input-type comparator]))
  :hierarchy #'sql-hierarchy)

(defmethod to-sql [:text :include]
  [{:keys [field-name input-value]}]
  {:q (format "%s like '%%%s%%'" field-name input-value)})

(defmethod to-sql [:bounded :less]
  [{:keys [field-name input-value input-type]}]
  {:q (format "%s < %s"
              (field-name-to-columns field-name)
              (if (= input-type "number") input-value (str "'" input-value "'")))})

(defmethod to-sql [:bounded :greater]
  [{:keys [field-name input-value input-type]}]
 {:q (format "%s > %s"
             (field-name-to-columns field-name)
             (if (= input-type "number") input-value (str "'" input-value "'")))})

(defmethod to-sql [:bounded :between]
  [{:keys [field-name input-value input-type max-input-value]}]
  {:q (format "%s between %s and %s"
              (field-name-to-columns field-name)
              (if (= input-type "number") input-value (str "'" input-value "'"))
              (if (= input-type "number") max-input-value (str "'" input-value "'")))})

(let [data (map to-sql params)]
  {:query (clojure.string/join " and " (map :q data))})


