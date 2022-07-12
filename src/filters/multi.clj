(ns filters.multi
  (:require [clojure.string :as st])
  (:gen-class))

;; number values : "equal", "not-equal", "greater", "less", "between"
;; date values: "equal", "not-equal", "greater", "less", "between"
;; text values: "equal", "not-equal", "contains", "not-contains"

;; "importo" "number"
;; "fee_best" "number"
;; "numero" "text"
;; "nome_cliente" "text"
;; "nome_buyer" "text"
;; "nome_seller" "text"
;; "data_pagamento" "date"
;; "data_scadenza_" "date"
;; "data_interessi_" "date"

(def params [
             {:field-name "importo" :comparator "equal" :input-value 23204800 :input-type "number"}
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
      (derive :text :bounded)
      (derive :text :equality)
      (derive :number :bounded)
      (derive :number :equality)
      (derive :date :bounded)
      (derive :date :equality)))

(defmulti to-sql
  (fn [{:keys [input-type comparator]}]
    (mapv keyword [input-type comparator]))
  :hierarchy #'sql-hierarchy)

(defmethod to-sql [:equality :equal]
  [{:keys [field-name input-value input-type]}]
  {:q (format "%s = %s"
              (field-name-to-columns field-name)
              (if (= input-type "number") input-value (str "'" input-value "'")))})

(defmethod to-sql [:equality :not-equal]
  [{:keys [field-name input-value input-type]}]
  {:q (format "%s != %s"
              (field-name-to-columns field-name)
              (if (= input-type "number") input-value (str "'" input-value "'")))})

(defmethod to-sql [:text :contains]
  [{:keys [field-name input-value]}]
  {:q (format "%s LIKE '%%%s%%'" (field-name-to-columns field-name) input-value)})

(defmethod to-sql [:text :not-contains]
  [{:keys [field-name input-value]}]
  {:q (format "%s NOT LIKE '%%%s%%'" (field-name-to-columns field-name) input-value)})

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
  (clojure.string/join " and " (map :q data)))


