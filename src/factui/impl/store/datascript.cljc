(ns factui.impl.store.datascript
  (:require [datascript.core :as d]
            [datascript.db :as db]
            [factui.impl.store :as st]
            [factui.facts :as f]))

(defn convert-schema
  [schema]
  (letfn [(select-compat [[k {:db/keys [valueType unique cardinality isComponent]}]]
            [k (cond-> {}
                 unique                               (assoc :db/unique unique)
                 (some? isComponent)                  (assoc :db/isComponent isComponent)
                 (= :db.cardinality/many cardinality) (assoc :db/cardinality cardinality)
                 (= :db.type/ref valueType)           (assoc :db/valueType valueType))])]
    (->> (zipmap (map :db/ident schema) schema)
         (map select-compat)
         (filter (fn [[k v]] (not-empty v)))
         (into {}))))

(defn- dsdatom->datom
  [dsdatom]
  (f/->Datom (:e dsdatom)
             (:a dsdatom)
             (:v dsdatom)))

(defn- datom->dsdatom
  [datom add?]
  (db/datom (:e datom)
            (:a datom)
            (:v datom)
            db/tx0
            add?))

(defrecord DatascriptStore [db]
  st/Store
  (resolve [store txdata]
    (let [result   (d/with db txdata)
          bindings (:tempids result)
          datoms   (-> result :tx-data)

          {insertions true, retractions false}
          (group-by db/datom-added datoms)
          insert-datoms  (map dsdatom->datom insertions)
          retract-datoms (map dsdatom->datom retractions)]
      [insert-datoms retract-datoms bindings]))

  (update [store insert-datoms retract-datoms]
    (let [add-datoms (map #(datom->dsdatom % true) insert-datoms)
          ret-datoms (map #(datom->dsdatom % false) retract-datoms)
          all-datoms (concat add-datoms ret-datoms)
          new-db (d/db-with db all-datoms)]
      (assoc store :db new-db)))

  (schema [store] (:schema db)))

(defn store
  [schema]
  (let [ds-schema (convert-schema schema)]
    (->DatascriptStore (d/empty-db ds-schema))))

(defn datoms
  [store]
  (let [dsdatoms (d/datoms (:db store) :eavt)]
    (map dsdatom->datom dsdatoms)))

(comment

  (def sample-schema
    [{:db/ident       :task/title
      :db/unique      :db.unique/identity
      :db/valueType   :db.type/string
      :db/cardinality :db.cardinality/one}
     {:db/ident       :task/completed
      :db/valueType   :db.type/boolean
      :db/cardinality :db.cardinality/one}])

  (convert-schema sample-schema)

  (store sample-schema)

  (st/schema (store sample-schema))

  (let [db     (d/empty-db)
        res    (d/with db [[:db/add -1 :test/item 10]])
        new-db (:db-after res)
        ret    (d/with new-db [[:db/retract 1 :test/item 10]])]
    (-> ret
        ;; :tx-data
        ;; first
        ;; db/datom-added
        ))

  (let [ds-store (store sample-schema)
        txdata [{:task/title "Task 1"}
                {:task/title "Task 2"}]
        [ins-datoms ret-datoms _] (st/resolve ds-store txdata)
        next-store (st/update ds-store ins-datoms ret-datoms)

        txdata [[:db/retractEntity [:task/title "Task 1"]]]
        [ins-datoms ret-datoms _] (st/resolve next-store txdata)
        nnext-store (st/update next-store ins-datoms ret-datoms)]
    (d/pull (:db nnext-store) '[*] [:task/title "Task 2"])
    #_(datoms nnext-store))

  )
