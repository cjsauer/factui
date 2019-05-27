(ns factui.impl.store.datascript
  (:require [datascript.core :as d]
            [datascript.db :as db]
            #?(:cljs [factui.impl.store :as fstore :refer [Store]]
               :clj [factui.impl.store :as fstore])
            [factui.facts :as f])
  #?(:clj (:import [factui.impl.store Store])))

(defn- attrs
  "From a datomic-style schema, builds a map from attr -> attr-schema"
  [schema]
  (zipmap (map :db/ident schema) schema))

(defn- convert-to-datascript-schema
  "Converts a datomic-style schema into a datascript schema"
  [schema]
  (letfn [(select-compat [[k {:db/keys [valueType unique cardinality isComponent]}]]
            [k (cond-> {}
                 unique                               (assoc :db/unique unique)
                 (some? isComponent)                  (assoc :db/isComponent isComponent)
                 (= :db.cardinality/many cardinality) (assoc :db/cardinality cardinality)
                 (= :db.type/ref valueType)           (assoc :db/valueType valueType))])]
    (->> (attrs schema)
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

(defrecord DatascriptStore [attrs db]
  Store
  (resolve [store txdata]
    (let [result   (d/with db txdata)
          bindings (:tempids result)
          datoms   (:tx-data result)

          {insertions true, retractions false} (group-by db/datom-added datoms)
          insert-datoms  (map dsdatom->datom insertions)
          retract-datoms (map dsdatom->datom retractions)]
      [insert-datoms retract-datoms bindings]))

  (update [store insert-datoms retract-datoms]
    (let [add-datoms (map #(datom->dsdatom % true) insert-datoms)
          ret-datoms (map #(datom->dsdatom % false) retract-datoms)
          all-datoms (concat add-datoms ret-datoms)
          new-db (d/db-with db all-datoms)]
      (assoc store :db new-db)))

  (schema [store] attrs)
  (datoms [store]
    (let [dsdatoms (d/datoms (:db store) :eavt)]
      (map dsdatom->datom dsdatoms))) )

(defn store
  [schema]
  (let [full-schema (concat fstore/base-schema schema)
        attrs       (attrs full-schema)
        ds-schema   (convert-to-datascript-schema full-schema)]
    (->DatascriptStore attrs (d/empty-db ds-schema))))

(comment

  (def sample-schema
    [{:db/ident       :task/title
      :db/unique      :db.unique/identity
      :db/valueType   :db.type/string
      :db/cardinality :db.cardinality/one}
     {:db/ident       :task/completed
      :db/valueType   :db.type/boolean
      :db/cardinality :db.cardinality/one}

     {:db/ident       :task/selected?
      :db/valueType   :db.type/boolean
      :db/cardinality :db.cardinality/one
      :factui/isTransient true}])

  (attrs sample-schema)

  (convert-to-datascript-schema sample-schema)

  (fstore/transient? (store sample-schema) :task/title)

  (fstore/schema (store sample-schema))
  (fstore/datoms (store sample-schema))

  (:db (store sample-schema))

  (let [db     (d/empty-db)
        res    (d/with db [[:db/add -1 :test/item 10]])
        new-db (:db-after res)
        ret    (d/with new-db [[:db/retract 1 :test/item 10]])]
    (-> ret
        ;; :tx-data
        ;; first
        ;; db/datom-added
        ))

  (let [ds-store                  (store sample-schema)
        txdata                    [{:task/title "Task 1"}
                                   {:task/title "Task 2"}]
        [ins-datoms ret-datoms _] (fstore/resolve ds-store txdata)
        next-store                (fstore/update ds-store ins-datoms ret-datoms)

        txdata                    [[:db/retractEntity [:task/title "Task 1"]]]
        [ins-datoms ret-datoms _] (fstore/resolve next-store txdata)
        nnext-store               (fstore/update next-store ins-datoms ret-datoms)]
    (fstore/datoms next-store)
    #_(d/with (:db nnext-store) [[:db.fn/retractAttribute 2 :task/title]])
    #_(d/pull (:db nnext-store) '[*] [:task/title "Task 2"])
    #_(datoms nnext-store))

  )
