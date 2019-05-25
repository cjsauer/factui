(ns factui.impl.store
  "Protocol and helper fns for Store implementations"
  (:refer-clojure :exclude [update resolve]))

(defprotocol Store
  (update [this insert-datoms retract-datoms]
    "Update the store by inserting and retracting the specified concrete datoms (such as those returned by `resolve`).")
  (resolve [this operations]
    "Given a seq of Datomid operations, resolve against the existing contents of the store to return a 3-tuple of:

    1. concrete tuples to add
    2. concrete tuples to remove
    3. map of tempid to concrete EIDs

    This handles all Datomic-style semantics such as upsert, tempid resolution, and preventing duplicates")
  (schema [this] "Returns a map of {attr attributes} for the store's schema.")
  (datoms [this] "Returns a lazy seq of datoms in this store"))

(def ^:no-doc base-schema
  "Initial built-in schema"
  [{:db/ident :db/valueType
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/keyword}
   {:db/ident :db/unique
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/keyword}
   {:db/ident :db/ident
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/valueType :db.type/keyword}
   {:db/ident :db/cardinality
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/keyword}
   {:db/ident :db/cardinality
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/keyword}
   {:db/ident :db/doc
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/string}
   {:db/ident :db/isComponent
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/boolean}
   ;; Compatibility purposes only
   {:db/ident :db/index
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/boolean}
   {:db/ident :db/fulltext
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/boolean}
   {:db/ident :db/noHistory
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/boolean}])

(defn card-one?
  "Efficiently check if a given attr is card-one?"
  [store attr]
  (= :db.cardinality/one (-> store schema attr :db/cardinality)))

(defn identity?
  "Efficiently check if a given attr is an identity attr"
  [store attr]
  (= :db.unique/identity (-> store schema attr :db/unique)))

(defn ref?
  "Efficiently check if a given attr is a ref"
  [store attr]
  (= :db.type/ref (-> store schema attr :db/valueType)))

(defn component?
  "Efficiently check if a given attr is a component"
  [store attr]
  (true? (-> store schema attr :db/isComponent)))

(defn transient?
  "Efficiently check if a given attr is transient"
  [store attr]
  (true? (-> store schema attr :factui/isTransient)))
