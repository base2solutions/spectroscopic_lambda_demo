(ns spectral-matching
  (:gen-class)
  [:require [clojure.pprint]
            [clojure.core.reducers :as r]])

(defn- reduce-fn
  [result-map line-data]
  (let [[wavelength absorb] (clojure.string/split line-data #"\s+")]
    (assoc result-map (keyword wavelength) (Float/parseFloat absorb))))

(defn- get-sample-data
  [sample-filename]
  (with-open [rdr (clojure.java.io/reader sample-filename)]
    (r/reduce reduce-fn {} (line-seq rdr))))

(defn- create-known-data-fn
  [sample-folder]
  (fn [index]
    (let [sample-name (str "sample_" index)
          sample-filename (str sample-folder "/" sample-name ".spr")
          sample-data (get-sample-data sample-filename)]
      {sample-name sample-data})))

(defn- get-known-data
  [sample-folder range-begin range-end]
  (let [map-fn (create-known-data-fn sample-folder)]
    (r/fold merge (r/map map-fn (range range-begin range-end)))))

(defn- percentage-fn
  [sample known]
  (try
    (if (> sample known)
      (/ known sample)
      (/ sample known))
    (catch java.lang.ArithmeticException ae 1)))

(defn- create-compare-sample-fn
  [sample-data]
  (fn
    ([k v]
      (let [sample-value (get sample-data k)]
        (percentage-fn sample-value v)))))

(defn- compare-sample-to-known
  [sample-data known-data]
  (-> (r/fold + (r/map (create-compare-sample-fn sample-data) known-data))
      (#(if (seq sample-data)
          (/ % (count sample-data))
          0.0))))

(defn- create-compare-reduce-fn
  [sample-data]
  (fn
    ([] [])
    ([results known-sample data]
      (let [compare-data (compare-sample-to-known sample-data data)]
        (cond
          (empty? results)
            [known-sample compare-data]
          (> compare-data (second results))
            [known-sample compare-data]
          :else
            results)))))

(defn- compare-sample-to-knowns
  [sample-data known-datas]
  (r/reduce (create-compare-reduce-fn sample-data) known-datas))

(defn -main [& args]
  ""
  (let [[sample-folder range-begin range-end & _] args
        range-begin (Integer/parseInt range-begin)
        range-end (Integer/parseInt range-end)
        known-datas (get-known-data sample-folder range-begin range-end)
        sample-data (get-sample-data (str sample-folder "/sample.spr"))
        sample-compares (compare-sample-to-knowns sample-data known-datas)]
    (clojure.pprint/pprint sample-compares)))
