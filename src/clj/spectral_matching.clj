(ns spectral-matching
  [:require [clojure.pprint]
            [clojure.core.reducers :as r]])

(defn- reduce-fn
  [result-map line-data]
  (let [[wavelength absorb] (clojure.string/split line-data #"\s+")]
    (assoc result-map (keyword wavelength) (Float/parseFloat absorb))))

(defn- get-sample-data
  [sample-filename]
  (with-open [rdr (clojure.java.io/reader sample-filename)]
    (reduce reduce-fn {} (line-seq rdr))))

(defn- known-data-reduce-fn
  [sample-folder result-map index]
  (let [sample-name (str "sample_" index)
        sample-filename (str sample-folder "/" sample-name ".spr")
        sample-data (get-sample-data sample-filename)]
    (assoc result-map sample-name sample-data)))

(defn- get-known-data
  [sample-folder range-begin range-end]
  (reduce (partial known-data-reduce-fn sample-folder)
          {} (range range-begin range-end)))

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
    ([] [0 0.0])
    ([accumulator k v]
      (let [sample-value (get sample-data k)]
        [(inc (first accumulator))
         (+ (second accumulator) (percentage-fn sample-value v))]))))

(defn- compare-sample-to-known
  [sample-data known-data]
  (-> (r/reduce (create-compare-sample-fn sample-data) known-data)
      (#(if (pos? (second %))
          (/ (second %) (first %))
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
