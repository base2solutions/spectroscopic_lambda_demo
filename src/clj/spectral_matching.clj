(ns spectral-matching
  [:require [clojure.pprint]])

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

(defn- compare-sample-fn
  [sample-data known-data sum lookup-key]
  (let [sample (get sample-data lookup-key)
        known (get known-data lookup-key)
        percentage (percentage-fn sample known)]
    (+ sum percentage)))

(defn- compare-sample-to-known
  [sample-data known-data]
  (let [compare-sum (reduce (partial compare-sample-fn sample-data known-data) 0 (keys known-data))]
    (/ compare-sum (count (keys known-data)))))

(defn- compare-reduce-fn
  [sample-data results known-data]
  (let [[known-sample data] known-data
        compare-data (compare-sample-to-known sample-data data)]
    (cond
      (empty? results)
        [known-sample compare-data]
      (> compare-data (second results))
        [known-sample compare-data]
      :else
        results)))
      

(defn- compare-sample-to-knowns
  [sample-data known-datas]
  (reduce (partial compare-reduce-fn sample-data) [] known-datas)
)

(defn -main [& args]
  ""
  (let [[sample-folder range-begin range-end & _] args
        range-begin (Integer/parseInt range-begin)
        range-end (Integer/parseInt range-end)
        known-datas (get-known-data sample-folder range-begin range-end)
        sample-data (get-sample-data (str sample-folder "/sample.spr"))
        sample-compares (compare-sample-to-knowns sample-data known-datas)
        ]
    (clojure.pprint/pprint sample-compares)
))
