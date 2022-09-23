(ns lab-1-3.core
  (:gen-class))

(defn my-map [f coll]
  (reduce (fn [acc elem] (conj acc (f elem))) [] coll))

(defn my-filter [pred coll]
  (reduce (fn [acc elem] (if (pred elem) (conj acc elem) acc)) [] coll))

(defn -main [& args]
  (println(my-map inc [1 2 3 4 5]))
  (println(my-filter even? (range 10)))
  )