(ns lab-1-2.core
  (:gen-class))

(defn generate-word [word alphabet]
  (loop [alphabet alphabet
         result ()]
    (if (not-empty alphabet)
      (let [current-element (first alphabet)
            current-res (if (not= current-element (first word))
                          (cons (conj word current-element)
                                result)
                          result)]
        (recur (rest alphabet) current-res))
      result)))

(defn generate-words [words alphabet]
  (loop [words words
         result ()]
    (if (not-empty words)
      (recur (rest words)
             (concat result
                     (generate-word (first words)
                                  alphabet)))
      result)))

(defn generate-permutations [n alphabet]
  (loop [n n
         result '(())]
    (if (> n 0)
      (recur (dec n)
             (generate-words result alphabet))
      result)))

(defn -main []
  (println(generate-permutations 2 '(\a \b \c)))
  (println(generate-permutations 2 '("a" "b" "c")))
  (println(generate-permutations 0 '(:a [:b (:c)])))
  )
