(ns markov.core
  (:require [clojure.string :as string]
            [clojure.pprint :refer [pprint]]))

(defn parse-text
  [filename]
  (as-> filename $
        (slurp $)
        (string/lower-case $)
        (string/split $ #"[\.\n]+")
        (remove empty? $)
        (map #(string/split % #"[^\w']+") $)
        (map #(remove empty? %) $)
        ))


(defn words-to-pairs [words]
  (let [words-with-boundaries (concat [:start] words [:end])]
    (map (fn [e1 e2] {e1 [e2]})
         words-with-boundaries
         (rest words-with-boundaries))))

(defn merge-graphs
  [& graphs]
  (reduce (partial merge-with concat)
          {}
          graphs))

(defn walk-graph
  [graph]
  (loop [current :start
         accumulator []]
    (let [possible-words (graph current)
          a-word (first (shuffle possible-words))]
      (if (= a-word :end)
        (string/join " " accumulator)
        (recur a-word
               (conj accumulator a-word))))))

(defn file-to-graph
  [filename]
  (->> filename
       parse-text
       (map words-to-pairs)
       flatten
       (reduce merge-graphs)))

(defn -main
  ([] (-main "lyrics/i-saw-her-standing-there.txt"
             "lyrics/like-a-rolling-stone.txt"
             "lyrics/bittersweet-symphony.txt"))
  ([& files]
     (let [graph (->> files
                      (map file-to-graph)
                      (reduce merge-graphs))]
       (pprint (repeatedly 10 (fn [] (walk-graph graph)))))))
