(ns sorts.core
  (:use clojure.math.numeric-tower))

(defn- swap-indices
  [seq i1 i2]
  (let [vec (vec seq)]
    (assoc (assoc vec i1 (vec i2)) i2 (vec i1))))

(defn insertion-sort
  [seq]
  (loop [vec (vec seq)
         i 0
         j 1
         k 1]
    (if (= k (count vec))
      vec
      (if (= i -1)
        (recur vec k (inc k) (inc k))
        (if (< (vec j) (vec i))
          (recur (swap-indices vec i j) (dec i) i k)
          (recur vec (dec i) j k))))))

(defn merge-seqs
  [seq1 seq2]
  (loop [seq '()
         seq1 (reverse seq1)
         seq2 (reverse seq2)]
    (if (every? empty? [seq1 seq2])
      seq
      (let [el1 (first seq1)
            el2 (first seq2)]
        (if (or (nil? el1) (and (not (nil? el2)) (< el1 el2)))
          (recur (conj seq el2) seq1 (rest seq2))
          (recur (conj seq el1) (rest seq1) seq2))))))

(defn- split-seq
  [seq]
  (split-at (floor (/ (count seq) 2)) seq))

(defn merge-sort
  [seq]
  (if (= (count seq) 1)
    seq
    (apply merge-seqs (map merge-sort (split-seq seq)))))
