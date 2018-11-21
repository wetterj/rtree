(ns rtree.core-test
  (:require [clojure.test :refer :all]
            [rtree.core :refer :all]))

(deftest test-intersection
  (is (= false
         (intersects? nil nil))
      "empty boxes should not intersect")
  (is (= false
         (intersects? (make-bounding-box 0 0 0 0) nil))
      "empty boxes should not intersect")
  (is (= false
         (intersects? nil (make-bounding-box 0 0 0 0)))
      "empty boxes should not intersect")
  (is (= false
         (intersects? (make-bounding-box 0 0 10 10) (make-bounding-box 20 20 100 100)))
      "non-overlapping boxes should not intersect")
  (is (= false
         (intersects? (make-bounding-box 20 20 50 50) (make-bounding-box 0 0 10 10)))
      "non-overlapping boxes should not intersect")
  (is (= false
         (intersects? (make-bounding-box 0 0 10 10) (make-bounding-box 0 20 10 50)))
      "non-overlapping boxes should not intersect")
  (is (= true
         (intersects? (make-bounding-box 0 0 10 10) (make-bounding-box 5 5 10 10)))
      "overlapping boxes should intersect")
  (is (= true
         (intersects? (make-bounding-box 0 0 10 10) (make-bounding-box 10 10 20 20)))
      "touching boxes should intersect"))

(defn rand-floats
  "rand-floats generates a sequence of floats in the range [lo, hi) of length n."
  [lo hi n]
  (->> (repeatedly #(rand (- hi lo)))
       (map #(+ lo %))
       (take n)))

(defn random-data
  "random-data generate a sequence of nodes with random data of length n."
  [n]
  (let [x0s (rand-floats 0 1 n)
        y0s (rand-floats 0 1 n)
        ws  (rand-floats 0.01 0.1 n)
        hs  (rand-floats 0.01 0.1 n)
        x1s (map + x0s ws)
        y1s (map + y0s hs)
        bbs (map make-bounding-box x0s y0s x1s y1s)
        ds  (->> (range n)
                 (map str))]
    (map make-leaf bbs ds)))

(defn regular-data
  "regular-data generates a sequence of nodes with highly regular data of length n."
  [n]
  (let [bbs (map make-bounding-box
                 (range 0 n)
                 (reverse (range 0 n))
                 (range 1 (inc n)) 
                 (reverse (range 1 (inc n))))
        ds  (->> (range n)
                 (map str))]
    (map make-leaf bbs ds)))

(deftest test-create
  (is (nil? (create []))
      "empty list should produce empty tree")
  (is (nil? (create {:max-children 25} []))
      "empty list should produce empty tree")
  (let [nodes  (random-data 10)
        ds     (map :data nodes)
        bbox   (compute-bounding-box nodes)
        tree   (create nodes)
        ds'    (search-intersection tree bbox)]
      (is (= (sort ds)
             (sort ds))
          "not all nodes were inserted")))
