(ns rtree.core)

(defrecord BoundingBox [x0 y0 x1 y1])
(defrecord Node [bounding-box data children])

(defn intersects?
  "intersects? returns true if the two bounding boxes overlap, false otherwise. If either box is nil false is returned."
  [a b]
  (not
    (or
      (nil? a)
      (nil? b)
      (< (:x1 a) (:x0 b))
      (< (:x1 b) (:x0 a))
      (< (:y1 a) (:y0 b))
      (< (:y1 b) (:y0 a)))))

(defn make-bounding-box
  "make-bounding-box constructs a new bounding box."
  [x0 y0 x1 y1]
  (BoundingBox. 
    (min x0 x1)
    (min y0 y1)
    (max x0 x1)
    (max y0 y1)))

(defn compute-bounding-box
  "compute-bounding-box computes the bounding box for a sequence of nodes"
  [nodes]
  (let [bb (map :bounding-box nodes)
        x0 (reduce min (map :x0 bb))
        y0 (reduce min (map :y0 bb))
        x1 (reduce max (map :x1 bb))
        y1 (reduce max (map :y1 bb))]
    (make-bounding-box x0 y0 x1 y1)))

(defn make-leaf
  "make-leaf creates a node record for the given data and bounding box with no children."
  [bbox data]
  (Node. bbox
         data
         nil))

(defn make-branch
  "make-branch creates a node record with no data and the given children. The bounding box is computed."
  [children]
  (Node. (compute-bounding-box children)
         nil
         children))

(declare top-down)

(defn- split
  "split partitions a sequence of data and recursivley calls top-down on the partitions."
  [level m nodes]
  (let [k      (quot (dec (+ (count nodes) m)) m)
        dim    (get [:x0 :y0] (mod level 2))]
    (->> nodes
         (sort-by #(get-in % [:bounding-box dim]))
         (partition k k nil)
         (map #(top-down (inc level) m %))
         (into [])
         make-branch)))

(defn- top-down
  "top-down implements top-down bulk-load algorithm. It returns the root node of a subtree."
  [level m nodes]
  (if (<= (count nodes) m)
    (make-branch nodes)
    (split level m nodes)))

(defn create
  "create constructs a new rtree containing the given leaf nodes."
  ([leaves] (create {} leaves))
  ([opts leaves]
   (when (not-empty leaves)
     (let [maxChildren (get opts :max-children 25)]
       (top-down 0 maxChildren leaves)))))

(defn- -search-intersection
  [tree box]
  (if (intersects? box (:bounding-box tree))
    (cons (:data tree)
          (mapcat #(-search-intersection % box) (:children tree)))))

(defn search-intersection
  "search-intersection searches the tree for all data that intersects with the given box"
  [tree box]
  (->> (-search-intersection tree box)
       (remove nil?)))

(defn bulk-update
  "bulk-update does a cheap update of object state, without re-organising the tree. func should map from leaf nodes to leaf nodes, and may return nil to delete an item."
  [{:keys [children] :as node} func]
  (if (nil? children)
    (func node)
    (let [children' (->> (map #(bulk-update % func) children)
                         (remove nil?))]
      (when (not-empty children')
        (-> (compute-bounding-box children')
            (assoc node :children children' :bounding-box))))))
