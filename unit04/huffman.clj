(use '[clojure.string :only (join split)])

(defn tokenize
  "Splits a text string into constituent symbols"
  [text]
  (split text #",?\s+"))

(defn group-symbols
  "doc-string"
  [symbols]
  (frequencies symbols))

(defrecord BinaryTree [frequency symbol zero one])

(defn create-subtree
  "Combine two items from frequency collection into a subtree"
  [first second]
  (->BinaryTree (+ (val first) (val second))
                nil
                (->BinaryTree (val first) (key first) nil nil)
                (->BinaryTree (val second) (key second) nil nil)))

(defn build-tree
  "Encode the symbols in a tree with weighted path length"
  [symbol-freqs]
  (let [sorted-symbols (sort-by val symbol-freqs)]
    (if (= 1 (count symbol-freqs)) (key (first symbol-freqs))
        (build-tree (assoc (dissoc symbol-freqs
                                   (key (first sorted-symbols))
                                   (key (second sorted-symbols)))
                      (create-subtree (first sorted-symbols)
                                      (second sorted-symbols))
                      (+ (val (first sorted-symbols)) (val (second sorted-symbols))))))))

(defn display-tree
  "doc-string"
  [tree output]
  (cond
   (string? (:symbol tree)) (join " " [output (:symbol tree) "\n"])
   (not (nil? (:symbol tree))) (display-tree (:symbol tree) output)
   :else (join [(display-tree (:zero tree) (join [output "0"]))
                (display-tree (:one tree) (join [output "1"]))])))
