(ns el-farol.core
  (:gen-class)
  (:use (incanter core charts)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (view (function-plot sin -10 10)))

;;; Prediction strategies

(def TOTAL 100)
(def THRESHOLD 60)

(def guess (rand-int (inc TOTAL)))

(defn predict-random
  "Just a wild guess!"
  [hist]
  guess)

(defn predict-last-wk
  "Prediction: this week will be the same as last week"
  [hist]
  (if (empty? hist) guess (last hist)))

(defn predict-alternate-last-wk
  "Prediction: those who went last week will stay at home, and vice versa"
  [hist]
  (if (empty? hist) guess (- TOTAL (last hist))))

(defn gen-predict-avg-recent-hist
  "Generate a prediction function which uses average of last n weeks"
  [n]
  (fn [hist] (cond (empty? hist) guess
                  (< (count hist) n) (quot (apply + hist) (count hist))
                  :default (quot (apply + (take-last n hist)) n))))

(defn gen-attendance-rule
  "Generate an attendance decision function based on the given predictor function"
  [predictor]
  (fn [hist] (< (predictor hist) THRESHOLD)))

;;; A sample population specfication for 100

(def sample-agent-spec {predict-random 20,
                        predict-last-wk 20,
                        predict-alternate-last-wk 20,
                        (gen-predict-avg-recent-hist 2) 20,
                        (gen-predict-avg-recent-hist 3) 20})

(def sample-history [34 55 56 40 61 70 50])

(defn agent-population
  "Build an agent population based on a map of predictors to quantity"
  [spec]
  (loop [predictors (keys spec) population []]
    (if (empty? predictors)
      population
      (recur (rest predictors) (concat population (repeat (get spec (first predictors)) (gen-attendance-rule (first predictors))))))))

(def sample-population (agent-population sample-agent-spec))

(defn gen-el-farol-fn
  "Generate a function that takes attendance history and returns attendance, based on a given group."
  [agents]
  (fn [history] (conj history (count (for [agent agents :when (agent history)] agent)))))

(defn gen-el-farol-seq
  "doc-string"
  [agents history]
  (let [attendance (gen-el-farol-fn agents)]
    (iterate attendance history)))

(def sample-el-farol-seq (gen-el-farol-seq sample-population sample-history))
