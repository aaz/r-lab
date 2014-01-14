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

(defn guess [] (rand-int (inc TOTAL)))

(defn predict-random
  "Just a wild guess!"
  [hist]
  (guess))

(defn predict-last-wk
  "Prediction: this week will be the same as last week"
  [hist]
  (if (empty? hist) (guess) (last hist)))

(defn predict-alternate-last-wk
  "Prediction: those who went last week will stay at home, and vice versa"
  [hist]
  (if (empty? hist) (guess) (- TOTAL (last hist))))

(defn gen-predict-avg-recent-hist
  "Generate a prediction function which uses average of last n weeks"
  [n]
  (fn [hist] (cond (empty? hist) (guess)
                  (< (count hist) n) (quot (apply + hist) (count hist))
                  :default (quot (apply + (take-last n hist)) n))))

;;; A sample population specfication for 100

(def sample-population-spec {predict-random 20,
                             predict-last-wk 20,
                             predict-alternate-last-wk 20,
                             (gen-predict-avg-recent-hist 2) 20,
                             (gen-predict-avg-recent-hist 3) 20})

(defn populate
  "Build a population based on a map of predictors to quantity"
  [spec]
  (reduce concat (map #(repeat (val %) (key %)) spec)))

(def sample-population (populate sample-population-spec))

(def sample-history [34 55 56 40 61 70 50])

(defn predictions
  "Takes a sequence of predictors and turnout history. Returns predictions for next event."
  [predictors hist]
  (loop [forecasts [],
         coll predictors]
    (if (empty? coll)
      forecasts
      (recur (conj forecasts {:strategy (first coll)
                              :prediction ((first coll) hist)})
             (rest coll)))))

(defn turnout
  "Calculate turnout at next event based on collective predictions"
  [predictors hist]
  (count
   (filter #(< (:prediction %) THRESHOLD)
           (predictions predictors hist))))

(defn update-turnout-record
  "Returns updated turnout history"
  [predictors hist]
  (conj hist (turnout predictors hist)))

;; Use (nth s 30) to see the outcome after 30 iterations

(defn el-farol-seq
  "A lazy sequence of turnout figures for given agent population and seed"
  [predictors hist]
  (let [next-turnout (partial update-turnout-record predictors)]
    (iterate next-turnout hist)))
