(ns lwfp1.core
  (:gen-class)
)
(use 'clojure.java.io)

(def hEps 0.5)
(def lEps 0.15)
(def Ra 3)
(def Rb (* Ra 1.5))

;parce input file

(defn IsAssigned? [Obj]
  (not (nil? Obj))
)

(defn ParseInteger [Line]
  (if (re-find #"^-?\d+\.?\d*$" Line)
    (read-string Line)
  )
)

(defn ParseString [Line]
  (->> (clojure.string/split Line #",")
       (map clojure.string/trim)
       (butlast)
       (map ParseInteger)
  )
)

(defn DeleteNotAssignedObjects [Coll]
  (filter IsAssigned? Coll)
)

(defn CreatePoint [E]
  {:coords (into [] E)}
)

(defn LoadDataFromFile [FileName]
  (let [Content (slurp FileName)]
    (reduce (fn [Coll E]
              (let [Line (DeleteNotAssignedObjects (ParseString E))]
                (conj Coll (CreatePoint Line))
              )
            )
            [] (clojure.string/split-lines Content)
    )
  )
)

;clasterization logic

(defn Sqr [Val]
  (* Val Val)
)

(defn CalcEucDistance [Pt1 Pt2]
  (->> (map - Pt1 Pt2) (map #(* % %)) (reduce +))
)

(defn CalcHamDistance [Pt1 Pt2]
  (count (filter true? (map not= Pt1 Pt2)))
)

(defn CalcPt_ [Distance Coef]
  (Math/exp (- (* (/ 4 (Sqr Coef)) Distance)))
)

(defn CalcPotential [Distance]
  (CalcPt_ Distance Ra)
)

(defn CalcCorrectedPotential [Distance]
  (CalcPt_ Distance Rb)
)

(defn CreatePointWithDistance [Point Distance]
  (assoc Point :dst Distance)
)

(defn CalcPotentialForPoint [Point Points CalcDistFunc]
  (let [Distance (reduce #(+ %1 (CalcPotential (CalcDistFunc (:coords Point) (:coords %2)))) 0 Points)]
    (CreatePointWithDistance Point Distance)
  )
)

(defn CalcPotentialsForPoints [Points CalcDistFunc]
  (map #(CalcPotentialForPoint %1 Points CalcDistFunc) Points)
)

(defn CorrectPotential [Point ClCenter CalcDistFunc]
  (let [CorPotential (CalcCorrectedPotential (CalcDistFunc (:coords Point) (:coords ClCenter)))
        NewDist (- (:dst Point) (* (:dst ClCenter) CorPotential) )]
    (assoc Point :dst NewDist)
  )
)

(defn CorrectPointsPotentials [Points ClCenter CalcDistFunc]
  (->> (map #(CorrectPotential %1 ClCenter CalcDistFunc) Points)
       (sort-by #(- (:dst %1)))
  )
)

(defn GetMinDistance [FromPoint ToPoints CalcDistFunc]
  (->> (map #(CalcDistFunc (:coords FromPoint) (:coords %1)) ToPoints)
       (apply min)
  )
)

(defn GetInitialPotentials [Points CalcDistFunc]
  (->> (CalcPotentialsForPoints Points CalcDistFunc)
       (sort-by #(- (:dst %1)))
  )
)

(defn CompCntr [NewCenter FirstCenter Eps]
  (let [Val (* (:dst FirstCenter) Eps)]
    (cond
      (> (:dst NewCenter) Val) 1
      (< (:dst NewCenter) Val) -1
      :else 0
    )
  )
)

(defn ProcessClusterization [Points CalcDistFunc]
  (let [InitPotentials (GetInitialPotentials Points CalcDistFunc)
        FirstCenter (first InitPotentials)]
    (loop [Centers [FirstCenter] Elements (rest InitPotentials)]
      (let [CorrectedPoints (CorrectPointsPotentials Elements (first Centers) CalcDistFunc)
            NewCenter (first CorrectedPoints)]
        (cond
          (= (CompCntr NewCenter FirstCenter hEps) 1)
            (recur (cons NewCenter Centers) (rest CorrectedPoints))
          (= (CompCntr NewCenter FirstCenter lEps) -1)
            (sort-by #(- (:dst %1)) Centers)
          (let [DMin (GetMinDistance NewCenter Centers CalcDistFunc)
                Rat (/ (:dst NewCenter) (:dst FirstCenter))]
            (>= (+ (/ DMin Ra) Rat) 1)
          )
            (recur (cons NewCenter Centers) (rest CorrectedPoints))
          :else
            (recur Centers (cons (assoc NewCenter :dst 0) (rest CorrectedPoints)))
        )
      )
    )
  )
)

(defn -main [& args]
  (if (= (count args) 2)
    (let  [CalcDistFunc (cond
                          (= (last args) "-h") CalcHamDistance
                          (= (last args) "-e") CalcEucDistance
                          :else nil
                        )]
      (if (IsAssigned? CalcDistFunc)
        (let [Points (LoadDataFromFile (first args))]
          (ProcessClusterization Points CalcDistFunc)
        )
        (println "Distance calculation Method is undefined")
      )
    )
    (println "Expected 2 arguments (filename distance_calculation_method)")
  )
)
