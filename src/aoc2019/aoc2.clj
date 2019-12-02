(ns aoc2019.aoc2)

(defn read-file [file]
  (slurp file))

(defn parse [file]
  (re-seq #"(\d+)" file))

(defn parseInt [s]
  (if (re-matches #"\d+" s)
    (Long/valueOf s)
    0))
(defn structure [s]
  (map (fn [x] (parseInt (nth x 1))) s))

(defn opcode [f file i]
  (println (str file i)
   (assoc 
     file 
     (nth file (+ i 2)) 
     (f (nth file (nth file i)) (nth file (nth file (+ i 1)))))))

(defn opcode1 [file i]
   (assoc 
     file 
     (nth file (+ i 2)) 
     (+ (nth file (nth file i)) (nth file (nth file (+ i 1))))))
    
(defn opcode2 [file i]
   (assoc 
     file 
     (nth file (+ i 2)) 
     (* (nth file (nth file i)) (nth file (nth file (+ i 1))))))

    

(def opcodes [opcode1, opcode2]) 

   
(defn get-opcode [file i]
  (if (= (nth file i) 99)
    -1
    (- (nth file i) 1)))

   
(defn iterator [file i c]
  (when (< i c)
    (do
      (let [ret (get-opcode file i)]
          (if (< ret 0)
            file
           (let [new_file ((nth opcodes ret) file (+ i 1))] 
            (iterator new_file (+ i 4) c)))))))

(defn run []
  (let [file (vec (doall (structure (parse (read-file "aoc2.txt")))))
        c (count file)]
    (iterator file 0 c)))

(defn to-cvs [data]
  (map (fn [x] (str ", " x)) data))
 
(defn t []
  (assert (= (opcode + [1, 0, 0, 0, 99] 1) [2, 0, 0, 0, 99])))

(defn t2 []
  (let [got (iterator [1, 0, 0, 0, 99] 0 5)
        expected [2, 0, 0, 0, 99]]
    (assert (= got expected) (str "Got " got " , expected " expected))))

