(defn get-divisors [n] 
  (range  2 
  (Math/round 
  (inc (Math/sqrt n)
  )))
)

(defn divides? [x n]
  (true?
    (= 0 (mod n x))
  )
)

(defn no-divisors? [n]
  (def list (get-divisors n))
  (not-any? #((fn [x] (divides? x n))%)list)
)

(defn is-prime? [n]
  (if (= n 2)
    true
    (if (= n 1)
      false
      (no-divisors? n)
    )
  )
)

(defn prime-seq [n1 n2]
  (def list (range n1 (inc n2)))
  (filter #((fn [x] (is-prime? x))%)list)
)

(defn print-top-primes [n1 n2]
  (def list (take 10 
  (reverse (prime-seq n1  n2 ))))
  (doseq [x list] 
  (println x)) 
  (reduce + list)
)

(println(str "Total=" (print-top-primes 50 100)))
(println(str "Total=" (print-top-primes 7 11)))