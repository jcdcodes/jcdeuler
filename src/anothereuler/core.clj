(ns anothereuler.core
  (:require clojure.contrib.combinatorics
	    clojure.contrib.duck-streams))

;;;;;;;;;;;;;;
;; Problem 001
;; fizz buzz
(defn multiples-of-3-or-5-below [n]
  (filter #(or (= 0 (mod % 3)) (= 0 (mod % 5)))  (range n)))
(reduce + (multiples-of-3-or-5-below 10))    ;; gives 23, like the man said
(reduce + (multiples-of-3-or-5-below 1000)) ;; gives 233168, the right answer
;;;;;;;;;;


;;;;;;;;;;;;;;
;; Problem 002
;;
(defn fibs-below
  [max]
  (loop [a 1 b 1 acc []]
    (if (>= b max) acc
	(recur b (+ a b) (conj acc b)))))
(reduce + (filter even? (fibs-below 4000000)))  ;;4613732 is the correct answer
;;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 003
;; largest prime factor of 600851475143
(def p003-big-number 600851475143)
;;(defn primes-below ;;(see superior definition in 007, below)
;;  [n]
;;  (loop [primes []
;;	 candidates (range 2 n)
;;	 first-candidate 2]
;;    (if (empty? candidates) primes
;;	(recur (conj primes first-candidate)
;;	       (filter #(> (mod % first-candidate) 0) candidates)
;;	       (second candidates)))))
;;(def primes-below (memoize primes-below))
;;

(defn prime-factorization-of
  [n]
  (if (< n 2) []
  (let [primes (primes-below 2000000)]
    (loop [remainder n remaining-primes primes factorization []]
      (if (empty? remaining-primes)
	(if (empty? factorization) [n] factorization)
	(if (zero? (mod remainder (first remaining-primes)))
	  (recur (/ remainder (first remaining-primes))
		 remaining-primes
		 (conj factorization (first remaining-primes)))
	  (recur remainder
		 (rest remaining-primes)
		 factorization)))))))
;;
;; (prime-factorization-of p003-big-number) ;; [71 839 1471 6857] (6857 is the answer)
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 004
;; Largest palindromic product of two 3-digit numbers
;;
(defn palindrome? [n] (= (reverse (reverse (str n))) (reverse (str n))))
(defn big-products
  []
  (filter palindrome?
	  (for [a (range 999 900 -1) b (range 999 900 -1)] (* a b))))
;;
;; (last (sort (big-products))) ;; 906609 (which is bigger than 900**2)
;; so that's the answer
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 005
;;
(map prime-factorization-of (range 2 21))
;; Then manually pulled out just enough of each prime to factor all the numbers below 21.
;; For 20 and below, it's 232792560. For 10 and below, it's 2520 (using the same list).
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 006
;; (Sum of squares) - square of sum of [1, 100].
(defn square [x] (* x x))
(defn square-of-sums-minus-sum-of-squares
  [n]
  (- (square (reduce + (range 1 (inc n))))
     (reduce + (map square (range 1 (inc n))))))
;;
;; (square-of-sums-minus-sum-of-squares 10) is 2640, like the man said.
;; (square-of-sums-minus-sum-of-squares 100) is 25164150.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 007
;; Find the 10001st prime
;;
;;This finds 100000 in about 7 seconds:
;;(defn primes-below
;;  [nmax]
;;  (if (< nmax 3) [] 
;;      (loop [primes '(2) n 3]
;;	(if (> n nmax) primes
;;	    (if (every? #(< 0 (mod n %)) primes)
;;	      (recur (cons n primes) (+ 2 n))
;;	      (recur primes (+ 2 n)))))))
;; but this does it way, way faster:
;;
(defn prime?
  [n]
  (cond (< n 2) false
	(= n 2) true
	(even? n) false
	true (loop [i 3 imax (int (Math/sqrt n))]
	       (if (> i imax)
		 true
		 (if (= 0 (mod n i))
		   false
		   (recur (+ 2 i) imax))))))
(def prime? (memoize prime?))

(defn primes-below
  [n]
  ;;(filter prime? (range (dec n) 0 -1)))
  (filter prime? (range n)))

(def primes-below (memoize primes-below))
;;
;; (nth (primes-below 110000) 10000) gives 104743
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 008
;; Scan for five consecutive digits with max product
;;
;; by eye, found 9,9,8,7,9. 40824 is the answer.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 009
;; Pythagorean triplet where a+b+c==1000
(defn pythagorean?
  [a b c]
  (= (+ (* a a) (* b b)) (* c c)))
;;
;;
(filter #(and (apply pythagorean? %) (= 1000 (apply + %)))
        (for [a (range 1 1000) b (range 1 1000)] [a b (- 1000 a b)]))
;;
;; ...gives ([200 375 425] [375 200 425]), which are the same thing.
;; (* 200 375 425) is 31875000, which is the answer.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 010
;; Sum of primes less than 2,000,000.
;;
;; (reduce + (primes-below 10)) gives 17, like the man said.
;; (reduce + (primes-below 2000000)) is terribly slow, so we're refactoring...
;; ...and now that we've gone and stolen Integer>>isPrime from Squeak, it's fast:
(reduce + (primes-below 2000000))
;; ...gives 142913828922, which is the right answer
;;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Promlem 011
;; Biggest product of line of four numbers
;;
(def g11
     [[ 8  2 22 97 38 15  0 40  0 75  4  5  7 78 52 12 50 77 91  8]
      [49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48  4 56 62  0]
      [81 49 31 73 55 79 14 29 93 71 40 67 53 88 30  3 49 13 36 65]
      [52 70 95 23  4 60 11 42 69 24 68 56  1 32 56 71 37  2 36 91]
      [22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80]
      [24 47 32 60 99  3 45  2 44 75 33 53 78 36 84 20 35 17 12 50]
      [32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70]
      [67 26 20 68  2 62 12 20 95 63 94 39 63  8 40 91 66 49 94 21]
      [24 55 58  5 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72]
      [21 36 23  9 75  0 76 44 20 45 35 14  0 61 33 97 34 31 33 95]
      [78 17 53 28 22 75 31 67 15 94  3 80  4 62 16 14  9 53 56 92]
      [16 39  5 42 96 35 31 47 55 58 88 24  0 17 54 24 36 29 85 57]
      [86 56  0 48 35 71 89  7  5 44 44 37 44 60 21 58 51 54 17 58]
      [19 80 81 68  5 94 47 69 28 73 92 13 86 52 17 77  4 89 55 40]
      [ 4 52  8 83 97 35 99 16  7 97 57 32 16 26 26 79 33 27 98 66]
      [88 36 68 87 57 62 20 72  3 46 33 67 46 55 12 32 63 93 53 69]
      [ 4 42 16 73 38 25 39 11 24 94 72 18  8 46 29 32 40 62 76 36]
      [20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74  4 36 16]
      [20 73 35 29 78 31 90  1 74 31 49 71 48 86 81 16 23 57  5 54]
      [ 1 70 54 71 83 51 54 69 16 92 33 48 61 43 52  1 89 19 67 48]])

(def fours-across
     (for [r (range 20) c (range 17)]
       [(nth (nth g11 r) (+ 0 c))
	(nth (nth g11 r) (+ 1 c))
	(nth (nth g11 r) (+ 2 c))
	(nth (nth g11 r) (+ 3 c))]))
(def fours-down
     (for [r (range 17) c (range 20)]
       [(nth (nth g11 (+ 0 r)) c)
	(nth (nth g11 (+ 1 r)) c)
	(nth (nth g11 (+ 2 r)) c)
	(nth (nth g11 (+ 3 r)) c)]))
(def fours-down-right
     (for [r (range 17) c (range 17)]
       [(nth (nth g11 (+ 0 r)) (+ 0 c))
	(nth (nth g11 (+ 1 r)) (+ 1 c))
	(nth (nth g11 (+ 2 r)) (+ 2 c))
	(nth (nth g11 (+ 3 r)) (+ 3 c))]))
(def fours-up-right
     (for [r (range 17) c (range 17)]
       [(nth (nth g11 (+ 0 r)) (+ 3 c))
	(nth (nth g11 (+ 1 r)) (+ 2 c))
	(nth (nth g11 (+ 2 r)) (+ 1 c))
	(nth (nth g11 (+ 3 r)) (+ 0 c))]))
(apply max (map #(reduce * %) fours-across))    ;;48477312
(apply max (map #(reduce * %) fours-down))      ;;51267128
(apply max (map #(reduce * %) fours-down-right));;40304286
(apply max (map #(reduce * %) fours-up-right))  ;;70600674 (biggest; the answer!)
;;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 012
;; First triangle number with at least 500 divisors.
;;
(defn triangle
  [n]
  (/ (* n (inc n)) 2))
(defn count-factors
  [n]
  (let [the-factorization (prime-factorization-of n)
	the-count (reduce * (map inc (map count (vals (group-by identity the-factorization)))))]
    [the-count the-factorization]))
;; (first (filter #(< 500 (first %)) (map count-factors (map triangle (iterate inc 10000)))))
;; gives us 76576500, which is the answer
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 013
;; First ten digits of:
(+
 37107287533902102798797998220837590246510135740250
 46376937677490009712648124896970078050417018260538
 74324986199524741059474233309513058123726617309629
 91942213363574161572522430563301811072406154908250
 23067588207539346171171980310421047513778063246676
 89261670696623633820136378418383684178734361726757
 28112879812849979408065481931592621691275889832738
 44274228917432520321923589422876796487670272189318
 47451445736001306439091167216856844588711603153276
 70386486105843025439939619828917593665686757934951
 62176457141856560629502157223196586755079324193331
 64906352462741904929101432445813822663347944758178
 92575867718337217661963751590579239728245598838407
 58203565325359399008402633568948830189458628227828
 80181199384826282014278194139940567587151170094390
 35398664372827112653829987240784473053190104293586
 86515506006295864861532075273371959191420517255829
 71693888707715466499115593487603532921714970056938
 54370070576826684624621495650076471787294438377604
 53282654108756828443191190634694037855217779295145
 36123272525000296071075082563815656710885258350721
 45876576172410976447339110607218265236877223636045
 17423706905851860660448207621209813287860733969412
 81142660418086830619328460811191061556940512689692
 51934325451728388641918047049293215058642563049483
 62467221648435076201727918039944693004732956340691
 15732444386908125794514089057706229429197107928209
 55037687525678773091862540744969844508330393682126
 18336384825330154686196124348767681297534375946515
 80386287592878490201521685554828717201219257766954
 78182833757993103614740356856449095527097864797581
 16726320100436897842553539920931837441497806860984
 48403098129077791799088218795327364475675590848030
 87086987551392711854517078544161852424320693150332
 59959406895756536782107074926966537676326235447210
 69793950679652694742597709739166693763042633987085
 41052684708299085211399427365734116182760315001271
 65378607361501080857009149939512557028198746004375
 35829035317434717326932123578154982629742552737307
 94953759765105305946966067683156574377167401875275
 88902802571733229619176668713819931811048770190271
 25267680276078003013678680992525463401061632866526
 36270218540497705585629946580636237993140746255962
 24074486908231174977792365466257246923322810917141
 91430288197103288597806669760892938638285025333403
 34413065578016127815921815005561868836468420090470
 23053081172816430487623791969842487255036638784583
 11487696932154902810424020138335124462181441773470
 63783299490636259666498587618221225225512486764533
 67720186971698544312419572409913959008952310058822
 95548255300263520781532296796249481641953868218774
 76085327132285723110424803456124867697064507995236
 37774242535411291684276865538926205024910326572967
 23701913275725675285653248258265463092207058596522
 29798860272258331913126375147341994889534765745501
 18495701454879288984856827726077713721403798879715
 38298203783031473527721580348144513491373226651381
 34829543829199918180278916522431027392251122869539
 40957953066405232632538044100059654939159879593635
 29746152185502371307642255121183693803580388584903
 41698116222072977186158236678424689157993532961922
 62467957194401269043877107275048102390895523597457
 23189706772547915061505504953922979530901129967519
 86188088225875314529584099251203829009407770775672
 11306739708304724483816533873502340845647058077308
 82959174767140363198008187129011875491310547126581
 97623331044818386269515456334926366572897563400500
 42846280183517070527831839425882145521227251250327
 55121603546981200581762165212827652751691296897789
 32238195734329339946437501907836945765883352399886
 75506164965184775180738168837861091527357929701337
 62177842752192623401942399639168044983993173312731
 32924185707147349566916674687634660915035914677504
 99518671430235219628894890102423325116913619626622
 73267460800591547471830798392868535206946944540724
 76841822524674417161514036427982273348055556214818
 97142617910342598647204516893989422179826088076852
 87783646182799346313767754307809363333018982642090
 10848802521674670883215120185883543223812876952786
 71329612474782464538636993009049310363619763878039
 62184073572399794223406235393808339651327408011116
 66627891981488087797941876876144230030984490851411
 60661826293682836764744779239180335110989069790714
 85786944089552990653640447425576083659976645795096
 66024396409905389607120198219976047599490197230297
 64913982680032973156037120041377903785566085089252
 16730939319872750275468906903707539413042652315011
 94809377245048795150954100921645863754710598436791
 78639167021187492431995700641917969777599028300699
 15368713711936614952811305876380278410754449733078
 40789923115535562561142322423255033685442488917353
 44889911501440648020369068063960672322193204149535
 41503128880339536053299340368006977710650566631954
 81234880673210146739058568557934581403627822703280
 82616570773948327592232845941706525094512325230608
 22918802058777319719839450180888072429661980811197
 77158542502016545090413245809786882778948721859617
 72107838435069186155435662884062257473692284509516
 20849603980134001723930671666823555245252804609722
 53503534226472524250874054075591789781264330331690
 )
;; 5537376230390876637302048746832985971773659831892672 is the sum
;; 5537376230 is the answer
;; I guess that was for folks without bignum libraries
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 014
;; Longest Collatz sequence under 1,000,000
;;
(defn collatz
  [start]
  (loop [n start cnt 1]
    (if (< n 2) cnt
	(if (even? n)
	  (recur (/ n 2) (inc cnt))
	  (recur (inc (* n 3)) (inc cnt))))))
;;
;; (time (apply max (map collatz (range 1000000)))) says 525, so...
;; ...redefine collatz to return the count and regurgitate the starting number...
(defn collatz
  [start]
  (loop [n start cnt 1]
    (if (< n 2) [start cnt]
	(if (even? n)
	  (recur (/ n 2) (inc cnt))
	  (recur (inc (* n 3)) (inc cnt))))))
;;
;; (filter #(< 500 (second %)) (map collatz (range 1000000))) gives:
;; ([626331 509] [704623 504] [837799 525] [939497 507])
;; so 837799 is the answer.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 015
;; Count forward diagonal paths through a 20x20 grid.
;;
;; Looks like 40 choose 20; but it's not.
(defn factorial
  ([n] (factorial n 1))
  ([n acc]
     (if (< n 2) acc
	 (recur (dec n) (* n acc)))))
;;
(defn re-pos
  "Stolen from http://stackoverflow.com/questions/3262195"
  [re s]
  (loop [m (re-matcher re s)
	 res {}]
    (if (.find m)
      (recur m (assoc res (.start m) (.group m)))
      res)))

(defn next-steps-from
  [s]
  (for [[i _] (re-pos #"01" s)]
    (str (subs s 0 i) "10" (subs s (+ i 2)))))

(defn all-steps-from
  ([s] (all-steps-from s []))
  ([s acc]
     (let [next-steps (next-steps-from s)]
       (if (empty? next-steps) acc
	   (conj (all-steps-from next-steps) s)))))
;;
;; so then I ran a bunch of stuff like this and the numbers
;; looked suspiciously familiar, and god damn it, it *is* 40c20.
(count
 (flatten
  (take-while #(not (empty? %))
	      (iterate #(distinct (flatten (map next-steps-from %)))
		       ["0000011111"]))))
;;
;; so I just ran:
(apply / (map factorial [40 20 20]))
;; and 137846528820, sure enough, is the answer.  That's annoying.
;;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 016
;; sum of digits in 2**1000
(reduce + (map #(- (int %) (int '\0)) (seq (str (nth (iterate #(* 2 %) 1) 1000)))))
;; 1366 is the answer
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 017
;;
(defn spell
  [n]
  (let [lookup {0 ""
	       1 "one"
	       2 "two"
	       3 "three"
	       4 "four"
	       5 "five"
	       6 "six"
	       7 "seven"
	       8 "eight"
	       9 "nine"
	       10 "ten"
	       11 "eleven"
	       12 "twelve"
	       13 "thirteen"
	       14 "fourteen"
	       15 "fifteen"
	       16 "sixteen"
	       17 "seventeen"
	       18 "eighteen"
	       19 "nineteen"
	       20 "twenty"
	       30 "thirty"
	       40 "forty"
	       50 "fifty"
	       60 "sixty"
	       70 "seventy"
	       80 "eighty"
	       90 "ninety"
	       100 "onehundred"
	       200 "twohundred"
	       300 "threehundred"
	       400 "fourhundred"
	       500 "fivehundred"
	       600 "sixhundred"
	       700 "sevenhundred"
	       800 "eighthundred"
	       900 "ninehundred"
	       1000 "onethousand"}]
    (cond (< n 20)          (lookup n)
	  (< n 100)         (str (lookup (int (* 10  (Math/floor (/ n 10 )))))
				 (lookup (rem n 10)))
	  (= 0 (mod n 100)) (lookup (int (* 100 (Math/floor (/ n 100)))))
	  true              (str (lookup (int (* 100 (Math/floor (/ n 100)))))
				 "and" (spell (rem n 100))))))
;; Many spot checks later, it's debugged.
;; (reduce + (map count (map spell (range 1 1001)))) gives 21124, which is the answer
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 018 and Problem 067
;; Miserable triangle
(def minitri
     '((3)
       (7 4)
       (2 4 6)
       (8 5 9 3)))
(def tri
     '((75)
       (95 64)
       (17 47 82)
       (18 35 87 10)
       (20  4 82 47 65)
       (19  1 23 75  3 34)
       (88  2 77 73  7 63 67)
       (99 65  4 28  6 16 70 92)
       (41 41 26 56 83 40 80 70 33)
       (41 48 72 33 47 32 37 16 94 29)
       (53 71 44 65 25 43 91 52 97 51 14)
       (70 11 33 28 77 73 17 78 39 68 17 57)
       (91 71 52 38 17 14 91 43 58 50 27 29 48)
       (63 66  4 68 89 53 67 30 73 16 69 87 40 31)
       ( 4 62 98 27 23  9 70 98 73 93 38 53 60  4 23)))
;;
;; We'd like to collapse this from the bottom up:
;; replace [[2 3] [5 7 11]] with [[9 14]]
;;
(defn collapse-two-rows
  [x y]
  (map max (map + x y) (rest (map + x (cons 0 y)))))
;;
;; (apply collapse-two-rows (take 2 (reverse minitri))) does the right thing
;; so now to recurse all the way through:
;;
(defn collapse-all
  ([] '())
  ([x] x)
  ([x y] (collapse-two-rows x y))
  ([x y & others]
     (let [collapsed-x (collapse-two-rows x y)]
       (apply collapse-all (cons collapsed-x others)))))
;;
;; (apply collapse-all (reverse minitri)) is 23, like the man said.
;; (apply collapse-all (reverse tri)) gives 1074, which is the answer.
(def bigtri (eval (read-string (slurp "triangle.txt"))))
;; (apply collapse-all (reverse bigtri)) is problem 67: it's 7273.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 019
;; Count Sundays on the first of the month between 01-01-1901 and 12-31-2000.
(def cal (java.util.GregorianCalendar.))
(.set cal 1901 java.util.Calendar/JANUARY 1)
(dotimes [i 1200]
  (.add cal java.util.Calendar/MONTH 1)
  (if (= java.util.Calendar/SUNDAY (.get cal java.util.Calendar/DAY_OF_WEEK))
    (println (.getTime cal))))
;; printed 171 rows, which is the answer.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 020
;; Sum of digits in 100!
(reduce + (map #(- (int %) (int '\0)) (seq (str (factorial 100)))))
;; it says 648, which is the answer.  Hooray for reuse!
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 021
;; Amicable pairs
;; Tried it with macros, but they got unwieldy because they can't be used like functions.
(defn divisors-of
  [n]
  (let [pf (prime-factorization-of n)
	factor-counts  (frequencies pf)]
    (map #(reduce * %) (apply clojure.contrib.combinatorics/cartesian-product
			      (for [p factor-counts]
				(map #(Math/pow (first p) %)
				     (range (inc (second p)))))))))

(defn proper-divisors-of
  [m]
  (let [divs (divisors-of m)]
    (map #(Math/round %) (filter #(> m %) divs))))

(defn amicable
  [n]
  (= n (reduce + (proper-divisors-of (reduce + (proper-divisors-of n))))))
(def amicable (memoize amicable))
;; (reduce + (filter amicable (range 10000))) takes a while, but gives 40284; wrong!
;; (+ 220 284 1184 1210 2620 2924 5020 5564 6232 6368) removes the ones that are self-amicable
;; and gives 31626, which is the answer.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 022
;; Name scores
;;(set! *print-length* 15)
(def names (sort (map read-string (.split (slurp "names.txt") ","))))
(defn sum-name-scores
  [names]
  (loop [i 1 score 0 remaining-names names]
    (if (empty? remaining-names)
      score
      (recur (inc i)
	     (+ score (* i (reduce + (map #(- (int %) 64) (first remaining-names)))))
	     (rest remaining-names)))))
;;(sum-name-scores names) gives 871198282, which is the answer
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 023
;; Abundant numbers
;;
(defn abundant?
  [n]
  (< n (reduce + (proper-divisors-of n))))
(def abundant? (memoize abundant?))
(def abundant-numbers (filter abundant? (range 28124)))
;; that took dozens of minutes to compute; probably need to speed it up later.
;; but now they're all cached.
(defn sum-of-abundants?
  [n]
  (loop [remaining-abundants abundant-numbers
	 a (first remaining-abundants)
	 b (- n a)]
    (if (< b a)
      false
      (if (abundant? b)
	true
	(recur (rest remaining-abundants)
	       (second remaining-abundants)
	       (- n (second remaining-abundants)))))))
;;
;; (reduce + (filter (complement sum-of-abundants?) (range 28123)))
;; very quickly gives 4179871, which is the answer.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 024
;; Lexicographic permutations of 0123456789
(nth (clojure.contrib.combinatorics/permutations [0 1 2 3 4 5 6 7 8 9]) 999999)
;; It's cheating, but 2 7 8 3 9 1 5 4 6 0 is the answer
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 025
;; Fibonacci number bigger than 10**1000
(def ten-to-the-999 (reduce * (repeat 999 10)))
;; the 1 is the 1000th digit.
(defn indexed-fibs-below
  [max]
  (loop [a 1 b 1 acc [] ct 2]
    (if (>= b max) acc
	(recur b (+ a b) (conj acc [ct b]) (inc ct)))))
;;
(first (filter #(< ten-to-the-999 (second %))
	       (indexed-fibs-below ten-to-the-thousand)))
;; gives 4782 and a super long number. 4782 is the answer.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 026
;; Recurring cycles in decimal fractions
;;
(defn cycle-for
  [d]
  (loop [i 0 n 10 acc #{}]
    (if (contains? acc [(rem n d) (int (/ n d))])
      acc
      (recur (inc i)
	     (* 10 (rem n d))
	     (if (< i 4) #{} (conj acc [(rem n d) (int (/ n d))]))))))
;;
;; (map #(count (cycle-for %)) (range 1 1000)) gives a bunch of cycle lengths,
;; and the biggest one in the list is 982.  Looking for it in the list
;; gives d == 983, which is the answer.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 027
;; n**2 + a*n + b

(defn consecutive-primes
  [a b]
  (loop [n 0 cnt 0]
    (if (prime? (+ (* n n) (* a n) b))
      (recur (inc n) (inc cnt))
      cnt)))
(defn argmax
  [f coll]
  (reduce #(if (> (f %1) (f %2)) %1 %2) coll))
(time (reduce * (argmax #(apply consecutive-primes %) (for [a (range -999 1000) b (primes-below 1000)] [a b]))))
;; ...gives -59231, which is the answer, in about 10 seconds.  Cool!
;; It's [-61 971], and gives 71 consecutive primes.
;; For fun, I cranked the 1000 up to 2000; it found [-79 1601], like the man said.
;; For fun, I cranked the 2000 up to 4000; should take about 15 minutes; same thing. Alas.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 028
;; Spiral's diagonal sums.
;; Drew out the math in a notebook (problem28.jpg).
(defn p28-sum
  []
  (inc (reduce + (map #(- (* 4
			     (+ (* 2 %) 1)
			     (+ (* 2 %) 1))
			  (* 12 %))
		      (range 1 501)))))
;; (p28-sum) gives 669171001, which is the answer.
;; Actually doing math on paper ftw!
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 029
(defn expt
  "Raises a to the b power.  For example, (expt 2 3) gives 8."
  [a b]
  (reduce * (repeat b a)))
(count (distinct (for [a (range 2 101) b (range 2 101)] (expt a b))))
;; ...gives 9183 very quickly; it is the answer.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 030
;; Fifth powers of its digits.
(def ctoi #(- (int %) (int '\0)))
(defn sum-fifth-powers-of-digits
  [n]
  (reduce + (map #(expt (ctoi %) 5) (str n))))
;;(reduce + (filter #(= % (sum-fifth-powers-of-digits %)) (range 1000000)))
;; gave 443839, which is the answer.
;; I tried scanning another order of magnitude, and it showed nothing new,
;; which at least suggests that I should have just looked for a decent
;; upper bound for this number.  Good enough for government work.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 031
;; Two pounds in coins.
;; Assume that we'll backfill with pence, so only count the 7 larger coins:
(def ways
     (filter #(>= 200 (reduce + (map * [200 100 50 20 10 5 2] %)))
     (for [p200 (range 0 2)
	   p100 (range 0 (- 3 (* 2 p200)))
	   p50 (range 0 (- 5 (* 2 p100) (* 4 p200)))
	   p20 (range 0 (- 11 (* 10 p200) (* 5 p100) (* (/ 5 2) p50)))
	   p10 (range 0 (- 21 (* 20 p200) (* 10 p100) (* 5 p50) (* 2 p20)))
	   p5 (range 0 (- 41 (* 40 p200) (* 20 p100) (* 10 p50) (* 4 p20) (* 2 p10)))
	   p2 (range 0 (- 101 (* 100 p200) (* 50 p100) (* 25 p50) (* 10 p20) (* 5 p10) (* (/ 5 2) p5)))]
       [p200 p100 p50 p20 p10 p5 p2])))
;; gives 73682, which is the answer.
;; I confess to having guessed wrong about a dozen times, only debugging slightly
;; at each try.  It was really ugly; don't ever do this with code that has to run again.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 032
;; 1-9 pandigital products
;; looks annoying
(defn pandigital?
  [& nums]
  (= (sort (apply str nums)) '(\1 \2 \3 \4 \5 \6 \7 \8 \9)))
;;
(reduce + (into #{} (map #(nth % 2)
			 (filter #(apply pandigital? %)
				 (for [a (range 1 988) b (range 12 9877)]
				   [a b (* a b)])))))
;; Takes about 30 seconds to give 45228,  which is the answer.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 033
;; Did this by hand: 16/64 49/98
(defn p33-special?
  [a b]
  (and (= (rem a 10) (int (Math/floor (/ b 10))))
       (< 0 (rem b 10))
       (< a b)
       (= (/ a b) (/ (int (Math/floor (/ a 10))) (rem b 10)))))

(filter #(apply p33-special? %) (for [a (range 10 100) b (range 10 100)] [a b]))
;; ([16 64] [19 95] [26 65] [49 98]) are they.
(reduce * (map #(apply / %) [[16 64] [19 95] [26 65] [49 98]]))
;; gives 1/100, so 100 is the answer.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 034
;; Numbers that are the sum of the factorials of their digits
(map factorial (range 1 10))
(defn factorial-of-digits-of
  [n]
  (reduce + (map factorial (map #(- (int %) (int '\0)) (str n)))))
(filter #(= % (factorial-of-digits-of %)) (range 10000000))
;; gives (1 2 145 40585).  Ignoring 1 and 2, sum is 40730, which is the answer.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 35
;; circular primes
(defn circular-permutations-of
  [n]
  (map #(Long/parseLong %)
       (for [i (range (count (str n)))]
	 (str (.substring (str n) i)
	      (.substring (str n) 0 i)))))
(defn circular-prime?
  [n]
  (every? prime? (circular-permutations-of n)))
(filter circular-prime? (primes-below 1000000))
;; gives 55 pretty fast; it's the answer.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 036
;; Palindromic primes base 10 and 2
;; (See palindrome? from Problem 004.)
;; (See expt from Problem 029.)
(defn to-binary
  [decimal]
   (apply str
	  (loop [n decimal acc [] exp (int (/ (Math/log decimal) (Math/log 2)))]
	    (let [next-divisor (expt 2 exp)
		  next-digit (int (/ n next-divisor))]
	      (if (zero? exp)
		(conj acc n)
		(recur (- n (* next-divisor next-digit))
		       (conj acc next-digit)
		       (dec exp)))))))
(defn binary-palindrome?
  [n]
  (palindrome? (to-binary n)))
(reduce +  (filter binary-palindrome? (filter palindrome? (range 1 1000000))))
;; gives 872187 pretty fast, which is the answer.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 037
;; Right- and left-truncatable primes.
(defn left-truncatable-prime?
  [n]
  (if (< n 10) (prime? n)
      (and (prime? n)
	   (left-truncatable-prime? (int (/ n 10))))))
(defn right-truncatable-prime?
  [n]
  (if (< n 10) (prime? n)
      (and (prime? n)
	   (right-truncatable-prime? (int (Long/parseLong (.substring (str n) 1)))))))
(count (filter #(< 10 %) (filter left-truncatable-prime? (filter right-truncatable-prime? (primes-below 1000000)))))
;; gives eleven numbers
(reduce + (filter #(< 10 %) (filter left-truncatable-prime? (filter right-truncatable-prime? (primes-below 1000000)))))
;; whose sum is 748317, which is the answer.
;; I wonder whether we can prove that there are only eleven? Probably by brute force on 7-digit primes.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 038
;; Pandigital concatenated products.
;; (See pandigital? in Problem 032.)
(defn concatenated-product
  [n]
  (loop [i 1 numbers []]
    (if (< 8 (count (apply str numbers)))
      (Long/parseLong (apply str numbers))
      (recur (inc i) (conj numbers (* n i))))))
(filter pandigital? (map concatenated-product (range 10000000)))

;; gives (123456789 918273645 192384576 219438657 273546819 327654981
;; 672913458 679213584 692713854 726914538 729314586 732914658
;; 769215384 792315846 793215864 926718534 927318546 932718654)
;; and the last one is the largest. It's the answer.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 039
;; Right triangles with integral length sides
;; (See pythagorean? from Problem 009.)
;; (See argmax from Problem 027.)
(defn triples-with-perimeter
  [p]
  (distinct (for [a (range 1 (- p 1 1))
		  b (range a (- p 1 a))]
	      (sort [a b (- p a b)]))))
(defn pythagorean-triples-with-perimeter
  [p]
  (filter #(apply pythagorean? %) (triples-with-perimeter p)))
(argmax #(count (pythagorean-triples-with-perimeter %)) (range 1000))
;; After a while, gives 840, which is the answer. But it took several minutes.:(
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 040
;; Concatenate counting numbers, find digits.
(defn nth-digit-040
  [n]
  (nth (apply str (range 1 (inc n))) (dec n)))
(map nth-digit-040 [1 10 100 1000 10000 100000 1000000])
;; gives (\1 \1 \5 \3 \7 \2 \1), whose product is:
(* 1 1 5 3 7 2 1)
;; and 210 is the answer. weird.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 041
;; Largest n-digit pandigital prime.
(defn biggest-pandigital-prime
  [n]
  (if (and (> n 1) (< n 10))
    (let [primes
	  (filter prime?
		  (map #(Integer/valueOf (apply str %))
		       (clojure.contrib.combinatorics/lex-permutations (range 1 (inc n)))))]
      (if (empty? primes) (str "No " n "-pandigital primes") (apply max primes)))
    "Error: must have 1<n<10"))
;;
;; gives ("Error: must have 1<n<10" "No 2-pandigital primes" "No
;; 3-pandigital primes" 4231 "No 5-pandigital primes" "No 6-pandigital
;; primes" 7652413 "No 8-pandigital primes" "No 9-pandigital primes")
;; so 7652413 is the answer.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 042
;; (See triangle from Problem 012)
(def words (sort (map read-string (.split (slurp "words.txt") ","))))
;; (count words) is 1786, similar to what the man said.
(def triangle-scores (into #{} (map triangle (range 50))))
(defn word-score
  [w]
  (reduce + (map #(inc (- (int %) (int '\A))) w)))
;; so (word-score "SKY") is 55, like the man said,
;; and (contains? triangle-scores (word-score "SKY")) is true.
(count (filter #(contains? triangle-scores %) (map word-score words)))
;; Gives 162, which is the answer.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 043
;; Substring divisibility weirdness.
(defn pandigital-vecs
  [n]
  (clojure.contrib.combinatorics/lex-permutations (range (inc n))))
(defn pandigital-nums
  [n]
  (map #(Integer/valueOf (apply str %)) (pandigital-vecs n)))

(defn p43-divisible?
  [digits]
  (and
   (zero? (rem (Integer/valueOf (apply str (drop 1 (take 4 digits))))   2))
   (zero? (rem (Integer/valueOf (apply str (drop 2 (take 5 digits))))   3))
   (zero? (rem (Integer/valueOf (apply str (drop 3 (take 6 digits))))   5))
   (zero? (rem (Integer/valueOf (apply str (drop 4 (take 7 digits))))   7))
   (zero? (rem (Integer/valueOf (apply str (drop 5 (take 8 digits))))  11))
   (zero? (rem (Integer/valueOf (apply str (drop 6 (take 9 digits))))  13))
   (zero? (rem (Integer/valueOf (apply str (drop 7 (take 10 digits)))) 17))))

(def p43-nums (filter p43-divisible? (pandigital-vecs 9)))
(reduce + (map #(Long/valueOf (apply str %)) p43-nums))
;; after a minute or two, gives 16695334890, which is the answer.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 044
;; Pentagonal numbers
(defn pentagonal
  [n]
  (/ (* n (dec (* 3 n))) 2))

(def pentagonals (map pentagonal (iterate inc 1)))

(defn pentagonal?
  [n]
  (and (> n 0)
       (= n
	  (pentagonal
	   (Math/round (/ (dec (Math/sqrt (inc (* 24 n)))) 6.))))))

(defn n-for-pentagonal
  [num]
  (loop [n (dec (Math/round (Math/sqrt (/ num 1.5))))]
    (let [num-candidate (/ (* n (dec (* 3 n))) 2)]
      (if (< num-candidate num)
	(recur (inc n))
	(if (= num-candidate num)
	  n
	  '())))))

(defn pentagonals-below
  [pent]
  (if (pentagonal? pent)
    (let [n (n-for-pentagonal pent)]
      (map pentagonal (range n 0 -1)))))

(defn differences-below-p-sub
  [n]
  (loop [ps (pentagonal n)
	 pk-candidates (take-while #(> % (/ ps 2)) (pentagonals-below ps))
	 quads []]
    (if (empty? pk-candidates) quads
	(let [pk (first pk-candidates)
	      pj (- ps pk)
	      pd (- pk pj)]
	  (if true;;(or (> pj pd) (> pd 5482660)) quads
	      (if (and (pentagonal? pd) (pentagonal? pj))
		(recur ps
		       (rest pk-candidates)
		       (conj quads [pd pj pk ps]))
		(recur ps
		       (rest pk-candidates)
		       quads)))))))

;;(def pentagonal? (memoize pentagonal?))
;; (map pentagonal? [5482660 1560090 7042750 8602840]) is the first useful output.
;; I've spent two nights not typing in the first number, there; I had
;; instead been typing in the second number.  How infuriating.
;; 5482660 is the answer.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 045
;; Second triangle number that's also pentagonal and hexagonal
;; See (triangle) from Problem 012.
;; See (pentagonal) and (pentagonal?) from Problem 044.
(defn hexagonal
  [n]
  (* n (dec (* 2 n))))

(defn hexagonal?
  [num]
  (and (> num 0)
       (= num
	  (hexagonal
	   (Math/round (/ (inc (Math/sqrt (inc (* 8 num)))) 4))))))

;; (take 3 (filter hexagonal? (filter pentagonal? (map triangle (iterate inc 1)))))
;; gives 1533776805, which is the answer.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 046
;; Goldbach: first odd composite that isn't a prime plus twice a square.
;; See primes stuff from Problem 007.
(def odd-composites
     (filter (complement prime?) (iterate #(+ 2 %) 1)))

(defn square?
  [n]
  (let [sqrt (Math/round (Math/sqrt n))]
    (= n (* sqrt sqrt))))

(defn is-sum-of-prime-and-twice-square?
  [n]
  (loop [primes (primes-below n)]
    (if (empty? primes)
      false
      (if (square? (/ (- n (first primes)) 2))
	[n (first primes)]
	(recur (rest primes))))))

;; (take 3 (filter (complement is-sum-of-prime-and-twice-square?) odd-composites))
;; quickly gives (1 5777 5993). 5777 is the answer.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 047
;; More prime factors stuff.  Redefining this from above:
(defn prime-factorization-of
  [n]
  (if (< n 2) []
  (let [primes (primes-below (inc (/ n 2)))]
    (loop [remainder n remaining-primes primes factorization []]
      (if (empty? remaining-primes)
	(if (empty? factorization) [n] factorization)
	(if (zero? (mod remainder (first remaining-primes)))
	  (recur (/ remainder (first remaining-primes))
		 remaining-primes
		 (conj factorization (first remaining-primes)))
	  (recur remainder
		 (rest remaining-primes)
		 factorization)))))))

;;(map #(into [] [(reduce * %) (count (distinct %))]) (map prime-factorization-of (range 1000)))
;; gives a list of [n (count (distinct (prime-factorization-of n)))]
;; so we filter by count and look for runs of n instances of n...



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; intermission
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;
;; Problem 097
;; last ten digits of 28433 * 2**7830451 + 1
;; (inc (* 28433 (nth (iterate #(let [n (* 2 %)] (mod n 1000000000000)) 1) 7830457)))
;; gives 23875198739992577, whose last ten digits are 8739992577, which is the answer.
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 048
;; Last ten digits of 1**1 + 2**2 + ... + 1000**1000

(defn pow
  [base exponent]
  (nth (iterate #(* base %) 1) exponent))

(reduce + (map #(pow % %) (range 1 1001)))
;; gives an exceptionally long number whose last
;; ten digits are 9110846700, which is the answer
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Problem 056
;; maximal digital sum of a**b for a,b < 100
;; See pow from Problem 048, above.
(defn digital-sum
  [n]
  (reduce + (map #(- (int %) (int '\0)) (str n))))

(apply max (for [a (range 99 0 -1) b (range 99 0 -1)] (digital-sum (pow a b))))
;; very quickly gives 972, which is the answer.
;;;;;;;;;;;;;;