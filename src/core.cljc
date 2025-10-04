(ns core)

(def initial-pieces
  {[1 1] {:color :white, :type :rook}   [1 2] {:color :white, :type :knight}
   [1 3] {:color :white, :type :bishop} [1 4] {:color :white, :type :queen}
   [1 5] {:color :white, :type :king}   [1 6] {:color :white, :type :bishop}
   [1 7] {:color :white, :type :knight} [1 8] {:color :white, :type :rook}
   [2 1] {:color :white, :type :pawn}   [2 2] {:color :white, :type :pawn}
   [2 3] {:color :white, :type :pawn}   [2 4] {:color :white, :type :pawn}
   [2 5] {:color :white, :type :pawn}   [2 6] {:color :white, :type :pawn}
   [2 7] {:color :white, :type :pawn}   [2 8] {:color :white, :type :pawn}
   [7 1] {:color :black, :type :pawn}   [7 2] {:color :black, :type :pawn}
   [7 3] {:color :black, :type :pawn}   [7 4] {:color :black, :type :pawn}
   [7 5] {:color :black, :type :pawn}   [7 6] {:color :black, :type :pawn}
   [7 7] {:color :black, :type :pawn}   [7 8] {:color :black, :type :pawn}
   [8 1] {:color :black, :type :rook}   [8 2] {:color :black, :type :knight}
   [8 3] {:color :black, :type :bishop} [8 4] {:color :black, :type :queen}
   [8 5] {:color :black, :type :king}   [8 6] {:color :black, :type :bishop}
   [8 7] {:color :black, :type :knight} [8 8] {:color :black, :type :rook}})

(defn print-board [pieces]
  (let [light-square  "\u001b[48;5;222m"
        dark-square   "\u001b[48;5;94m"
        white-piece   "\u001b[38;5;252m"
        black-piece   "\u001b[38;5;232m"
        ansi-reset    "\u001b[0m"
        piece->unicode {:rook " ♜ ", :knight " ♞ ", :bishop " ♝ ", :queen " ♛ ", :king " ♚ ", :pawn " ♟︎ "}
        get-square-color (fn [rank file] (if (even? (+ rank file)) dark-square light-square))
        get-piece-color (fn [piece] (if (= :black (:color piece)) black-piece white-piece))]
    (doseq [rank (range 8 0 -1)]
      (print (str " " rank " "))
      (doseq [file (range 1 9)]
        (let [background (get-square-color rank file)
              square-string (if-let [piece (get pieces [rank file])]
                           (str background (get-piece-color piece) (get piece->unicode (:type piece)) ansi-reset)
                           (str background "   " ansi-reset))]
          (print square-string)))
      (println))
    (println "    a  b  c  d  e  f  g  h ")))

(defn move-piece [pieces from to]
  (if-let [piece (get pieces from)]
    (-> pieces
        (dissoc from)
        (assoc to piece))
    pieces))

(def legal? true)
(def test-piece  {[1 1] {:color :white, :type :rook} })

(defn on-board? [from to]
  (let [valid-numbers #{1 2 3 4 5 6 7 8}]
    (every? valid-numbers (concat from to))))

(defn vector-difference [from to]
  (let [[from-x from-y] from
        [to-x to-y] to]
    [(- to-x from-x) (- to-y from-y)]))

(defn normalize-vector [[x y]]
  (letfn [(to-unit [n]
            (cond
              (> n 0) 1
              (< n 0) -1
              :else   0))]
    [(to-unit x) (to-unit y)]))

(defn legal-rook-move? [normalized-vector]
  (let [rook-moves #{[1 0] [-1 0] [0 1] [0 -1]}]
    (contains? rook-moves normalized-vector)))

(defn legal-bishop-move? [normalized-vector]
  (let [bishop-moves #{[1 1] [1 -1] [-1 1] [-1 -1]}]
    (contains? bishop-moves normalized-vector)))

(defn legal-queen-move? [normalized-vector]
  (let [queen-moves #{[1 0] [-1 0] [0 1] [0 -1]
                      [1 1] [1 -1] [-1 1] [-1 -1]}]
    (contains? queen-moves normalized-vector)))

(defn legal-king-move? [difference-vector]
  (let [king-moves #{[1 0] [-1 0] [0 1] [0 -1]
                     [1 1] [1 -1] [-1 1] [-1 -1]}]
    (contains? king-moves difference-vector)))

(defn legal-knight-move? [difference-vector]
  (let [knight-moves #{[2 1] [2 -1] [-2 1] [-2 -1]
                       [1 2] [1 -2] [-1 2] [-1 -2]}]
    (contains? knight-moves difference-vector)))

(defn legal-pawn-move? [difference-vector]
  (let [pawn-moves #{[0 1]}]
    (contains? pawn-moves difference-vector)))

(defn piece-vector [piece from to]
  (let [norm-need #{:rook :bishop :queen}
        dif-vec (vector-difference from to)]
    (if (contains? norm-need (:type piece)) (normalize-vector dif-vec) dif-vec)
    )
  )

(defn chess-game [pieces unchecked-move piece-rules-legal?]
  (let [next-pieces-state (if piece-rules-legal?
                     (unchecked-move pieces [3 3] [4 4])
                     pieces)]
    (print-board next-pieces-state))
  )

(println "Move Tests:")
(chess-game test-piece move-piece legal?)

(def from [1 1])
(def to [1 8])

(def dif (vector-difference from to))         ; -> [0 -7]
(def norm-dif (normalize-vector dif)) ; -> [0 -1]

(vector-difference from to)
(normalize-vector dif)
(legal-rook-move? norm-dif)

(legal-rook-move? [1 1])

(legal-bishop-move? norm-dif)

(legal-bishop-move? [1 1])

(legal-queen-move? norm-dif)

(legal-queen-move? [1 1])

(piece-vector (get test-piece [1 1]) [1 1] [1 8])



;blocking rules. same piece blocks path. opponent piece blocks path only for rook bishop queen. pinned piece rules. time rule input expected. no change rule
;pawn rules should be map of two sets take rule set and move rule set and first move rule set later can move [0 2]
;translation of input to coords
