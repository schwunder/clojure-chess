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

; translation between user input and move piece

(defn move-piece [pieces from to]
  (if-let [piece (get pieces from)]
    (-> pieces
        (dissoc from)
        (assoc to piece))
    pieces))

(def legal? true)
(def one-rook  {[1 1] {:color :white, :type :rook} })

(defn chess-game [pieces unchecked-move piece-rules-legal?]
  (let [next-pieces-state (if piece-rules-legal?
                     (unchecked-move pieces [3 3] [4 4])
                     pieces)]
    (print-board next-pieces-state))
  )

(println "Move Tests:")
(chess-game one-rook move-piece legal?)


; game mechanics rule: all pieces: rule two: both of these do not crash the game and leave the same as before. this is more a player issue.
; so check if a move happened this is a more meta rule than a move rule. so moving to the same square does not change the state therefore you did not move. this is illegal.
; same with taking an empty piece or the air nothing happens empty move no state change illegal
; other game mechanics rules are general wrong inputs etc. throwing the computer out the window. no playing i.e. maybe time chess clock

;move rules: all pieces: rule one: take from the board and stay on the board
(defn in-bounds? [square]
  (let [[file rank] square]
    (and
      (and (< 0 file) (> 9 file))
      (and (< 0 rank) (> 9 rank)))
    )
  )

(defn on-board? [from to]
  (and
    (in-bounds? from)
    (in-bounds? to))
  )

;sidenote castling will be done later

(def legal-rook-moves #{[1 0] [-1 0] [0 1] [0 -1]})
(def legal-bishop-moves #{[1 1] [1 -1] [-1 1] [-1 -1]})
(def legal-queen-moves (into legal-rook-moves legal-bishop-moves))
;legal king moves non normalized
(def legal-king-moves legal-queen-moves)

;legal pawn moves non normalized.
;extra map with two sets takes moves and normal moves. on passant and moving two in the beginning come later



(defn legal-rook-move? [normalized-vector]
  (contains? legal-rook-moves normalized-vector))

(defn legal-bishop-move? [normalized-vector]
  (contains? legal-bishop-moves normalized-vector))

(defn legal-queen-move? [normalized-vector]
  (contains? legal-queen-moves normalized-vector))

(def from [1 1])
(def to [1 8])

(defn vector-dif [from to]
  (let [[from-x from-y] from
        [to-x to-y] to]
    [(- from-x to-x) (- from-y to-y)]))

(defn normalize-vector [v]
  (letfn [(to-unit [n]
            (cond
              (> n 0) 1
              (< n 0) -1
              :else   0))]
    (vec (map to-unit v))))

(def dif (vector-dif from to))         ; -> [0 -7]
(def norm-dif (normalize-vector dif)) ; -> [0 -1]

(legal-rook-move? norm-dif)

(legal-rook-move? [1 1])

(legal-bishop-move? norm-dif)

(legal-bishop-move? [1 1])

(legal-queen-move? norm-dif)

(legal-queen-move? [1 1])






