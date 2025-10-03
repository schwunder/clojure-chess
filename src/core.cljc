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
        get-background-color (fn [rank file] (if (even? (+ rank file)) dark-square light-square))
        get-piece-color (fn [piece] (if (= :black (:color piece)) black-piece white-piece))]
    (doseq [rank (range 8 0 -1)]
      (print (str " " rank " "))
      (doseq [file (range 1 9)]
        (let [background (get-background-color rank file)
              ;; The string to print is now determined by one conditional
              square-string (if-let [piece (get pieces [rank file])]
                           (str background (get-piece-color piece) (get piece->unicode (:type piece)) ansi-reset)
                           (str background "   " ansi-reset))]
          (print square-string)))
      (println))
    (println "    a  b  c  d  e  f  g  h ")))

(print-board initial-pieces)