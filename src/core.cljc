(ns core)
(defn black-square?
  [rank
   file]
  (even?
    (+
      rank
      file)))
(def ranks (range 8 0 -1))
(def files (range 1 9))
(def squares (vec
                         (for [rank ranks
                               file files]
                           [rank file])))
(def initial-board
  {[1 1] {:color :white, :type :rook}
   [1 2] {:color :white, :type :knight}
   [1 3] {:color :white, :type :bishop}
   [1 4] {:color :white, :type :queen}
   [1 5] {:color :white, :type :king}
   [1 6] {:color :white, :type :bishop}
   [1 7] {:color :white, :type :knight}
   [1 8] {:color :white, :type :rook}
   [2 1] {:color :white, :type :pawn}
   [2 2] {:color :white, :type :pawn}
   [2 3] {:color :white, :type :pawn}
   [2 4] {:color :white, :type :pawn}
   [2 5] {:color :white, :type :pawn}
   [2 6] {:color :white, :type :pawn}
   [2 7] {:color :white, :type :pawn}
   [2 8] {:color :white, :type :pawn}
   [7 1] {:color :black, :type :pawn}
   [7 2] {:color :black, :type :pawn}
   [7 3] {:color :black, :type :pawn}
   [7 4] {:color :black, :type :pawn}
   [7 5] {:color :black, :type :pawn}
   [7 6] {:color :black, :type :pawn}
   [7 7] {:color :black, :type :pawn}
   [7 8] {:color :black, :type :pawn}
   [8 1] {:color :black, :type :rook}
   [8 2] {:color :black, :type :knight}
   [8 3] {:color :black, :type :bishop}
   [8 4] {:color :black, :type :queen}
   [8 5] {:color :black, :type :king}
   [8 6] {:color :black, :type :bishop}
   [8 7] {:color :black, :type :knight}
   [8 8] {:color :black, :type :rook}})
(print initial-board)
;; -------------------------------------------------------------------
;; MAIN FUNCTION
;; -------------------------------------------------------------------

(defn print-board [pieces-with-squares]
  (let [

        ;; --- Display Logic ---
        type->unicode
        {:rook "♜"
         :knight "♞"
         :bishop "♝"
         :queen "♛"
         :king "♚"
         :pawn "♟︎"
         :empty " "}

        ansi-reset
        "\u001b[0m"

        fg-black-piece
        "\u001b[38;5;254m"

        fg-white-piece
        "\u001b[38;5;235m"

        bg-dark-sq
        "\u001b[48;5;94m"

        bg-light-sq
        "\u001b[48;5;222m"

        format-square
        (fn [ranks
             files]
          (let [piece
                (get
                  pieces-with-squares
                  [ranks files]
                  {:type :empty, :color nil})

                piece-char
                (get
                  type->unicode
                  (:type piece))

                bg-color
                (if (black-square? ranks files)
                  bg-dark-sq
                  bg-light-sq)

                fg-color
                (case (:color piece)
                  :black fg-black-piece
                  :white fg-white-piece
                  "")]
            (str
              bg-color
              fg-color
              " "
              piece-char
              " "
              ansi-reset)))]

    ;; --- Execution Loop ---
    (println)
    (doseq [ranks (range 8 0 -1)]
      (print
        (str
          " "
          ranks
          " "))
      (doseq [files (range 1 9)]
        (print
          (format-square
            ranks
            files)))
      (println))

    (println
      "    a  b  c  d  e  f  g  h")
    (println)))

;; Run it!
(print-board initial-pieces-on-squares)

(let [new-board {[0 0] "kw" [0 1] "kb"}]
  (-> new-board
      (assoc [0 0] "__")
      (assoc [0 1] "kw")))

