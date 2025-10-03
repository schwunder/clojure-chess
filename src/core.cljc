(ns core)

(defn ranks-odd-and-col-odd? [ri ci]
  (and
    (odd? ri)
    (odd? ci)
    )
  )

(defn ranks-even-and-col-even? [ri ci]
  (and
    (even? ri)
    (even? ci)
    )
  )

(defn black-square? [ri ci]
  (or
    (ranks-odd-and-col-odd? ri ci)
    (ranks-even-and-col-even? ci ri)
    )
  )

(defn ranks-odd-and-col-even? [ri ci]
  (and
    (odd? ri)
    (even? ci)
    )
  )

(defn ranks-even-and-col-odd? [ri ci]
  (and
    (even? ri)
    (odd? ci)
    )
  )

(defn white-square? [ri ci]
  (or
    ( ranks-odd-and-col-even? ri ci)
    (ranks-even-and-col-odd? ci ri)
    )
  )

(def axis-sequence [1  2  3  4  5  6  7  8])

(def positions
  [[8 1] [8 2] [8 3] [8 4] [8 5] [8 6] [8 7] [8 8]
   [7 1] [7 2] [7 3] [7 4] [7 5] [7 6] [7 7] [7 8]
   [2 1] [2 2] [2 3] [2 4] [2 5] [2 6] [2 7] [2 8]
   [1 1] [1 2] [1 3] [1 4] [1 5] [1 6] [1 7] [1 8]])

(def pieces
  ["RB" "HB" "BB" "QB" "KB" "BB" "HB" "RB"
   "PB" "PB" "PB" "PB" "PB" "PB" "PB" "PB"
   "PW" "PW" "PW" "PW" "PW" "PW" "PW" "PW"
   "RW" "HW" "BW" "QW" "KW" "BW" "HW" "RW"])

(def board (zipmap positions pieces))

(def empty-piece "__")

(def black-square "----")

(def white-square "++++")

(defn get-piece [coordinate]
  (get board coordinate empty-piece))

(defn piece-on-square [figure square]
  (str (first square) figure (last square))
  )

(let [x axis-sequence]
  ;; Use doseq to iterate through each ranks, causing a side effect each time.
  (doseq [a x]
    ;; For each ranks, build and print the complete ranks string.
    (println
      (apply str
             ;; `for` generates the sequence of square strings for the current ranks.
             (for [b x]
               (if (black-square? a b)
                 (piece-on-square
                   (get-piece [a b])
                   black-square)
                 (piece-on-square
                   (get-piece [a b])
                   white-square)))))))

(ns core)

;; -------------------------------------------------------------------
;; HELPER FUNCTION
;; -------------------------------------------------------------------

(defn black-square?
  "Checks if a square at [ranks col] should be black."
  [ranks
   col]
  (even?
    (+
      ranks
      col)))

;; -------------------------------------------------------------------
;; MAIN FUNCTION
;; -------------------------------------------------------------------

(defn print-board []
  (let [pieces
        (let [
              black-piece
              {:color :black}

              white-piece
              {:color :white}

              back-rank-types
              [:rook
               :knight
               :bishop
               :queen
               :king
               :bishop
               :knight
               :rook]

              make-piece (fn [color-template piece-type]
                           (assoc
                             color-template
                             :piece-type piece-type)
                           )

              piece-maker (fn [color-template]
                            ;order important signature: (partial f arg1 arg2 ...)
                            (partial
                              make-piece
                              color-template)
                            )

              create-pawn-rank (fn [color-template]
                                 (repeat 8
                                         (make-piece
                                           color-template
                                           :pawn)
                                         )
                                 )

              create-back-rank (fn [color-template]
                                 (map
                                   (piece-maker color-template)
                                   back-rank-types)
                                 )
              ]

          (vec
            (concat
              (create-back-rank black-piece)
              (create-pawn-rank black-piece)
              (create-pawn-rank white-piece)
              (create-back-rank white-piece)
              )
            )
          )

        squares
        (let [ranks (range 8 0 -1)
              files (range 1 9)]
          (vec
            (for [rank ranks
                  file files]
              [rank file])))

        board
        (let [piece-positions
              (concat
                (subvec
                  squares
                  0
                  16)
                (subvec
                  squares
                  48
                  64))]
          (zipmap
            piece-positions
            pieces))

        ;; --- Display Logic ---
        piece-type->unicode
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
                  board
                  [ranks files]
                  {:piece-type :empty, :color nil})

                piece-char
                (get
                  piece-type->unicode
                  (:piece-type piece))

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
(print-board)