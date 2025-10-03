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