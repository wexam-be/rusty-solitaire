; Rusty Solitaire - Hearts
;
; The trick-taking game for four, played against three computer players.
; Avoid taking hearts (1 point each) and the queen of spades (13 points),
; or take them all to "shoot the moon". The game ends when someone reaches
; 100 points; the lowest score wins.
;
; Copyright (C) 2026 The Rusty Solitaire authors
; SPDX-License-Identifier: GPL-3.0-or-later
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (aisleriot interface) (aisleriot api))

;; The players, in playing order (clockwise): you at the bottom, then the
;; computer players on the left, at the top and on the right. Player p
;; holds slot p and plays into slot p + 4. Your trick slot also holds the
;; cards you pick to pass.
(define hand-slots '(0 1 2 3))

; The computer players get three of these names in each game.
(define computer-names
  '("Ada" "Bram" "Chloé" "Dario" "Elif" "Femke" "Gus" "Hana" "Ivo" "Jonas"
    "Kiki" "Lars" "Mila" "Nils" "Olga" "Pim" "Rosa" "Sami" "Tess" "Umar"
    "Vera" "Wout" "Yara" "Zeno"))
(define trick-slots '(4 5 6 7))
(define (trick-slot p) (+ p 4))

(define game-length 100)
; How long the computer players think, and how long a finished trick stays
; on the table, in milliseconds.
(define play-delay 550)
(define trick-delay 1300)

(def-save-var player-names (list (G_"You") "West" "North" "East"))
(def-save-var totals '(0 0 0 0))
(def-save-var points '(0 0 0 0))
(def-save-var hand-number 0)
; pass, play, hand-over or game-over
(def-save-var phase 'pass)
(def-save-var turn 0)
(def-save-var leader 0)
(def-save-var tricks-played 0)
(def-save-var hearts-broken #f)
(def-save-var queen-played #f)
(def-save-var received '())
(def-save-var news "")
; The message for the end-of-game dialog when you lose.
(def-save-var game-over-message "")

; The card under the last press, as (slot . card). button-clicked only
; gets the slot.
(define pressed #f)

;; ---- Cards ----

(define (same-card? a b)
  (and (= (get-value a) (get-value b)) (= (get-suit a) (get-suit b))))

(define (queen-of-spades? card)
  (and (= (get-suit card) spade) (= (get-value card) queen)))

(define (two-of-clubs? card)
  (and (= (get-suit card) club) (= (get-value card) 2)))

(define (heart? card) (= (get-suit card) heart))

(define (card-points card)
  (cond ((heart? card) 1)
        ((queen-of-spades? card) 13)
        (else 0)))

(define (without card cards)
  (filter (lambda (c) (not (same-card? c card))) cards))

(define (in-suit suit cards)
  (filter (lambda (c) (= (get-suit c) suit)) cards))

; Clubs, diamonds, spades, hearts: the colours alternate in your hand.
(define (card-key card)
  (+ (* 20 (case (get-suit card) ((0) 0) ((1) 1) ((3) 2) (else 3)))
     (get-value card)))

; Lowest card on the left. A slot's list is top (rightmost) first.
(define (sort-hand cards)
  (sort cards (lambda (a b) (> (card-key a) (card-key b)))))

(define (best cards better?)
  (let loop ((rest (cdr cards)) (pick (car cards)))
    (cond ((null? rest) pick)
          ((better? (car rest) pick) (loop (cdr rest) (car rest)))
          (else (loop (cdr rest) pick)))))

(define (highest cards) (best cards (lambda (a b) (> (get-value a) (get-value b)))))
(define (lowest cards) (best cards (lambda (a b) (< (get-value a) (get-value b)))))

(define (card-names cards)
  (cond ((null? cards) "")
        ((null? (cdr cards)) (get-name (car cards)))
        ((null? (cddr cards))
         (string-append (get-name (car cards)) (G_" and ") (get-name (cadr cards))))
        (else (string-append (get-name (car cards)) ", " (card-names (cdr cards))))))

(define (player-name p) (list-ref player-names p))

(define (add-at lst p n)
  (let loop ((l lst) (i 0))
    (if (null? l)
        '()
        (cons (if (= i p) (+ (car l) n) (car l)) (loop (cdr l) (+ i 1))))))

;; ---- The table ----

(define (place x y)
  (set! HORIZPOS x)
  (set! VERTPOS y))

;; Where things go, in card-sized units. Your hand along the bottom at
;; full size, the computer players' hands as small fans on the other three
;; sides, the trick in the middle with the cards overlapping a little, and
;; the scores in the top left corner.
(define table-width 6.6)
(define table-height 3.9)
(define fan-scale 0.5)
(define trick-scale 0.86)
; The middle of each player's hand and trick card.
(define hand-places '((3.3 3.35) (0.6 1.75) (3.3 0.32) (6.0 1.75)))
(define trick-places '((3.42 1.92) (2.82 1.5) (3.18 1.05) (3.8 1.45)))

(define (place-at x y)
  (place (- x 0.5) (- y 0.5)))

(define (pick-names)
  (let loop ((names computer-names) (picked '()))
    (if (= 3 (length picked))
        (cons (G_"You") (reverse picked))
        (let ((name (list-ref names (aisleriot-random (length names)))))
          (loop (delete name names) (cons name picked))))))

(define (new-game)
  (initialize-playing-area)
  (set-ace-high)
  (apply place-at (car hand-places))
  (add-extended-slot '() right)
  (center-slot! 0)
  (for-each (lambda (p rotation)
              (apply place-at (list-ref hand-places p))
              (add-normal-slot '())
              (set-slot-scale! p fan-scale)
              (set-slot-fan! p 50 rotation)
              (hide-empty-slot! p))
            '(1 2 3) '(90 180 -90))
  (for-each (lambda (p)
              (apply place-at (list-ref trick-places p))
              (if (= p 0)
                  (add-extended-slot '() right)
                  (add-normal-slot '()))
              (set-slot-scale! (trick-slot p) trick-scale)
              (hide-empty-slot! (trick-slot p)))
            hand-slots)
  (set-slot-x-expansion! 0 0.42)
  (set-slot-x-expansion! (trick-slot 0) 0.3)
  (hide-empty-slot! 0)

  (set! player-names (pick-names))
  (set! totals '(0 0 0 0))
  (set! hand-number 0)
  (set! game-over-message "")
  (deal-hand)
  (list table-width table-height))

(define (deal-hand)
  (make-standard-deck)
  (shuffle-deck)
  (let loop ((p 0) (deck DECK))
    (if (< p 4)
        (let ((cards (list-head deck 13)))
          (set-cards! p (sort-hand (if (= p 0) (map make-visible cards) cards)))
          (loop (+ p 1) (list-tail deck 13)))))
  (for-each (lambda (s) (set-cards! s '())) trick-slots)
  (set! points '(0 0 0 0))
  (set! tricks-played 0)
  (set! hearts-broken #f)
  (set! queen-played #f)
  (set! received '())
  (set! news "")
  (set-score! (car totals))
  (if (= 0 (pass-offset))
      (start-play)
      (begin
        (set! phase 'pass)
        (show-status))))

;; ---- Passing ----

; Who player p passes to is p + offset: left, right, across, then no pass.
(define (pass-offset)
  (case (modulo hand-number 4) ((0) 1) ((1) 3) ((2) 2) (else 0)))

(define (pass-target) (player-name (pass-offset)))

(define (chosen) (get-cards (trick-slot 0)))

; How keen a computer player is to get rid of a card.
(define (pass-danger card hand)
  (let ((value (get-value card))
        (suit-length (length (in-suit (get-suit card) hand)))
        (spades (length (in-suit spade hand))))
    (cond ((queen-of-spades? card) 100)
          ((and (= (get-suit card) spade) (>= value king))
           (if (< spades 5) (+ 70 value) value))
          ((heart? card) (+ 10 (* 2 value)))
          ((two-of-clubs? card) 0)
          (else (+ value (if (<= suit-length 3) 12 0))))))

(define (pass-choice hand)
  (list-head (sort hand (lambda (a b) (> (pass-danger a hand) (pass-danger b hand)))) 3))

(define (do-pass)
  (let* ((offset (pass-offset))
         (gifts (cons (chosen) (map (lambda (p) (pass-choice (get-cards p))) '(1 2 3)))))
    (set-cards! (trick-slot 0) '())
    (for-each (lambda (p)
                (set-cards! p (let loop ((hand (get-cards p)) (give (list-ref gifts p)))
                                (if (null? give) hand (loop (without (car give) hand) (cdr give))))))
              '(1 2 3))
    (for-each (lambda (p)
                (let ((to (modulo (+ p offset) 4))
                      (cards (list-ref gifts p)))
                  (set-cards! to (sort-hand (append (if (= to 0)
                                                        (map make-visible cards)
                                                        (map (lambda (c) (list (get-value c) (get-suit c) #f)) cards))
                                                    (get-cards to))))))
              hand-slots)
    (set! received (sort-hand (list-ref gifts (modulo (- 4 offset) 4))))
    (start-play)))

(define (pass-click slot card)
  (cond ((and (= slot 0) card (< (length (chosen)) 3))
         (set-cards! 0 (without card (get-cards 0)))
         (set-cards! (trick-slot 0) (append (get-cards (trick-slot 0)) (list card)))
         (show-status)
         #t)
        ((and (= slot (trick-slot 0)) card)
         (set-cards! (trick-slot 0) (without card (chosen)))
         (set-cards! 0 (sort-hand (cons card (get-cards 0))))
         (show-status)
         #t)
        ((and (= 3 (length (chosen))) (not (= slot 0)))
         (do-pass)
         #t)
        (else #f)))

;; ---- Playing ----

(define (start-play)
  (set! phase 'play)
  (let ((p (or (find (lambda (p) (any two-of-clubs? (get-cards p))) hand-slots) 0)))
    (set! leader p)
    (set! turn p))
  (show-status)
  (schedule-next))

(define (trick-card p) (get-top-card (trick-slot p)))

(define (trick-cards)
  (append-map get-cards trick-slots))

(define (trick-empty?) (null? (trick-cards)))

(define (trick-complete?) (= 4 (length (trick-cards))))

(define (led-suit) (get-suit (trick-card leader)))

(define (trick-winner)
  (let ((suit (led-suit)))
    (best (filter (lambda (p) (and (not (empty-slot? (trick-slot p)))
                                   (= suit (get-suit (trick-card p)))))
                  hand-slots)
          (lambda (a b) (> (get-value (trick-card a)) (get-value (trick-card b)))))))

(define (trick-points)
  (apply + (map card-points (trick-cards))))

; The cards player p may play now.
(define (legal-cards p)
  (let ((hand (get-cards p))
        (first-trick (= tricks-played 0)))
    (if (trick-empty?)
        (cond (first-trick (let ((two (filter two-of-clubs? hand))) (if (null? two) hand two)))
              (hearts-broken hand)
              (else (let ((safe (filter (lambda (c) (not (heart? c))) hand)))
                      (if (null? safe) hand safe))))
        (let ((follow (in-suit (led-suit) hand)))
          (cond ((pair? follow) follow)
                (first-trick (let ((safe (filter (lambda (c) (= 0 (card-points c))) hand)))
                               (if (null? safe) hand safe)))
                (else hand))))))

(define (legal? p card)
  (any (lambda (c) (same-card? c card)) (legal-cards p)))

(define (play-card p card)
  (set-cards! p (without card (get-cards p)))
  (set-cards! (trick-slot p) (list (make-visible card)))
  (if (heart? card) (set! hearts-broken #t))
  (if (queen-of-spades? card) (set! queen-played #t))
  (set! turn (modulo (+ p 1) 4))
  (show-status))

(define (finish-trick)
  (let ((winner (trick-winner))
        (taken (trick-points)))
    (set! points (add-at points winner taken))
    (for-each (lambda (s) (set-cards! s '())) trick-slots)
    (set! tricks-played (+ tricks-played 1))
    (set! leader winner)
    (set! turn winner)
    (set! news (string-append
                (if (= winner 0) (G_"You took the trick") (string-append (player-name winner) (G_" took the trick")))
                (cond ((= taken 0) ". ")
                      ((= taken 1) (G_" (1 point). "))
                      (else (string-append " (" (number->string taken) (G_" points). "))))))
    (if (= tricks-played 13)
        (end-hand)
        (show-status))))

; The next step of the computer players: finish a full trick, or play.
(define (advance)
  (and (eq? phase 'play)
       (cond ((trick-complete?) (finish-trick) #t)
             ((= turn 0) #f)
             (else (play-card turn (choose-card turn)) #t))
       (schedule-next)))

(define (schedule-next)
  (cond ((not (eq? phase 'play)) #f)
        ((delayed-call-pending?) #f)
        ((trick-complete?) (delayed-call advance trick-delay))
        ((not (= turn 0)) (delayed-call advance play-delay))))

(define (play-click slot card)
  (cond ((trick-complete?)
         ; Don't wait for the trick to be cleared.
         (finish-trick)
         (schedule-next)
         #t)
        ((not (= turn 0))
         ; After an undo or redo in the middle of a trick, get it going again.
         (schedule-next)
         #f)
        ((and (= slot 0) card (legal? 0 card))
         (play-card 0 card)
         (schedule-next)
         #t)
        (else #f)))

;; ---- The computer players ----

(define (choose-card p)
  (let ((legal (legal-cards p)))
    (cond ((null? (cdr legal)) (car legal))
          ((trick-empty?) (choose-lead p legal))
          ((= (get-suit (car legal)) (led-suit)) (choose-follow p legal))
          (else (choose-discard p legal)))))

(define (choose-lead p legal)
  (let* ((hand (get-cards p))
         (spades (in-suit spade legal))
         (no-spades (filter (lambda (c) (not (= (get-suit c) spade))) legal))
         (high-spade? (lambda (c) (and (= (get-suit c) spade) (>= (get-value c) queen))))
         (shorter (lambda (a b) (or (< (get-value a) (get-value b))
                                    (and (= (get-value a) (get-value b))
                                         (< (length (in-suit (get-suit a) hand))
                                            (length (in-suit (get-suit b) hand))))))))
    (cond ((and (not queen-played) (pair? spades) (not (any high-spade? hand)))
           ; Flush out the queen of spades.
           (lowest spades))
          ((and (any high-spade? hand) (pair? no-spades))
           (best no-spades shorter))
          (else (best legal shorter)))))

(define (choose-follow p legal)
  (let* ((top (get-value (trick-card (trick-winner))))
         (under (filter (lambda (c) (< (get-value c) top)) legal))
         (safe (filter (lambda (c) (not (queen-of-spades? c))) legal))
         (last? (= 3 (length (trick-cards)))))
    (cond ((and last? (= 0 (trick-points)) (not (= (led-suit) heart)))
           ; Taking the trick is free: get rid of a high card.
           (highest (if (null? safe) legal safe)))
          ((pair? under) (highest under))
          ((= tricks-played 0) (highest legal))
          ((and (= (led-suit) spade) (not queen-played) (not last?))
           ; Someone may still have to drop the queen on this.
           (lowest (if (null? safe) legal safe)))
          (else (highest (if (null? safe) legal safe))))))

(define (choose-discard p legal)
  (let* ((hand (get-cards p))
         (the-queen (filter queen-of-spades? legal))
         (big-spades (filter (lambda (c) (and (= (get-suit c) spade) (> (get-value c) queen))) legal))
         (hearts (in-suit heart legal)))
    (cond ((pair? the-queen) (car the-queen))
          ((and (not queen-played) (pair? big-spades)) (highest big-spades))
          ((pair? hearts) (highest hearts))
          (else (best legal (lambda (a b)
                              (or (> (get-value a) (get-value b))
                                  (and (= (get-value a) (get-value b))
                                       (< (length (in-suit (get-suit a) hand))
                                          (length (in-suit (get-suit b) hand)))))))))))

;; ---- Scoring ----

(define (end-hand)
  (let ((moon (find (lambda (p) (= 26 (list-ref points p))) hand-slots)))
    (set! totals (map (lambda (total p)
                        (+ total (cond ((not moon) (list-ref points p))
                                       ((= p moon) 0)
                                       (else 26))))
                      totals hand-slots))
    (set-score! (car totals))
    (set! hand-number (+ hand-number 1))
    (set! news (string-append
                news
                (cond ((not moon) "")
                      ((= moon 0) (G_"You shot the moon! "))
                      (else (string-append (player-name moon) (G_" shot the moon! "))))
                (G_"Hand over: ")
                (scores-text points)
                ". "))
    (if (any (lambda (t) (>= t game-length)) totals)
        (let ((winner (best hand-slots (lambda (a b) (< (list-ref totals a) (list-ref totals b))))))
          (set! phase 'game-over)
          (set! game-over-message
                (string-append (player-name winner) (G_" won with ")
                               (number->string (list-ref totals winner)) (G_" points; you have ")
                               (number->string (car totals)) ".")))
        (set! phase 'hand-over))
    (show-status)))

(define (next-hand)
  (deal-hand)
  #t)

(define (scores-text scores)
  (let loop ((p 0) (text ""))
    (if (= p 4)
        text
        (loop (+ p 1)
              (string-append text (if (= p 0) "" ", ") (player-name p) " "
                             (number->string (list-ref scores p)))))))

; The names by the computer players' hands, and the score table. Whoever
; is to play (or to receive your passed cards) is highlighted.
(define (show-labels)
  (let* ((active (case phase
                   ((pass) (pass-offset))
                   ((play) (if (trick-complete?) -1 turn))
                   (else -1)))
         (name-label (lambda (p x y align)
                       (list x y (player-name p) align 0.17 (= p active))))
         (row (lambda (p y)
                (list (list 0.1 y (player-name p) 'left 0.12 (= p active))
                      (list 1.15 y (number->string (list-ref points p)) 'right 0.12 #f)
                      (list 1.65 y (number->string (list-ref totals p)) 'right 0.12 #f)))))
    (set-table-labels!
     (append
      (list (name-label 1 0.6 2.42 'center)
            (name-label 2 3.95 0.2 'left)
            (name-label 3 6.0 2.42 'center)
            (list 1.15 0.14 (G_"Hand") 'right 0.1 #f)
            (list 1.65 0.14 (G_"Total") 'right 0.1 #f))
      (append-map (lambda (p) (row p (+ 0.33 (* 0.17 p)))) hand-slots)))))

(define (show-status)
  (show-labels)
  (set-statusbar-message
   (string-append
    (case phase
      ((pass)
       (let ((n (length (chosen))))
         (if (< n 3)
             (string-append (G_"Choose 3 cards to pass to ") (pass-target)
                            " (" (number->string n) (G_" chosen). "))
             (string-append (G_"Click an opponent's hand or press Deal to pass to ")
                            (pass-target) ". "))))
      ((play)
       (string-append
        news
        (if (and (= tricks-played 0) (pair? received) (empty-slot? (trick-slot 0)))
            (string-append (G_"You received ") (card-names received) ". ")
            "")
        (cond ((trick-complete?) "")
              ((= turn 0) (if (trick-empty?) (G_"Your lead. ") (G_"Your turn. ")))
              (else ""))))
      ((hand-over) (string-append news (G_"Click or press Deal for the next hand. ")))
      (else (string-append news (G_"Game over. ")))))))

;; ---- The lambdas ----

(define (button-pressed slot-id card-list)
  (set! pressed (and (pair? card-list) (cons slot-id (car (reverse card-list)))))
  #f)

(define (button-released start-slot card-list end-slot) #f)

(define (button-clicked slot-id)
  (let ((card (and pressed
                   (= (car pressed) slot-id)
                   (find (lambda (c) (same-card? c (cdr pressed))) (get-cards slot-id)))))
    (set! pressed #f)
    (case phase
      ((pass) (pass-click slot-id card))
      ((play) (play-click slot-id card))
      ((hand-over) (next-hand))
      (else #f))))

(define (button-double-clicked slot-id)
  (button-clicked slot-id))

(define (dealable?)
  (or (and (eq? phase 'pass) (= 3 (length (chosen))))
      (eq? phase 'hand-over)))

(define (do-deal-next-cards)
  (cond ((and (eq? phase 'pass) (= 3 (length (chosen)))) (do-pass))
        ((eq? phase 'hand-over) (next-hand))))

(define (game-over)
  (not (eq? phase 'game-over)))

(define (game-won)
  (let ((mine (car totals)))
    (every (lambda (t) (<= mine t)) totals)))

(define (get-hint)
  (case phase
    ((pass)
     (let ((hand (append (get-cards 0) (chosen))))
       (list 0 (string-append (G_"Consider passing ") (card-names (pass-choice hand))))))
    ((play)
     (cond ((trick-complete?) (list 0 (G_"Click to clear the trick")))
           ((= turn 0) (list 0 (string-append (G_"Consider playing ") (get-name (choose-card 0)))))
           (else (list 0 (G_"Wait for the other players")))))
    ((hand-over) (list 0 (G_"Click or press Deal for the next hand")))
    (else (list 0 (G_"The game is over")))))

(define (get-options) '())

(define (apply-options options) #f)

(define (timeout) #f)

(set-features dealable-feature)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout dealable?)
