;; Simple Lottery Contract
;; Simple Lottery Contract

(define-data-var ticket-price uint u1000000) ;; 1 STX = 1,000,000 microSTX
(define-data-var tickets (list 100 principal) (list))
(define-data-var lottery-open bool true)
(define-data-var lottery-admin principal 'SP24Z6ZS8X3ZFS22C51GJ79HKTVJQFS6CFBWWS7YP)
(define-data-var winner (optional principal) none)

(define-private (is-admin (sender principal))
  (is-eq sender (var-get lottery-admin))
)

(define-public (buy-ticket)
  (if (var-get lottery-open)
    (let ((sender tx-sender))
      (if (>= (stx-get-balance sender) (var-get ticket-price))
        (if (< (len (var-get tickets)) u100)
          (begin
            (stx-transfer? (var-get ticket-price) sender (as-contract tx-sender))
            (var-set tickets (try! (append-principal (var-get tickets) sender)))
            (ok u0)
          )
          (err u107) ;; Error: Ticket limit reached
        )
        (err u101) ;; Error: Insufficient funds
      )
    )
    (err u100) ;; Error: Lottery is closed
  )
)

(define-public (close-lottery)
  (if (is-admin tx-sender)
    (begin
      (var-set lottery-open false)
      (ok u0)
    )
    (err u102) ;; Error: Unauthorized
  )
)

(define-public (draw-winner)
  (if (is-admin tx-sender)
    (if (not (var-get lottery-open))
      (let ((ticket-list (var-get tickets)))
        (if (> (len ticket-list) u0)
          (let ((block-height (at-block (get-block-info? id-header-hash (get-block-info? block-height)) block-height)))
            (let ((winner (element-at ticket-list (mod block-height (len ticket-list)))))
              (var-set winner (some winner))
              (ok winner)
            )
          )
          (err u104) ;; Error: No tickets sold
        )
      )
      (err u103) ;; Error: Lottery is still open
    )
    (err u102) ;; Error: Unauthorized
  )
)

(define-public (claim-prize)
  (match (var-get winner)
    winner-principal
      (if (is-eq tx-sender winner-principal)
        (let ((prize (* (var-get ticket-price) (len (var-get tickets)))))
          (stx-transfer? prize (as-contract tx-sender) winner-principal)
          (ok u0)
        )
        (err u105) ;; Error: Not the winner
      )
    none (err u106) ;; Error: No winner drawn
  )
)

(define-private (append-principal (ticket-list (list 100 principal)) (ticket principal))
  (if (< (len ticket-list) u100)
    (ok (append-helper ticket-list ticket 0 (len ticket-list)))
    (err u107) ;; Error: Ticket limit reached
  )
)

(define-private (append-helper (ticket-list (list 100 principal)) (ticket principal) (index uint) (len uint))
  (if (>= index len)
    (list ticket)
    (let ((current (nth index ticket-list)))
      (append (list current) (append-helper ticket-list ticket (+ index 1) len))
    )
  )
)
