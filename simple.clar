;; Simple Lottery Contract

(define-data-var ticket-price uint u1000000) ;; 1 STX = 1,000,000 microSTX
(define-data-var tickets (list 100 principal) (list))
(define-data-var lottery-open bool true)
(define-data-var lottery-admin principal 'SP3FBR2AGK2PB0BR8DS3PZMEGZ9Z2KYGJK6XXPDTB)
(define-data-var winner (optional principal) none)

(define-private (is-admin (sender principal))
  (is-eq sender (var-get lottery-admin))
)

(define-public (buy-ticket)
  (if (var-get lottery-open)
    (let ((sender tx-sender))
      (if (>= (stx-get-balance sender) (var-get ticket-price))
        (begin
          (stx-transfer? (var-get ticket-price) sender (as-contract tx-sender))
          (var-set tickets (append (var-get tickets) (list sender)))
          (ok u0)
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
      (let ((tickets (var-get tickets)))
        (if (> (len tickets) u0)
          (let ((block-height (at-block (get-block-info? id-header-hash (get-block-info? block-height)) block-height)))
            (let ((winner (element-at tickets (mod block-height (len tickets)))))
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
