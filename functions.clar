(define-data-var ticket-price uint u1000000) ;; 1 STX = 1,000,000 microSTX
(define-data-var tickets (list 100 principal) (list))
(define-data-var lottery-open bool false)
(define-data-var lottery-admin principal 'SP24Z6ZS8X3ZFS22C51GJ79HKTVJQFS6CFBWWS7YP)
(define-data-var winner (optional principal) none)
(define-data-var token-addresses (map principal principal)
  {
    'SP3NE50GEXFG9SZGTT51P40X2CKYSZ5CC4ZTZ7A2G.welshcorgicoin-token 'SP3NE50GEXFG9SZGTT51P40X2CKYSZ5CC4ZTZ7A2G.welshcorgicoin-token,
    'SP1AY6K3PQV5MRT6R4S671NWW2FRVPKM0BR162CT6.leo-token 'SP1AY6K3PQV5MRT6R4S671NWW2FRVPKM0BR162CT6.leo-token,
    'SP1N4EXSR8DP5GRN2XCWZEW9PR32JHNRYW7MVPNTA.PomerenianBoo-Pomboo 'SP1N4EXSR8DP5GRN2XCWZEW9PR32JHNRYW7MVPNTA.PomerenianBoo-Pomboo
  }
)
(define-data-var lottery-start-block uint u0)
(define-constant two-minutes-in-blocks uint 2)

(define-private (is-admin (sender principal))
  (is-eq sender (var-get lottery-admin))
)

(define-private (has-time-elapsed (start-block uint))
  (>= (- (block-height) start-block) (var-get two-minutes-in-blocks))
)

(define-public (start-lottery)
  (if (is-admin tx-sender)
    (begin
      (var-set lottery-open true)
      (var-set lottery-start-block (block-height))
      (ok u0)
    )
    (err u102) ;; Error: Unauthorized
  )
)

(define-public (buy-ticket (token principal))
  (if (var-get lottery-open)
    (if (not (has-time-elapsed (var-get lottery-start-block)))
      (let ((sender tx-sender))
        (if (map-get? token (var-get token-addresses))
          (let ((token-address (unwrap! (map-get token (var-get token-addresses)) (err u108)))) ;; Error: Unsupported token
            (if (>= (ft-get-balance token-address sender) (var-get ticket-price))
              (if (< (len (var-get tickets)) u100)
                (begin
                  (ft-transfer? token-address (var-get ticket-price) sender (as-contract tx-sender))
                  (var-set tickets (try! (append-principal (var-get tickets) sender)))
                  (ok u0)
                )
                (err u107) ;; Error: Ticket limit reached
              )
              (err u101) ;; Error: Insufficient funds
            )
          )
        )
        (err u109) ;; Error: Token not supported
      )
    )
    (begin
      (var-set lottery-open false)
      (err u100) ;; Error: Lottery is closed
    )
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
          (let ((block-height (block-height)))
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
          (ft-transfer? (unwrap! (map-get token (var-get token-addresses)) (err u108)) prize (as-contract tx-sender) winner-principal)
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
