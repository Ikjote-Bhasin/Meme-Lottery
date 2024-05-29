(define-map tickets
  { round-id: uint, ticket-number: uint }
  { user: principal })

(define-map rounds
  { round-id: uint }
  { end-height: uint, total-tickets: uint })

(define-public (buy-ticket (round-id uint))
  (let ((current-height (block-height))
        (ticket-number (default-to u0 (map-get? tickets { round-id: round-id, ticket-number: 1 }))))
    (map-set tickets { round-id: round-id, ticket-number: (+ ticket-number u1) }
             { user: tx-sender })
    (ok ticket-number)))

(define-public (create-round (end-height uint))
  (let ((round-id (block-height)))
    (map-set rounds { round-id: round-id }
             { end-height: end-height, total-tickets: u0 })
    (ok round-id)))

(define-public (end-round (round-id uint))
  (let ((round-info (unwrap! (map-get? rounds { round-id: round-id }) (err "Round not found"))))
    (asserts! (<= (get end-height round-info) (block-height)) (err "Round not yet ended"))
    (begin
      (let ((winner (select-winner round-id (get end-height round-info))))
        ;; Distribute prize to winner
        (ok winner)))))

(define-private (select-winner (round-id uint) (block-height uint))
  (let ((block-info (unwrap! (get-burn-block-info? block-height) (err "Invalid block height")))
        (total-tickets (unwrap! (map-get? rounds { round-id: round-id }) {total-tickets: u0})))
    (let ((block-hash (get "header-hash" block-info))
          (random-number (mod (hash160 block-hash) total-tickets)))
      (unwrap! (map-get? tickets { round-id: round-id, ticket-number: random-number }) (err "No ticket found")))))

(define (hash160 (input (buff 32)))
  (as-max-len? (sha256 input) 32))

(define-public (get-burn-block-info? (height uint))
  (unwrap! (match (get-burn-block-info? height)
    block-info (ok block-info)
    (err "Invalid block height"))))
