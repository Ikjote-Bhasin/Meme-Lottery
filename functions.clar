(define-map tickets
  { round-id: uint, ticket-number: uint }
  { user: principal })

(define-map rounds
  { round-id: uint }
  { end-height: uint, total-tickets: uint })

(define-public (buy-ticket (round-id uint))
  (begin
    ;; Validate input
    (asserts! (>= round-id u0) (err "Invalid round id"))
    (let ((round-info (unwrap! (map-get? rounds { round-id: round-id }) (err "Round not found")))
          (current-tickets (get total-tickets round-info))
          (new-ticket-number (+ current-tickets u1)))
      (begin
        (map-set tickets { round-id: round-id, ticket-number: new-ticket-number } { user: tx-sender })
        (map-set rounds { round-id: round-id }
                 { end-height: (get end-height round-info), total-tickets: new-ticket-number })
        (ok new-ticket-number)))))

(define-public (create-round (end-height uint))
  (begin
    ;; Validate input
    (asserts! (>= end-height u0) (err "Invalid end height"))
    (let ((current-block-height block-height))
      (map-set rounds { round-id: current-block-height }
               { end-height: end-height, total-tickets: u0 })
      (ok current-block-height))))

(define-public (end-round (round-id uint))
  (begin
    ;; Validate input
    (asserts! (>= round-id u0) (err "Invalid round id"))
    (let ((round-info (unwrap! (map-get? rounds { round-id: round-id }) (err "Round not found"))))
      (asserts! (<= (get end-height round-info) block-height) (err "Round not yet ended"))
      (let ((winner (select-winner round-id)))
        ;; Distribute prize to winner
        (ok winner)))))

(define-private (select-winner (round-id uint))
  (let ((round-info (unwrap! (map-get? rounds { round-id: round-id }) (err "Round not found")))
        (total-tickets (get total-tickets round-info))
        (block-height block-height)
        (block-info (unwrap! (get-block-info block-height) (err "Invalid block height"))))
    (let ((block-hash (get header-hash block-info))
          (random-number (mod (buffer-to-uint-wrapper (sha256 block-hash)) total-tickets)))
      (unwrap! (map-get? tickets { round-id: round-id, ticket-number: random-number }) (err "No ticket found")))))
