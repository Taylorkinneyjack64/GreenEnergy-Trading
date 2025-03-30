;; GreenEnergy-Trading Contract
;; Enables tracking and trading of renewable energy credits

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-credits (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-unauthorized (err u104))

;; Data Variables
(define-data-var credit-price uint u1000) ;; Price in STX per credit

;; Data Maps
(define-map energy-producers 
    principal 
    {
        is-verified: bool,
        total-production: uint,
        available-credits: uint
    }
)

(define-map credit-holdings
    principal
    uint
)

(define-map trading-history
    {seller: principal, buyer: principal}
    {amount: uint, price: uint, timestamp: uint}
)

;; Public Functions

;; Register a new energy producer
(define-public (register-producer)
    (begin
        (asserts! (is-none (map-get? energy-producers tx-sender)) (err u105))
        (ok (map-set energy-producers tx-sender 
            {
                is-verified: false,
                total-production: u0,
                available-credits: u0
            }))
    )
)

;; Verify an energy producer (owner only)
(define-public (verify-producer (producer principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-some (map-get? energy-producers producer)) err-not-found)
        (ok (map-set energy-producers producer
            (merge (unwrap-panic (map-get? energy-producers producer))
                  {is-verified: true})))
    )
)

;; Record energy production and mint credits
(define-public (record-production (amount uint))
    (let (
        (producer-data (unwrap! (map-get? energy-producers tx-sender) err-not-found))
        (is-verified (get is-verified producer-data))
    )
        (asserts! is-verified err-unauthorized)
        (asserts! (> amount u0) err-invalid-amount)
        (ok (map-set energy-producers tx-sender
            {
                is-verified: (get is-verified producer-data),
                total-production: (+ (get total-production producer-data) amount),
                available-credits: (+ (get available-credits producer-data) amount)
            }))
    )
)

;; Buy credits from a producer
(define-public (buy-credits (producer principal) (amount uint))
    (let (
        (producer-data (unwrap! (map-get? energy-producers producer) err-not-found))
        (available (get available-credits producer-data))
        (total-cost (* amount (var-get credit-price)))
    )
        (asserts! (>= available amount) err-insufficient-credits)
        (asserts! (is-eq (stx-transfer? total-cost tx-sender producer) (ok true)) (err u106))
        
        ;; Update producer credits
        (map-set energy-producers producer
            (merge producer-data 
                  {available-credits: (- available amount)}))
        
        ;; Update buyer holdings
        (map-set credit-holdings tx-sender 
            (+ (default-to u0 (map-get? credit-holdings tx-sender)) amount))
        
        ;; Record trade
        (map-set trading-history {seller: producer, buyer: tx-sender}
            {amount: amount, price: total-cost, timestamp: stacks-block-height})
        
        (ok true)
    )
)

;; Read-only functions

;; Get producer information
(define-read-only (get-producer-info (producer principal))
    (map-get? energy-producers producer)
)

;; Get credit balance
(define-read-only (get-credit-balance (holder principal))
    (default-to u0 (map-get? credit-holdings holder))
)

;; Get current credit price
(define-read-only (get-credit-price)
    (var-get credit-price)
)

;; Get trade history
(define-read-only (get-trade-history (seller principal) (buyer principal))
    (map-get? trading-history {seller: seller, buyer: buyer})
)

;; Private functions

;; Update credit price (owner only)
(define-public (update-credit-price (new-price uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> new-price u0) err-invalid-amount)
        (ok (var-set credit-price new-price))
    )
)


