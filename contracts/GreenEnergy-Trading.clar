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


;; ==============================================================================


(define-public (transfer-credits (recipient principal) (amount uint))
    (let (
        (sender-balance (default-to u0 (map-get? credit-holdings tx-sender)))
    )
        (asserts! (>= sender-balance amount) err-insufficient-credits)
        (map-set credit-holdings tx-sender (- sender-balance amount))
        (map-set credit-holdings recipient 
            (+ (default-to u0 (map-get? credit-holdings recipient)) amount))
        (ok true)
    )
)



(define-map producer-ratings
    {producer: principal, rater: principal}
    {rating: uint, timestamp: uint}
)

(define-public (rate-producer (producer principal) (rating uint))
    (begin
        (asserts! (and (>= rating u1) (<= rating u5)) (err u109))
        (asserts! (is-some (map-get? energy-producers producer)) err-not-found)
        (ok (map-set producer-ratings
            {producer: producer, rater: tx-sender}
            {rating: rating, timestamp: stacks-block-height}))
    )
)

(define-read-only (get-producer-rating (producer principal) (rater principal))
    (map-get? producer-ratings {producer: producer, rater: rater})
)


(define-constant credit-expiration-blocks u52560)

(define-map credit-expiry
    principal
    {block-height: uint, amount: uint}
)

(define-public (set-credit-expiry (amount uint))
    (begin
        (asserts! (>= (get-credit-balance tx-sender) amount) err-insufficient-credits)
        (ok (map-set credit-expiry tx-sender
            {block-height: (+ stacks-block-height credit-expiration-blocks), amount: amount}))
    )
)

(define-read-only (check-expired-credits (holder principal))
    (match (map-get? credit-expiry holder)
        expired (> stacks-block-height (get block-height expired))
        false
    )
)


(define-map discount-tiers
    uint
    uint
)

(define-data-var tier-threshold uint u100)

(define-public (set-discount-tier (purchase-amount uint) (discount-percentage uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= discount-percentage u50) (err u110))
        (ok (map-set discount-tiers purchase-amount discount-percentage))
    )
)

(define-read-only (calculate-discounted-price (amount uint))
    (let (
        (base-price (* amount (var-get credit-price)))
        (discount-rate (default-to u0 (map-get? discount-tiers amount)))
    )
        (- base-price (/ (* base-price discount-rate) u100))
    )
)


(define-map certification-levels
    principal
    {level: uint, certified-at: uint}
)

(define-constant max-certification-level u3)

(define-public (update-certification-level (producer principal) (new-level uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-level max-certification-level) (err u111))
        (asserts! (is-some (map-get? energy-producers producer)) err-not-found)
        (ok (map-set certification-levels producer
            {level: new-level, certified-at: stacks-block-height}))
    )
)

(define-read-only (get-certification-level (producer principal))
    (default-to 
        {level: u0, certified-at: u0}
        (map-get? certification-levels producer)
    )
)



(define-map staking-positions
    principal
    {amount: uint, start-height: uint, last-claim: uint}
)

(define-constant blocks-per-cycle u144)
(define-constant reward-rate u5)

(define-public (stake-credits (amount uint))
    (let (
        (balance (default-to u0 (map-get? credit-holdings tx-sender)))
    )
        (asserts! (>= balance amount) err-insufficient-credits)
        (map-set credit-holdings tx-sender (- balance amount))
        (map-set staking-positions tx-sender
            {
                amount: amount,
                start-height: stacks-block-height,
                last-claim: stacks-block-height
            })
        (ok true)
    )
)

(define-public (claim-staking-rewards)
    (let (
        (position (unwrap! (map-get? staking-positions tx-sender) err-not-found))
        (cycles-elapsed (/ (- stacks-block-height (get last-claim position)) blocks-per-cycle))
        (reward-amount (/ (* (get amount position) reward-rate cycles-elapsed) u100))
    )
        (asserts! (> cycles-elapsed u0) (err u130))
        (map-set credit-holdings tx-sender 
            (+ (default-to u0 (map-get? credit-holdings tx-sender)) reward-amount))
        (map-set staking-positions tx-sender
            (merge position {last-claim: stacks-block-height}))
        (ok reward-amount)
    )
)

(define-public (unstake-credits)
    (let (
        (position (unwrap! (map-get? staking-positions tx-sender) err-not-found))
    )
        (map-set credit-holdings tx-sender 
            (+ (default-to u0 (map-get? credit-holdings tx-sender)) (get amount position)))
        (map-delete staking-positions tx-sender)
        (ok true)
    )
)

(define-read-only (get-staking-rewards (holder principal))
    (let (
        (position (unwrap! (map-get? staking-positions holder) err-not-found))
        (cycles-elapsed (/ (- stacks-block-height (get last-claim position)) blocks-per-cycle))
        (reward-amount (/ (* (get amount position) reward-rate cycles-elapsed) u100))
    )
        (ok reward-amount)
    )
)


(define-read-only (get-staking-balance (holder principal))
    (default-to u0 (map-get? credit-holdings holder))
)


(define-read-only (get-staking-position (holder principal))
    (default-to 
        {amount: u0, start-height: u0, last-claim: u0}
        (map-get? staking-positions holder)
    )
)

(define-read-only (get-staking-history (holder principal))
    (map-get? staking-positions holder)
)

(define-read-only (get-staking-positions)
    (map-get? staking-positions tx-sender)
)
