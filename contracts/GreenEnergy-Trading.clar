;; GreenEnergy-Trading Contract
;; Enables tracking and trading of renewable energy credits

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-credits (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-unauthorized (err u104))


(define-map offset-requirements
    principal
    {
        required-amount: uint,
        deadline-block: uint,
        max-price-per-credit: uint,
        fulfilled-amount: uint,
        compliance-status: (string-ascii 20)
    }
)

(define-map offset-purchases
    {buyer: principal, purchase-id: uint}
    {
        amount: uint,
        price-paid: uint,
        purchase-block: uint,
        seller: principal
    }
)

(define-map compliance-reports
    {company: principal, period: uint}
    {
        required: uint,
        purchased: uint,
        compliance-percentage: uint,
        report-generated: uint
    }
)

(define-data-var purchase-id-nonce uint u0)
(define-data-var compliance-period uint u1)

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



(define-map market-listings
    uint
    {
        seller: principal,
        amount: uint,
        price-per-credit: uint,
        active: bool
    }
)

(define-data-var listing-nonce uint u0)

(define-public (create-market-listing (amount uint) (price-per-credit uint))
    (let (
        (seller-credits (default-to u0 (map-get? credit-holdings tx-sender)))
        (current-nonce (var-get listing-nonce))
    )
        (asserts! (>= seller-credits amount) err-insufficient-credits)
        (asserts! (> price-per-credit u0) err-invalid-amount)
        
        (map-set credit-holdings tx-sender (- seller-credits amount))
        
        (map-set market-listings current-nonce
            {
                seller: tx-sender,
                amount: amount,
                price-per-credit: price-per-credit,
                active: true
            })
            
        (var-set listing-nonce (+ current-nonce u1))
        (ok current-nonce)
    )
)

(define-public (purchase-listed-credits (listing-id uint) (purchase-amount uint))
    (let (
        (listing (unwrap! (map-get? market-listings listing-id) err-not-found))
        (total-cost (* purchase-amount (get price-per-credit listing)))
    )
        (asserts! (get active listing) (err u140))
        (asserts! (<= purchase-amount (get amount listing)) err-insufficient-credits)
        (asserts! (is-eq (stx-transfer? total-cost tx-sender (get seller listing)) (ok true)) (err u141))
        
        (map-set credit-holdings tx-sender 
            (+ (default-to u0 (map-get? credit-holdings tx-sender)) purchase-amount))
        
        (if (is-eq purchase-amount (get amount listing))
            (map-set market-listings listing-id (merge listing {active: false}))
            (map-set market-listings listing-id 
                (merge listing {amount: (- (get amount listing) purchase-amount)}))
        )
        
        (ok true)
    )
)



(define-public (register-offset-requirement (required-amount uint) (deadline-blocks uint) (max-price uint))
    (begin
        (asserts! (> required-amount u0) err-invalid-amount)
        (asserts! (> max-price u0) err-invalid-amount)
        (ok (map-set offset-requirements tx-sender
            {
                required-amount: required-amount,
                deadline-block: (+ stacks-block-height deadline-blocks),
                max-price-per-credit: max-price,
                fulfilled-amount: u0,
                compliance-status: "pending"
            }))
    )
)

(define-public (auto-purchase-offsets (seller principal) (amount uint))
    (let (
        (requirement (unwrap! (map-get? offset-requirements tx-sender) err-not-found))
        (seller-credits (default-to u0 (map-get? credit-holdings seller)))
        (current-price (var-get credit-price))
        (total-cost (* amount current-price))
        (current-purchase-id (var-get purchase-id-nonce))
        (remaining-need (- (get required-amount requirement) (get fulfilled-amount requirement)))
    )
        (asserts! (<= current-price (get max-price-per-credit requirement)) (err u200))
        (asserts! (>= seller-credits amount) err-insufficient-credits)
        (asserts! (<= amount remaining-need) (err u201))
        (asserts! (< stacks-block-height (get deadline-block requirement)) (err u202))
        
        (try! (stx-transfer? total-cost tx-sender seller))
        
        (map-set credit-holdings seller (- seller-credits amount))
        (map-set credit-holdings tx-sender 
            (+ (default-to u0 (map-get? credit-holdings tx-sender)) amount))
        
        (map-set offset-purchases {buyer: tx-sender, purchase-id: current-purchase-id}
            {
                amount: amount,
                price-paid: total-cost,
                purchase-block: stacks-block-height,
                seller: seller
            })
        
        (let (
            (new-fulfilled (+ (get fulfilled-amount requirement) amount))
            (new-status (if (>= new-fulfilled (get required-amount requirement)) "compliant" "partial"))
        )
            (map-set offset-requirements tx-sender
                (merge requirement 
                    {fulfilled-amount: new-fulfilled, compliance-status: new-status}))
        )
        
        (var-set purchase-id-nonce (+ current-purchase-id u1))
        (ok current-purchase-id)
    )
)

(define-public (generate-compliance-report)
    (let (
        (requirement (unwrap! (map-get? offset-requirements tx-sender) err-not-found))
        (current-period (var-get compliance-period))
        (compliance-pct (/ (* (get fulfilled-amount requirement) u100) (get required-amount requirement)))
    )
        (map-set compliance-reports {company: tx-sender, period: current-period}
            {
                required: (get required-amount requirement),
                purchased: (get fulfilled-amount requirement),
                compliance-percentage: compliance-pct,
                report-generated: stacks-block-height
            })
        (ok compliance-pct)
    )
)

(define-public (update-compliance-period (new-period uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set compliance-period new-period))
    )
)

(define-read-only (get-offset-requirement (company principal))
    (map-get? offset-requirements company)
)

(define-read-only (get-compliance-status (company principal))
    (match (map-get? offset-requirements company)
        requirement (get compliance-status requirement)
        "not-registered"
    )
)

(define-read-only (get-purchase-history (buyer principal) (purchase-id uint))
    (map-get? offset-purchases {buyer: buyer, purchase-id: purchase-id})
)

(define-read-only (get-compliance-report (company principal) (period uint))
    (map-get? compliance-reports {company: company, period: period})
)

(define-read-only (calculate-compliance-gap (company principal))
    (match (map-get? offset-requirements company)
        requirement 
            (if (> (get required-amount requirement) (get fulfilled-amount requirement))
                (- (get required-amount requirement) (get fulfilled-amount requirement))
                u0)
        u0
    )
)

(define-read-only (get-deadline-status (company principal))
    (match (map-get? offset-requirements company)
        requirement
            (if (> stacks-block-height (get deadline-block requirement))
                "expired"
                "active")
        "not-found"
    )
)

(define-read-only (estimate-compliance-cost (company principal))
    (let (
        (gap (calculate-compliance-gap company))
        (current-price (var-get credit-price))
    )
        (* gap current-price)
    )
)
(define-read-only (get-active-listings)
    (filter active-listing-filter (map unwrap-listing (get-listing-ids)))
)

(define-private (get-listing-ids)
    (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9)
)

(define-private (unwrap-listing (id uint))
    {id: id, listing: (default-to {
        seller: contract-owner,
        amount: u0,
        price-per-credit: u0,
        active: false
    } (map-get? market-listings id))}
)


(define-private (active-listing-filter (listing {id: uint, listing: {seller: principal, amount: uint, price-per-credit: uint, active: bool}}))
    (get active (get listing listing))
)


