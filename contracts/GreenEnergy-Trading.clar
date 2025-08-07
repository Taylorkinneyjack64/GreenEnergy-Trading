;; GreenEnergy-Trading Contract
;; Enables tracking and trading of renewable energy credits

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-credits (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-unauthorized (err u104))

(define-map escrow-agreements
    uint
    {
        buyer: principal,
        seller: principal,
        credit-amount: uint,
        stx-amount: uint,
        delivery-deadline: uint,
        energy-delivered: bool,
        funds-released: bool,
        dispute-raised: bool
    }
)

(define-data-var escrow-id-nonce uint u0)

(define-constant err-escrow-not-found (err u300))
(define-constant err-escrow-expired (err u301))
(define-constant err-escrow-already-completed (err u302))
(define-constant err-escrow-not-seller (err u303))
(define-constant err-escrow-not-buyer (err u304))


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

;; ==============================================================================
;; DYNAMIC GRID BALANCING SYSTEM
;; ==============================================================================

;; Grid state tracking
(define-map grid-state
    uint ;; time-period (block height divided by period length)
    {
        total-supply: uint,
        total-demand: uint,
        stability-score: uint,
        price-multiplier: uint,
        peak-demand-active: bool
    }
)

(define-map grid-contributions
    {producer: principal, period: uint}
    {
        energy-supplied: uint,
        peak-contribution: uint,
        stability-bonus: uint,
        last-contribution-block: uint
    }
)

(define-map demand-registrations
    {consumer: principal, period: uint}
    {
        requested-amount: uint,
        priority-level: uint,
        max-price-willing: uint,
        demand-fulfilled: uint
    }
)

;; Grid balancing constants
(define-constant grid-period-blocks u144) ;; ~24 hours in blocks
(define-constant base-stability-score u100)
(define-constant max-price-multiplier u300) ;; 3x base price maximum
(define-constant min-price-multiplier u50)  ;; 0.5x base price minimum
(define-constant peak-demand-threshold u80) ;; 80% supply utilization triggers peak
(define-constant stability-bonus-rate u10)  ;; 10% bonus for grid stabilization

;; Grid balancing errors
(define-constant err-invalid-demand (err u400))
(define-constant err-grid-period-not-found (err u401))
(define-constant err-insufficient-grid-supply (err u402))
(define-constant err-demand-not-registered (err u403))

;; Current grid period calculation
(define-read-only (get-current-grid-period)
    (/ stacks-block-height grid-period-blocks)
)

;; Helper function for minimum value
(define-private (min-value (a uint) (b uint))
    (if (< a b) a b)
)

;; Helper function for maximum value
(define-private (max-value (a uint) (b uint))
    (if (> a b) a b)
)

;; Initialize grid state for current period
(define-private (ensure-grid-period-exists (period uint))
    (match (map-get? grid-state period)
        existing-state true
        (begin
            (map-set grid-state period
                {
                    total-supply: u0,
                    total-demand: u0,
                    stability-score: base-stability-score,
                    price-multiplier: u100,
                    peak-demand-active: false
                })
            true
        )
    )
)

;; Register energy demand for current period
(define-public (register-energy-demand (amount uint) (priority uint) (max-price uint))
    (let (
        (current-period (get-current-grid-period))
        (consumer tx-sender)
    )
        (asserts! (> amount u0) err-invalid-demand)
        (asserts! (<= priority u3) err-invalid-demand) ;; Priority levels 1-3
        (asserts! (> max-price u0) err-invalid-demand)
        
        (ensure-grid-period-exists current-period)
        
        ;; Update grid demand
        (let (
            (current-grid (unwrap! (map-get? grid-state current-period) err-grid-period-not-found))
            (new-total-demand (+ (get total-demand current-grid) amount))
        )
            (map-set grid-state current-period
                (merge current-grid {total-demand: new-total-demand}))
        )
        
        ;; Register consumer demand
        (map-set demand-registrations {consumer: consumer, period: current-period}
            {
                requested-amount: amount,
                priority-level: priority,
                max-price-willing: max-price,
                demand-fulfilled: u0
            })
        
        (ok true)
    )
)

;; Enhanced energy production recording with grid balancing
(define-public (record-grid-production (amount uint))
    (let (
        (producer-data (unwrap! (map-get? energy-producers tx-sender) err-not-found))
        (is-verified (get is-verified producer-data))
        (current-period (get-current-grid-period))
    )
        (asserts! is-verified err-unauthorized)
        (asserts! (> amount u0) err-invalid-amount)
        
        (ensure-grid-period-exists current-period)
        
        ;; Update producer data
        (map-set energy-producers tx-sender
            {
                is-verified: (get is-verified producer-data),
                total-production: (+ (get total-production producer-data) amount),
                available-credits: (+ (get available-credits producer-data) amount)
            })
        
        ;; Update grid supply
        (let (
            (current-grid (unwrap! (map-get? grid-state current-period) err-grid-period-not-found))
            (new-total-supply (+ (get total-supply current-grid) amount))
        )
            (map-set grid-state current-period
                (merge current-grid {total-supply: new-total-supply}))
        )
        
        ;; Track producer contribution
        (let (
            (existing-contribution (default-to
                {
                    energy-supplied: u0,
                    peak-contribution: u0,
                    stability-bonus: u0,
                    last-contribution-block: u0
                }
                (map-get? grid-contributions {producer: tx-sender, period: current-period})
            ))
        )
            (map-set grid-contributions {producer: tx-sender, period: current-period}
                (merge existing-contribution
                    {
                        energy-supplied: (+ (get energy-supplied existing-contribution) amount),
                        last-contribution-block: stacks-block-height
                    }))
        )
        
        ;; Update grid stability and pricing
        (try! (update-grid-stability current-period))
        (ok true)
    )
)

;; Calculate and update grid stability metrics
(define-private (update-grid-stability (period uint))
    (let (
        (current-grid (unwrap! (map-get? grid-state period) err-grid-period-not-found))
        (supply (get total-supply current-grid))
        (demand (get total-demand current-grid))
    )
        (if (> demand u0)
            (let (
                ;; Calculate supply/demand ratio (scaled by 100)
                (supply-ratio (/ (* supply u100) demand))
                ;; Determine if peak demand conditions exist
                (is-peak (>= supply-ratio peak-demand-threshold))
                ;; Calculate stability score based on balance
                (stability (if (>= supply-ratio u100)
                    (min-value base-stability-score (+ base-stability-score (/ (- supply-ratio u100) u10)))
                    (max-value u0 (- base-stability-score (/ (- u100 supply-ratio) u10)))))
                ;; Calculate dynamic price multiplier
                (price-mult (if (< supply-ratio u50)
                    max-price-multiplier
                    (if (> supply-ratio u150)
                        min-price-multiplier
                        (- u150 (/ supply-ratio u2)))))
            )
                (map-set grid-state period
                    (merge current-grid
                        {
                            stability-score: stability,
                            price-multiplier: price-mult,
                            peak-demand-active: is-peak
                        }))
                (ok true)
            )
            (ok true) ;; No demand registered yet
        )
    )
)

;; Contribute to peak demand fulfillment with bonus
(define-public (contribute-peak-energy (amount uint))
    (let (
        (current-period (get-current-grid-period))
        (current-grid (unwrap! (map-get? grid-state current-period) err-grid-period-not-found))
        (producer-data (unwrap! (map-get? energy-producers tx-sender) err-not-found))
        (available (get available-credits producer-data))
    )
        (asserts! (get peak-demand-active current-grid) (err u404))
        (asserts! (>= available amount) err-insufficient-credits)
        
        ;; Calculate stability bonus
        (let (
            (stability-bonus (/ (* amount stability-bonus-rate) u100))
            (existing-contribution (default-to
                {
                    energy-supplied: u0,
                    peak-contribution: u0,
                    stability-bonus: u0,
                    last-contribution-block: u0
                }
                (map-get? grid-contributions {producer: tx-sender, period: current-period})
            ))
        )
            ;; Update producer credits with bonus
            (map-set energy-producers tx-sender
                (merge producer-data
                    {available-credits: (+ (- available amount) stability-bonus)}))
            
            ;; Track peak contribution
            (map-set grid-contributions {producer: tx-sender, period: current-period}
                (merge existing-contribution
                    {
                        peak-contribution: (+ (get peak-contribution existing-contribution) amount),
                        stability-bonus: (+ (get stability-bonus existing-contribution) stability-bonus)
                    }))
            
            (ok stability-bonus)
        )
    )
)

;; Get dynamic credit price based on grid conditions
(define-read-only (get-dynamic-credit-price)
    (let (
        (current-period (get-current-grid-period))
        (base-price (var-get credit-price))
    )
        (match (map-get? grid-state current-period)
            grid-data (* base-price (/ (get price-multiplier grid-data) u100))
            base-price
        )
    )
)

;; Get current grid status
(define-read-only (get-grid-status (period uint))
    (map-get? grid-state period)
)

;; Get producer grid contribution
(define-read-only (get-grid-contribution (producer principal) (period uint))
    (map-get? grid-contributions {producer: producer, period: period})
)

;; Get demand registration
(define-read-only (get-demand-registration (consumer principal) (period uint))
    (map-get? demand-registrations {consumer: consumer, period: period})
)

;; Calculate grid balance ratio (supply/demand * 100)
(define-read-only (calculate-grid-balance (period uint))
    (match (map-get? grid-state period)
        grid-data
            (if (> (get total-demand grid-data) u0)
                (/ (* (get total-supply grid-data) u100) (get total-demand grid-data))
                u0)
        u0
    )
)

;; Check if producer qualifies for stability rewards
(define-read-only (check-stability-rewards (producer principal) (period uint))
    (match (map-get? grid-contributions {producer: producer, period: period})
        contribution
            (and 
                (> (get peak-contribution contribution) u0)
                (> (get stability-bonus contribution) u0))
        false
    )
)



(define-public (create-escrow (seller principal) (credit-amount uint) (delivery-blocks uint))
    (let (
        (escrow-id (var-get escrow-id-nonce))
        (stx-payment (* credit-amount (var-get credit-price)))
        (delivery-deadline (+ stacks-block-height delivery-blocks))
        (seller-credits (default-to u0 (map-get? credit-holdings seller)))
    )
        (asserts! (>= seller-credits credit-amount) err-insufficient-credits)
        (asserts! (> credit-amount u0) err-invalid-amount)
        (asserts! (> delivery-blocks u0) err-invalid-amount)
        
        (try! (stx-transfer? stx-payment tx-sender (as-contract tx-sender)))
        
        (map-set credit-holdings seller (- seller-credits credit-amount))
        
        (map-set escrow-agreements escrow-id
            {
                buyer: tx-sender,
                seller: seller,
                credit-amount: credit-amount,
                stx-amount: stx-payment,
                delivery-deadline: delivery-deadline,
                energy-delivered: false,
                funds-released: false,
                dispute-raised: false
            })
        
        (var-set escrow-id-nonce (+ escrow-id u1))
        (ok escrow-id)
    )
)

(define-public (confirm-delivery (escrow-id uint))
    (let (
        (escrow (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found))
    )
        (asserts! (is-eq tx-sender (get seller escrow)) err-escrow-not-seller)
        (asserts! (not (get energy-delivered escrow)) err-escrow-already-completed)
        (asserts! (< stacks-block-height (get delivery-deadline escrow)) err-escrow-expired)
        
        (map-set escrow-agreements escrow-id
            (merge escrow {energy-delivered: true}))
        
        (ok true)
    )
)

(define-public (release-escrow-funds (escrow-id uint))
    (let (
        (escrow (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found))
    )
        (asserts! (get energy-delivered escrow) (err u305))
        (asserts! (not (get funds-released escrow)) err-escrow-already-completed)
        
        (try! (as-contract (stx-transfer? (get stx-amount escrow) tx-sender (get seller escrow))))
        
        (map-set credit-holdings (get buyer escrow) 
            (+ (default-to u0 (map-get? credit-holdings (get buyer escrow))) (get credit-amount escrow)))
        
        (map-set escrow-agreements escrow-id
            (merge escrow {funds-released: true}))
        
        (ok true)
    )
)

(define-public (raise-dispute (escrow-id uint))
    (let (
        (escrow (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found))
    )
        (asserts! (or (is-eq tx-sender (get buyer escrow)) (is-eq tx-sender (get seller escrow))) err-unauthorized)
        (asserts! (not (get funds-released escrow)) err-escrow-already-completed)
        
        (map-set escrow-agreements escrow-id
            (merge escrow {dispute-raised: true}))
        
        (ok true)
    )
)

(define-public (resolve-dispute (escrow-id uint) (refund-buyer bool))
    (let (
        (escrow (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found))
    )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (get dispute-raised escrow) (err u306))
        (asserts! (not (get funds-released escrow)) err-escrow-already-completed)
        
        (if refund-buyer
            (begin
                (try! (as-contract (stx-transfer? (get stx-amount escrow) tx-sender (get buyer escrow))))
                (map-set credit-holdings (get seller escrow) 
                    (+ (default-to u0 (map-get? credit-holdings (get seller escrow))) (get credit-amount escrow)))
            )
            (begin
                (try! (as-contract (stx-transfer? (get stx-amount escrow) tx-sender (get seller escrow))))
                (map-set credit-holdings (get buyer escrow) 
                    (+ (default-to u0 (map-get? credit-holdings (get buyer escrow))) (get credit-amount escrow)))
            )
        )
        
        (map-set escrow-agreements escrow-id
            (merge escrow {funds-released: true}))
        
        (ok true)
    )
)

(define-public (cancel-expired-escrow (escrow-id uint))
    (let (
        (escrow (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found))
    )
        (asserts! (> stacks-block-height (get delivery-deadline escrow)) err-escrow-expired)
        (asserts! (not (get energy-delivered escrow)) (err u307))
        (asserts! (not (get funds-released escrow)) err-escrow-already-completed)
        
        (try! (as-contract (stx-transfer? (get stx-amount escrow) tx-sender (get buyer escrow))))
        
        (map-set credit-holdings (get seller escrow) 
            (+ (default-to u0 (map-get? credit-holdings (get seller escrow))) (get credit-amount escrow)))
        
        (map-set escrow-agreements escrow-id
            (merge escrow {funds-released: true}))
        
        (ok true)
    )
)

(define-read-only (get-escrow-details (escrow-id uint))
    (map-get? escrow-agreements escrow-id)
)

(define-read-only (check-escrow-status (escrow-id uint))
    (match (map-get? escrow-agreements escrow-id)
        escrow 
            (if (get funds-released escrow)
                "completed"
                (if (get dispute-raised escrow)
                    "disputed"
                    (if (get energy-delivered escrow)
                        "delivered"
                        (if (> stacks-block-height (get delivery-deadline escrow))
                            "expired"
                            "active"))))
        "not-found"
    )
)


