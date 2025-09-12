;; Energy Performance Index (EPI) Tracking System
;; Tracks and calculates efficiency metrics for energy producers

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u600))
(define-constant err-not-found (err u601))
(define-constant err-invalid-data (err u602))
(define-constant err-insufficient-history (err u603))
(define-constant err-already-calculated (err u604))

;; Performance tracking period (blocks)
(define-constant performance-period u1008) ;; ~7 days in blocks
(define-constant min-production-entries u3) ;; Minimum entries for EPI calculation

;; EPI calculation weights (sum to 100)
(define-constant efficiency-weight u40)
(define-constant consistency-weight u30)
(define-constant reliability-weight u20)
(define-constant sustainability-weight u10)

;; Performance metrics storage
(define-map producer-performance
    {producer: principal, period: uint}
    {
        total-production: uint,
        production-entries: uint,
        peak-output: uint,
        downtime-blocks: uint,
        efficiency-score: uint,
        consistency-score: uint,
        reliability-score: uint,
        sustainability-score: uint,
        final-epi-score: uint,
        calculation-timestamp: uint
    }
)

;; Daily production records
(define-map daily-production
    {producer: principal, day: uint}
    {
        output: uint,
        capacity-utilization: uint,
        operational-hours: uint,
        timestamp: uint
    }
)

;; Producer capacity declarations
(define-map producer-capacity
    principal
    {
        declared-capacity: uint,
        installation-type: (string-ascii 20),
        verification-status: bool,
        last-updated: uint
    }
)

;; EPI rankings per period
(define-map epi-rankings
    {period: uint, rank: uint}
    {
        producer: principal,
        epi-score: uint,
        tier-classification: (string-ascii 20)
    }
)

;; Nonce for tracking periods and rankings
(define-data-var current-period uint u1)
(define-data-var ranking-nonce uint u0)

;; Utility functions
(define-private (min (a uint) (b uint))
    (if (< a b) a b)
)

(define-private (max (a uint) (b uint))
    (if (> a b) a b)
)

;; Get current performance period
(define-read-only (get-current-period)
    (/ stacks-block-height performance-period)
)

;; Declare producer capacity (required for EPI calculation)
(define-public (declare-capacity (capacity uint) (installation-type (string-ascii 20)))
    (begin
        (asserts! (> capacity u0) err-invalid-data)
        (map-set producer-capacity tx-sender
            {
                declared-capacity: capacity,
                installation-type: installation-type,
                verification-status: false,
                last-updated: stacks-block-height
            })
        (ok true)
    )
)

;; Verify producer capacity (owner only)
(define-public (verify-producer-capacity (producer principal))
    (let (
        (capacity-data (unwrap! (map-get? producer-capacity producer) err-not-found))
    )
        (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
        (map-set producer-capacity producer
            (merge capacity-data {verification-status: true}))
        (ok true)
    )
)

;; Record daily production data
(define-public (record-daily-production (output uint) (operational-hours uint))
    (let (
        (current-day (/ stacks-block-height u144)) ;; ~24 hours in blocks
        (capacity-data (unwrap! (map-get? producer-capacity tx-sender) err-not-found))
        (declared-capacity (get declared-capacity capacity-data))
        (utilization (/ (* output u100) declared-capacity))
    )
        (asserts! (> output u0) err-invalid-data)
        (asserts! (<= operational-hours u24) err-invalid-data)
        (asserts! (get verification-status capacity-data) err-unauthorized)
        
        (map-set daily-production {producer: tx-sender, day: current-day}
            {
                output: output,
                capacity-utilization: utilization,
                operational-hours: operational-hours,
                timestamp: stacks-block-height
            })
        (ok true)
    )
)

;; Calculate efficiency score (output vs capacity)
(define-private (calculate-efficiency-score (producer principal) (period uint))
    (let (
        (capacity-data (unwrap! (map-get? producer-capacity producer) (ok u0)))
        (declared-capacity (get declared-capacity capacity-data))
        (performance (default-to 
            {
                total-production: u0, production-entries: u0, peak-output: u0,
                downtime-blocks: u0, efficiency-score: u0, consistency-score: u0,
                reliability-score: u0, sustainability-score: u0, final-epi-score: u0,
                calculation-timestamp: u0
            }
            (map-get? producer-performance {producer: producer, period: period})
        ))
        (total-production (get total-production performance))
        (entries (get production-entries performance))
    )
        (if (and (> total-production u0) (> entries u0) (> declared-capacity u0))
            (let (
                (avg-output (/ total-production entries))
                (efficiency-ratio (/ (* avg-output u100) declared-capacity))
            )
                (ok (min u100 efficiency-ratio))
            )
            (ok u0)
        )
    )
)

;; Calculate consistency score (variance in daily production)
(define-private (calculate-consistency-score (producer principal) (period uint))
    (let (
        (performance (default-to 
            {
                total-production: u0, production-entries: u0, peak-output: u0,
                downtime-blocks: u0, efficiency-score: u0, consistency-score: u0,
                reliability-score: u0, sustainability-score: u0, final-epi-score: u0,
                calculation-timestamp: u0
            }
            (map-get? producer-performance {producer: producer, period: period})
        ))
        (entries (get production-entries performance))
        (peak-output (get peak-output performance))
        (total-production (get total-production performance))
    )
        (if (and (> entries u0) (> total-production u0) (> peak-output u0))
            (let (
                (avg-output (/ total-production entries))
                (consistency-ratio (/ (* avg-output u100) peak-output))
            )
                (ok (max u20 consistency-ratio)) ;; Minimum 20% consistency
            )
            (ok u0)
        )
    )
)

;; Calculate reliability score (operational uptime)
(define-private (calculate-reliability-score (producer principal) (period uint))
    (let (
        (performance (default-to 
            {
                total-production: u0, production-entries: u0, peak-output: u0,
                downtime-blocks: u0, efficiency-score: u0, consistency-score: u0,
                reliability-score: u0, sustainability-score: u0, final-epi-score: u0,
                calculation-timestamp: u0
            }
            (map-get? producer-performance {producer: producer, period: period})
        ))
        (downtime (get downtime-blocks performance))
        (total-blocks performance-period)
    )
        (if (< downtime total-blocks)
            (let (
                (uptime-blocks (- total-blocks downtime))
                (reliability-pct (/ (* uptime-blocks u100) total-blocks))
            )
                (ok reliability-pct)
            )
            (ok u0)
        )
    )
)

;; Calculate sustainability score (based on installation type and efficiency)
(define-private (calculate-sustainability-score (producer principal))
    (let (
        (capacity-data (unwrap! (map-get? producer-capacity producer) (ok u0)))
        (installation-type (get installation-type capacity-data))
    )
        (if (is-eq installation-type "solar")
            (ok u95)
            (if (is-eq installation-type "wind")
                (ok u90)
                (if (is-eq installation-type "hydro")
                    (ok u85)
                    (if (is-eq installation-type "geothermal")
                        (ok u80)
                        (ok u60) ;; Default for other types
                    )
                )
            )
        )
    )
)

;; Update performance metrics for a producer
(define-public (update-performance-metrics (producer principal) (production-amount uint) (is-peak bool))
    (let (
        (period-now (get-current-period))
        (existing-performance (default-to
            {
                total-production: u0, production-entries: u0, peak-output: u0,
                downtime-blocks: u0, efficiency-score: u0, consistency-score: u0,
                reliability-score: u0, sustainability-score: u0, final-epi-score: u0,
                calculation-timestamp: u0
            }
            (map-get? producer-performance {producer: producer, period: period-now})
        ))
        (new-total (+ (get total-production existing-performance) production-amount))
        (new-entries (+ (get production-entries existing-performance) u1))
        (new-peak (if is-peak 
            (max (get peak-output existing-performance) production-amount)
            (get peak-output existing-performance)))
    )
        (asserts! (> production-amount u0) err-invalid-data)
        
        (map-set producer-performance {producer: producer, period: period-now}
            (merge existing-performance
                {
                    total-production: new-total,
                    production-entries: new-entries,
                    peak-output: new-peak
                }))
        (ok true)
    )
)

;; Calculate comprehensive EPI score
(define-public (calculate-epi-score (producer principal))
    (let (
        (active-period (get-current-period))
        (performance (unwrap! (map-get? producer-performance {producer: producer, period: active-period}) err-not-found))
        (entries (get production-entries performance))
    )
        (asserts! (>= entries min-production-entries) err-insufficient-history)
        (asserts! (is-eq (get calculation-timestamp performance) u0) err-already-calculated)
        
        (let (
            (efficiency (unwrap! (calculate-efficiency-score producer active-period) err-invalid-data))
            (consistency (unwrap! (calculate-consistency-score producer active-period) err-invalid-data))
            (reliability (unwrap! (calculate-reliability-score producer active-period) err-invalid-data))
            (sustainability (unwrap! (calculate-sustainability-score producer) err-invalid-data))
            
            ;; Weighted EPI calculation
            (weighted-efficiency (/ (* efficiency efficiency-weight) u100))
            (weighted-consistency (/ (* consistency consistency-weight) u100))
            (weighted-reliability (/ (* reliability reliability-weight) u100))
            (weighted-sustainability (/ (* sustainability sustainability-weight) u100))
            
            (final-epi (+ weighted-efficiency weighted-consistency weighted-reliability weighted-sustainability))
        )
            (map-set producer-performance {producer: producer, period: active-period}
                (merge performance
                    {
                        efficiency-score: efficiency,
                        consistency-score: consistency,
                        reliability-score: reliability,
                        sustainability-score: sustainability,
                        final-epi-score: final-epi,
                        calculation-timestamp: stacks-block-height
                    }))
            (ok final-epi)
        )
    )
)

;; Read-only functions

;; Get producer performance data
(define-read-only (get-performance-data (producer principal) (period uint))
    (map-get? producer-performance {producer: producer, period: period})
)

;; Get producer capacity info
(define-read-only (get-capacity-info (producer principal))
    (map-get? producer-capacity producer)
)

;; Get daily production record
(define-read-only (get-daily-production (producer principal) (day uint))
    (map-get? daily-production {producer: producer, day: day})
)

;; Get current EPI score for producer
(define-read-only (get-current-epi (producer principal))
    (match (map-get? producer-performance {producer: producer, period: (get-current-period)})
        performance (get final-epi-score performance)
        u0
    )
)

;; Check if producer qualifies for EPI calculation
(define-read-only (can-calculate-epi (producer principal))
    (let (
        (check-period (get-current-period))
        (performance (default-to
            {
                total-production: u0, production-entries: u0, peak-output: u0,
                downtime-blocks: u0, efficiency-score: u0, consistency-score: u0,
                reliability-score: u0, sustainability-score: u0, final-epi-score: u0,
                calculation-timestamp: u0
            }
            (map-get? producer-performance {producer: producer, period: check-period})
        ))
        (capacity-verified (match (map-get? producer-capacity producer)
            cap-data (get verification-status cap-data)
            false))
    )
        (and 
            capacity-verified
            (>= (get production-entries performance) min-production-entries)
            (is-eq (get calculation-timestamp performance) u0)
        )
    )
)

;; Get EPI tier classification
(define-read-only (get-epi-tier (epi-score uint))
    (if (>= epi-score u90)
        "premium"
        (if (>= epi-score u75)
            "excellent"
            (if (>= epi-score u60)
                "good"
                (if (>= epi-score u40)
                    "standard"
                    "developing"
                )
            )
        )
    )
)
