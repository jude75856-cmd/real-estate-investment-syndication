(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-status (err u104))
(define-constant err-insufficient-funds (err u105))
(define-constant err-deal-closed (err u106))

(define-map investment-deals
  { deal-id: uint }
  {
    syndicator: principal,
    property-address: (string-ascii 300),
    total-investment-needed: uint,
    minimum-investment: uint,
    expected-return-rate: uint,
    deal-description: (string-ascii 500),
    funding-deadline: uint,
    total-raised: uint,
    investor-count: uint,
    deal-status: (string-ascii 20)
  }
)

(define-map investor-positions
  { deal-id: uint, investor: principal }
  {
    investment-amount: uint,
    investment-date: uint,
    verified-status: bool,
    ownership-percentage: uint
  }
)

(define-map distribution-records
  { deal-id: uint, distribution-id: uint }
  {
    distribution-date: uint,
    total-amount: uint,
    distribution-type: (string-ascii 50),
    performance-notes: (string-ascii 300)
  }
)

(define-map performance-reports
  { deal-id: uint, report-id: uint }
  {
    reporting-period: (string-ascii 20),
    revenue-generated: uint,
    expenses-incurred: uint,
    net-income: uint,
    occupancy-rate: uint,
    report-date: uint
  }
)

(define-data-var deal-counter uint u0)
(define-data-var distribution-counter uint u0)
(define-data-var report-counter uint u0)

(define-read-only (get-investment-deal (deal-id uint))
  (map-get? investment-deals { deal-id: deal-id })
)

(define-read-only (get-investor-position (deal-id uint) (investor principal))
  (map-get? investor-positions { deal-id: deal-id, investor: investor })
)

(define-read-only (get-distribution-record (deal-id uint) (distribution-id uint))
  (map-get? distribution-records { deal-id: deal-id, distribution-id: distribution-id })
)

(define-read-only (get-performance-report (deal-id uint) (report-id uint))
  (map-get? performance-reports { deal-id: deal-id, report-id: report-id })
)

(define-public (create-deal (property-address (string-ascii 300)) (total-investment-needed uint) (minimum-investment uint) (expected-return-rate uint) (deal-description (string-ascii 500)) (funding-period-months uint))
  (let
    (
      (deal-id (+ (var-get deal-counter) u1))
      (funding-deadline (+ stacks-block-height (* funding-period-months u144)))
    )
    (map-set investment-deals
      { deal-id: deal-id }
      {
        syndicator: tx-sender,
        property-address: property-address,
        total-investment-needed: total-investment-needed,
        minimum-investment: minimum-investment,
        expected-return-rate: expected-return-rate,
        deal-description: deal-description,
        funding-deadline: funding-deadline,
        total-raised: u0,
        investor-count: u0,
        deal-status: "open"
      }
    )
    (var-set deal-counter deal-id)
    (ok deal-id)
  )
)

(define-public (invest-in-deal (deal-id uint) (investment-amount uint))
  (let
    (
      (deal (unwrap! (get-investment-deal deal-id) err-not-found))
      (existing-position (map-get? investor-positions { deal-id: deal-id, investor: tx-sender }))
      (new-total-raised (+ (get total-raised deal) investment-amount))
      (ownership-percentage (/ (* investment-amount u10000) (get total-investment-needed deal)))
    )
    (asserts! (is-eq (get deal-status deal) "open") err-deal-closed)
    (asserts! (< stacks-block-height (get funding-deadline deal)) err-deal-closed)
    (asserts! (>= investment-amount (get minimum-investment deal)) err-insufficient-funds)
    (asserts! (is-none existing-position) err-already-exists)
    (map-set investor-positions
      { deal-id: deal-id, investor: tx-sender }
      {
        investment-amount: investment-amount,
        investment-date: stacks-block-height,
        verified-status: false,
        ownership-percentage: ownership-percentage
      }
    )
    (map-set investment-deals
      { deal-id: deal-id }
      (merge deal {
        total-raised: new-total-raised,
        investor-count: (+ (get investor-count deal) u1)
      })
    )
    (ok true)
  )
)

(define-public (verify-investor (deal-id uint) (investor principal))
  (let
    (
      (deal (unwrap! (get-investment-deal deal-id) err-not-found))
      (position (unwrap! (get-investor-position deal-id investor) err-not-found))
    )
    (asserts! (is-eq tx-sender (get syndicator deal)) err-unauthorized)
    (map-set investor-positions
      { deal-id: deal-id, investor: investor }
      (merge position { verified-status: true })
    )
    (ok true)
  )
)

(define-public (close-deal (deal-id uint))
  (let
    (
      (deal (unwrap! (get-investment-deal deal-id) err-not-found))
    )
    (asserts! (is-eq tx-sender (get syndicator deal)) err-unauthorized)
    (asserts! (is-eq (get deal-status deal) "open") err-invalid-status)
    (asserts! (>= (get total-raised deal) (get total-investment-needed deal)) err-insufficient-funds)
    (map-set investment-deals
      { deal-id: deal-id }
      (merge deal { deal-status: "funded" })
    )
    (ok true)
  )
)

(define-public (record-performance (deal-id uint) (reporting-period (string-ascii 20)) (revenue-generated uint) (expenses-incurred uint) (occupancy-rate uint))
  (let
    (
      (deal (unwrap! (get-investment-deal deal-id) err-not-found))
      (report-id (+ (var-get report-counter) u1))
      (net-income (- revenue-generated expenses-incurred))
    )
    (asserts! (is-eq tx-sender (get syndicator deal)) err-unauthorized)
    (asserts! (is-eq (get deal-status deal) "funded") err-invalid-status)
    (map-set performance-reports
      { deal-id: deal-id, report-id: report-id }
      {
        reporting-period: reporting-period,
        revenue-generated: revenue-generated,
        expenses-incurred: expenses-incurred,
        net-income: net-income,
        occupancy-rate: occupancy-rate,
        report-date: stacks-block-height
      }
    )
    (var-set report-counter report-id)
    (ok report-id)
  )
)

(define-public (distribute-returns (deal-id uint) (total-amount uint) (distribution-type (string-ascii 50)) (performance-notes (string-ascii 300)))
  (let
    (
      (deal (unwrap! (get-investment-deal deal-id) err-not-found))
      (distribution-id (+ (var-get distribution-counter) u1))
    )
    (asserts! (is-eq tx-sender (get syndicator deal)) err-unauthorized)
    (asserts! (is-eq (get deal-status deal) "funded") err-invalid-status)
    (map-set distribution-records
      { deal-id: deal-id, distribution-id: distribution-id }
      {
        distribution-date: stacks-block-height,
        total-amount: total-amount,
        distribution-type: distribution-type,
        performance-notes: performance-notes
      }
    )
    (var-set distribution-counter distribution-id)
    (ok distribution-id)
  )
)
