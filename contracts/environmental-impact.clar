;; Environmental Impact Contract
;; Tracks emission reductions and environmental benefits

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u500))
(define-constant ERR_INVALID_DATA (err u501))

;; Environmental impact data per user
(define-map user-impact
  principal
  {
    total-co2-saved: uint,    ;; in kg
    total-fuel-saved: uint,   ;; in gallons
    equivalent-trees: uint,   ;; trees planted equivalent
    last-updated: uint
  }
)

;; Global environmental statistics
(define-map global-impact
  { period: (string-ascii 10) } ;; "daily", "monthly", "yearly"
  {
    total-co2-reduction: uint,
    total-fuel-savings: uint,
    total-participants: uint,
    last-updated: uint
  }
)

;; Carbon offset projects
(define-map offset-projects
  { project-id: uint }
  {
    name: (string-ascii 100),
    description: (string-ascii 200),
    co2-capacity: uint,
    co2-allocated: uint,
    status: (string-ascii 20)
  }
)

(define-data-var next-project-id uint u1)

;; Constants for calculations
(define-constant CO2_PER_GALLON u8887) ;; grams CO2 per gallon of gasoline
(define-constant TREES_PER_TON_CO2 u16) ;; trees needed to offset 1 ton CO2
(define-constant MILES_PER_GALLON u25) ;; average ICE vehicle efficiency

;; Record environmental impact
(define-public (record-impact (miles-driven uint) (energy-used uint))
  (let ((fuel-saved (/ miles-driven MILES_PER_GALLON))
        (co2-saved (/ (* fuel-saved CO2_PER_GALLON) u1000)) ;; convert to kg
        (trees-equivalent (/ co2-saved (/ u1000 TREES_PER_TON_CO2))))
    (let ((current-impact (default-to
      { total-co2-saved: u0, total-fuel-saved: u0, equivalent-trees: u0, last-updated: u0 }
      (map-get? user-impact tx-sender)
    )))
      (map-set user-impact
        tx-sender
        {
          total-co2-saved: (+ (get total-co2-saved current-impact) co2-saved),
          total-fuel-saved: (+ (get total-fuel-saved current-impact) fuel-saved),
          equivalent-trees: (+ (get equivalent-trees current-impact) trees-equivalent),
          last-updated: block-height
        }
      )
      (update-global-impact co2-saved fuel-saved)
      (ok { co2-saved: co2-saved, fuel-saved: fuel-saved, trees-equivalent: trees-equivalent })
    )
  )
)

;; Update global impact statistics
(define-private (update-global-impact (co2-saved uint) (fuel-saved uint))
  (let ((current-global (default-to
    { total-co2-reduction: u0, total-fuel-savings: u0, total-participants: u0, last-updated: u0 }
    (map-get? global-impact { period: "monthly" })
  )))
    (map-set global-impact
      { period: "monthly" }
      {
        total-co2-reduction: (+ (get total-co2-reduction current-global) co2-saved),
        total-fuel-savings: (+ (get total-fuel-savings current-global) fuel-saved),
        total-participants: (get total-participants current-global),
        last-updated: block-height
      }
    )
  )
)

;; Create carbon offset project
(define-public (create-offset-project (name (string-ascii 100)) (description (string-ascii 200)) (co2-capacity uint))
  (if (is-eq tx-sender CONTRACT_OWNER)
    (let ((project-id (var-get next-project-id)))
      (map-set offset-projects
        { project-id: project-id }
        {
          name: name,
          description: description,
          co2-capacity: co2-capacity,
          co2-allocated: u0,
          status: "active"
        }
      )
      (var-set next-project-id (+ project-id u1))
      (ok project-id)
    )
    ERR_UNAUTHORIZED
  )
)

;; Allocate CO2 offset to project
(define-public (allocate-to-project (project-id uint) (co2-amount uint))
  (if (is-eq tx-sender CONTRACT_OWNER)
    (let ((project-data (map-get? offset-projects { project-id: project-id })))
      (if (is-some project-data)
        (let ((project (unwrap-panic project-data)))
          (if (<= (+ (get co2-allocated project) co2-amount) (get co2-capacity project))
            (begin
              (map-set offset-projects
                { project-id: project-id }
                (merge project { co2-allocated: (+ (get co2-allocated project) co2-amount) })
              )
              (ok true)
            )
            ERR_INVALID_DATA
          )
        )
        ERR_INVALID_DATA
      )
    )
    ERR_UNAUTHORIZED
  )
)

;; Get user environmental impact
(define-read-only (get-user-impact (user principal))
  (map-get? user-impact user)
)

;; Get global impact statistics
(define-read-only (get-global-impact (period (string-ascii 10)))
  (map-get? global-impact { period: period })
)

;; Get offset project information
(define-read-only (get-offset-project (project-id uint))
  (map-get? offset-projects { project-id: project-id })
)

;; Calculate environmental impact for given miles
(define-read-only (calculate-impact (miles uint))
  (let ((fuel-saved (/ miles MILES_PER_GALLON))
        (co2-saved (/ (* fuel-saved CO2_PER_GALLON) u1000))
        (trees-equivalent (/ co2-saved (/ u1000 TREES_PER_TON_CO2))))
    {
      fuel-saved: fuel-saved,
      co2-saved: co2-saved,
      trees-equivalent: trees-equivalent
    }
  )
)

;; Get environmental constants
(define-read-only (get-environmental-constants)
  {
    co2-per-gallon: CO2_PER_GALLON,
    trees-per-ton-co2: TREES_PER_TON_CO2,
    miles-per-gallon: MILES_PER_GALLON
  }
)
