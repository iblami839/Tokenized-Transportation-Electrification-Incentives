;; Incentive Calculation Contract
;; Determines and distributes rewards for electric vehicle usage

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u400))
(define-constant ERR_INSUFFICIENT_FUNDS (err u401))
(define-constant ERR_INVALID_AMOUNT (err u402))
(define-constant ERR_USER_NOT_FOUND (err u403))

;; Incentive rates (per unit)
(define-constant ENERGY_INCENTIVE_RATE u10) ;; 10 tokens per kWh
(define-constant DISTANCE_INCENTIVE_RATE u5)  ;; 5 tokens per mile
(define-constant CARBON_INCENTIVE_RATE u20)   ;; 20 tokens per kg CO2 offset

;; User incentive balances
(define-map user-incentives
  principal
  {
    total-earned: uint,
    total-claimed: uint,
    pending-rewards: uint,
    last-calculation: uint
  }
)

;; Incentive pool
(define-data-var incentive-pool uint u1000000) ;; 1M tokens initially

;; Calculate incentives for user based on usage
(define-public (calculate-incentives (user principal) (energy-used uint) (distance uint) (carbon-offset uint))
  (if (is-eq tx-sender CONTRACT_OWNER)
    (let ((energy-reward (* energy-used ENERGY_INCENTIVE_RATE))
          (distance-reward (* distance DISTANCE_INCENTIVE_RATE))
          (carbon-reward (* carbon-offset CARBON_INCENTIVE_RATE))
          (total-reward (+ energy-reward (+ distance-reward carbon-reward))))
      (let ((current-incentives (default-to
        { total-earned: u0, total-claimed: u0, pending-rewards: u0, last-calculation: u0 }
        (map-get? user-incentives user)
      )))
        (map-set user-incentives
          user
          {
            total-earned: (+ (get total-earned current-incentives) total-reward),
            total-claimed: (get total-claimed current-incentives),
            pending-rewards: (+ (get pending-rewards current-incentives) total-reward),
            last-calculation: block-height
          }
        )
        (ok total-reward)
      )
    )
    ERR_UNAUTHORIZED
  )
)

;; Claim pending rewards
(define-public (claim-rewards)
  (let ((user-data (map-get? user-incentives tx-sender)))
    (if (is-some user-data)
      (let ((incentives (unwrap-panic user-data))
            (pending (get pending-rewards incentives)))
        (if (> pending u0)
          (if (>= (var-get incentive-pool) pending)
            (begin
              (map-set user-incentives
                tx-sender
                (merge incentives {
                  total-claimed: (+ (get total-claimed incentives) pending),
                  pending-rewards: u0
                })
              )
              (var-set incentive-pool (- (var-get incentive-pool) pending))
              (ok pending)
            )
            ERR_INSUFFICIENT_FUNDS
          )
          ERR_INVALID_AMOUNT
        )
      )
      ERR_USER_NOT_FOUND
    )
  )
)

;; Add funds to incentive pool (admin only)
(define-public (add-to-pool (amount uint))
  (if (is-eq tx-sender CONTRACT_OWNER)
    (begin
      (var-set incentive-pool (+ (var-get incentive-pool) amount))
      (ok true)
    )
    ERR_UNAUTHORIZED
  )
)

;; Get user incentive information
(define-read-only (get-user-incentives (user principal))
  (map-get? user-incentives user)
)

;; Get current incentive pool balance
(define-read-only (get-pool-balance)
  (var-get incentive-pool)
)

;; Calculate potential rewards for given usage
(define-read-only (calculate-potential-rewards (energy uint) (distance uint) (carbon-offset uint))
  (let ((energy-reward (* energy ENERGY_INCENTIVE_RATE))
        (distance-reward (* distance DISTANCE_INCENTIVE_RATE))
        (carbon-reward (* carbon-offset CARBON_INCENTIVE_RATE)))
    {
      energy-reward: energy-reward,
      distance-reward: distance-reward,
      carbon-reward: carbon-reward,
      total-reward: (+ energy-reward (+ distance-reward carbon-reward))
    }
  )
)

;; Get incentive rates
(define-read-only (get-incentive-rates)
  {
    energy-rate: ENERGY_INCENTIVE_RATE,
    distance-rate: DISTANCE_INCENTIVE_RATE,
    carbon-rate: CARBON_INCENTIVE_RATE
  }
)
