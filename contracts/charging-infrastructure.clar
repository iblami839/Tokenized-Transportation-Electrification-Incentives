;; Charging Infrastructure Contract
;; Manages charging stations and their operations

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_STATION_EXISTS (err u201))
(define-constant ERR_STATION_NOT_FOUND (err u202))
(define-constant ERR_STATION_OFFLINE (err u203))

;; Charging station data
(define-map charging-stations
  { station-id: (string-ascii 20) }
  {
    operator: principal,
    location: (string-ascii 100),
    power-rating: uint, ;; in kW
    status: (string-ascii 10), ;; "online", "offline", "maintenance"
    total-sessions: uint,
    total-energy: uint, ;; in kWh
    installation-block: uint
  }
)

;; Charging session data
(define-map charging-sessions
  { session-id: uint }
  {
    station-id: (string-ascii 20),
    vehicle-vin: (string-ascii 17),
    user: principal,
    start-time: uint,
    end-time: uint,
    energy-delivered: uint, ;; in kWh
    cost: uint
  }
)

(define-data-var next-session-id uint u1)

;; Register a new charging station
(define-public (register-station (station-id (string-ascii 20)) (location (string-ascii 100)) (power-rating uint))
  (let ((existing-station (map-get? charging-stations { station-id: station-id })))
    (if (is-some existing-station)
      ERR_STATION_EXISTS
      (begin
        (map-set charging-stations
          { station-id: station-id }
          {
            operator: tx-sender,
            location: location,
            power-rating: power-rating,
            status: "online",
            total-sessions: u0,
            total-energy: u0,
            installation-block: block-height
          }
        )
        (ok true)
      )
    )
  )
)

;; Start a charging session
(define-public (start-charging-session (station-id (string-ascii 20)) (vehicle-vin (string-ascii 17)))
  (let ((station-data (map-get? charging-stations { station-id: station-id })))
    (if (is-some station-data)
      (let ((station (unwrap-panic station-data)))
        (if (is-eq (get status station) "online")
          (let ((session-id (var-get next-session-id)))
            (map-set charging-sessions
              { session-id: session-id }
              {
                station-id: station-id,
                vehicle-vin: vehicle-vin,
                user: tx-sender,
                start-time: block-height,
                end-time: u0,
                energy-delivered: u0,
                cost: u0
              }
            )
            (var-set next-session-id (+ session-id u1))
            (ok session-id)
          )
          ERR_STATION_OFFLINE
        )
      )
      ERR_STATION_NOT_FOUND
    )
  )
)

;; End a charging session
(define-public (end-charging-session (session-id uint) (energy-delivered uint) (cost uint))
  (let ((session-data (map-get? charging-sessions { session-id: session-id })))
    (if (is-some session-data)
      (let ((session (unwrap-panic session-data)))
        (if (is-eq (get user session) tx-sender)
          (let ((station-data (unwrap-panic (map-get? charging-stations { station-id: (get station-id session) }))))
            (map-set charging-sessions
              { session-id: session-id }
              (merge session {
                end-time: block-height,
                energy-delivered: energy-delivered,
                cost: cost
              })
            )
            (map-set charging-stations
              { station-id: (get station-id session) }
              (merge station-data {
                total-sessions: (+ (get total-sessions station-data) u1),
                total-energy: (+ (get total-energy station-data) energy-delivered)
              })
            )
            (ok true)
          )
          ERR_UNAUTHORIZED
        )
      )
      ERR_STATION_NOT_FOUND
    )
  )
)

;; Get station information
(define-read-only (get-station (station-id (string-ascii 20)))
  (map-get? charging-stations { station-id: station-id })
)

;; Get session information
(define-read-only (get-session (session-id uint))
  (map-get? charging-sessions { session-id: session-id })
)

;; Update station status (operator only)
(define-public (update-station-status (station-id (string-ascii 20)) (new-status (string-ascii 10)))
  (let ((station-data (map-get? charging-stations { station-id: station-id })))
    (if (is-some station-data)
      (let ((station (unwrap-panic station-data)))
        (if (is-eq (get operator station) tx-sender)
          (begin
            (map-set charging-stations
              { station-id: station-id }
              (merge station { status: new-status })
            )
            (ok true)
          )
          ERR_UNAUTHORIZED
        )
      )
      ERR_STATION_NOT_FOUND
    )
  )
)
