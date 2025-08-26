;; WhistleblowerPlatform Contract
;; Anonymous reporting system for corporate fraud and misconduct with protection guarantees

;; Define constants for error handling
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u100))
(define-constant err-invalid-report (err u101))
(define-constant err-report-not-found (err u102))
(define-constant err-insufficient-stake (err u103))
(define-constant err-report-already-exists (err u104))

;; Data structures
(define-map reports 
  { report-id: uint }
  {
    reporter-hash: (buff 32),
    report-hash: (buff 64),
    timestamp: uint,
    stake-amount: uint,
    status: (string-ascii 20),
    reward-claimed: bool
  })

(define-map reporter-protection
  { reporter-hash: (buff 32) }
  {
    protection-level: uint,
    anonymity-guarantee: bool,
    stake-locked: uint
  })

;; Contract state variables
(define-data-var next-report-id uint u1)
(define-data-var total-reports uint u0)
(define-data-var minimum-stake uint u1000000) ;; 1 STX minimum stake
(define-data-var protection-fund uint u0)

;; Function 1: Submit Anonymous Report
;; Allows whistleblowers to submit reports with anonymity protection
(define-public (submit-report (reporter-hash (buff 32)) (report-hash (buff 64)) (stake-amount uint))
  (let (
    (report-id (var-get next-report-id))
    (current-block stacks-block-height)
  )
    (begin
      ;; Validate minimum stake requirement
      (asserts! (>= stake-amount (var-get minimum-stake)) err-insufficient-stake)
      
      ;; Ensure report doesn't already exist for this reporter
      (asserts! (is-none (map-get? reports {report-id: report-id})) err-report-already-exists)
      
      ;; Transfer stake to contract for protection
      (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
      
      ;; Store the report with anonymized data
      (map-set reports 
        { report-id: report-id }
        {
          reporter-hash: reporter-hash,
          report-hash: report-hash,
          timestamp: current-block,
          stake-amount: stake-amount,
          status: "submitted",
          reward-claimed: false
        })
      
      ;; Set up reporter protection
      (map-set reporter-protection
        { reporter-hash: reporter-hash }
        {
          protection-level: u100,
          anonymity-guarantee: true,
          stake-locked: stake-amount
        })
      
      ;; Update contract state
      (var-set next-report-id (+ report-id u1))
      (var-set total-reports (+ (var-get total-reports) u1))
      (var-set protection-fund (+ (var-get protection-fund) stake-amount))
      
      ;; Print event for off-chain monitoring (without revealing identity)
      (print {
        action: "report-submitted",
        report-id: report-id,
        timestamp: current-block,
        stake-amount: stake-amount
      })
      
      (ok report-id))))

;; Function 2: Verify Report and Release Protection
;; Allows authorized parties to verify reports and manage protection status
(define-public (verify-report (report-id uint) (verification-status (string-ascii 20)) (release-protection bool))
  (let (
    (report-data (unwrap! (map-get? reports {report-id: report-id}) err-report-not-found))
    (reporter-hash (get reporter-hash report-data))
    (stake-amount (get stake-amount report-data))
    (protection-data (unwrap! (map-get? reporter-protection {reporter-hash: reporter-hash}) err-report-not-found))
  )
    (begin
      ;; Only contract owner can verify reports (in production, this could be a DAO or multisig)
      (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
      
      ;; Update report status
      (map-set reports
        { report-id: report-id }
        (merge report-data { status: verification-status }))
      
      ;; Handle protection release if requested
      (if release-protection
        (begin
          ;; If report is verified as legitimate, provide additional protection
          (if (is-eq verification-status "verified")
            (begin
              ;; Enhance protection level and return stake with bonus
              (map-set reporter-protection
                { reporter-hash: reporter-hash }
                (merge protection-data { 
                  protection-level: u200,
                  stake-locked: u0
                }))
              
              ;; Return stake with 50% bonus for legitimate report
              (let ((reward-amount (+ stake-amount (/ stake-amount u2))))
                ;; Note: In a real implementation, you would need a separate mechanism
                ;; to allow anonymous reward claiming by the reporter
                ;; For now, we'll hold the reward in the contract
                (var-set protection-fund (- (var-get protection-fund) stake-amount))
                
                ;; Mark reward as available for claiming
                (map-set reports
                  { report-id: report-id }
                  (merge report-data { reward-claimed: false, status: verification-status }))
              )
            )
            ;; If report is rejected, forfeit stake to protection fund
            (begin
              (map-set reporter-protection
                { reporter-hash: reporter-hash }
                (merge protection-data { 
                  protection-level: u0,
                  anonymity-guarantee: false,
                  stake-locked: u0
                }))
            )
          )
        )
        ;; If protection not released, maintain current status
        true
      )
      
      ;; Print verification event
      (print {
        action: "report-verified",
        report-id: report-id,
        status: verification-status,
        protection-released: release-protection,
        timestamp: stacks-block-height
      })
      
      (ok true))))

;; Read-only functions for transparency

;; Get report details (anonymized)
(define-read-only (get-report (report-id uint))
  (map-get? reports {report-id: report-id}))

;; Get protection status for a reporter hash
(define-read-only (get-protection-status (reporter-hash (buff 32)))
  (map-get? reporter-protection {reporter-hash: reporter-hash}))

;; Get total number of reports
(define-read-only (get-total-reports)
  (ok (var-get total-reports)))

;; Get protection fund balance
(define-read-only (get-protection-fund-balance)
  (ok (var-get protection-fund)))

;; Get minimum stake requirement
(define-read-only (get-minimum-stake)
  (ok (var-get minimum-stake)))

;; Additional function to allow reward claiming by reporters
(define-public (claim-reward (report-id uint) (reporter-hash (buff 32)))
  (let (
    (report-data (unwrap! (map-get? reports {report-id: report-id}) err-report-not-found))
    (protection-data (unwrap! (map-get? reporter-protection {reporter-hash: reporter-hash}) err-report-not-found))
    (stake-amount (get stake-amount report-data))
  )
    (begin
      ;; Verify the reporter hash matches and report is verified
      (asserts! (is-eq (get reporter-hash report-data) reporter-hash) err-unauthorized)
      (asserts! (is-eq (get status report-data) "verified") err-unauthorized)
      (asserts! (is-eq (get reward-claimed report-data) false) err-unauthorized)
      
      ;; Calculate reward (stake + 50% bonus)
      (let ((reward-amount (+ stake-amount (/ stake-amount u2))))
        ;; Transfer reward to the caller (reporter must reveal themselves to claim)
        (try! (as-contract (stx-transfer? reward-amount (as-contract tx-sender) tx-sender)))
        
        ;; Mark reward as claimed
        (map-set reports
          { report-id: report-id }
          (merge report-data { reward-claimed: true }))
        
        (ok reward-amount)))))

;; Contract management function (owner only)
(define-public (update-minimum-stake (new-minimum uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (var-set minimum-stake new-minimum)
    (ok true)))