;; Health Insurance Marketplace Smart Contract
;; Written in Clarity for Stacks Blockchain

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u1))
(define-constant ERR_INVALID_AMOUNT (err u2))
(define-constant ERR_ALREADY_EXISTS (err u3))
(define-constant ERR_NOT_FOUND (err u4))
(define-constant ERR_INVALID_STATUS (err u5))
(define-constant ERR_INSUFFICIENT_BALANCE (err u6))
(define-constant ERR_INVALID_INPUT (err u7))
(define-constant MAX_AMOUNT u1000000000) ;; Maximum amount for various number inputs
(define-constant MIN_AMOUNT u1) ;; Minimum amount for various number inputs

;; Data Variables
(define-data-var contract-enabled bool true)
(define-data-var admin-address principal CONTRACT_OWNER)

;; Insurance Plan Structure
(define-map insurance-plans
    uint
    {
        name: (string-ascii 64),
        monthly-premium: uint,
        coverage-amount: uint,
        deductible: uint,
        provider: principal,
        active: bool
    }
)

;; Policy Holder Structure
(define-map policy-holders
    principal
    {
        plan-id: uint,
        start-date: uint,
        end-date: uint,
        premium-paid: uint,
        claims-filed: uint,
        status: (string-ascii 20)
    }
)

;; Claims Structure
(define-map claims
    uint
    {
        policy-holder: principal,
        amount: uint,
        description: (string-ascii 256),
        date: uint,
        status: (string-ascii 20),
        approved-amount: uint
    }
)

;; Counter for IDs
(define-data-var plan-id-counter uint u1)
(define-data-var claim-id-counter uint u1)

;; Read-only functions

(define-read-only (get-insurance-plan (id uint))
    (map-get? insurance-plans id)
)

(define-read-only (get-policy-holder (address principal))
    (map-get? policy-holders address)
)

(define-read-only (get-claim (id uint))
    (map-get? claims id)
)

(define-read-only (is-contract-owner)
    (is-eq tx-sender CONTRACT_OWNER)
)

;; Validation functions
(define-private (validate-amount (amount uint))
    (and 
        (>= amount MIN_AMOUNT)
        (<= amount MAX_AMOUNT)
    )
)

(define-private (validate-provider (provider principal))
    (and 
        (not (is-eq provider CONTRACT_OWNER))
        (not (is-eq provider tx-sender))
    )
)

;; Administrative functions

(define-public (set-contract-status (new-status bool))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set contract-enabled new-status)
        (ok true)
    )
)

(define-public (add-insurance-plan 
    (name (string-ascii 64))
    (monthly-premium uint)
    (coverage-amount uint)
    (deductible uint)
    (provider principal)
)
    (begin
        (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
        (asserts! (var-get contract-enabled) ERR_INVALID_STATUS)
        (asserts! (validate-amount monthly-premium) ERR_INVALID_AMOUNT)
        (asserts! (validate-amount coverage-amount) ERR_INVALID_AMOUNT)
        (asserts! (validate-amount deductible) ERR_INVALID_AMOUNT)
        (asserts! (validate-provider provider) ERR_INVALID_INPUT)
        (asserts! (> (len name) u0) ERR_INVALID_INPUT)
        
        (let ((new-id (var-get plan-id-counter)))
            (map-set insurance-plans new-id
                {
                    name: name,
                    monthly-premium: monthly-premium,
                    coverage-amount: coverage-amount,
                    deductible: deductible,
                    provider: provider,
                    active: true
                }
            )
            (var-set plan-id-counter (+ new-id u1))
            (ok new-id)
        )
    )
)

;; Policy Management Functions

(define-public (purchase-insurance-plan (plan-id uint))
    (begin
        (asserts! (var-get contract-enabled) ERR_INVALID_STATUS)
        (asserts! (>= plan-id u1) ERR_INVALID_INPUT)
        (asserts! (< plan-id (var-get plan-id-counter)) ERR_INVALID_INPUT)
        
        (match (map-get? insurance-plans plan-id)
            plan
            (begin
                (asserts! (get active plan) ERR_INVALID_STATUS)
                (asserts! (is-none (map-get? policy-holders tx-sender)) ERR_ALREADY_EXISTS)
                (try! (stx-transfer? (get monthly-premium plan) tx-sender (get provider plan)))
                
                (map-set policy-holders tx-sender
                    {
                        plan-id: plan-id,
                        start-date: block-height,
                        end-date: (+ block-height u52560), ;; Approximately 1 year in blocks
                        premium-paid: (get monthly-premium plan),
                        claims-filed: u0,
                        status: "ACTIVE"
                    }
                )
                (ok true)
            )
            ERR_NOT_FOUND
        )
    )
)

(define-public (file-claim 
    (amount uint)
    (description (string-ascii 256))
)
    (begin
        (asserts! (var-get contract-enabled) ERR_INVALID_STATUS)
        (asserts! (validate-amount amount) ERR_INVALID_AMOUNT)
        (asserts! (> (len description) u0) ERR_INVALID_INPUT)
        
        (match (map-get? policy-holders tx-sender)
            policy
            (begin
                (asserts! (is-eq (get status policy) "ACTIVE") ERR_INVALID_STATUS)
                (let ((claim-id (var-get claim-id-counter)))
                    (map-set claims claim-id
                        {
                            policy-holder: tx-sender,
                            amount: amount,
                            description: description,
                            date: block-height,
                            status: "PENDING",
                            approved-amount: u0
                        }
                    )
                    (var-set claim-id-counter (+ claim-id u1))
                    (ok claim-id)
                )
            )
            ERR_NOT_FOUND
        )
    )
)

(define-public (process-claim
    (claim-id uint)
    (approved bool)
    (approved-amount uint)
)
    (begin
        (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
        (asserts! (var-get contract-enabled) ERR_INVALID_STATUS)
        (asserts! (>= claim-id u1) ERR_INVALID_INPUT)
        (asserts! (< claim-id (var-get claim-id-counter)) ERR_INVALID_INPUT)
        (asserts! (validate-amount approved-amount) ERR_INVALID_AMOUNT)
        
        (match (map-get? claims claim-id)
            claim
            (begin
                (asserts! (is-eq (get status claim) "PENDING") ERR_INVALID_STATUS)
                
                (map-set claims claim-id
                    (merge claim {
                        status: (if approved "APPROVED" "REJECTED"),
                        approved-amount: approved-amount
                    })
                )
                
                (if approved
                    (match (map-get? insurance-plans 
                        (get plan-id (unwrap! (map-get? policy-holders (get policy-holder claim)) ERR_NOT_FOUND)))
                        plan
                        (begin
                            (try! (stx-transfer? 
                                approved-amount
                                (get provider plan)
                                (get policy-holder claim)
                            ))
                            (ok true)
                        )
                        ERR_NOT_FOUND
                    )
                    (ok true)
                )
            )
            ERR_NOT_FOUND
        )
    )
)

;; Utility Functions

(define-private (can-file-claim (policy-holder principal))
    (match (map-get? policy-holders policy-holder)
        policy
        (and
            (is-eq (get status policy) "ACTIVE")
            (<= block-height (get end-date policy))
        )
        false
    )
)

(define-public (renew-policy)
    (begin
        (asserts! (var-get contract-enabled) ERR_INVALID_STATUS)
        (match (map-get? policy-holders tx-sender)
            policy
            (begin
                (asserts! (is-eq (get status policy) "ACTIVE") ERR_INVALID_STATUS)
                (match (map-get? insurance-plans (get plan-id policy))
                    plan
                    (begin
                        (try! (stx-transfer? (get monthly-premium plan) tx-sender (get provider plan)))
                        (map-set policy-holders tx-sender
                            (merge policy {
                                end-date: (+ (get end-date policy) u52560),
                                premium-paid: (+ (get premium-paid policy) (get monthly-premium plan))
                            })
                        )
                        (ok true)
                    )
                    ERR_NOT_FOUND
                )
            )
            ERR_NOT_FOUND
        )
    )
)

(define-public (cancel-policy)
    (begin
        (asserts! (var-get contract-enabled) ERR_INVALID_STATUS)
        (match (map-get? policy-holders tx-sender)
            policy
            (begin
                (asserts! (is-eq (get status policy) "ACTIVE") ERR_INVALID_STATUS)
                (map-set policy-holders tx-sender
                    (merge policy {
                        status: "CANCELLED",
                        end-date: block-height
                    })
                )
                (ok true)
            )
            ERR_NOT_FOUND
        )
    )
)
