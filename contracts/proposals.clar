(use-trait proposal-trait .proposal-trait.proposal-trait)
(define-constant CONTRACT_OWNER tx-sender)

;;=================
;; ERROR CODES
(define-constant ERR_UNKNOWN_PROPOSAL u4000)
(define-constant ERR_UNAUTHORIZED u4001)
(define-constant ERR_ARGUMENT_ALREADY_EXISTS u4002)
(define-constant ERR_PROPOSAL_ALREADY_ACTIVE u4005)
(define-constant ERR_PROPOSAL_NOT_ACTIVE u4006)
(define-constant ERR_PROPOSAL_ALREADY_EXECUTED u4007)
(define-constant ERR_CONTRACT_MISMATCH u4008)


(define-data-var lastProposalId uint u0)

(define-map Proposals
  uint
  {
    creator: principal,
    contract: principal,
    votes: uint,
    isActive: bool,
    isExecuted: bool
  }
)

(define-map UIntArgumentsByName
  { proposalId: uint, name: (string-ascii 255) }
  { argumentId: uint, value: uint }
)

(define-map UIntArgumentsById
  { proposalId: uint, argumentId: uint }
  { name: (string-ascii 255), value: uint}
)

(define-map IntArgumentsByName
  { proposalId: uint, name: (string-ascii 255) }
  { argumentId: uint, value: int }
)

(define-map IntArgumentsById
  { proposalId: uint, argumentId: uint }
  { name: (string-ascii 255), value: int}
)

(define-map PrincipalArgumentsByName
  { proposalId: uint, name: (string-ascii 255) }
  { argumentId: uint, value: principal }
)

(define-map PrincipalArgumentsById
  { proposalId: uint, argumentId: uint }
  { name: (string-ascii 255), value: principal}
)

(define-map StringArgumentsByName
  { proposalId: uint, name: (string-ascii 255) }
  { argumentId: uint, value: (string-ascii 255) }
)

(define-map StringArgumentsById
  { proposalId: uint, argumentId: uint }
  { name: (string-ascii 255), value: (string-ascii 255) }
)

(define-map ArgumentLastIds
  { proposalId: uint, argumentType: (string-ascii 25)}
  uint
)

(define-read-only (get-argument-last-id (proposalId uint) (argumentType (string-ascii 25)))
  (default-to u0 (map-get? ArgumentLastIds { proposalId: proposalId, argumentType: argumentType }))
)

(define-private (set-argument-last-id (proposalId uint) (argumentType (string-ascii 25)) (lastId uint))
  (map-set ArgumentLastIds { proposalId: proposalId, argumentType: argumentType } lastId)
)

(define-private (generate-argument-id (proposalId uint) (argumentType (string-ascii 25)))
  (let
    (
      (argumentId (+ (get-argument-last-id proposalId argumentType) u1))
    )
    (set-argument-last-id proposalId argumentType argumentId)
    argumentId
  )
)

(define-private (guard-add-argument (proposalId uint))
  (let
    (
      (proposal (unwrap! (map-get? Proposals proposalId) (err ERR_UNKNOWN_PROPOSAL)))
    )
    (asserts! (is-eq contract-caller (get creator proposal)) (err ERR_UNAUTHORIZED))
    (asserts! (not (get isActive proposal)) (err ERR_PROPOSAL_ALREADY_ACTIVE))
    (ok true)
  )
)


(define-public (add-uint-argument (proposalId uint) (name (string-ascii 255)) (value uint))
  (let
    (
      (argumentId (generate-argument-id proposalId "uint"))
    )
    (try! (guard-add-argument proposalId))
    (asserts! 
      (map-insert UIntArgumentsByName
        { proposalId: proposalId, name: name } 
        { argumentId: argumentId, value: value }
      ) 
      (err ERR_ARGUMENT_ALREADY_EXISTS)
    )
    (map-insert UIntArgumentsById
      { proposalId: proposalId, argumentId: argumentId }
      { name: name, value: value}
    )
    (ok true)
  )
)

(define-public (add-int-argument (proposalId uint) (name (string-ascii 255)) (value int))
  (let
    (
      (argumentId (generate-argument-id proposalId "int"))
    )
    (try! (guard-add-argument proposalId))
    (asserts! 
      (map-insert IntArgumentsByName 
        { proposalId: proposalId, name: name }
        { argumentId: argumentId, value: value }
      ) 
      (err ERR_ARGUMENT_ALREADY_EXISTS)
    )
    (map-insert IntArgumentsById
      { proposalId: proposalId, argumentId: argumentId }
      { name: name, value: value}
    )
    (ok true)
  )
)

(define-public (add-principal-argument (proposalId uint) (name (string-ascii 255)) (value principal))
  (let
    (
      (argumentId (generate-argument-id proposalId "principal"))
    )
    (try! (guard-add-argument proposalId))
    (asserts! 
      (map-insert PrincipalArgumentsByName
        { proposalId: proposalId, name: name } 
        { argumentId: argumentId, value: value }
      ) 
      (err ERR_ARGUMENT_ALREADY_EXISTS)
    )
    (map-insert PrincipalArgumentsById
      { proposalId: proposalId, argumentId: argumentId }
      { name: name, value: value}
    )
    (ok true)
  )
)

(define-public (add-string-argument (proposalId uint) (name (string-ascii 255)) (value (string-ascii 255)))
  (let
    (
      (argumentId (generate-argument-id proposalId "string"))
    )
    (try! (guard-add-argument proposalId))
    (asserts!
      (map-insert StringArgumentsByName
        { proposalId: proposalId, name: name }
        { argumentId: argumentId, value: value }
      )
      (err ERR_ARGUMENT_ALREADY_EXISTS)
    )
    (map-insert StringArgumentsById
      { proposalId: proposalId, argumentId: argumentId }
      { name: name, value: value }
    )
    (ok true)
  )
)

;; UINT getters
(define-read-only (get-uint-argument-by-name (proposalId uint) (name (string-ascii 255)))
  (map-get? UIntArgumentsByName { proposalId: proposalId, name: name })
)

(define-read-only (get-uint-value-by-name (proposalId uint) (name (string-ascii 255)))
  (get value (get-uint-argument-by-name proposalId name))
)

(define-read-only (get-uint-argument-by-id (proposalId uint) (argumentId uint))
  (map-get? UIntArgumentsById { proposalId: proposalId, argumentId: argumentId })
)

(define-read-only (get-uint-value-by-id (proposalId uint) (argumentId uint))
  (get value (get-uint-argument-by-id proposalId argumentId))
)

;; INT getters
(define-read-only (get-int-argument-by-name (proposalId uint) (name (string-ascii 255)))
  (map-get? IntArgumentsByName { proposalId: proposalId, name: name })
)

(define-read-only (get-int-value-by-name (proposalId uint) (name (string-ascii 255)))
  (get value (get-int-argument-by-name proposalId name))
)

(define-read-only (get-int-argument-by-id (proposalId uint) (argumentId uint))
  (map-get? IntArgumentsById { proposalId: proposalId, argumentId: argumentId })
)

(define-read-only (get-int-value-by-id (proposalId uint) (argumentId uint))
  (get value (get-int-argument-by-id proposalId argumentId))
)

;; PRINCIPAL getters
(define-read-only (get-principal-argument-by-name (proposalId uint) (name (string-ascii 255)))
  (map-get? PrincipalArgumentsByName { proposalId: proposalId, name: name })
)

(define-read-only (get-principal-value-by-name (proposalId uint) (name (string-ascii 255)))
  (get value (get-principal-argument-by-name proposalId name))
)

(define-read-only (get-principal-argument-by-id (proposalId uint) (argumentId uint))
  (map-get? PrincipalArgumentsById { proposalId: proposalId, argumentId: argumentId })
)

(define-read-only (get-principal-value-by-id (proposalId uint) (argumentId uint))
  (get value (get-principal-argument-by-id proposalId argumentId))
)

;; STRING getters
(define-read-only (get-string-argument-by-name (proposalId uint) (name (string-ascii 255)))
  (map-get? StringArgumentsByName { proposalId: proposalId, name: name })
)

(define-read-only (get-string-value-by-name (proposalId uint) (name (string-ascii 255)))
  (get value (get-string-argument-by-name proposalId name))
)

(define-read-only (get-string-argument-by-id (proposalId uint) (argumentId uint))
  (map-get? StringArgumentsById { proposalId: proposalId, argumentId: argumentId })
)

(define-read-only (get-string-value-by-id (proposalId uint) (argumentId uint))
  (get value (get-string-argument-by-id proposalId argumentId))
)

(define-public (add-proposal (contract <proposal-trait>))
  (let
    (
      (newProposalId (+ (var-get lastProposalId) u1))
    )
    ;; some security checks
    (map-set Proposals 
      newProposalId
      { 
        creator: tx-sender,
        contract: (contract-of contract),
        votes: u0, 
        isActive: false,
        isExecuted: false
      }
    )
    (var-set lastProposalId newProposalId)
    (ok newProposalId)
  )
)

(define-read-only (get-proposal (proposalId uint))
  (map-get? Proposals proposalId)
)

(define-public (activate-proposal (proposalId uint))
  (let
    (
      (proposal (unwrap! (map-get? Proposals proposalId) (err ERR_UNKNOWN_PROPOSAL)))
    )
    (asserts! (is-eq contract-caller (get creator proposal)) (err ERR_UNAUTHORIZED))
    (asserts! (not (get isActive proposal)) (err ERR_PROPOSAL_ALREADY_ACTIVE))
    (map-set Proposals 
      proposalId 
      (merge proposal { isActive: true })
    )
    (ok true)
  )
)

(define-public (vote (proposalId uint))
  (let
    (
      (proposal (unwrap! (map-get? Proposals proposalId) (err ERR_UNKNOWN_PROPOSAL)))
    )
    (asserts! (get isActive proposal) (err ERR_PROPOSAL_NOT_ACTIVE))
    (asserts! (not (get isExecuted proposal)) (err ERR_PROPOSAL_ALREADY_EXECUTED))
    (map-set Proposals 
      proposalId 
      (merge proposal { votes: (+ (get votes proposal) u1) })
    )
    (ok true)
  )
)

(define-public (execute (contract <proposal-trait>) (proposalId uint))
  (let
    (
      (proposal (unwrap! (map-get? Proposals proposalId) (err ERR_UNKNOWN_PROPOSAL)))
    )
    (asserts! (is-eq (contract-of contract) (get contract proposal)) (err ERR_CONTRACT_MISMATCH))
    (asserts! (get isActive proposal) (err ERR_PROPOSAL_NOT_ACTIVE))
    (asserts! (not (get isExecuted proposal)) (err ERR_PROPOSAL_ALREADY_EXECUTED))
    (map-set Proposals 
      proposalId 
      (merge proposal { isExecuted: true })
    )
    (contract-call? contract execute proposalId)
  )
)