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

(define-map UIntArguments
  { proposalId: uint, name: (string-ascii 255) }
  uint
)

(define-map IntArguments
  { proposalId: uint, name: (string-ascii 255) }
  int
)

(define-map PrincipalArguments
  { proposalId: uint, name: (string-ascii 255) }
  principal
)


(define-public (add-uint-argument (proposalId uint) (name (string-ascii 255)) (value uint))
  (let
    (
      (proposal (unwrap! (map-get? Proposals proposalId) (err ERR_UNKNOWN_PROPOSAL)))
    )
    (asserts! (is-eq contract-caller (get creator proposal)) (err ERR_UNAUTHORIZED))
    (asserts! (map-insert UIntArguments { proposalId: proposalId, name: name } value) 
      (err ERR_ARGUMENT_ALREADY_EXISTS))
      
    (ok true)
  )
)

(define-read-only (get-uint-argument (proposalId uint) (name (string-ascii 255)))
  (map-get? UIntArguments { proposalId: proposalId, name: name })
)


(define-public (add-int-argument (proposalId uint) (name (string-ascii 255)) (value int))
  (let
    (
      (proposal (unwrap! (map-get? Proposals proposalId) (err ERR_UNKNOWN_PROPOSAL)))
    )
    (asserts! (is-eq contract-caller (get creator proposal)) (err ERR_UNAUTHORIZED))
    (asserts! (map-insert IntArguments { proposalId: proposalId, name: name } value) 
      (err ERR_ARGUMENT_ALREADY_EXISTS))
      
    (ok true)
  )
)

(define-read-only (get-int-argument (proposalId uint) (name (string-ascii 255)))
  (map-get? UIntArguments { proposalId: proposalId, name: name })
)

(define-public (add-principal-argument (proposalId uint) (name (string-ascii 255)) (value principal))
  (let
    (
      (proposal (unwrap! (map-get? Proposals proposalId) (err ERR_UNKNOWN_PROPOSAL)))
    )
    (asserts! (is-eq contract-caller (get creator proposal)) (err ERR_UNAUTHORIZED))
    (asserts! (map-insert PrincipalArguments { proposalId: proposalId, name: name } value) 
      (err ERR_ARGUMENT_ALREADY_EXISTS))
      
    (ok true)
  )
)

(define-read-only (get-principal-argument (proposalId uint) (name (string-ascii 255)))
  (map-get? PrincipalArguments { proposalId: proposalId, name: name })
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