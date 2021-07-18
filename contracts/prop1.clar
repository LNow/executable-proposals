(impl-trait .proposal-trait.proposal-trait)

(define-constant ERR_UNAUTHORIZED u5000)

(define-public (execute (proposalId uint))
  (begin
    (asserts! (is-eq contract-caller .proposals) (err ERR_UNAUTHORIZED))

    (print (contract-call? .proposals get-uint-value-by-name proposalId "arg1"))
    (print (contract-call? .proposals get-uint-value-by-name proposalId "arg2"))
    (print (contract-call? .proposals get-uint-value-by-name proposalId "arg3"))
    (ok true)
  )
)