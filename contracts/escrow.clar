;; escrow.clar
;; Purpose: This smart contract implements a secure escrow system for the Aether Store marketplace,
;; facilitating transactions between buyers and sellers by holding funds until delivery conditions
;; are met or disputes are resolved. The contract enforces a structured lifecycle for transactions
;; with built-in timeouts and dispute resolution mechanisms.

;; =========================================
;; Constants and Error Codes
;; =========================================

;; Transaction status constants
(define-constant TRANSACTION-STATUS-PENDING u1)
(define-constant TRANSACTION-STATUS-COMPLETED u2)
(define-constant TRANSACTION-STATUS-DISPUTED u3)
(define-constant TRANSACTION-STATUS-REFUNDED u4)
(define-constant TRANSACTION-STATUS-EXPIRED u5)
(define-constant TRANSACTION-STATUS-RESOLVED u6)

;; Dispute resolution results
(define-constant RESOLUTION-FULL-REFUND u1)
(define-constant RESOLUTION-PARTIAL-REFUND u2)
(define-constant RESOLUTION-RELEASE-TO-SELLER u3)

;; Dispute timeframes (in blocks)
(define-constant DISPUTE-WINDOW-BLOCKS u144) ;; ~24 hours at 10 minute block time
(define-constant DELIVERY-WINDOW-BLOCKS u1008) ;; ~7 days at 10 minute block time

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u1000))
(define-constant ERR-INVALID-TRANSACTION (err u1001))
(define-constant ERR-INVALID-AMOUNT (err u1002))
(define-constant ERR-ALREADY-CONFIRMED (err u1003))
(define-constant ERR-ALREADY-DISPUTED (err u1004))
(define-constant ERR-DISPUTE-WINDOW-EXPIRED (err u1005))
(define-constant ERR-DISPUTE-WINDOW-ACTIVE (err u1006))
(define-constant ERR-INVALID-STATUS (err u1007))
(define-constant ERR-DELIVERY-WINDOW-EXPIRED (err u1008))
(define-constant ERR-INSUFFICIENT-FUNDS (err u1009))
(define-constant ERR-NOT-DISPUTER (err u1010))
(define-constant ERR-NOT-ARBITRATOR (err u1011))
(define-constant ERR-EVIDENCE-TOO-LARGE (err u1012))

;; Contract owner - could be a DAO or multisig in production
(define-constant CONTRACT-OWNER tx-sender)

;; =========================================
;; Data Maps and Variables
;; =========================================

;; Transactions map to track all escrow transactions
(define-map transactions
  { transaction-id: uint }
  {
    buyer: principal,
    seller: principal,
    amount: uint,
    status: uint,
    created-at: uint,
    confirmed-at: uint,
    delivery-deadline: uint,
    dispute-deadline: uint,
    evidence-buyer: (optional (string-utf8 1024)),
    evidence-seller: (optional (string-utf8 1024)),
    resolution-type: (optional uint),
    refund-amount: (optional uint)
  }
)

;; Track transaction IDs for each user (buyer or seller)
(define-map user-transactions
  { user: principal }
  { tx-ids: (list 50 uint) }
)

;; Arbitrators map for users authorized to resolve disputes
(define-map arbitrators
  { arbitrator: principal }
  { active: bool }
)

;; Transaction counter for generating unique IDs
(define-data-var transaction-counter uint u0)

;; Fee percentage (in basis points, e.g., 100 = 1%)
(define-data-var fee-basis-points uint u100)

;; Arbitrator account where fees are sent
(define-data-var fee-collector principal CONTRACT-OWNER)

;; =========================================
;; Private Functions
;; =========================================

;; Generates a new unique transaction ID
(define-private (generate-transaction-id)
  (let ((current-id (var-get transaction-counter)))
    (var-set transaction-counter (+ current-id u1))
    current-id
  )
)

;; Adds a transaction ID to a user's list of transactions
(define-private (add-tx-to-user (user principal) (tx-id uint))
  (let (
    (current-txs (default-to { tx-ids: (list) } (map-get? user-transactions { user: user })))
    (updated-txs (unwrap-panic (as-max-len? (append (get tx-ids current-txs) tx-id) u50)))
  )
    (map-set user-transactions 
      { user: user } 
      { tx-ids: updated-txs }
    )
  )
)

;; Calculates the fee amount for a transaction
(define-private (calculate-fee (amount uint))
  (/ (* amount (var-get fee-basis-points)) u10000)
)

;; Validates that the sender is the buyer for a given transaction
(define-private (is-buyer (transaction-id uint))
  (let ((tx (unwrap! (map-get? transactions { transaction-id: transaction-id }) false)))
    (is-eq tx-sender (get buyer tx))
  )
)

;; Validates that the sender is the seller for a given transaction
(define-private (is-seller (transaction-id uint))
  (let ((tx (unwrap! (map-get? transactions { transaction-id: transaction-id }) false)))
    (is-eq tx-sender (get seller tx))
  )
)

;; Validates that the sender is an active arbitrator
(define-private (is-arbitrator)
  (default-to false
    (get active (map-get? arbitrators { arbitrator: tx-sender }))
  )
)

;; Validates transaction exists and returns it
(define-private (get-transaction (transaction-id uint))
  (map-get? transactions { transaction-id: transaction-id })
)

;; =========================================
;; Read-Only Functions
;; =========================================

;; Get transaction details
(define-read-only (get-transaction-details (transaction-id uint))
  (map-get? transactions { transaction-id: transaction-id })
)

;; Get user's transactions
(define-read-only (get-user-transactions (user principal))
  (default-to { tx-ids: (list) } (map-get? user-transactions { user: user }))
)

;; Check if a user is an active arbitrator
(define-read-only (is-active-arbitrator (user principal))
  (default-to false
    (get active (map-get? arbitrators { arbitrator: user }))
  )
)

;; Get the current fee rate
(define-read-only (get-fee-basis-points)
  (var-get fee-basis-points)
)

;; Get the fee collector address
(define-read-only (get-fee-collector)
  (var-get fee-collector)
)

;; =========================================
;; Public Functions
;; =========================================

;; Create a new escrow transaction
;; Buyer places funds in escrow to purchase from seller
(define-public (create-transaction (seller principal) (amount uint))
  (let (
    (transaction-id (generate-transaction-id))
    (buyer tx-sender)
    (fee (calculate-fee amount))
    (current-block-height block-height)
    (delivery-deadline (+ current-block-height DELIVERY-WINDOW-BLOCKS))
  )
    ;; Validate inputs
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (not (is-eq buyer seller)) ERR-INVALID-TRANSACTION)
    
    ;; Transfer funds from buyer to contract
    (try! (stx-transfer? amount buyer (as-contract tx-sender)))
    
    ;; Record the transaction
    (map-set transactions
      { transaction-id: transaction-id }
      {
        buyer: buyer,
        seller: seller,
        amount: amount,
        status: TRANSACTION-STATUS-PENDING,
        created-at: current-block-height,
        confirmed-at: u0,
        delivery-deadline: delivery-deadline,
        dispute-deadline: u0,
        evidence-buyer: none,
        evidence-seller: none,
        resolution-type: none,
        refund-amount: none
      }
    )
    
    ;; Add transaction ID to both buyer and seller records
    (add-tx-to-user buyer transaction-id)
    (add-tx-to-user seller transaction-id)
    
    ;; Return the transaction ID
    (ok transaction-id)
  )
)

;; Confirm delivery for a transaction
;; Called by buyer to confirm goods/services were received satisfactorily
(define-public (confirm-delivery (transaction-id uint))
  (let (
    (tx (unwrap! (get-transaction transaction-id) ERR-INVALID-TRANSACTION))
    (current-block-height block-height)
    (dispute-deadline (+ current-block-height DISPUTE-WINDOW-BLOCKS))
  )
    ;; Verify caller is the buyer
    (asserts! (is-eq tx-sender (get buyer tx)) ERR-NOT-AUTHORIZED)
    
    ;; Verify transaction is in PENDING status
    (asserts! (is-eq (get status tx) TRANSACTION-STATUS-PENDING) ERR-INVALID-STATUS)
    
    ;; Verify delivery deadline hasn't passed
    (asserts! (<= current-block-height (get delivery-deadline tx)) ERR-DELIVERY-WINDOW-EXPIRED)
    
    ;; Update transaction with confirmation and dispute window
    (map-set transactions
      { transaction-id: transaction-id }
      (merge tx {
        status: TRANSACTION-STATUS-PENDING,
        confirmed-at: current-block-height,
        dispute-deadline: dispute-deadline
      })
    )
    
    (ok true)
  )
)

;; Release funds after dispute window expires
;; Can be called by anyone, but typically the seller once dispute window has passed
(define-public (release-funds (transaction-id uint))
  (let (
    (tx (unwrap! (get-transaction transaction-id) ERR-INVALID-TRANSACTION))
    (current-block-height block-height)
    (seller (get seller tx))
    (amount (get amount tx))
    (fee (calculate-fee amount))
    (seller-amount (- amount fee))
  )
    ;; Verify transaction is confirmed and dispute window has passed
    (asserts! (> (get confirmed-at tx) u0) ERR-INVALID-STATUS)
    (asserts! (is-eq (get status tx) TRANSACTION-STATUS-PENDING) ERR-INVALID-STATUS)
    (asserts! (> current-block-height (get dispute-deadline tx)) ERR-DISPUTE-WINDOW-ACTIVE)
    
    ;; Update transaction status
    (map-set transactions
      { transaction-id: transaction-id }
      (merge tx { status: TRANSACTION-STATUS-COMPLETED })
    )
    
    ;; Transfer funds to seller minus fee
    (try! (as-contract (stx-transfer? seller-amount tx-sender seller)))
    
    ;; Transfer fee to fee collector
    (try! (as-contract (stx-transfer? fee tx-sender (var-get fee-collector))))
    
    (ok true)
  )
)

;; File a dispute for a transaction
;; Can be called by buyer only during the dispute window
(define-public (file-dispute (transaction-id uint) (evidence (optional (string-utf8 1024))))
  (let (
    (tx (unwrap! (get-transaction transaction-id) ERR-INVALID-TRANSACTION))
    (current-block-height block-height)
  )
    ;; Verify caller is the buyer
    (asserts! (is-eq tx-sender (get buyer tx)) ERR-NOT-AUTHORIZED)
    
    ;; Verify transaction is confirmed
    (asserts! (> (get confirmed-at tx) u0) ERR-INVALID-STATUS)
    
    ;; Verify transaction is in PENDING status
    (asserts! (is-eq (get status tx) TRANSACTION-STATUS-PENDING) ERR-INVALID-STATUS)
    
    ;; Verify we're within dispute window
    (asserts! (<= current-block-height (get dispute-deadline tx)) ERR-DISPUTE-WINDOW-EXPIRED)
    
    ;; Update transaction status and add buyer evidence
    (map-set transactions
      { transaction-id: transaction-id }
      (merge tx {
        status: TRANSACTION-STATUS-DISPUTED,
        evidence-buyer: evidence
      })
    )
    
    (ok true)
  )
)

;; Submit evidence for a disputed transaction
;; Called by seller to provide their side of the story
(define-public (submit-evidence (transaction-id uint) (evidence (optional (string-utf8 1024))))
  (let (
    (tx (unwrap! (get-transaction transaction-id) ERR-INVALID-TRANSACTION))
  )
    ;; Verify caller is the seller
    (asserts! (is-eq tx-sender (get seller tx)) ERR-NOT-AUTHORIZED)
    
    ;; Verify transaction is in DISPUTED status
    (asserts! (is-eq (get status tx) TRANSACTION-STATUS-DISPUTED) ERR-INVALID-STATUS)
    
    ;; Update seller evidence
    (map-set transactions
      { transaction-id: transaction-id }
      (merge tx { evidence-seller: evidence })
    )
    
    (ok true)
  )
)

;; Resolve a dispute
;; Called by an authorized arbitrator to make final decision
(define-public (resolve-dispute (transaction-id uint) (resolution-type uint) (refund-amount uint))
  (let (
    (tx (unwrap! (get-transaction transaction-id) ERR-INVALID-TRANSACTION))
    (buyer (get buyer tx))
    (seller (get seller tx))
    (total-amount (get amount tx))
    (fee (calculate-fee total-amount))
  )
    ;; Verify caller is an arbitrator
    (asserts! (is-arbitrator) ERR-NOT-ARBITRATOR)
    
    ;; Verify transaction is in DISPUTED status
    (asserts! (is-eq (get status tx) TRANSACTION-STATUS-DISPUTED) ERR-INVALID-STATUS)
    
    ;; Verify resolution type is valid
    (asserts! (or 
      (is-eq resolution-type RESOLUTION-FULL-REFUND)
      (is-eq resolution-type RESOLUTION-PARTIAL-REFUND)
      (is-eq resolution-type RESOLUTION-RELEASE-TO-SELLER)
    ) ERR-INVALID-TRANSACTION)
    
    ;; If partial refund, verify refund amount is less than total
    (when (is-eq resolution-type RESOLUTION-PARTIAL-REFUND)
      (asserts! (and (> refund-amount u0) (< refund-amount total-amount)) ERR-INVALID-AMOUNT)
    )
    
    ;; Update transaction status
    (map-set transactions
      { transaction-id: transaction-id }
      (merge tx {
        status: TRANSACTION-STATUS-RESOLVED,
        resolution-type: (some resolution-type),
        refund-amount: (some refund-amount)
      })
    )
    
    ;; Process refund or release based on resolution type
    (if (is-eq resolution-type RESOLUTION-FULL-REFUND)
      ;; Full refund to buyer
      (try! (as-contract (stx-transfer? total-amount tx-sender buyer)))
      
      (if (is-eq resolution-type RESOLUTION-PARTIAL-REFUND)
        (begin
          ;; Partial refund - split between buyer and seller
          (try! (as-contract (stx-transfer? refund-amount tx-sender buyer)))
          (try! (as-contract (stx-transfer? (- total-amount refund-amount fee) tx-sender seller)))
          (try! (as-contract (stx-transfer? fee tx-sender (var-get fee-collector))))
        )
        
        ;; Release to seller
        (begin
          (try! (as-contract (stx-transfer? (- total-amount fee) tx-sender seller)))
          (try! (as-contract (stx-transfer? fee tx-sender (var-get fee-collector))))
        )
      )
    )
    
    (ok true)
  )
)

;; Process refund for expired delivery
;; If seller fails to deliver and delivery window expires, buyer can claim refund
(define-public (claim-expired-delivery-refund (transaction-id uint))
  (let (
    (tx (unwrap! (get-transaction transaction-id) ERR-INVALID-TRANSACTION))
    (current-block-height block-height)
    (buyer (get buyer tx))
    (total-amount (get amount tx))
  )
    ;; Verify caller is the buyer
    (asserts! (is-eq tx-sender buyer) ERR-NOT-AUTHORIZED)
    
    ;; Verify transaction is in PENDING status
    (asserts! (is-eq (get status tx) TRANSACTION-STATUS-PENDING) ERR-INVALID-STATUS)
    
    ;; Verify delivery deadline has passed and no confirmation
    (asserts! (> current-block-height (get delivery-deadline tx)) ERR-INVALID-STATUS)
    (asserts! (is-eq (get confirmed-at tx) u0) ERR-INVALID-STATUS)
    
    ;; Update transaction status
    (map-set transactions
      { transaction-id: transaction-id }
      (merge tx { status: TRANSACTION-STATUS-EXPIRED })
    )
    
    ;; Process refund to buyer
    (try! (as-contract (stx-transfer? total-amount tx-sender buyer)))
    
    (ok true)
  )
)

;; =========================================
;; Administrative Functions
;; =========================================

;; Add a new arbitrator
(define-public (add-arbitrator (arbitrator principal))
  (begin
    ;; Only contract owner can add arbitrators
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    ;; Set arbitrator as active
    (map-set arbitrators
      { arbitrator: arbitrator }
      { active: true }
    )
    
    (ok true)
  )
)

;; Remove an arbitrator
(define-public (remove-arbitrator (arbitrator principal))
  (begin
    ;; Only contract owner can remove arbitrators
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    ;; Set arbitrator as inactive
    (map-set arbitrators
      { arbitrator: arbitrator }
      { active: false }
    )
    
    (ok true)
  )
)

;; Update fee basis points
(define-public (set-fee-basis-points (new-fee-basis-points uint))
  (begin
    ;; Only contract owner can update fees
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    ;; Update fee percentage (max 10%)
    (asserts! (<= new-fee-basis-points u1000) ERR-INVALID-AMOUNT)
    (var-set fee-basis-points new-fee-basis-points)
    
    (ok true)
  )
)

;; Update fee collector address
(define-public (set-fee-collector (new-fee-collector principal))
  (begin
    ;; Only contract owner can update fee collector
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    ;; Update fee collector
    (var-set fee-collector new-fee-collector)
    
    (ok true)
  )
)