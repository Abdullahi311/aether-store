;; marketplace.clar
;; A smart contract for Aether Store, a decentralized e-commerce marketplace
;; This contract manages product listings, purchases, and transaction records
;; on the Stacks blockchain, enabling secure peer-to-peer transactions.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-DETAILS (err u101))
(define-constant ERR-ZERO-QUANTITY (err u102))
(define-constant ERR-ZERO-PRICE (err u103))
(define-constant ERR-LISTING-NOT-FOUND (err u104))
(define-constant ERR-INSUFFICIENT-QUANTITY (err u105))
(define-constant ERR-BUYER-IS-SELLER (err u106))
(define-constant ERR-INSUFFICIENT-FUNDS (err u107))
(define-constant ERR-ALREADY-DELIVERED (err u108))
(define-constant ERR-TRANSACTION-NOT-FOUND (err u109))
(define-constant ERR-NOT-PARTICIPANT (err u110))
(define-constant ERR-ALREADY-REFUNDED (err u111))
(define-constant ERR-NOT-PENDING-DELIVERY (err u112))

;; Listing statuses
(define-constant STATUS-ACTIVE u1)
(define-constant STATUS-SOLD-OUT u2)
(define-constant STATUS-INACTIVE u3)
(define-constant STATUS-FLAGGED u4)

;; Transaction statuses
(define-constant TX-STATUS-PENDING u1)
(define-constant TX-STATUS-DELIVERED u2)
(define-constant TX-STATUS-REFUNDED u3)
(define-constant TX-STATUS-DISPUTED u4)

;; Data maps
;; Stores product listings information
(define-map listings
  { listing-id: uint }
  {
    seller: principal,
    title: (string-utf8 100),
    description: (string-utf8 500),
    price: uint,
    quantity: uint,
    image-url: (optional (string-utf8 256)),
    category: (string-utf8 50),
    status: uint,
    shipping-info: (string-utf8 256),
    created-at: uint
  }
)

;; Stores transaction records
(define-map transactions
  { tx-id: uint }
  {
    listing-id: uint,
    buyer: principal,
    seller: principal,
    quantity: uint,
    total-price: uint,
    status: uint,
    created-at: uint,
    delivered-at: (optional uint),
    shipping-address: (string-utf8 256)
  }
)

;; Maps to track user purchases and sales
(define-map user-purchases
  { user: principal }
  { tx-ids: (list 100 uint) }
)

(define-map user-sales
  { user: principal }
  { tx-ids: (list 100 uint) }
)

;; Maps to track listings by category and seller
(define-map listings-by-category
  { category: (string-utf8 50) }
  { listing-ids: (list 100 uint) }
)

(define-map listings-by-seller
  { seller: principal }
  { listing-ids: (list 100 uint) }
)

;; Counters for generating unique IDs
(define-data-var listing-id-counter uint u0)
(define-data-var transaction-id-counter uint u0)

;; -------------------------------------
;; Private functions
;; -------------------------------------

;; Generate a new unique listing ID
(define-private (generate-listing-id)
  (let ((current-id (var-get listing-id-counter)))
    (var-set listing-id-counter (+ current-id u1))
    current-id
  )
)

;; Generate a new unique transaction ID
(define-private (generate-tx-id)
  (let ((current-id (var-get transaction-id-counter)))
    (var-set transaction-id-counter (+ current-id u1))
    current-id
  )
)

;; Add a listing ID to a category
(define-private (add-listing-to-category (category (string-utf8 50)) (listing-id uint))
  (match (map-get? listings-by-category { category: category })
    existing-entry (map-set listings-by-category
                    { category: category }
                    { listing-ids: (unwrap-panic (as-max-len? (append (get listing-ids existing-entry) listing-id) u100)) })
    (map-insert listings-by-category
                { category: category }
                { listing-ids: (list listing-id) })
  )
)

;; Add a listing ID to a seller's listings
(define-private (add-listing-to-seller (seller principal) (listing-id uint))
  (match (map-get? listings-by-seller { seller: seller })
    existing-entry (map-set listings-by-seller
                    { seller: seller }
                    { listing-ids: (unwrap-panic (as-max-len? (append (get listing-ids existing-entry) listing-id) u100)) })
    (map-insert listings-by-seller
                { seller: seller }
                { listing-ids: (list listing-id) })
  )
)

;; Add a transaction ID to a user's purchases
(define-private (add-tx-to-purchases (buyer principal) (tx-id uint))
  (match (map-get? user-purchases { user: buyer })
    existing-entry (map-set user-purchases
                    { user: buyer }
                    { tx-ids: (unwrap-panic (as-max-len? (append (get tx-ids existing-entry) tx-id) u100)) })
    (map-insert user-purchases
                { user: buyer }
                { tx-ids: (list tx-id) })
  )
)

;; Add a transaction ID to a user's sales
(define-private (add-tx-to-sales (seller principal) (tx-id uint))
  (match (map-get? user-sales { user: seller })
    existing-entry (map-set user-sales
                    { user: seller }
                    { tx-ids: (unwrap-panic (as-max-len? (append (get tx-ids existing-entry) tx-id) u100)) })
    (map-insert user-sales
                { user: seller }
                { tx-ids: (list tx-id) })
  )
)

;; Update listing quantity after purchase
(define-private (update-listing-quantity (listing-id uint) (quantity-purchased uint))
  (match (map-get? listings { listing-id: listing-id })
    listing 
      (let ((new-quantity (- (get quantity listing) quantity-purchased))
            (new-status (if (is-eq new-quantity u0) STATUS-SOLD-OUT STATUS-ACTIVE)))
        (map-set listings
          { listing-id: listing-id }
          (merge listing { quantity: new-quantity, status: new-status })))
    ERR-LISTING-NOT-FOUND
  )
)

;; -------------------------------------
;; Read-only functions
;; -------------------------------------

;; Get listing details by ID
(define-read-only (get-listing (listing-id uint))
  (map-get? listings { listing-id: listing-id })
)

;; Get transaction details by ID
(define-read-only (get-transaction (tx-id uint))
  (map-get? transactions { tx-id: tx-id })
)

;; Get all listings for a category
(define-read-only (get-listings-by-category (category (string-utf8 50)))
  (match (map-get? listings-by-category { category: category })
    entry (get listing-ids entry)
    (list)
  )
)

;; Get all listings for a seller
(define-read-only (get-listings-by-seller (seller principal))
  (match (map-get? listings-by-seller { seller: seller })
    entry (get listing-ids entry)
    (list)
  )
)

;; Get all purchases for a user
(define-read-only (get-user-purchases (user principal))
  (match (map-get? user-purchases { user: user })
    entry (get tx-ids entry)
    (list)
  )
)

;; Get all sales for a user
(define-read-only (get-user-sales (user principal))
  (match (map-get? user-sales { user: user })
    entry (get tx-ids entry)
    (list)
  )
)

;; Check if transaction exists
(define-read-only (transaction-exists? (tx-id uint))
  (is-some (map-get? transactions { tx-id: tx-id }))
)

;; Check if listing exists
(define-read-only (listing-exists? (listing-id uint))
  (is-some (map-get? listings { listing-id: listing-id }))
)

;; -------------------------------------
;; Public functions
;; -------------------------------------

;; Create a new product listing
(define-public (create-listing 
  (title (string-utf8 100)) 
  (description (string-utf8 500)) 
  (price uint) 
  (quantity uint) 
  (image-url (optional (string-utf8 256))) 
  (category (string-utf8 50))
  (shipping-info (string-utf8 256)))

  (let ((listing-id (generate-listing-id))
        (seller tx-sender)
        (block-height block-height))
    
    ;; Input validation
    (asserts! (> quantity u0) ERR-ZERO-QUANTITY)
    (asserts! (> price u0) ERR-ZERO-PRICE)
    
    ;; Create the listing
    (map-insert listings
      { listing-id: listing-id }
      {
        seller: seller,
        title: title,
        description: description,
        price: price,
        quantity: quantity,
        image-url: image-url,
        category: category,
        status: STATUS-ACTIVE,
        shipping-info: shipping-info,
        created-at: block-height
      }
    )
    
    ;; Update indexes
    (add-listing-to-category category listing-id)
    (add-listing-to-seller seller listing-id)
    
    (ok listing-id)
  )
)

;; Update an existing listing
(define-public (update-listing
  (listing-id uint)
  (title (string-utf8 100))
  (description (string-utf8 500))
  (price uint)
  (quantity uint)
  (image-url (optional (string-utf8 256)))
  (category (string-utf8 50))
  (shipping-info (string-utf8 256))
  (status uint))
  
  (let ((listing (unwrap! (map-get? listings { listing-id: listing-id }) ERR-LISTING-NOT-FOUND)))
    
    ;; Authorization check
    (asserts! (is-eq tx-sender (get seller listing)) ERR-NOT-AUTHORIZED)
    
    ;; Input validation
    (asserts! (> price u0) ERR-ZERO-PRICE)
    (asserts! (or (> quantity u0) (is-eq status STATUS-INACTIVE)) ERR-ZERO-QUANTITY)
    
    ;; If category changed, update indexes
    (if (not (is-eq category (get category listing)))
      (add-listing-to-category category listing-id)
      true)
      
    ;; Update the listing
    (map-set listings
      { listing-id: listing-id }
      {
        seller: (get seller listing),
        title: title,
        description: description,
        price: price,
        quantity: quantity,
        image-url: image-url,
        category: category,
        status: status,
        shipping-info: shipping-info,
        created-at: (get created-at listing)
      }
    )
    
    (ok true)
  )
)

;; Purchase a product
(define-public (purchase-product
  (listing-id uint)
  (quantity uint)
  (shipping-address (string-utf8 256)))
  
  (let ((listing (unwrap! (map-get? listings { listing-id: listing-id }) ERR-LISTING-NOT-FOUND))
        (buyer tx-sender)
        (seller (get seller listing))
        (price (get price listing))
        (available-qty (get quantity listing))
        (total-price (* price quantity))
        (block-height block-height)
        (tx-id (generate-tx-id)))
    
    ;; Input validation
    (asserts! (> quantity u0) ERR-ZERO-QUANTITY)
    (asserts! (>= available-qty quantity) ERR-INSUFFICIENT-QUANTITY)
    (asserts! (not (is-eq buyer seller)) ERR-BUYER-IS-SELLER)
    (asserts! (is-eq (get status listing) STATUS-ACTIVE) ERR-LISTING-NOT-FOUND)
    
    ;; Transfer funds from buyer to contract (escrow)
    (try! (stx-transfer? total-price buyer (as-contract tx-sender)))
    
    ;; Update listing quantity
    (try! (as-contract (update-listing-quantity listing-id quantity)))
    
    ;; Record the transaction
    (map-insert transactions
      { tx-id: tx-id }
      {
        listing-id: listing-id,
        buyer: buyer,
        seller: seller,
        quantity: quantity,
        total-price: total-price,
        status: TX-STATUS-PENDING,
        created-at: block-height,
        delivered-at: none,
        shipping-address: shipping-address
      }
    )
    
    ;; Update user records
    (add-tx-to-purchases buyer tx-id)
    (add-tx-to-sales seller tx-id)
    
    (ok tx-id)
  )
)

;; Confirm delivery and release payment to seller
(define-public (confirm-delivery (tx-id uint))
  (let ((tx (unwrap! (map-get? transactions { tx-id: tx-id }) ERR-TRANSACTION-NOT-FOUND))
        (buyer tx-sender)
        (seller (get seller tx))
        (block-height block-height))
    
    ;; Authorization check
    (asserts! (is-eq buyer (get buyer tx)) ERR-NOT-AUTHORIZED)
    
    ;; Status check
    (asserts! (is-eq (get status tx) TX-STATUS-PENDING) ERR-ALREADY-DELIVERED)
    
    ;; Transfer funds from contract to seller
    (try! (as-contract (stx-transfer? (get total-price tx) tx-sender seller)))
    
    ;; Update transaction status
    (map-set transactions
      { tx-id: tx-id }
      (merge tx { 
        status: TX-STATUS-DELIVERED, 
        delivered-at: (some block-height)
      })
    )
    
    (ok true)
  )
)

;; Request refund (cancel order)
(define-public (request-refund (tx-id uint))
  (let ((tx (unwrap! (map-get? transactions { tx-id: tx-id }) ERR-TRANSACTION-NOT-FOUND))
        (buyer tx-sender)
        (listing-id (get listing-id tx)))
    
    ;; Authorization check
    (asserts! (is-eq buyer (get buyer tx)) ERR-NOT-AUTHORIZED)
    
    ;; Status check
    (asserts! (is-eq (get status tx) TX-STATUS-PENDING) ERR-NOT-PENDING-DELIVERY)
    
    ;; Transfer funds from contract back to buyer
    (try! (as-contract (stx-transfer? (get total-price tx) tx-sender buyer)))
    
    ;; Update transaction status
    (map-set transactions
      { tx-id: tx-id }
      (merge tx { status: TX-STATUS-REFUNDED })
    )
    
    ;; Restore listing quantity
    (match (map-get? listings { listing-id: listing-id })
      listing 
        (let ((updated-quantity (+ (get quantity listing) (get quantity tx)))
              (updated-status (if (is-eq (get status listing) STATUS-SOLD-OUT) 
                                STATUS-ACTIVE 
                                (get status listing))))
          (map-set listings
            { listing-id: listing-id }
            (merge listing { 
              quantity: updated-quantity, 
              status: updated-status 
            })))
      ERR-LISTING-NOT-FOUND
    )
    
    (ok true)
  )
)

;; Flag a listing for review
(define-public (flag-listing (listing-id uint) (reason (string-utf8 256)))
  (let ((listing (unwrap! (map-get? listings { listing-id: listing-id }) ERR-LISTING-NOT-FOUND)))
    
    ;; Update the listing status to flagged
    (map-set listings
      { listing-id: listing-id }
      (merge listing { status: STATUS-FLAGGED })
    )
    
    ;; In a real implementation, could store the reason and reporter in a separate map
    ;; or emit an event with the information
    
    (ok true)
  )
)

;; Open a dispute for a transaction
(define-public (open-dispute (tx-id uint) (reason (string-utf8 256)))
  (let ((tx (unwrap! (map-get? transactions { tx-id: tx-id }) ERR-TRANSACTION-NOT-FOUND))
        (user tx-sender))
    
    ;; Check that user is either buyer or seller
    (asserts! (or (is-eq user (get buyer tx)) (is-eq user (get seller tx))) ERR-NOT-PARTICIPANT)
    
    ;; Check that transaction is not already refunded
    (asserts! (not (is-eq (get status tx) TX-STATUS-REFUNDED)) ERR-ALREADY-REFUNDED)
    
    ;; Update transaction status
    (map-set transactions
      { tx-id: tx-id }
      (merge tx { status: TX-STATUS-DISPUTED })
    )
    
    ;; In a real implementation, could store the reason and dispute details in a separate map
    ;; or emit an event with the information
    
    (ok true)
  )
)