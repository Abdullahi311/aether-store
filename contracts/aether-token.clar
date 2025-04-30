;; aether-token
;; A utility token contract for the Aether decentralized marketplace platform
;; This token enables governance voting, fee discounts, and rewards for marketplace participation

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-BALANCE (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-TOKEN-ALREADY-INITIALIZED (err u103))
(define-constant ERR-NOT-ENOUGH-STAKED (err u104))
(define-constant ERR-STAKE-IN-LOCK-PERIOD (err u105))
(define-constant ERR-PROPOSAL-ALREADY-EXISTS (err u106))
(define-constant ERR-PROPOSAL-DOES-NOT-EXIST (err u107))
(define-constant ERR-ALREADY-VOTED (err u108))
(define-constant ERR-PROPOSAL-CLOSED (err u109))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant TOKEN-NAME "Aether Token")
(define-constant TOKEN-SYMBOL "AETH")
(define-constant TOKEN-DECIMALS u6)
(define-constant TOKEN-SUPPLY u1000000000000) ;; 1 billion tokens with 6 decimals
(define-constant MINIMUM-STAKE u1000000) ;; Minimum amount to stake (1 token)
(define-constant STAKE-LOCK-PERIOD u1440) ;; ~10 days in blocks (assuming 10 min block times)
(define-constant PROPOSAL-DURATION u4320) ;; ~30 days in blocks

;; SIP-010 token trait
(impl-trait 'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE.sip-010-trait-ft-standard.sip-010-trait)

;; Data variables
(define-data-var token-initialized bool false)
(define-data-var total-supply uint TOKEN-SUPPLY)
(define-data-var proposal-count uint u0)

;; Data maps
(define-map token-balances principal uint)
(define-map token-staked-balances 
  { staker: principal } 
  { amount: uint, lock-until: uint })

(define-map token-allowances 
  { owner: principal, spender: principal } 
  uint)

(define-map proposals 
  uint 
  { title: (string-ascii 100), 
    description: (string-utf8 500), 
    proposer: principal, 
    expiration: uint, 
    for-votes: uint, 
    against-votes: uint })

(define-map user-votes 
  { proposal-id: uint, voter: principal } 
  { vote: bool })

;; Private functions

;; Initialize the token
(define-private (initialize-token)
  (begin
    (asserts! (not (var-get token-initialized)) ERR-TOKEN-ALREADY-INITIALIZED)
    (map-set token-balances CONTRACT-OWNER TOKEN-SUPPLY)
    (var-set token-initialized true)
    (print { type: "token-initialized", total-supply: TOKEN-SUPPLY })
    (ok true)))

;; Transfer tokens with validation
(define-private (transfer-token (sender principal) (recipient principal) (amount uint))
  (let ((sender-balance (default-to u0 (map-get? token-balances sender))))
    (asserts! (>= sender-balance amount) ERR-INSUFFICIENT-BALANCE)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (map-set token-balances sender (- sender-balance amount))
    (map-set token-balances recipient 
      (+ (default-to u0 (map-get? token-balances recipient)) amount))
    (print { type: "ft_transfer", sender: sender, recipient: recipient, amount: amount })
    (ok true)))

;; Read-only functions

;; Get token name
(define-read-only (get-name)
  (ok TOKEN-NAME))

;; Get token symbol
(define-read-only (get-symbol)
  (ok TOKEN-SYMBOL))

;; Get token decimals
(define-read-only (get-decimals)
  (ok TOKEN-DECIMALS))

;; Get token total supply
(define-read-only (get-total-supply)
  (ok (var-get total-supply)))

;; Get balance of a token holder
(define-read-only (get-balance (owner principal))
  (ok (default-to u0 (map-get? token-balances owner))))

;; Get staked balance of a token holder
(define-read-only (get-staked-balance (staker principal))
  (let ((staked-info (map-get? token-staked-balances { staker: staker })))
    (if (is-some staked-info)
      (ok (get amount (unwrap-panic staked-info)))
      (ok u0))))

;; Get allowance for a spender
(define-read-only (get-token-uri)
  (ok (some u"https://aether.xyz/token-metadata.json")))

;; Get proposal details
(define-read-only (get-proposal (proposal-id uint))
  (let ((proposal (map-get? proposals proposal-id)))
    (match proposal
      prop (ok prop)
      (err ERR-PROPOSAL-DOES-NOT-EXIST))))

;; Get user vote for a specific proposal
(define-read-only (get-user-vote (proposal-id uint) (voter principal))
  (let ((vote (map-get? user-votes { proposal-id: proposal-id, voter: voter })))
    (match vote
      v (ok v)
      (ok { vote: false }))))

;; Calculate fee discount based on staked amount
(define-read-only (get-fee-discount (user principal))
  (let ((staked-info (map-get? token-staked-balances { staker: user })))
    (if (is-some staked-info)
      (let ((staked-amount (get amount (unwrap-panic staked-info))))
        (cond
          ((>= staked-amount (* u1000 MINIMUM-STAKE)) (ok u50)) ;; 50% discount for 1000+ tokens staked
          ((>= staked-amount (* u100 MINIMUM-STAKE)) (ok u25))  ;; 25% discount for 100+ tokens staked 
          ((>= staked-amount (* u10 MINIMUM-STAKE)) (ok u10))   ;; 10% discount for 10+ tokens staked
          ((>= staked-amount MINIMUM-STAKE) (ok u5))            ;; 5% discount for 1+ token staked
          (true (ok u0))))
      (ok u0))))

;; SIP-010 Functions
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (or (is-eq tx-sender sender) 
                  (is-eq tx-sender CONTRACT-OWNER)) 
              ERR-NOT-AUTHORIZED)
    
    (match (transfer-token sender recipient amount)
      success 
        (begin
          (match memo 
            m (print { type: "ft_transfer_memo", memo: m })
            none)
          (ok true))
      error (err error))))

(define-public (get-allowance (owner principal) (spender principal))
  (ok (default-to u0 (map-get? token-allowances { owner: owner, spender: spender }))))

(define-public (set-allowance (spender principal) (amount uint))
  (begin
    (map-set token-allowances { owner: tx-sender, spender: spender } amount)
    (print { type: "ft_approve", spender: spender, amount: amount })
    (ok true)))

(define-public (revoke-allowance (spender principal))
  (begin
    (map-delete token-allowances { owner: tx-sender, spender: spender })
    (print { type: "ft_revoke", spender: spender })
    (ok true)))

(define-public (transfer-from (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (let ((allowance (default-to u0 (map-get? token-allowances { owner: sender, spender: tx-sender }))))
    (asserts! (>= allowance amount) ERR-NOT-AUTHORIZED)
    (map-set token-allowances { owner: sender, spender: tx-sender } (- allowance amount))
    
    (match (transfer-token sender recipient amount)
      success 
        (begin
          (match memo 
            m (print { type: "ft_transfer_memo", memo: m })
            none)
          (ok true))
      error (err error))))

;; Staking functions

;; Stake tokens for platform benefits
(define-public (stake-tokens (amount uint))
  (let ((current-balance (default-to u0 (map-get? token-balances tx-sender)))
        (current-stake-info (map-get? token-staked-balances { staker: tx-sender })))
    
    ;; Validate stake amount
    (asserts! (>= amount MINIMUM-STAKE) ERR-INVALID-AMOUNT)
    (asserts! (>= current-balance amount) ERR-INSUFFICIENT-BALANCE)
    
    ;; Update balances
    (map-set token-balances tx-sender (- current-balance amount))
    
    ;; Update staked amount
    (if (is-some current-stake-info)
      (let ((current-stake (get amount (unwrap-panic current-stake-info))))
        (map-set token-staked-balances 
          { staker: tx-sender } 
          { amount: (+ current-stake amount), 
            lock-until: (+ block-height STAKE-LOCK-PERIOD) }))
      (map-set token-staked-balances 
        { staker: tx-sender } 
        { amount: amount, 
          lock-until: (+ block-height STAKE-LOCK-PERIOD) }))
    
    (print { type: "stake", staker: tx-sender, amount: amount })
    (ok true)))

;; Unstake tokens
(define-public (unstake-tokens (amount uint))
  (let ((current-stake-info (map-get? token-staked-balances { staker: tx-sender })))
    
    ;; Validate unstake
    (asserts! (is-some current-stake-info) ERR-NOT-ENOUGH-STAKED)
    
    (let ((staked-data (unwrap-panic current-stake-info))
          (staked-amount (get amount staked-data))
          (lock-until (get lock-until staked-data)))
      
      ;; Check staked amount and lock period
      (asserts! (>= staked-amount amount) ERR-NOT-ENOUGH-STAKED)
      (asserts! (>= block-height lock-until) ERR-STAKE-IN-LOCK-PERIOD)
      
      ;; Update stake balance
      (if (is-eq staked-amount amount)
        (map-delete token-staked-balances { staker: tx-sender })
        (map-set token-staked-balances 
          { staker: tx-sender } 
          { amount: (- staked-amount amount), 
            lock-until: lock-until }))
      
      ;; Return tokens to user balance
      (map-set token-balances 
        tx-sender 
        (+ (default-to u0 (map-get? token-balances tx-sender)) amount))
      
      (print { type: "unstake", staker: tx-sender, amount: amount })
      (ok true))))

;; Governance functions

;; Create a new governance proposal
(define-public (create-proposal (title (string-ascii 100)) (description (string-utf8 500)))
  (let ((staked-info (map-get? token-staked-balances { staker: tx-sender }))
        (proposal-id (var-get proposal-count)))
    
    ;; Must have significant stake to create proposal
    (asserts! (and (is-some staked-info) 
                  (>= (get amount (unwrap-panic staked-info)) 
                      (* u100 MINIMUM-STAKE))) 
              ERR-NOT-ENOUGH-STAKED)
    
    ;; Create the proposal
    (map-set proposals 
      proposal-id
      { title: title,
        description: description,
        proposer: tx-sender,
        expiration: (+ block-height PROPOSAL-DURATION),
        for-votes: u0,
        against-votes: u0 })
    
    ;; Increment proposal counter
    (var-set proposal-count (+ proposal-id u1))
    
    (print { type: "proposal-created", proposal-id: proposal-id, proposer: tx-sender })
    (ok proposal-id)))

;; Vote on a proposal
(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let ((proposal (map-get? proposals proposal-id))
        (staked-info (map-get? token-staked-balances { staker: tx-sender })))
    
    ;; Validations
    (asserts! (is-some proposal) ERR-PROPOSAL-DOES-NOT-EXIST)
    (asserts! (is-some staked-info) ERR-NOT-ENOUGH-STAKED)
    
    (let ((prop (unwrap-panic proposal))
          (staked-amount (get amount (unwrap-panic staked-info))))
      
      ;; Check if proposal is still active
      (asserts! (< block-height (get expiration prop)) ERR-PROPOSAL-CLOSED)
      
      ;; Check if user already voted
      (asserts! (is-none (map-get? user-votes { proposal-id: proposal-id, voter: tx-sender })) 
                ERR-ALREADY-VOTED)
      
      ;; Record the vote
      (map-set user-votes 
        { proposal-id: proposal-id, voter: tx-sender } 
        { vote: vote-for })
      
      ;; Update vote counts based on staked amount (voting power = stake amount)
      (if vote-for
        (map-set proposals 
          proposal-id 
          (merge prop { for-votes: (+ (get for-votes prop) staked-amount) }))
        (map-set proposals 
          proposal-id 
          (merge prop { against-votes: (+ (get against-votes prop) staked-amount) })))
      
      (print { type: "vote", proposal-id: proposal-id, voter: tx-sender, vote: vote-for })
      (ok true))))

;; Rewards functions

;; Reward user for marketplace activity
;; Can only be called by contract owner or authorized principals
(define-public (reward-user (user principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (transfer amount CONTRACT-OWNER user none)))

;; Initialize the token on contract deployment
(begin
  (initialize-token))