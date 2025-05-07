;; user-profiles
;; A contract for managing user identities, reputation scores, and verification status
;; in the Aether Store decentralized marketplace.
;;
;; This contract enables users to:
;; - Create and manage their profile information
;; - Verify their identity through attestations
;; - Build reputation through successful transactions and community feedback
;; - Progress through reputation tiers based on marketplace activity

;; Error constants
(define-constant ERR-NOT-FOUND (err u100))
(define-constant ERR-USER-EXISTS (err u101))
(define-constant ERR-UNAUTHORIZED (err u102))
(define-constant ERR-INVALID-REPUTATION (err u103))
(define-constant ERR-NOT-VERIFIED (err u104))
(define-constant ERR-INVALID-INPUT (err u105))
(define-constant ERR-SELF-REVIEW (err u106))
(define-constant ERR-MAX-REPUTATION (err u107))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-REPUTATION u1000)
(define-constant MIN-REPUTATION u1)
(define-constant INITIAL-REPUTATION u100)
(define-constant VERIFICATION-WEIGHT u2)

;; Reputation tier thresholds
(define-constant TIER-1-THRESHOLD u100)  ;; Beginner
(define-constant TIER-2-THRESHOLD u300)  ;; Established
(define-constant TIER-3-THRESHOLD u600)  ;; Trusted
(define-constant TIER-4-THRESHOLD u900)  ;; Elite

;; Data structures
(define-map user-profiles
  { user: principal }
  {
    username: (string-utf8 64),
    bio: (string-utf8 256),
    reputation: uint,
    verified: bool,
    joined-at: uint,
    tier: uint,
    review-count: uint
  }
)

(define-map user-reviews
  { reviewer: principal, reviewee: principal }
  {
    rating: uint,
    comment: (string-utf8 256),
    timestamp: uint
  }
)

;; Track authorized verifiers who can validate user identities
(define-map authorized-verifiers
  { verifier: principal }
  { active: bool }
)

;; Data variables
(define-data-var total-users uint u0)
(define-data-var total-verified-users uint u0)

;; Private functions

;; Calculate the appropriate reputation tier based on user's reputation score
(define-private (calculate-tier (reputation uint))
  (if (>= reputation TIER-4-THRESHOLD)
    u4
    (if (>= reputation TIER-3-THRESHOLD)
      u3
      (if (>= reputation TIER-2-THRESHOLD)
        u2
        u1
      )
    )
  )
)

;; Check if a principal is an authorized verifier
(define-private (is-authorized-verifier (verifier principal))
  (default-to false (get active (map-get? authorized-verifiers { verifier: verifier })))
)

;; Public functions

;; Create a new user profile
(define-public (create-profile (username (string-utf8 64)) (bio (string-utf8 256)))
  (let (
    (user tx-sender)
    (current-block-height block-height)
  )
    ;; Check if profile already exists
    (asserts! (is-none (map-get? user-profiles { user: user })) ERR-USER-EXISTS)
    
    ;; Validate inputs
    (asserts! (> (len username) u0) ERR-INVALID-INPUT)
    
    ;; Create new profile
    (map-set user-profiles
      { user: user }
      {
        username: username,
        bio: bio,
        reputation: INITIAL-REPUTATION,
        verified: false,
        joined-at: current-block-height,
        tier: u1,
        review-count: u0
      }
    )
    
    ;; Increment total users
    (var-set total-users (+ (var-get total-users) u1))
    
    (ok true)
  )
)

;; Update existing profile information
(define-public (update-profile (username (string-utf8 64)) (bio (string-utf8 256)))
  (let (
    (user tx-sender)
    (existing-profile (unwrap! (map-get? user-profiles { user: user }) ERR-NOT-FOUND))
  )
    ;; Validate inputs
    (asserts! (> (len username) u0) ERR-INVALID-INPUT)
    
    ;; Update profile with new information
    (map-set user-profiles
      { user: user }
      (merge existing-profile {
        username: username,
        bio: bio
      })
    )
    
    (ok true)
  )
)

;; Add a verifier who can validate user identities
(define-public (add-verifier (verifier principal))
  (begin
    ;; Only contract owner can add verifiers
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    
    ;; Add the verifier
    (map-set authorized-verifiers
      { verifier: verifier }
      { active: true }
    )
    
    (ok true)
  )
)

;; Remove a verifier
(define-public (remove-verifier (verifier principal))
  (begin
    ;; Only contract owner can remove verifiers
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    
    ;; Remove the verifier
    (map-set authorized-verifiers
      { verifier: verifier }
      { active: false }
    )
    
    (ok true)
  )
)

;; Read-only functions

;; Get a user's profile information
(define-read-only (get-profile (user principal))
  (map-get? user-profiles { user: user })
)

;; Get a review submitted by a reviewer for a specific user
(define-read-only (get-review (reviewer principal) (reviewee principal))
  (map-get? user-reviews { reviewer: reviewer, reviewee: reviewee })
)

;; Check if a user is verified
(define-read-only (is-verified (user principal))
  (match (map-get? user-profiles { user: user })
    profile (get verified profile)
    false
  )
)

;; Get user's current reputation tier
(define-read-only (get-user-tier (user principal))
  (match (map-get? user-profiles { user: user })
    profile (get tier profile)
    u0
  )
)

;; Get the total number of users
(define-read-only (get-total-users)
  (var-get total-users)
)

;; Get the total number of verified users
(define-read-only (get-total-verified-users)
  (var-get total-verified-users)
)

;; Check if a user exists
(define-read-only (user-exists (user principal))
  (is-some (map-get? user-profiles { user: user }))
)