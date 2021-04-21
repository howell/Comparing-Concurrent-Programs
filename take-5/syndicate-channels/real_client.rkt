#lang racket

(require "struct.rkt")

(define MESSAGE-HEADER "------------------------------------------------------------------------\n")
(define REPEAT-STATE 'curr)
(define EXIT-STATE 'death)
;; a StateName is a unique String
;; a StructName is a prefab key (which is a Symbol)

;; a Transition is a String StateName
(struct transition (msg state-name) #:transparent)

;; a UserAction represents the sequence of events a user must take to fulfill an action
;; a UserAction is a String [List-of String] Lambda [Hash-of StructName Transition]
(struct user-action (desc inputs msg-ctor resps) #:transparent)

;; Special cased StateNames: 'curr, 'parent

(define login-resp-handler (hash 'logged-in (transition (λ (_) "Successfully logged in.\n") 'lobby)))
(define login-action (user-action "Login to the Server."
                                  (list "What is your id?\n" "What is your password?\n")
                                  login
                                  login-resp-handler))

(define registered-transition (transition (λ (register-struct)
                                            (match-define (registered pwd) register-struct)
                                            (format "Successfully signed up. Your password is ~a\n" pwd))
                                          'lobby))
(define register-resp-handler (hash 'registered registered-transition
                                    'taken-id (transition (λ (_) "That ID is taken. Try again.\n") REPEAT-STATE)))
(define register-action (user-action "Create an account on the Server"
                                     (list "What id would you like?\n")
                                     register
                                     register-resp-handler))

#;(define list-action (user-action "List all open rooms in the lobby.\n"
                                 '()
                                 list-rooms))

;; TODO
;; 1. It doesn't make sense for the description of the action to be in the user-action, probably.
;; 2. If there's consistent transition behavior or always want to be able to process one kind of message, then you should abstract reading
;; 3. How are you gonna handle 'global variables' like `user-id`? Seems like you can push that responsibility down to the server? Or have 'global' variables?

(define transition-table
  (hash 'start (hash 'login login-action
                     'signup register-action)
        'lobby (hash 'list list-action)))

#;(define transition-table
  (hash 'start
      (hash 'login (user-action "Login to the Server."
                                (list "What is your id?\n" "What is your password?\n")
                                 login
                                 (hash 'logged-in (transition
                                                   (λ (_) "Successfully logged in.\n") 'lobby)))
            'signup (user-action "Create an account on the Server."
                                 (list "What id would you like?\n")
                                 register
                                 (hash 'registered (transition
                                                    (λ (register-struct)
                                                      (match-define (registered pwd) register-struct)
                                                      (format "Successfully signed up. Your password is ~a\n" pwd))
                                                    'lobby)
                                       'taken-id (transition
                                                  (λ (_) "That ID is taken. Try again.\n") REPEAT-STATE))))))

(define (client-driver i o table start-state)
  (let state-loop ([curr-state start-state])
    (define state-handler (hash-ref table curr-state))
    (printf MESSAGE-HEADER)
    (printf "Welcome to the ~a.\n" curr-state) ;; would be nice to have a pretty name
    (printf "What would you like to do?\n")

    (for ([(key action) (in-hash state-handler)])
      (match-define (user-action desc _ _ _) action)
      (printf "[~a] ~a\n" key desc))

    (letrec ([handle-choose-action
              (λ ()
                (let ([input (read-line)])
                  (if (hash-has-key? state-handler (string->symbol input))
                      (handle-action input)
                      (begin (printf "Invalid input. Try again.\n") (handle-choose-action)))))]
             [handle-action
              (λ (action)
                (match-define (user-action _ input-reqs msg-ctor resp-handler) (hash-ref state-handler (string->symbol action)))
                (define user-inputs (for/list ([req input-reqs])
                  (printf req)
                  (read-line)))

                (write (apply msg-ctor user-inputs) o)
                (let ([server-resp (read i)])
                  (cond
                    [(hash-has-key? resp-handler (prefab-struct-key server-resp))
                     (match-define (transition print-ctor state) (hash-ref resp-handler (prefab-struct-key server-resp)))
                     (printf (print-ctor server-resp))
                     (cond
                       [(symbol=? state REPEAT-STATE) (handle-action action)]
                       [(symbol=? state EXIT-STATE) (printf "Client test over on exit!\n")]
                       [(hash-has-key? table state) (state-loop state)]
                       [else (printf "Client test over on transition to ~a!\n" state)])])))])

      (handle-choose-action))))

(define (client-welcome i o)
  (printf "========================================================================\n")
  (printf "Welcome to the Take-5 Game Server.\n")
  (printf "What would you like to do first?\n")
  (printf "[login] Login to the Server.\n")
  (printf "[signup] Create an account on the Server.\n")
  (printf "[exit] Close the client.\n")
  (let loop ()
    (let ([input (read-line)])
      (match input
        ["login" (client-login-screen i o)]
        ["signup" (client-signup-screen i o)]
        ["exit" (printf "Thanks for visiting the Take-5 Game Server.\n")]
        [_ (printf "Invalid message, try again.\n") (loop)]))))

;; (define signup-action (user-action '("What id would you like?\n")
;;                                    (λ (id))))

(define (client-signup-screen i o)
  (printf MESSAGE-HEADER)
  (printf "What id would you like?\n")
  (let loop ()
    (let ([proposed-user-id (read-line)])
      (write (register proposed-user-id) o)
      (match (read i)
        [(registered password)
         (printf "Successfully signed up. Your password is ~a\n" password)
         (client-lobby-screen i o)]
        [(taken-id _)
         (printf "That ID is taken. Try again.\n")
         (loop)]))))
  ;; need to check for duplicates
  ;; and then automatically log in 

(define (client-login-screen i o)
  (printf MESSAGE-HEADER)
  (printf "What is your id?\n")
  (define user-id (read-line))
  (printf "What is your password?\n")
  (define password (read-line))
  (write (login user-id password) o)
  (match (read i)
    [(logged-in) (printf "Successfully logged in.\n") (client-lobby-screen i o)]))

(define (client-lobby-screen i o)
  (printf MESSAGE-HEADER)
  (printf "Welcome to the lobby.\n")
  (printf "What would you like to do?\n")
  (printf "[list] List all open rooms in the lobby.\n")
  (printf "[results] Display all results of games you played.\n")
  (printf "[create] Create a room.\n")
  (printf "[join] Join a room.\n")
  (printf "[logout] Log out from the lobby.\n")

  (close-ports i o))

;; -> [List-of Port]
(define (create-connection)
  (let-values ([(i o) (tcp-connect CONNECT-HOSTNAME CONNECT-PORT)])
    (remove-tcp-buffer i o)
    (values i o)))

(let-values ([(i o) (create-connection)])
  (client-driver i o transition-table 'start))

;; Would be cool to:
;; 1. have an exit mechanism from any phase
;; 2. 
