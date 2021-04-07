#lang racket

(require "struct.rkt")

(define (client-welcome i o)
  (printf "========================================================================\n")
  (printf "Welcome to the Take-5 Game Server.\n")
  (printf "What would you like to do first?\n")
  (printf "[login] Login to the Server.\n")
  (printf "[signup] Create an account on the Server.\n")
  (let loop ()
    (let ([input (read-line)])
      (match input
        ["login" (client-login-screen i o)]
        ["signup" (client-signup-screen i o)]
        [_ (printf "Invalid message, try again.\n") (loop)]))))

(define (cliet-signup-screen i o)
  (printf "------------------------------------------------------------------------\n")
  (printf "What id would you like?\n")
  (let loop ()
    (let ([proposed-user-id (read-line)])
      (write (register proposed-user-id) o)
      (match (read i)
        [(registered password)
         (printf "Successfully signed up. Your password is ~a\n" password)
         (client-lobby-screen i o)]
        [(taken-id)
         (printf "That ID is taken. Try again.\n")
         (loop)]))))
  ;; need to check for duplicates
  ;; and then automatically log in 

(define (client-login-screen i o)
  (printf "------------------------------------------------------------------------\n")
  (printf "What is your id?\n")
  (define user-id (read-line))
  (printf "What is your password?\n")
  (define password (read-line))
  (write (login user-id password) o)
  (match (read i)
    [(logged-in) (printf "Successfully logged in.\n") (client-lobby-screen i o)]))

(define (client-lobby-screen i o)
  (printf "------------------------------------------------------------------------\n")
  (printf "Welcome to the lobby.\n")
  (printf "What would you like to do?\n")
  (close-ports i o))

;; -> [List-of Port]
(define (create-connection)
  (let-values ([(i o) (tcp-connect CONNECT-HOSTNAME CONNECT-PORT)])
    (remove-tcp-buffer i o)
    (values i o)))

(let-values ([(i o) (create-connection)])
  (client-welcome i o))
