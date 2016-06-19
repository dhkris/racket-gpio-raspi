;;;; RaRa GPIO
;;;; Raspberry Pi GPIO binding for Racket
;;;; Version 0.1/jun16
;;;; Copyright © David H. Christensen <me@davidh.info>, 2016
;;;; Licensed under the MIT license
;;;;
;;;; Requires wiringPi library installation. If you're using Raspbian,
;;;; this can be accomplished by entering
;;;;      sudo apt-get install wiringpi
;;;; in your favorite shell.
;;;; WiringPi is the property of Drogon at wiringpi.com. This is only a
;;;; binding for the world's favorite LISP dialect, with a few extensions.
;;;;
;;;; To make development more "natural" and LISP-y, several helper functions
;;;; are supplied.

#lang racket
(require ffi/unsafe)
(define wiringpi-lib (ffi-lib "/usr/lib/libwiringPi"))


;;; General-purpose IO

;; Setup
(define gpio-setup
  (get-ffi-obj "wiringPiSetup" wiringpi-lib (_fun _void -> _int)
               (lambda () (error 'wiringpi-lib "WiringPI does not provide wiringPiSetup"))))

(define gpio-pin-mode
  (get-ffi-obj "pinMode" wiringpi-lib (_fun _int _int -> _void)
               (lambda () (error 'wiringpi-lib "pinMode missing"))))

(define gpio-pull-up-down-control
  (get-ffi-obj "pullUpDnControl" wiringpi-lib (_fun _int _int -> _void)
               (lambda () (error 'wiringpi-lib "pullUpDnControl missing"))))

;; Digital interfacing
(define gpio-digital-write
  (get-ffi-obj "digitalWrite" wiringpi-lib (_fun _int _int -> _void)
               (lambda () (error 'wiringpi-lib "digitalWrite missing"))))

(define gpio-digital-read
  (get-ffi-obj "digitalRead" wiringpi-lib (_fun _int -> _int)
               (lambda () (error 'wiringpi-lib "digitalRead missing"))))

;; PWM 
(define gpio-pwm-write
  (get-ffi-obj "pwmWrite" wiringpi-lib (_fun _int _int -> _void)
               (lambda () (error 'wiringpi-lib "pwmWrite missing"))))

;; Analog interfacing
(define gpio-analog-write
  (get-ffi-obj "analogWrite" wiringpi-lib (_fun _int _int -> _void)
               (lambda () (error 'wiringpi-lib "digitalWrite missing"))))

(define gpio-analog-read
  (get-ffi-obj "analogRead" wiringpi-lib (_fun _int -> _int)
               (lambda () (error 'wiringpi-lib "digitalRead missing"))))

;;; Timing
(define gpio-elapsed-ms
  (get-ffi-obj "millis" wiringpi-lib (_fun _void -> _uint)
               (lambda () (error 'wiringpi-lib "millis missing"))))

(define gpio-elapsed-µs
  (get-ffi-obj "micros" wiringpi-lib (_fun _void -> _uint)
               (lambda () (error 'wiringpi-lib "micros missing"))))

(define gpio-delay-ms
  (get-ffi-obj "delay" wiringpi-lib (_fun _uint -> _void)
               (lambda () (error 'wiringpi-lib "delay missing"))))

(define gpio-delay-µs
  (get-ffi-obj "delayMicroseconds" wiringpi-lib (_fun _uint -> _void)
               (lambda () (error 'wiringpi-lib "delayMicroseconds missing"))))



;;; Custom helpers
(define gpio-delay-seconds
  (lambda (seconds)
    (gpio-delay-µs (floor (* seconds 1000000)))))

(define gpio-write-serial-byte
  (lambda (pin byte bit-duration)
    (letrec ([gpio-write-byte-op
           (lambda (offset)
             (if (<= offset 7)
                 (begin
                   (if (bitwise-bit-set? byte offset) (gpio-digital-write pin 1) (gpio-digital-write pin 0))
                   (gpio-delay-seconds bit-duration)
                   (gpio-write-byte-op (+ offset 1)))
                 (#t)))]) (gpio-write-byte-op 0))))