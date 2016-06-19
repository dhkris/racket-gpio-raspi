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
  (get-ffi-obj "wiringPiSetup" wiringpi-lib (_fun -> _int32)
               (lambda () (error 'wiringpi-lib "WiringPI does not provide wiringPiSetup"))))

(define gpio-pin-mode-native
  (get-ffi-obj "pinMode" wiringpi-lib (_fun _int32 _int32 -> _void)
               (lambda () (error 'wiringpi-lib "pinMode missing"))))

(define gpio-pull-up-down-control
  (get-ffi-obj "pullUpDnControl" wiringpi-lib (_fun _int32 _int32 -> _void)
               (lambda () (error 'wiringpi-lib "pullUpDnControl missing"))))

;; Digital interfacing
(define gpio-digital-write
  (get-ffi-obj "digitalWrite" wiringpi-lib (_fun _int32 _int32 -> _void)
               (lambda () (error 'wiringpi-lib "digitalWrite missing"))))

(define gpio-digital-read
  (get-ffi-obj "digitalRead" wiringpi-lib (_fun _int32 -> _int32)
               (lambda () (error 'wiringpi-lib "digitalRead missing"))))

;; PWM 
(define gpio-pwm-write
  (get-ffi-obj "pwmWrite" wiringpi-lib (_fun _int32 _int32 -> _void)
               (lambda () (error 'wiringpi-lib "pwmWrite missing"))))

;; Analog interfacing
(define gpio-analog-write
  (get-ffi-obj "analogWrite" wiringpi-lib (_fun _int32 _int32 -> _void)
               (lambda () (error 'wiringpi-lib "digitalWrite missing"))))

(define gpio-analog-read
  (get-ffi-obj "analogRead" wiringpi-lib (_fun _int32 -> _int32)
               (lambda () (error 'wiringpi-lib "digitalRead missing"))))

;;; Timing
(define gpio-elapsed-ms
  (get-ffi-obj "millis" wiringpi-lib (_fun -> _uint32)
               (lambda () (error 'wiringpi-lib "millis missing"))))

(define gpio-elapsed-µs
  (get-ffi-obj "micros" wiringpi-lib (_fun -> _uint32)
               (lambda () (error 'wiringpi-lib "micros missing"))))

(define gpio-delay-ms
  (get-ffi-obj "delay" wiringpi-lib (_fun _uint32 -> _void)
               (lambda () (error 'wiringpi-lib "delay missing"))))

(define gpio-delay-µs
  (get-ffi-obj "delayMicroseconds" wiringpi-lib (_fun _uint32 -> _void)
               (lambda () (error 'wiringpi-lib "delayMicroseconds missing"))))


;; Pin mode defines
(define gpio-pin-input 0)
(define gpio-pin-output 1)
(define gpio-pin-pwm 2)
(define gpio-pin-clock 3)
(define gpio-pin-soft-pwm 4)
(define gpio-pin-soft-tone 5)
(define gpio-pin-pwm-tone 6)

;; Pullup/down modes
(define gpio-pull-up 2)
(define gpio-pull-down 1)
(define gpio-pull-off 0)



;;; Custom helpers
;; More "native" pin mode setter
(define gpio-set-pin-mode
  (lambda (pin mode)
    (cond
      [(= mode 'input) (gpio-pin-mode-native pin gpio-pin-input)]
      [(= mode 'output) (gpio-pin-mode-native pin gpio-pin-output)]
      [(= mode 'pwm) (gpio-pin-mode-native pin gpio-pin-pwm)]
      [(= mode 'clock) (gpio-pin-mode-native pin gpio-pin-clock)]
      [(= mode 'soft-pwm) (gpio-pin-mode-native pin gpio-pin-soft-pwm)]
      [(= mode 'soft-tone) (gpio-pin-mode-native pin gpio-pin-soft-tone)]
      [(= mode 'pwm-tone) (gpio-pin-mode-native pin gpio-pin-pwm-tone)]
      [else (error 'gpio-set-pin-mode (string-join "Erroneous pin mode passed: " (symbol->string mode) ". Valid: 'input 'output 'pwm 'clock 'soft-pwm 'soft-tone 'pwm-tone"))])))

(define gpio-set-pull-resistor
  (lambda (pin mode)
    (cond
      [(= mode 'up) (gpio-pull-up-down-control pin gpio-pull-up)]
      [(= mode 'down) (gpio-pull-up-down-control pin gpio-pull-down)]
      [(= mode 'off) (gpio-pull-up-down-control pin gpio-pull-off)]
      [else (error 'gpio-set-pull-resistor (string-join "Erroneous pullup/down mode passed: " (symbol->string mode) ". Valid: 'up, 'down, 'off'"))])))
                            

;; GPIO delay
(define gpio-delay-seconds
  (lambda (seconds)
    (gpio-delay-µs (floor (* seconds 1000000)))))

;; Write a byte of data sequentially to specified pin
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