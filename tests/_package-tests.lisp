;;;; Package definition for the test suite.
;;;;
;;;; This package does :USE CL-UTILS, which is the exception the style guide
;;;; allows: the suite exercises most of the several hundred exported symbols,
;;;; so an :IMPORT-FROM clause would list essentially the whole package.

(defpackage :cl-utils-tests
  (:use :cl :cl-utils)
  (:documentation "Test suite for cl-utils."))

(in-package :cl-utils-tests)
