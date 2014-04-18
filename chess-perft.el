;;; chess-perft.el --- Perft tests for emacs-chess   -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Mario Lang

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The classic perft function counts all leave nodes at a certain depth.
;; To make it easier to identify specific problems we also count properties
;; of the individual (final) plies.  We count capturing plies, en passant plies,
;; castling plies, plies that promote to a piece,
;; plies which bring the opponent king in check and plies which result in
;; checkmate.

;; Typically depths greater than 4 will result in very long runtimes.
;; We only define tests which do not take a lot of execution time
;; (less than a million nodes).

;; To make it easier to selectively run tests, all tests provide tags
;; to indentify which type of ply they are covering.
;; The available ERT tags are:
;; :capture, :en-passant, :castle, :promotion, :check and :checkmate.
;;
;; For instance, to make sure castling plies work as expected, run
;; M-: (ert '(tag :castle)) RET

;;; Code:

(require 'chess-fen)
(require 'chess-ply)
(require 'chess-pos)
(require 'cl-lib)
(require 'ert)

(defun chess-perft (position depth)
  "Count all leave nodes of the tree starting at POSITION pruned at DEPTH.
The result is a list of the form
 (LEAVES CAPTURES EN-PASSANTS CASTLES PROMOTIONS CHECKS CHECKMATES)."
  (if (zerop depth)
      (cl-values 1 0 0 0 0)
    (let ((plies (chess-legal-plies position
				    :color (chess-pos-side-to-move position))))
      (if (= depth 1)
	  (cl-values (length plies)
		     ;; Captures
		     (cl-count-if
		      (lambda (ply)
			(or (chess-pos-piece-p
			     (chess-ply-pos ply) (chess-ply-target ply)
			     (not (chess-pos-side-to-move (chess-ply-pos ply))))
			    (chess-ply-keyword ply :en-passant)))
		      plies)
		     ;; En passants
		     (cl-count-if
		      (lambda (ply)
			(chess-ply-keyword ply :en-passant))
		      plies)
		     ;; Castles
		     (cl-count-if
		      (lambda (ply)
			(chess-ply-any-keyword ply :castle :long-castle))
		      plies)
		     ;; Promotions
		     (cl-count-if
		      (lambda (ply)
			(chess-ply-keyword ply :promote))
		      plies)
		     ;; Checks
		     (cl-count-if
		      (lambda (ply)
			(chess-ply-any-keyword ply :check :checkmate))
		      plies)
		     ;; Checkmates
		     (cl-count-if
		      (lambda (ply)
			(chess-ply-any-keyword ply :checkmate))
		      plies))
	(let ((nodes 0) (captures 0) (en-passants 0)
	      (castles 0) (promotions 0)
	      (checks 0) (checkmates 0))
	  (dolist (ply plies (cl-values nodes
					captures en-passants
					castles promotions
					checks checkmates))
	    (cl-multiple-value-bind (n c e ca p ch cm)
		(chess-perft (chess-ply-next-pos ply) (1- depth))
	      (cl-incf nodes n)
	      (cl-incf captures c)
	      (cl-incf en-passants e)
	      (cl-incf castles ca)
	      (cl-incf promotions p)
	      (cl-incf checks ch)
	      (cl-incf checkmates cm))))))))

(ert-deftest chess-perft-startpos-depth1 ()
  (should (equal (chess-perft (chess-pos-create) 1) '(20 0 0 0 0 0 0))))

(ert-deftest chess-perft-startpos-depth2 ()
  (should (equal (chess-perft (chess-pos-create) 2) '(400 0 0 0 0 0 0))))

(ert-deftest chess-perft-startpos-depth3 ()
  :tags '(:capture :check)
  (should (equal (chess-perft (chess-pos-create) 3) '(8902 34 0 0 0 12 0))))

(ert-deftest chess-perft-startpos-depth4 ()
  :tags '(:capture :check :checkmate)
  (should (equal (chess-perft (chess-pos-create) 4) '(197281 1576 0 0 0 469 8))))

(ert-deftest chess-perft-kiwipete-depth1 ()
  :tags '(:capture :castle)
  (let ((position
	 (chess-fen-to-pos
	  "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -")))
    (should (equal (chess-perft position 1) '(48 8 0 2 0 0 0)))))

(ert-deftest chess-perft-kiwipete-depth2 ()
  :tags '(:capture :en-passant :castle :check)
  (let ((position
	 (chess-fen-to-pos
	  "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -")))
    (should (equal (chess-perft position 2) '(2039 351 1 91 0 3 0)))))

(ert-deftest chess-perft-kiwipete-depth3 ()
  "This test is expected to fail due to a (undetermined) bug in castling
generation.  We do generate too many castling moves."
  :tags '(:capture :en-passant :castle :check :checkmate)
  (let ((position
	 (chess-fen-to-pos
	  "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -")))
    (should (equal (chess-perft position 3) '(97862 17102 45 3162 0 993 1)))))

(ert-deftest chess-perft-pos3-depth1 ()
  :tags '(:capture :check)
  (let ((position (chess-fen-to-pos "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -")))
    (should (equal (chess-perft position 1) '(14 1 0 0 0 2 0)))))

(ert-deftest chess-perft-pos3-depth2 ()
  :tags '(:capture :check)
  (let ((position (chess-fen-to-pos "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -")))
    (should (equal (chess-perft position 2) '(191 14 0 0 0 10 0)))))

(ert-deftest chess-perft-pos3-depth3 ()
  :tags '(:capture :en-passant :check)
  (let ((position (chess-fen-to-pos "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -")))
    (should (equal (chess-perft position 3) '(2812 209 2 0 0 267 0)))))

(ert-deftest chess-perft-pos3-depth4 ()
  :tags '(:capture :en-passant :check :checkmate)
  (let ((position (chess-fen-to-pos "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -")))
    (should (equal (chess-perft position 4) '(43238 3348 123 0 0 1680 17)))))

(ert-deftest chess-perft-pos3-depth5 ()
  :tags '(:capture :en-passant :check)
  (let ((position (chess-fen-to-pos "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -")))
    (should (equal (chess-perft position 5) '(674624 52051 1165 0 0 52950 0)))))

(ert-deftest chess-perft-pos4-depth1 ()
  (let ((chess-ply-allow-interactive-query nil)
	(position
	 (chess-fen-to-pos
	  "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq -")))
    (should (equal (chess-perft position 1) '(6 0 0 0 0 0 0)))))

(ert-deftest chess-perft-pos4-depth2 ()
  :tags '(:capture :castle :promotion :check)
  (let ((chess-ply-allow-interactive-query nil)
	(position
	 (chess-fen-to-pos
	  "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq -")))
    (should (equal (chess-perft position 2) '(264 87 0 6 48 10 0)))))

(ert-deftest chess-perft-pos4-depth3 ()
  :tags '(:capture :en-passant :promotion :check :checkmate)
  (let ((chess-ply-allow-interactive-query nil)
	(position
	 (chess-fen-to-pos
	  "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq -")))
    (should (equal (chess-perft position 3) '(9467 1021 4 0 120 38 22)))))

(ert-deftest chess-perft-pos4-depth4 ()
  :tags '(:capture :castle :promotion :check :checkmate)
  (let ((chess-ply-allow-interactive-query nil)
	(position
	 (chess-fen-to-pos
	  "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq -")))
    (should (equal (chess-perft position 4) '(422333 131393 0 7795 60032 15492 5)))))

(provide 'chess-perft)
;;; chess-perft.el ends here
