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

;; Counts all leave nodes to a certain depth.

;;; Code:

(require 'chess-fen)
(require 'chess-ply)
(require 'chess-pos)
(require 'ert)

(defun chess-perft (position depth)
  (if (zerop depth)
      (cl-values 1 0 0 0 0)
    (let ((plies (chess-legal-plies position
				    :color (chess-pos-side-to-move position))))
      (if (= depth 1)
	  (cl-values (length plies)
		     (cl-count-if
		      (lambda (ply)
			(or (chess-pos-piece-p
			     (chess-ply-pos ply) (chess-ply-target ply)
			     (not (chess-pos-side-to-move (chess-ply-pos ply))))
			    (let ((en-passant (chess-pos-en-passant
					       (chess-ply-pos ply))))
			      (and en-passant
				   (/= (chess-pos-piece
					(chess-ply-pos ply) en-passant)
				       (chess-pos-piece
					(chess-ply-next-pos ply) en-passant))))))
		      plies)
		     (cl-count-if
		      (lambda (ply)
			(chess-ply-any-keyword ply :castle :long-castle))
		      plies)
		     (cl-count-if
		      (lambda (ply)
			(chess-ply-any-keyword ply :check :checkmate))
		      plies)
		     (cl-count-if
		      (lambda (ply)
			(chess-ply-any-keyword ply :checkmate))
		      plies))
	(let ((nodes 0) (captures 0) (castles 0) (checks 0) (checkmates 0))
	  (dolist (ply plies (cl-values nodes captures castles checks checkmates))
	    (cl-multiple-value-bind (n c ca ch cm)
		(chess-perft (chess-ply-next-pos ply) (1- depth))
	      (cl-incf nodes n)
	      (cl-incf captures c)
	      (cl-incf castles ca)
	      (cl-incf checks ch)
	      (cl-incf checkmates cm))))))))

(ert-deftest chess-perft-startpos-depth1 ()
  (should (equal (chess-perft (chess-pos-create) 1) '(20 0 0 0 0))))

(ert-deftest chess-perft-startpos-depth2 ()
  (should (equal (chess-perft (chess-pos-create) 2) '(400 0 0 0 0))))

(ert-deftest chess-perft-startpos-depth3 ()
  (should (equal (chess-perft (chess-pos-create) 3) '(8902 34 0 12 0))))

(ert-deftest chess-perft-startpos-depth4 ()
  (should (equal (chess-perft (chess-pos-create) 4) '(197281 1576 0 469 8))))

(ert-deftest chess-perft-kiwipete-depth1 ()
  (let ((position
	 (chess-fen-to-pos
	  "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -")))
    (should (equal (chess-perft position 1) '(48 8 2 0 0)))))

(ert-deftest chess-perft-kiwipete-depth2 ()
  (let ((position
	 (chess-fen-to-pos
	  "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -")))
    (should (equal (chess-perft position 2) '(2039 351 91 3 0)))))

(ert-deftest chess-perft-kiwipete-depth3 ()
  (let ((position
	 (chess-fen-to-pos
	  "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -")))
    (should (equal (chess-perft position 3) '(97862 17102 3162 993 1)))))

(ert-deftest chess-perft-pos3-depth1 ()
  (let ((position (chess-fen-to-pos "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -")))
    (should (equal (chess-perft position 1) '(14 1 0 2 0)))))

(ert-deftest chess-perft-pos3-depth2 ()
  (let ((position (chess-fen-to-pos "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -")))
    (should (equal (chess-perft position 2) '(191 14 0 10 0)))))

(ert-deftest chess-perft-pos3-depth3 ()
  (let ((position (chess-fen-to-pos "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -")))
    (should (equal (chess-perft position 3) '(2812 209 0 267 0)))))

(ert-deftest chess-perft-pos3-depth4 ()
  (let ((position (chess-fen-to-pos "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -")))
    (should (equal (chess-perft position 4) '(43238 3348 0 1680 17)))))

(ert-deftest chess-perft-pos3-depth5 ()
  (let ((position (chess-fen-to-pos "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -")))
    (should (equal (chess-perft position 5) '(674624 52051 0 52950 0)))))

(ert-deftest chess-perft-pos4-depth1 ()
  (let ((chess-ply-allow-interactive-query nil)
	(position
	 (chess-fen-to-pos
	  "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq -")))
    (should (equal (chess-perft position 1) '(6 0 0 0 0)))))

(ert-deftest chess-perft-pos4-depth2 ()
  (let ((chess-ply-allow-interactive-query nil)
	(position
	 (chess-fen-to-pos
	  "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq -")))
    (should (equal (chess-perft position 2) '(264 87 6 10 0)))))

(ert-deftest chess-perft-pos4-depth3 ()
  (let ((chess-ply-allow-interactive-query nil)
	(position
	 (chess-fen-to-pos
	  "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq -")))
    (should (equal (chess-perft position 3) '(9467 1021 0 38 22)))))

(ert-deftest chess-perft-pos4-depth4 ()
  (let ((chess-ply-allow-interactive-query nil)
	(position
	 (chess-fen-to-pos
	  "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq -")))
    (should (equal (chess-perft position 4) '(422333 131393 7795 15492 5)))))

(provide 'chess-perft)
;;; chess-perft.el ends here
