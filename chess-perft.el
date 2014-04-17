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
  (if (= depth 1)
      (length (chess-legal-plies position :color (chess-pos-side-to-move position)))
    (let ((nodes 0))
      (dolist (ply
	       (chess-legal-plies position
				  :color (chess-pos-side-to-move position))
	       nodes)
	(setq nodes (+ nodes (chess-perft (chess-ply-next-pos ply)
					     (1- depth))))))))

(ert-deftest chess-perft-1 ()
  (should (= (chess-perft (chess-pos-create) 1) 20)))

(ert-deftest chess-perft-2 ()
  (should (= (chess-perft (chess-pos-create) 2) 400)))

(ert-deftest chess-perft-3 ()
  (should (= (chess-perft (chess-pos-create) 3) 8902)))

(ert-deftest chess-perft-4 ()
  (should (= (chess-perft (chess-pos-create) 4) 197281)))

(ert-deftest chess-perft-5 ()
  (should (= (chess-perft (chess-pos-create) 5) 4865609)))

(ert-deftest chess-perft-kiwipete-1 ()
  (let ((position (chess-fen-to-pos
		   "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -")))
    (should (= (chess-perft position 1) 48))))

(ert-deftest chess-perft-kiwipete-2 ()
  (let ((position (chess-fen-to-pos
		   "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -")))
    (should (= (chess-perft position 2) 2039))))

(ert-deftest chess-perft-kiwipete-3 ()
  (let ((position (chess-fen-to-pos
		   "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -")))
    (should (= (chess-perft position 3) 97862))))

(provide 'chess-perft)
;;; chess-perft.el ends here
