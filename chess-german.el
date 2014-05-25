;;; chess-german.el --- German translation of the chess.el message catalog

;; Copyright (C) 2014 Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: games, i18n

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'chess-message)

(chess-message-catalog 'german
  '((queen	            . "Dame")
    (king	            . "König")
    (bishop	            . "Läufer")
    (knight	            . "Springer")
    (rook	            . "Turm")
    (pawn	            . "Bauer")
    (short-castle           . "Kurze Rochade")
    (long-castle            . "Lange Rochade")
    (check	            . "Schach")
    (checkmate	            . "Schach matt")
    (stalemate	            . "Patt")
    (en-passant	            . "on possont")
    (piece-moves            . "%s nach %s")
    (piece-takes            . "%s schlägt %s auf %s")

    (add-to-completed	    . "Kann keine Züge zu abgeschlossenem Spiel hinzufügen")
    (bad-game-read	    . "Beim lesen des Spiels ist ein Fehler an Position %d aufgetreten")
    (cannot-mount	    . "Es ist nicht Möglich eine Figur auf die andere zu stellen")
    (cannot-yet-add	    . "Kann noch keine Züge zu Spiel hinzufügen")
    (challenge-whom	    . "Wehn willst du herausfordern? ")
    (chess-delete-autosave  . "Soll die autosave Datei gelöscht werden? ")
    (chess-disable-autosave . "Disable autosaving for this game? ")
    (chess-read-autosave    . "Es existiert eine Schach autosave Datei, soll sie geladen werden? ")
    (clarify-piece	    . "Clarify piece to move by rank or file")
    (congratulations	    . "Gratulation!")
    (could-not-clarify	    . "Could not determine which piece to use")
    (could-not-diff	    . "Could not differentiate piece")
    (could-not-find-engine  . "Cannot find %s executable; check `%s'")
    (could-not-read-pgn     . "Kann PGN Datei nicht lesen oder finden")
    (draw-offer		    . "Du bietest ein Unentschieden an")
    (draw-offer-declined    . "Dein Angebot zum Unentschieden wurde abgelehnt")
    (editing-directly	    . "Now editing position directly, use S when complete...")
    (end-of-puzzles	    . "Es gibt keine weiteren Puzzles in dieser Sammlung")
    (engine-not-running     . "Die Engine die Du verwendet hast läuft nicht mehr")
    (failed-start	    . "Failed to start chess engine process")
    (game-is-over	    . "Dieses Spiel ist abgeschlossen")
    (ics-connected	    . "Verbindungsaufbau mit Internet Chess Server '%s'...done")
    (ics-connecting	    . "Verbindungsaufbau mit Internet Chess Server '%s'...")
    (ics-server-prompt	    . "Verbindung zu Schach Server: ")
    (illegal-move	    . "Illegalaler Zug")
    (illegal-notation	    . "Illegale Zug notation: %s")
    (invalid-fen	    . "Ungültiger FEN string: %s")
    (invalid-pgn	    . "Ungültiger PGN text empfangen")
    (irc-challenge	    . "IRC nick of user to challenge: ")
    (irc-connecting	    . "Verbindungsaufbau mit IRC server '%s:%d'...")
    (irc-logging-in	    . "Connected, now logging in as '%s'...")
    (irc-waiting	    . "Now waiting for 'name USER' via /msg, or `M-x chess-irc-engage'")
    (knight-1-done	    . "Goal: take all the pawns, without letting the queen take your knight")
    (mode-black		    . "Schwarz")
    (mode-checkmate	    . "SCHACHMATT")
    (mode-drawn		    . "DRAWN")
    (mode-edit		    . "EDIT")
    (mode-flag-fell	    . "FLAG FELL")
    (mode-resigned	    . "RESIGNED")
    (mode-stalemate	    . "PATT")
    (mode-start		    . "START")
    (mode-white		    . "Weiß")
    (move-from-blank	    . "Du versuchst eine Figur vom leeren Feld %s zu bewegen")
    (move-not-legal	    . "Dies ist kein legaler Zug")
    (move-passed	    . "Your opponent has passed the move to you")
    (network-starting	    . "Starting network client/server...")
    (network-waiting	    . "Now waiting for your opponent to connect...")
    (no-candidates	    . "There are no candidate moves for '%s'")
    (no-engines-found	    . "Could not find any chess engines to play against; install gnuchess!")
    (no-images		    . "Cannot find any piece images; check `chess-images-directory'")
    (no-images-fallback     . "Could not find any suitable or properly sized chess images")
    (no-such-database	    . "There is no such chess database module '%s'")
    (no-such-module	    . "There is no module named '%s'")
    (no-such-style	    . "There is no such chessboard display style '%s'")
    (not-your-move	    . "It is not your turn to move")
    (now-black		    . "Dein Gegner hat den ersten Zug gemacht, du bist nun Schwarz")
    (opp-abort		    . "Dein Gegner will das Spiel abbrechen, akzeptierst Du? ")
    (opp-abort-acc	    . "Your offer to abort was accepted")
    (opp-abort-dec	    . "Your offer to abort was declined")
    (opp-abort-ret	    . "Your opponent has retracted their offer to abort")
    (opp-draw		    . "Dein Gegner bietet ein Unentschieden an, willst Du annehmen? ")
    (opp-draw-acc	    . "Dein Remie Angebot wurde akzeptiert")
    (opp-draw-dec	    . "Your draw offer was declined")
    (opp-draw-ret	    . "Your opponent has retracted their draw offer")
    (opp-illegal	    . "Your opponent states your last command was illegal")
    (opp-quit		    . "Your opponent has quit playing")
    (opp-ready		    . "%s ist nun bereits zu spielen")
    (opp-ready-a	    . "Dein Anonymer Gegner ist nun bereit zu spielen")
    (opp-resigned	    . "Dein Gegner hat aufgegeben")
    (opp-undo		    . "Dein Gegner will %d Züge zurück nehmen, akzeptierst Du? ")
    (opp-undo-acc	    . "Request to undo %d moves was accepted")
    (opp-undo-dec	    . "Your request to undo %d moves was decline")
    (opp-undo-ret	    . "Your opponent has retracted their request to undo %d moves")
    (opponent-says	    . "Dein Gegner sagt: %s")
    (pgn-parse-error	    . "Error parsing PGN syntax")
    (pgn-read-error	    . "Error reading move: %s")
    (piece-images-loaded    . "Loading chess piece images...done")
    (piece-images-loading   . "Loading chess piece images...")
    (piece-immobile	    . "That piece cannot move now")
    (piece-unrecognized     . "Unrecognized piece identifier")
    (queen-would-take	    . "Die Dame würde deinen Springer schlagen!")
    (redrawing-frame	    . "Redrawing chess display with different size...")
    (redrawing-frame-done   . "Redrawing chess display with different size...done")
    (return-to-current	    . "Use '>' to return to the current position")
    (san-not-found	    . "Could not find a matching move")
    (selected-empty	    . "You cannot select an empty square")
    (starting-engine	    . "Starting chess program '%s'...")
    (starting-engine-done   . "Starting chess program '%s'...done")
    (undo-limit-reached     . "Cannot undo further")
    (want-to-play	    . "Willst Du eine Partie Schach gegen %s spielen? ")
    (want-to-play-a	    . "Willst Du eine Partie Schach gegen einen Anonymen Gegner spielen? ")
    (want-to-quit	    . "Do you really want to quit? ")
    (wrong-color	    . "Du kannst die Figuren deines Gegners nicht bewegen")))

(provide 'chess-german)

;;; chess-german.el ends here
