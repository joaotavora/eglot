(with-temp-file "file.txt"
  (let ((positions ()))
    (dotimes (line 1000)
      (dotimes (char (abs (% (random) 90)))
        (insert (if (zerop (abs (% (random) 10))) "𐐀" "y"))
        (when (zerop (% (random) 20))
          (push `(:line ,(1+ line) :character ,char) positions)))
      (insert "\n"))
    (with-temp-file "positions.el"
      (let ((print-length nil))
        (pp positions (current-buffer))))))
