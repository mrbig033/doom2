;; Find a file with which you are working and move towards its middle.
;; Find its buffer name, file name, length, and your position in the file.

(defun my-multiply-by-seven (number)
  "Multiplies NUMBER by seven."
  (interactive "p")
  (message "The result is %d" (* 7 number)))

(interactive "p\ncZap to char: ")

daveXjohnXmaryX
