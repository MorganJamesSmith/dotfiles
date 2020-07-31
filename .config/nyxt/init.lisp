(define-configuration buffer
  ((default-modes (append '(vi-normal-mode) %slot-default))
   (override-map (let ((map (make-keymap "override-map")))
                   (define-key map
                     "g d" 'delete-current-buffer
                     ">" 'switch-buffer-next
                     "<" 'switch-buffer-previous)))))
