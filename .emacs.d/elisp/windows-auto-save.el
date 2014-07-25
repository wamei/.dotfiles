;; Idleを見計らって自動的にwin-save-all-configurationsを呼び出す設定
(require 'windows)

(defvar auto-win-save-all-conf-timer 30
  "何秒Idleになれば`win-save-all-configurations'を呼び出すか．")

(defvar already-win-load-all-configurations nil
  "`win-load-all-configurations'が呼び出されたらtになる．
多重にタイマーが動くのを防ぐため．")

;; prefix C-r rで呼び出すResume-allにタイマー実行をアドバイスする
(defadvice win-load-all-configurations (after win-load-all-configurations-after-advice (&optional preserve))
  (unless already-win-load-all-configurations
    (run-with-idle-timer auto-win-save-all-conf-timer t 'win-save-all-configurations)
    (setq already-win-load-all-configurations t)))
(ad-activate 'win-load-all-configurations)
;; (ad-deactivate 'win-load-all-configurations)

(provide 'windows-auto-save)
