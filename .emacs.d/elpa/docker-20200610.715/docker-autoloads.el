;;; docker-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "docker" "../../../../.emacs.d/elpa/docker-20200610.715/docker.el"
;;;;;;  "af02ddb3dae7f9bbc189ba40fced2897")
;;; Generated autoloads from ../../../../.emacs.d/elpa/docker-20200610.715/docker.el
 (autoload 'docker "docker" nil t)

;;;### (autoloads "actual autoloads are elsewhere" "docker" "../../../../.emacs.d/elpa/docker-20200610.715/docker.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/docker-20200610.715/docker.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker" '("docker-read-")))

;;;***

;;;***

;;;### (autoloads nil "docker-compose" "../../../../.emacs.d/elpa/docker-20200610.715/docker-compose.el"
;;;;;;  "55200aab6758b8a71e1332527e4b99ce")
;;; Generated autoloads from ../../../../.emacs.d/elpa/docker-20200610.715/docker-compose.el
 (autoload 'docker-compose "docker-compose" nil t)

;;;### (autoloads "actual autoloads are elsewhere" "docker-compose"
;;;;;;  "../../../../.emacs.d/elpa/docker-20200610.715/docker-compose.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/docker-20200610.715/docker-compose.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker-compose" '("docker-compose-")))

;;;***

;;;***

;;;### (autoloads nil "docker-container" "../../../../.emacs.d/elpa/docker-20200610.715/docker-container.el"
;;;;;;  "4dc6d4df78a283a12551ff22c2622261")
;;; Generated autoloads from ../../../../.emacs.d/elpa/docker-20200610.715/docker-container.el

(autoload 'docker-container-eshell "docker-container" "\
Open `eshell' in CONTAINER.

\(fn CONTAINER)" t nil)

(autoload 'docker-container-find-directory "docker-container" "\
Inside CONTAINER open DIRECTORY.

\(fn CONTAINER DIRECTORY)" t nil)

(autoload 'docker-container-find-file "docker-container" "\
Open FILE inside CONTAINER.

\(fn CONTAINER FILE)" t nil)

(autoload 'docker-container-shell "docker-container" "\
Open `shell' in CONTAINER.  When READ-SHELL is not nil, ask the user for it.

\(fn CONTAINER &optional READ-SHELL)" t nil)

(autoload 'docker-containers "docker-container" "\
List docker containers.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "docker-container"
;;;;;;  "../../../../.emacs.d/elpa/docker-20200610.715/docker-container.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/docker-20200610.715/docker-container.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker-container" '("docker-container-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "docker-core"
;;;;;;  "../../../../.emacs.d/elpa/docker-20200610.715/docker-core.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/docker-20200610.715/docker-core.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker-core" '("docker-")))

;;;***

;;;### (autoloads nil "docker-image" "../../../../.emacs.d/elpa/docker-20200610.715/docker-image.el"
;;;;;;  "692ca3a41079a1cbea6a6fedbedfec9f")
;;; Generated autoloads from ../../../../.emacs.d/elpa/docker-20200610.715/docker-image.el

(autoload 'docker-image-pull-one "docker-image" "\
Pull the image named NAME.  If ALL is set, use \"-a\".

\(fn NAME &optional ALL)" t nil)

(autoload 'docker-images "docker-image" "\
List docker images.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "docker-image"
;;;;;;  "../../../../.emacs.d/elpa/docker-20200610.715/docker-image.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/docker-20200610.715/docker-image.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker-image" '("docker-image-")))

;;;***

;;;***

;;;### (autoloads nil "docker-machine" "../../../../.emacs.d/elpa/docker-20200610.715/docker-machine.el"
;;;;;;  "c5d4e2a82a2ae8375e18037e8e6c37cd")
;;; Generated autoloads from ../../../../.emacs.d/elpa/docker-20200610.715/docker-machine.el

(autoload 'docker-machine-create "docker-machine" "\
Create a machine NAME using DRIVER.

\(fn NAME DRIVER)" t nil)

(autoload 'docker-machine-env-one "docker-machine" "\
Parse and set environment variables from \"docker-machine env NAME\" output.

\(fn NAME)" t nil)

(autoload 'docker-machines "docker-machine" "\
List docker machines.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "docker-machine"
;;;;;;  "../../../../.emacs.d/elpa/docker-20200610.715/docker-machine.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/docker-20200610.715/docker-machine.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker-machine" '("docker-machine-")))

;;;***

;;;***

;;;### (autoloads nil "docker-network" "../../../../.emacs.d/elpa/docker-20200610.715/docker-network.el"
;;;;;;  "e0a321c4bf952328ea8b0e178e6468a0")
;;; Generated autoloads from ../../../../.emacs.d/elpa/docker-20200610.715/docker-network.el

(autoload 'docker-networks "docker-network" "\
List docker networks.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "docker-network"
;;;;;;  "../../../../.emacs.d/elpa/docker-20200610.715/docker-network.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/docker-20200610.715/docker-network.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker-network" '("docker-network-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "docker-utils"
;;;;;;  "../../../../.emacs.d/elpa/docker-20200610.715/docker-utils.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/docker-20200610.715/docker-utils.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker-utils" '("docker-utils-")))

;;;***

;;;### (autoloads nil "docker-volume" "../../../../.emacs.d/elpa/docker-20200610.715/docker-volume.el"
;;;;;;  "a2df69b1dd4ae7f5bdaca337061c9239")
;;; Generated autoloads from ../../../../.emacs.d/elpa/docker-20200610.715/docker-volume.el

(autoload 'docker-volume-dired "docker-volume" "\
Enter `dired' in the volume named NAME.

\(fn NAME)" t nil)

(autoload 'docker-volumes "docker-volume" "\
List docker volumes.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "docker-volume"
;;;;;;  "../../../../.emacs.d/elpa/docker-20200610.715/docker-volume.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/docker-20200610.715/docker-volume.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "docker-volume" '("docker-volume-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/docker-20200610.715/docker-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/docker-20200610.715/docker-compose.el"
;;;;;;  "../../../../.emacs.d/elpa/docker-20200610.715/docker-container.el"
;;;;;;  "../../../../.emacs.d/elpa/docker-20200610.715/docker-core.el"
;;;;;;  "../../../../.emacs.d/elpa/docker-20200610.715/docker-faces.el"
;;;;;;  "../../../../.emacs.d/elpa/docker-20200610.715/docker-image.el"
;;;;;;  "../../../../.emacs.d/elpa/docker-20200610.715/docker-machine.el"
;;;;;;  "../../../../.emacs.d/elpa/docker-20200610.715/docker-network.el"
;;;;;;  "../../../../.emacs.d/elpa/docker-20200610.715/docker-pkg.el"
;;;;;;  "../../../../.emacs.d/elpa/docker-20200610.715/docker-utils.el"
;;;;;;  "../../../../.emacs.d/elpa/docker-20200610.715/docker-volume.el"
;;;;;;  "../../../../.emacs.d/elpa/docker-20200610.715/docker.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; docker-autoloads.el ends here
