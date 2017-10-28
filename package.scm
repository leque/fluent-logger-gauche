;;
;; Package fluent-logger-gauche
;;

(define-gauche-package "fluent-logger-gauche"
  ;;
  :version "0.1"

  ;; Description of the package.  The first line is used as a short
  ;; summary.
  :description "A structured logger for Fluentd implemented in Gauche"

  ;; List of dependencies.
  ;; Example:
  ;;     :require (("Gauche" (>= "0.9.5"))  ; requires Gauche 0.9.5 or later
  ;;               ("Gauche-gl" "0.6"))     ; and Gauche-gl 0.6
  :require (("Gauche" (>= "0.9.5")))

  ;; List name and contact info of authors.
  ;; e.g. ("Eva Lu Ator <eval@example.com>"
  ;;       "Alyssa P. Hacker <lisper@example.com>")
  :authors ("OOHASHI Daichi <dico.leque.comicron@gmail.com")

  ;; List name and contact info of package maintainers, if they differ
  ;; from authors.
  ;; e.g. ("Cy D. Fect <c@example.com>")
  :maintainers ()

  ;; List licenses
  ;; e.g. ("BSD")
  :licenses ("MIT")

  ;; Homepage URL, if any.
  :homepage "https://github.com/leque/fluent-logger-gauche/"

  ;; Repository URL, e.g. github
  :repository "https://github.com/leque/fluent-logger-gauche.git"
  )
