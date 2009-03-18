(asdf:defsystem common-worm
  :version "0"
  :description "Classic worm game"
  :maintainer "Kat <kzm@sykosomatic.org>"
  :author "Kat <kzm@sykosomatic.org>"
  :licence "MIT"
  :depends-on (lispbuilder-sdl lispbuilder-sdl-image)
  :serial t
  :components
  ((:file "packages")
   (:file "common-worm")))