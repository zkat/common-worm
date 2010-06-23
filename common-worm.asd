(asdf:defsystem common-worm
  :version "0"
  :description "Classic worm game"
  :maintainer "Kat <kzm@sykosomatic.org>"
  :author "Kat <kzm@sykosomatic.org>"
  :licence "MIT"
  :depends-on (until-it-dies.base)
  :serial t
  :components
  ((:file "packages")
   (:file "common-worm")))