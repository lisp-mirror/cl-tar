(require "asdf")

(asdf:load-system :tar)

(asdf:load-system :40ants-doc-full)

(40ants-doc/builder:update-asdf-system-html-docs tar::@manual :tar)
