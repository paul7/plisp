(defsystem plisp
  :depends-on (:anaphora)
  :components ((:module "src"
			:components ((:file "defmodule")
				     (:file "sync"
					    :depends-on ("defmodule"))))))
