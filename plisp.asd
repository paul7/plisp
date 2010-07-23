(defsystem plisp
  :depends-on ()
  :components ((:module "src"
			:components ((:file "defmodule")
				     (:file "sync"
					    :depends-on ("defmodule"))))))
