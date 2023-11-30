test: 
	guile -L . tests/fixnum.scm
	guile -L . tests/decimal.scm
	guile -L . tests/string.scm
	guile -L . tests/bool.scm
	guile -L . tests/number.scm
	guile -L . tests/parser.scm
