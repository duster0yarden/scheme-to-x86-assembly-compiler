%:
	echo '(load "project/compiler.scm") (compile-scheme-file "$(MAKECMDGOALS).scm" "$(MAKECMDGOALS).s")' | scheme -q
	nasm -g -f elf64 $(MAKECMDGOALS).s
	gcc -lc -o $(MAKECMDGOALS) $(MAKECMDGOALS).o