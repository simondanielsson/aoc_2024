.PHONY: % 
%:
	dune exec bin/main_$@.exe < inputs/$@.txt

.PHONY: %_test
%_test:
	dune exec bin/main_$*.exe < inputs/$*-test.txt
