test2:
	ocamlbuild test2.native

git_update:
	git add -A && git commit -m 'made some changes by Ruize' && git push draft


# running test case produce input and output
runtest:
	./test2.native < testcase.cheez
checkgrammar:
	ocamlyacc -v parser.mly
.PHONY: clean
clean:
	rm -rf _build/  *.native
