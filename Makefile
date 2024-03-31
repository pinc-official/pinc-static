.PHONY: all build dev install update fmt fmt-check test clean clear
.SILENT: all build dev install update fmt fmt-check test clean clear

all: build

build:
	opam exec -- dune build @default

dev:
	opam exec -- dune build --watch

install:
	if ! [ -e _opam ]; then \
		opam switch create . --empty ; \
	fi
	opam install . --deps-only --with-test --with-dev-setup --yes
	opam lock .

update:
	opam update
	opam upgrade

fmt:
	dune build @fmt --auto-promote

fmt-check:
	dune build @fmt

test:
	dune exec -- pinc ./test ./out

test-coverage:
	if [ -d /tmp/pinc-static ]; then rm -r /tmp/pinc-static; fi
	mkdir -p /tmp/pinc-static
	BISECT_FILE=/tmp/pinc-official/pinc-static dune runtest --no-print-directory --instrument-with bisect_ppx --force
	bisect-ppx-report html --coverage-path /tmp/pinc-static
	bisect-ppx-report summary --coverage-path /tmp/pinc-static

clean:
	rm -rf _build
	rm -rf out

clear: clean
	rm -rf _opam

