.PHONY:
.SILENT:

compile:
	jai-linux -x64 src/main.jai

release:
	jai-linux -release src/main.jai

run-release: release
	./src/main

run: compile
	./src/main