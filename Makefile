DEPENDENCIES = reprovide-lang-lib

all: build

build:
	mkdir -p out/
	raco exe -o out/aoc23 main.rkt

deps:
	raco pkg install --skip-installed $(DEPENDENCIES)
