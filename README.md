# mal-ocaml

My implementation of [Mal](https://github.com/kanaka/mal) (Make-A-Lisp) in [OCaml](https://ocaml.org/).

[Make-A-Lisp Guide](https://github.com/kanaka/mal/blob/master/process/guide.md)

## Build

### Dependencies

- ocaml compiler *(tested on 4.07.0)*
- [dune](https://dune.build/): for building executables *(tested on 1.10.0)*
- make
- rlwrap: for better repl experience
- python *(optional)*: run mal project test suite

### Build mal-ocaml

```bash
make ocaml
```

### Clean up

```bash
make "clean^ocaml"
```

## Run mal-ocaml REPL

```bash
ocaml/_build/default/stepA_mal.exe
```

## Run a Mal Program

```bash
ocaml/_build/default/stepA_mal.exe examples/hello.mal
```

## License

Mal (make-a-lisp) is created by Joel Martin and licensed under the MPL 2.0 (Mozilla Public License 2.0). See LICENSE for more details.
