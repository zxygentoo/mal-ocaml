# mal-ocaml

My implementation of [Mal](https://github.com/kanaka/mal) (Make-A-Lisp) in [OCaml](https://ocaml.org/).

[Make-A-Lisp Guide](https://github.com/kanaka/mal/blob/master/process/guide.md)

## Build

You'll need a working ocaml compiler and [dune](https://dune.build/) for building things, and make, obviously, with python for running the mal project test suite.

*(tested on ocaml 4.07.0 and dune 1.10.0)*

```bash
make rust
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

Mal (make-a-lisp) is created by Joel Martin and licensed under the MPL 2.0 (Mozilla Public License 2.0). See LICENSE.txt for more details.
