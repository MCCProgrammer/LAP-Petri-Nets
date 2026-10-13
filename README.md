# 🐫 Petri Nets — Programming Languages and Environments

<p>
  <img alt="OCaml" src="https://img.shields.io/badge/OCaml-1f2328?style=flat-square&logo=ocaml&logoColor=white">
  <img alt="Course" src="https://img.shields.io/badge/Course-Programming%20Languages%20and%20Environments%20(LAP)-1f2328?style=flat-square">
  <img alt="Year" src="https://img.shields.io/badge/2024%2F25-2nd%20year%2C%202nd%20semester-1f2328?style=flat-square">
  <img alt="Tests" src="https://img.shields.io/badge/tests-66%2F66%20passing-1f2328?style=flat-square">
</p>

Projects 2 and 3 of *Linguagens e Ambientes de Programação* (Programming Languages and Environments), Computer Science degree at **NOVA School of Science and Technology (FCT NOVA)** — a course focused on **functional programming in OCaml**.

## About

An OCaml library to build and simulate **Petri nets**, a mathematical model for concurrent systems: *places* hold tokens (up to a capacity), *transitions* consume tokens from their input places and produce tokens in their output places through weighted *arcs*.

- **Project 2 — representation:** an abstract data type (`petri_net`, kept opaque by the module signature) with functions to add places, transitions and arcs and to query the net, all **purely functional and immutable**.
- **Project 3 — semantics:** checking whether a transition is enabled, **firing** a transition, firing everything that can fire, and detecting when the net is **stuck** (no transition can fire).

Operations that can fail return an `option`, so building and running a net reads as a pipeline:

```ocaml
let p = empty |> add_place idle 1 1
              |> add_place coin_inserted 1 0
              |> add_transition insert_coin
              (* … *)

let p = p |>> fire insert_coin
          |>> fire make_choice
          |>> fire heat_water
          (* … *)
```

The example nets model a **coffee machine** (insert coin → choose → heat water and grind coffee in parallel → brew → remove cup) and a fire-fighting system.

## Running it

Requires OCaml with `dune` and `ounit2` (e.g. installed with `opam install dune ounit2`).

```bash
dune build
dune test            # 66 unit tests
dune exec petrinets  # runs the coffee machine twice
```

```text
$ dune test
Ran: 24 tests in: 0.12 seconds.
OK
Ran: 27 tests in: 0.11 seconds.
OK
Ran: 8 tests in: 0.11 seconds.
OK
Ran: 7 tests in: 0.11 seconds.
OK
$ dune exec petrinets
Ready to insert coin
```

## Project structure

```
lib/petrinets.mli   # module signature (provided by the course)
lib/petrinets.ml    # our implementation
bin/main.ml         # coffee machine example
test/               # unit tests and example nets
```

## What I learned

- Abstract data types with opaque types and module signatures
- Pure functions over immutable data, higher-order functions and pattern matching
- Error handling with `option` and composing failing steps with a bind operator
- Modelling concurrency with Petri nets

## Credits

- **Miguel Carmo** ([@MCCProgrammer](https://github.com/MCCProgrammer))
- **Tomás Ramos Alves** ([@Alvesss04](https://github.com/Alvesss04))

The project template — the module signature `petrinets.mli`, the `dune` setup, the license and the test examples — was provided by the LAP teaching team; `petrinets.ml` was implemented by us.

---

<sub>The assignment statement is not included in this repository, as it belongs to the course's teaching staff. This is a student project shared for portfolio purposes.</sub>
