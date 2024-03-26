# Helo —— My Toy Language

For detailed introduction to Helo and examples, refer to Github Pages [here](https://danzou1ge6.github.io/helo/).

## Building
Because of unstable features applied, `nightly` channel is required to build Helo.
After installing `nightly` channel toolchain, set toolchain override of this repository by

```bash
rustup override set nightly
```

### CLI
The binary name is called `heloc` and `helo_vm`, located under crate `crates/helo_bin`.
To build them, run

```bash
cd crates/helo_wasm
cargo build --bin heloc --features terminal_size
cargo build --bin helo_vm
```

### Web UI
To compile the web pages hosted on Github Pages, `wasm-pack` and Node.js 20 is required

```bash
cargo install wasm-pack
cargo install wasm-opt  # will be automatically downloaded by `wasm-pack`
```

Then WASM artifects can be compiled with

```bash
cd crates/helo_wasm
./build.sh
```

The web pages are written with [Quasar Framework](https://quasar.dev/).
It can be built with

```bash
cd web
yarn  # Install dependencies
yarn quasar build
```
