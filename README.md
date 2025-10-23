You're right\! My previous version was formatted well, but it lacked the absolute standard for GitHub READMEs, such as the initial heading structure and ensuring all badges are correctly rendered.

Here is the final **copy-and-paste ready** version, perfect for GitHub:

-----

````markdown
#  Integral Parallel: High-Performance Multithreaded Integration in Haskell

[![Haskell](https://img.shields.io/badge/Language-Haskell-5D4F85?style=for-the-badge&logo=haskell)](https://www.haskell.org/)
[![Build](https://img.shields.io/badge/Build-Passing-brightgreen?style=for-the-badge)]()
[![License](https://img.shields.io/badge/License-MIT-blue?style=for-the-badge)](LICENSE)

A high-performance command-line application built in **Haskell (GHC)** for computing **definite integrals**. It leverages GHC's powerful runtime system for **multithreaded execution**, implementing the **Simpson’s method** with adaptive accuracy control.

Parallelization is seamlessly achieved using the [`Control.Parallel.Strategies`](https://hackage.haskell.org/package/parallel) module, enabling full utilization of GHC’s `-threaded` runtime.

---

##  Project Overview: Parallel Numerical Computing

This project serves as a robust demonstration of building **parallel numerical computations** within a functional programming paradigm. It is designed to be highly scalable and precise.

The program estimates definite integrals for various mathematical functions, featuring:

* **Massive Parallelism:** Scales computation across multiple CPU cores.
* **Adaptive Precision:** Supports configurable precision ($\varepsilon$) with automatic refinement.
* **Robust Algorithm:** Uses the highly accurate **Adaptive Simpson's Method**.

---

##  Key Features

| Feature | Description |
| :--- | :--- |
| ✅ **Multithreaded Execution** | Parallel computation using GHC's **`-threaded`** runtime and `+RTS -N` for core utilization. |
| ✅ **Adaptive Integration** | Automatic precision refinement via the **Adaptive Simpson’s Method**. |
| ✅ **CLI Configuration** | Easily configurable integral bounds (`--a`, `--b`), precision (`--eps`), and function (`--func`). |
| ✅ **Explicit Thread Control** | Runtime thread management via `setNumCapabilities` and `+RTS -N`. |
| ✅ **Full Unit Testing** | Comprehensive unit test suite powered by **Hspec**. |

---

##  Algorithm: Adaptive Simpson's Rule

The project utilizes the **Simpson’s rule** to efficiently approximate the definite integral of a continuous function $f(x)$ over an interval $[a, b]$.

### Formula

The core approximation formula is:

$$I_n = \frac{h}{3} \sum_{i=0}^{n} w_i f(x_i)$$

where $h = (b - a) / n$, and the weights $w_i$ are defined piecewise:

$$
w_i =
\begin{cases}
1, & i = 0, n \\
4, & i\ \text{odd} \\
2, & i\ \text{even}
\end{cases}
$$

### Parallel Strategy

Parallelization is achieved by dividing the integration interval into smaller **chunks** and processing these in parallel using Haskell's `Strategies`:

```haskell
-- Divides the list of integration points (xs) into chunks,
-- evaluates each chunk in parallel, and forces deep evaluation.
withStrategy (parListChunk chunk rdeepseq) (map f xs)
````

-----

##  Technologies & Setup

| Component | Detail |
| :--- | :--- |
| **Language** | **Haskell** |
| **Compiler** | GHC 9.6.7+ |
| **Build Tool** | Cabal |
| **Parallelism** | `Control.Parallel.Strategies`, `deepseq` |
| **Testing** | Hspec |

### Project Structure

```text
integral-parallel/
├── app/
│   └── Main.hs               # CLI entry point and main logic
├── src/
│   └── Numerics/
│       └── Integrate.hs      # Parallel Simpson integration core
├── test/
│   ├── Spec.hs               # Hspec test discovery
│   └── NumericsSpec.hs       # Unit tests for integration logic
└── integral-parallel.cabal   # Project configuration
```

### Build Instructions

To prepare the project, run the following commands:

```bash
cabal clean
cabal v2-update
cabal v2-build
```

-----

##  Running the Application

Execute the application using `cabal v2-run`. To fully enable parallelism, you **must** include the **GHC Runtime System (RTS) flags** (e.g., `+RTS -N4 -s`) *after* the main application arguments (`--`).

### Example: Integrate $sin(x)$

Integrate $sin(x)$ from **0** to **$\pi$** using **4 threads** and a precision of **$10^{-9}$**.

```bash
cabal v2-run integral-parallel -- \
  --a 0 \
  --b 3.1415926535 \
  --eps 1e-9 \
  --func sin \
  --threads 4 \
  +RTS -N4 -s
```

  * **`+RTS -N4`**: Instructs the GHC RTS to use a maximum of 4 processor cores.
  * **`-s`**: Prints runtime statistics, including garbage collection and parallel execution time.

##  Testing

Run the full suite of unit tests with:

```bash
cabal v2-test
```

```
```
