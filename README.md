#  Functional Programming Lab 2 — Haskell Parallel Integration Project

##  Project Overview

  This project implements a **multithreaded numerical integration system** in **Haskell**, using the **Simpson’s method** with adaptive accuracy control.  
  The goal is to demonstrate parallel computation, functional decomposition, and runtime performance scaling via the GHC threaded runtime system.

  The application supports integration of several functions (`sin`, `cos`, `exp`, `poly`), adaptive refinement by precision `ε`, and configurable multithreading.

---

## 📦 Versions and Requirements

| Tool / Dependency | Version / Notes |
| ------------------ | --------------- |
| **GHC** | 9.6.7 |
| **Cabal** | 3.10+ |
| **IntelliJ IDEA** | 2023.3+ (with Haskell plugin) |
| **Libraries** | `parallel`, `deepseq`, `containers`, `hspec` |
| **OS Tested** | macOS (ARM64), Ubuntu 22.04 |

---

##  Motivation & Authors

This project was developed as part of the **Functional Programming course** to explore:
- Parallel evaluation in **pure functional languages**
- **Lazy evaluation** and performance trade-offs
- Designing reproducible **numerical algorithms** in Haskell
- Using **testing frameworks** to verify convergence and correctness

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

  Execute the application using **`cabal v2-run`**. To fully enable parallelism, you **must** include the **GHC Runtime System (RTS) flags** (e.g., `+RTS -N4 -s`) *after* the main application arguments (`--`).

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

**Author:** *Diana Velychko*  
**Year:** 2025
