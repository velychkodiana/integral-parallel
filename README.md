# Multithreaded Numerical Integration in Haskell

This project implements **parallel numerical integration** using the **Simpson method** with **adaptive refinement to a target precision ε**.  
The program demonstrates **concurrency and parallel evaluation** in a pure functional setting using:

- `Control.Parallel.Strategies` for controlled parallel computation
- `deepseq` for deterministic evaluation
- `hspec` for correctness and convergence tests

The application supports several predefined functions and can run both **sequential** and **parallel** integration for performance comparison.

---

##  Requirements

| Component | Version / Notes |
|----------|----------------|
| GHC      | 9.6.7+
| Cabal    | 3.10+
| Libraries | `parallel`, `deepseq`, `hspec`
| IDE (optional) | IntelliJ IDEA + Haskell plugin
| OS Tested | macOS ARM64 / Linux compatible |

---

##  Purpose of the Project

This project was developed as part of the **Functional Programming** course to:

- Apply **Simpson’s numerical integration algorithm**
- Implement **parallelization via strategies**
- Analyze **speedup** and **scaling behavior**
- Use **testing** to validate algorithm correctness and convergence

---

##  Project Structure

```

integral-parallel/
├── app/
│   └── Main.hs                  # CLI entry point (runs seq + parallel comparison)
├── src/
│   └── Numerics/
│       ├── Funcs.hs             # Standard functions to integrate (sin, exp, polynomial)
│       ├── SimpsonSeq.hs        # Sequential Simpson integration
│       ├── SimpsonPar.hs        # Parallel Simpson integration (chunking + parListChunk)
│       └── Refine.hs            # Adaptive refinement until target epsilon is reached
├── test/
│   ├── Spec.hs                  # hspec-discover entry
│   ├── SimpsonSpec.hs           # Mathematical correctness & consistency tests
│   ├── RefinementSpec.hs        # Tests for adaptive precision refinement
│   └── PerformanceSpec.hs       # Seq vs Par consistency & performance sanity
└── integral-parallel.cabal      # Build configuration

````

---

## Build Instructions

```bash
cabal v2-clean
cabal v2-update
cabal v2-build
````

---

## Running the Program

Basic run:

```bash
cabal v2-run
```

Run with full **parallel runtime statistics**:

```bash
cabal v2-run -- -- +RTS -N -s
```

Run using **4 threads explicitly**:

```bash
cabal v2-run -- -- +RTS -N4 -s
```

---

##  Example: Integrate `sin(x)` on [0, π] with ε = 1e-8

```bash
cabal v2-run -- --func sin --a 0 --b 3.1415926535 --eps 1e-8 +RTS -N4 -s
```

Meaning:

| Flag         | Description                     |
| ------------ | ------------------------------- |
| `--func`     | Function (`sin`, `exp`, `poly`) |
| `--a`, `--b` | Integration bounds              |
| `--eps`      | Target precision                |
| `+RTS -N4`   | Use 4 CPU threads               |
| `-s`         | Show GC & CPU timing statistics |

---

##  Testing

Run all tests:

```bash
cabal v2-test --test-show-details=direct
```

What is tested:

| Test Group        | Purpose                                             |
| ----------------- | --------------------------------------------------- |
| `SimpsonSpec`     | Numerical correctness of Simpson’s rule             |
| `RefinementSpec`  | Ensure adaptive refinement reaches target precision |
| `PerformanceSpec` | Parallel result matches sequential result           |

All core requirements are automatically verified.

---

**Author:** *Diana Velychko* **Year:** 2025
