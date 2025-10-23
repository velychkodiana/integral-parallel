# Integral Parallel — Multithreaded Integration in Haskell

[![Haskell](https://img.shields.io/badge/Language-Haskell-5D4F85?style=for-the-badge&logo=haskell)](https://www.haskell.org/)
[![Build](https://img.shields.io/badge/Build-Passing-brightgreen?style=for-the-badge)]()

A multithreaded application written in **Haskell (GHC)** that computes definite integrals using the **Simpson’s method** with adaptive accuracy control.  
Parallelization is implemented via `Control.Parallel.Strategies` and fully utilizes GHC’s `-threaded` runtime.

---

## Project Overview

This lab project demonstrates how to build **parallel numerical computations** in functional programming.  
The program estimates definite integrals for several mathematical functions, supports configurable precision (`ε`),  
and scales across multiple threads.

## 📂 Project Structure

```text
integral-parallel/
├── app/
│   └── Main.hs               # CLI entry point
├── src/
│   └── Numerics/
│       └── Integrate.hs      # Parallel Simpson integration logic
├── test/
│   ├── Spec.hs               # Hspec test discovery
│   └── NumericsSpec.hs       # Unit tests for integration
├── integral-parallel.cabal   # Project configuration
├── cabal.project.local       # Local build/test settings
└── README.md                 # (This file)


### Features
- ✅ Parallel computation using multiple CPU cores  
- ✅ Adaptive Simpson’s method with automatic precision refinement  
- ✅ Configurable input parameters via CLI  
- ✅ Thread management via `setNumCapabilities` and `+RTS -N`  
- ✅ Full unit testing suite (Hspec)  

---

## Technologies

| Component | Description |
|------------|-------------|
| **Language** | Haskell |
| **Compiler** | GHC 9.6.7 |
| **Build Tool** | Cabal |
| **IDE** | IntelliJ IDEA with Haskell plugin |
| **Libraries** | `parallel`, `deepseq`, `hspec`, `containers` |

---

## 🧮 Algorithm — Simpson’s Rule

The **Simpson’s rule** is used to approximate the definite integral of a continuous function `f(x)` on an interval `[a, b]`.

It divides the range into an even number of subintervals `n`, each of width `h = (b - a) / n`, and applies weighted summation to estimate the area under the curve.

### Formula

\[
I_n = \frac{h}{3} \sum_{i=0}^{n} w_i f(x_i)
\]

where the weights \( w_i \) are defined as:

\[
w_i =
\begin{cases}
1, & i = 0, n \\
4, & i\ \text{odd} \\
2, & i\ \text{even}
\end{cases}
\]

where `h = (b - a) / n`.

Parallel summation is achieved by dividing the interval into **chunks**:

```haskell
withStrategy (parListChunk chunk rdeepseq) (map f xs)

Then build the project using Cabal:
cabal clean
cabal v2-update
cabal v2-build

## Running the Application

Example: integrate `sin(x)` from `0` to `π` using **4 threads** and precision **1e-9**.

### Command

```bash
cabal v2-run integral-parallel -- \
  --a 0 --b 3.1415926535 --eps 1e-9 --func sin --threads 4 +RTS -N4 -s

## Testing
```bash
cabal v2-test
