# Wright-Fisher Model Path Simulation

Simulates a path of allele frequencies under the Wright-Fisher model
with mutation.

## Usage

``` r
wright_fisher_path(n_steps, x0, N = 100, u = 0.35, v = 0.5)
```

## Arguments

- n_steps:

  Integer. Number of time steps to simulate.

- x0:

  Integer. Initial number of copies of the allele (must be between 0 and
  N).

- N:

  Integer. Population size (number of alleles). Default is 100.

- u:

  Numeric. Mutation rate from allele type 1 to type 0 (between 0 and 1).
  Default is 0.35.

- v:

  Numeric. Mutation rate from allele type 0 to type 1 (between 0 and 1).
  Default is 0.5.

## Value

Numeric vector of length n_steps + 1 containing the number of copies of
the allele at each time step, starting with x0.
