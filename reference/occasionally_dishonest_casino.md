# Simulate the Occasionally Dishonest Casino Hidden Markov Model

This function simulates a sequence of observations from the
"Occasionally Dishonest Casino" hidden Markov model (HMM). The model
consists of two states: "Fair" and "Loaded". In the "Fair" state, a
six-sided die is rolled with equal probability for each face (1-6). In
the "Loaded" state, the die has unequal probabilities for each face
(1-6). The state transitions are governed by a Markov process with
specified transition probabilities.

## Usage

``` r
occasionally_dishonest_casino(
  n_obs = 100,
  initial_dist = c(0.5, 0.5),
  p_fair_to_loaded = 0.05,
  p_loaded_to_fair = 0.1,
  loaded_probs = c(0.1, 0.1, 0.5, 0.1, 0.1, 0.1)
)
```

## Arguments

- n_obs:

  Positive integer. The number of observations (die rolls) to simulate.
  Default is 100.

- initial_dist:

  Numeric vector of length 2. The initial distribution over the states.
  Must sum to 1. Default is c(0.5, 0.5) (starting in either state with
  equal probability).

- p_fair_to_loaded:

  Numeric scalar between 0 and 1. Transition probability from the "Fair"
  state to the "Loaded" state. Default is 0.02.

- p_loaded_to_fair:

  Numeric scalar between 0 and 1. Transition probability from the
  "Loaded" state to the "Fair" state. Default is 0.05.

- loaded_probs:

  Numeric vector of length 6. The probabilities of rolling each face
  (1-6) when in the "Loaded" state. Must sum to 1. Default is c(0.1,
  0.1, 0.1, 0.1, 0.1, 0.5) (biased towards rolling a 6).

## Value

A list containing the following components:

- states:

  Integer vector of length `n_obs` indicating the hidden state at each
  time point (1 for "Fair", 2 for "Loaded").

- observations:

  Integer vector of length `n_obs` containing the simulated die rolls
  (values between 1 and 6).
