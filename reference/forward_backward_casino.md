# Forward-Backward Algorithm for the Occasionally Dishonest Casino HMM

Runs the forward and backward algorithms for a 2-state HMM (1=fair,
2=loaded) with discrete emissions (die rolls 1-6).

## Usage

``` r
forward_backward_casino(
  y,
  initial_dist = c(0.5, 0.5),
  p_fair_to_loaded = 0.02,
  p_loaded_to_fair = 0.05,
  loaded_probs = c(0.1, 0.1, 0.5, 0.1, 0.1, 0.1)
)
```

## Arguments

- y:

  Integer vector of observed die rolls, each in {1,2,3,4,5,6}.

- initial_dist:

  Numeric length-2 vector giving P(x_1=1) and P(x_1=2). Must sum to 1.

- p_fair_to_loaded:

  Scalar, transition prob P(x_t=2 \| x\_{t-1}=1).

- p_loaded_to_fair:

  Scalar, transition prob P(x_t=1 \| x\_{t-1}=2).

- loaded_probs:

  Numeric length-6 vector of emission probs for loaded state (sum to 1).

## Value

A list with:

- alpha:

  T x 2 matrix of forward messages. Row t is alpha_t(1:2).

- beta:

  T x 2 matrix of backward messages. Row t is beta_t(1:2).

- gamma:

  T x 2 matrix of smoothed state probabilities P(x_t=i \| y\_{1:T}).

- P:

  2 x 2 transition matrix used.

- E:

  2 x 6 emission matrix used (rows=states, cols=roll outcomes 1..6).
