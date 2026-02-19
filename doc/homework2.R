## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  fig.width = 8,
  collapse = TRUE,
  rmarkdown.html_vignette.check_title = FALSE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(STATS230)

## -----------------------------------------------------------------------------
# Simulate 100 realizations of a 4-dimensional multivariate normal distribution
set.seed(12345)
# Generate a mean vector
mu <- rpois(4, 1)
# Generate a covariance matrix
A <- matrix(rpois(16, 1), nrow = 4)
A[upper.tri(A)] <- 0
Sigma <- crossprod(A)

# Simulate from the multivariate normal distribution
mvn_sims <- rmvnorm(mean = mu,
                    cov = Sigma,
                    N = 100)

# Compute the sample mean and compare
sample_means <- rowMeans(mvn_sims)
compare_means <- data.frame("True" = mu,
                            "Sample" = sample_means,
                            "Relative Difference" = (mu - sample_means) / mu)
kableExtra::kable(compare_means)

# Compute sample covariance
sample_covs <- cov(t(mvn_sims))

# Create tables
kableExtra::kable(Sigma, digits = 3, caption = "True Sigma") |>
  kableExtra::kable_styling(full_width = FALSE)
kableExtra::kable(sample_covs, digits = 3, caption = "Sample Covariance") |>
  kableExtra::kable_styling(full_width = FALSE)

## -----------------------------------------------------------------------------
# Use bench package for microbenchmarking
# bench::press to press results over many dimensions
set.seed(12345)
benchmarks <- bench::press(
  dims = c(4, 16, 32, 64, 128, 256),
  {
    x <- rep(0, dims)
    mu <- rep(0, dims)
    A <- matrix(rnorm(dims^2), nrow = dims)
    A[upper.tri(A)] <- 0
    Sigma <- crossprod(A) + diag(dims) * 0.01

    bench::mark(
      "Cholesky" = dmvnorm(x, mu, Sigma),
      "Eigen" = dmvnorm(x, mu, Sigma, method = "eigen")
    )
  }
)

# Plot the results with ggplot
library(ggplot2)
benchmarks <- benchmarks[, c("dims", "median", "expression")]
colnames(benchmarks) <- c("Dimensions", "CPUTime", "Method")
benchmarks$Method <- as.character(benchmarks$Method)
ggplot(data = benchmarks, aes(x = Dimensions, y = CPUTime, color = Method)) +
  geom_point() +
  geom_line() +
  labs(title = "Benchmarking Multivariate Normal Density Evaluation",
       x = "Matrix/Vector Dimension",
       y = "Median Time") +
  bench::scale_y_bench_time(base = NULL) +
  theme_bw()

## -----------------------------------------------------------------------------
# load data
data <- read.csv(system.file("homework2_regression.csv",
                             package = "STATS230"))

# run each regression
# use 0 + in formula because we already have a column for intercept
chol_lm <- my_lm(y ~ 0 + ., data = data, method = "chol", se = TRUE)
qr_lm <- my_lm(y ~ 0 + ., data = data, method = "qr", se = TRUE)
svd_lm <- my_lm(y ~ 0 + ., data = data, method = "svd", se = TRUE)
stats_lm <- stats::lm(y ~ 0 + ., data = data) # note that lm does qr by default

# compare the coefficients in a table
kableExtra::kable(
  data.frame(
    chol = chol_lm$coefficients,
    qr = qr_lm$coefficients,
    svd = svd_lm$coefficients,
    stats = stats_lm$coefficients
  ),
  caption = "Comparison of coefficients from different methods"
)

# compare the standard errors in a table
kableExtra::kable(
  data.frame(
    chol = chol_lm$standard_errors,
    qr = qr_lm$standard_errors,
    svd = svd_lm$standard_errors,
    stats = summary(stats_lm)$coefficients[, "Std. Error"]
  ),
  caption = "Comparison of standard errors from different methods"
)

## -----------------------------------------------------------------------------
benchmarks <- bench::mark(
  "Cholesky" = my_lm(y ~ 0 + ., data, "chol"),
  "QR" = my_lm(y ~ 0 + ., data, "qr"),
  "SVD" = my_lm(y ~ 0 + ., data, "svd"),
  iterations = 1000
)

benchmark_times <- do.call(c, benchmarks$time)
benchmarks_df <- data.frame(
  method = rep(names(benchmarks$expression), each = 1000),
  time = benchmark_times
)
ggplot(benchmarks_df, aes(x = method, y = time)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Benchmarking Linear Regression Methods",
       x = "Method",
       y = "Time in Nanoseconds, Log Scale") +
  theme_bw() +
  scale_y_log10()

## -----------------------------------------------------------------------------
X <- as.matrix(data[, -1])
XTX <- crossprod(X)
lambda <- eigen(XTX)$values
max(lambda) / min(lambda)

