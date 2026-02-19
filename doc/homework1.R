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
# Use bench package for microbenchmarking
# bench::press to press results over many dimensions
benchmarks <- bench::press(
  dims = seq(10, 100, by = 10),
  {
    A <- matrix(rnorm(dims^2), nrow = dims)
    B <- matrix(rnorm(dims^2), nrow = dims)
    x <- rnorm(dims)
    # bench::mark to do the actual benchmarking
    bench::mark("A(Bx)" = mult_ABx(A, B, x),
                "(AB)x" = mult_ABx(A, B, x, slow = TRUE)
    )
  }
)

# Plot the results with ggplot
library(ggplot2)
benchmarks <- benchmarks[, c("expression", "dims", "median")]
colnames(benchmarks) <- c("Method", "Dimensions", "CPUTime")
benchmarks$Method <- factor(benchmarks$Method, levels = c("A(Bx)", "(AB)x"))
ggplot(data = benchmarks, aes(x = Dimensions, y = CPUTime, color = Method)) +
  geom_point() +
  labs(title = "Benchmarking Matrix/Vector Multiplication Methods",
       x = "Matrix/Vector Dimension",
       y = "Median Time",
       color = "Method") +
  bench::scale_y_bench_time(base = NULL) +
  theme_bw()

## -----------------------------------------------------------------------------
benchmarks2 <- bench::press(
  dims = seq(10, 100, by = 10),
  {
    A <- matrix(rnorm(dims^2), nrow = dims)
    c <- rnorm(dims)
    bench::mark(
      "solve(A, c)" = mult_Ainv_c(A, c, direct = TRUE),
      "solve(A) %*% c" = mult_Ainv_c(A, c, direct = FALSE)
    )
  }
)

benchmarks2 <- benchmarks2[, c("expression", "dims", "median")]
colnames(benchmarks2) <- c("Method", "Dimensions", "CPUTime")
benchmarks2$Method <- factor(benchmarks2$Method, levels = c("solve(A, c)", "solve(A) %*% c"))

ggplot(data = benchmarks2, aes(x = Dimensions, y = CPUTime, color = Method)) +
  geom_point() +
  labs(title = "Benchmarking Methods for Computing A^{-1}c",
       x = "Matrix/Vector Dimension",
       y = "Median Time",
       color = "Method") +
  bench::scale_y_bench_time(base = NULL) +
  theme_bw()

