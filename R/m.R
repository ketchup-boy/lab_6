RNGversion(min(as.character(getRversion()),"3.5.3"))

set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 600
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )


system.time({
  knapsack_dynamic_programming(knapsack_objects, 3000)
})

system.time({
  knapsack_dynamic_programming_optimized(knapsack_objects, 3000)
})