knapsack_dynamic_programming_optimized <- function(x, W) {
  stopifnot("v" %in% names(x) & "w" %in% names(x) & all(x$v > 0) & all(x$w > 0))
  x <- subset(x, w <= W)
  n <- nrow(x)
  dp <- matrix(0, nrow = n + 1, ncol = W + 1)
  
  for (i in 1:n) {
    for (w in 0:W) {
      if (x$w[i] <= w) {
        dp[i + 1, w + 1] <- max(dp[i, w + 1], dp[i, w - x$w[i] + 1] + x$v[i])
      } else {
        dp[i + 1, w + 1] <- dp[i, w + 1]
      }
    }
  }
  
  max_value <- dp[n + 1, W + 1]
  
  selected_items <- c()
  remaining_capacity <- W
  for (i in n:1) {
    if (dp[i + 1, remaining_capacity + 1] != dp[i, remaining_capacity + 1]) {
      selected_items <- c(selected_items, i)
      remaining_capacity <- remaining_capacity - x$w[i]
    }
  }
  
  return(list(max_value = max_value, selected_items = selected_items))
}

RNGversion(min(as.character(getRversion()),"3.5.3"))

set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 500
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

#o <- profvis({dynamic_programming_knapsack_optimized(knapsack_objects, 4033)})
#o

#system.time({
 # result_optimized <- dynamic_programming_knapsack_optimized(knapsack_objects, 3300)
#})