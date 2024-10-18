knapsack_dynamic_programming_optimized <- function(x, W) {
  stopifnot("v" %in% names(x) & "w" %in% names(x) & all(x$v > 0) & all(x$w > 0) & (W >= 0))
  x$original_index <- 1:length(x$v)
  x <- subset(x, w <= W)
  n <- nrow(x)
  dp <- matrix(0, nrow = n + 1, ncol = W + 1)
  
  # Fill DP table
  for (i in 2:(n + 1)) {
    for (w in 1:(W + 1)) {
      if (x$w[i - 1] > w - 1) { 
        dp[i, w] <- dp[i - 1, w]
      } else {
        dp[i, w] <- max(dp[i - 1, w], dp[i - 1, w - x$w[i - 1]] + x$v[i - 1])
      }
    }
  }
  
  # Backtrack to find which items were picked
  selected_items <- c()
  remaining_capacity <- W
  for (i in n:1) {
    if (remaining_capacity <= 0) {
      break  # No remaining capacity to consider further items
    }
    if (dp[i + 1, remaining_capacity + 1] != dp[i, remaining_capacity + 1]) {
      selected_items <- c(i, selected_items)  # Item i was chosen
      remaining_capacity <- remaining_capacity - x$w[i]
    }
  }
  
  max_value <- dp[n + 1, W + 1]
  
  return(list(max_value = round(max_value), selected_items = x$original_index[selected_items]))
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

system.time({
 result_optimized <- knapsack_dynamic_programming_optimized(knapsack_objects, 3300)
})