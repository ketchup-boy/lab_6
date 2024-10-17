
knapsack_dynamic_programming <- function(x, W) {
  stopifnot("v" %in% names(x) & "w" %in% names(x) & all(x$v > 0) & all(x$w > 0))
  n <- nrow(x)
  dp <- matrix(0, nrow = n + 1, ncol = W + 1)
  
  for (i in 2:(n+1)) {
    for (w in 2:(W+1)) {
      if (x$w[i] > W) {
        dp[i, w] <- dp[i-1,w]
      } else {
        dp[i, w] <- max(dp[i-1, w], dp[i-1, w-x$w[i]] + x$v[i])
      }
    }
  }
  
  max_value <- dp[n + 1, W + 1]
  
  # selected_items <- c()
  # remaining_capacity <- W
  # for (i in n:1) {
  #   if (dp[i + 1, remaining_capacity + 1] != dp[i, remaining_capacity + 1]) {
  #     selected_items <- c(selected_items, i)
  #     remaining_capacity <- remaining_capacity - x$w[i]
  #   }
  # }
  # 
  # return(list(max_value = max_value, selected_items = selected_items))
  return(max_value)
}

RNGversion(min(as.character(getRversion()),"3.5.3"))

set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 16
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

#o <- profvis({dynamic_programming_knapsack(knapsack_objects, 4033)})
#o

#system.time({
result <- knapsack_dynamic_programming(knapsack_objects, 3300)
#})
