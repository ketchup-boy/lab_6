

# knapsack_dynamic_programming <- function(x, W){
#   n <- length(x$v)
#   m <- matrix(0, nrow = (n+1), ncol = (W+1))
#   for(i in 2:(n)){
#     for(j in 2:(W)){
#       if(x$w[i] > j){
#         m[i,j] <- m[i-1,j]
#       }
#       else{
#         m[i,j] <- max(m[i-1, j], m[i-1, j-x$w[i]] + x$v[i])
#       }
#     }
#   }
#   return(m[n, W])
# }


knapsack_dynamic_programming <- function(x, W) {
  stopifnot("v" %in% names(x) & "w" %in% names(x) & all(x$v > 0) & all(x$w > 0))
  
  # Number of items
  n <- nrow(x)
  
  # Create a matrix to store the maximum value at each weight from 0 to W
  dp <- matrix(0, nrow = n + 1, ncol = W + 1)
  
  # Fill the dp table
  for (i in 1:n) {
    for (w in 0:W) {
      if (x$w[i] <= w) {
        dp[i + 1, w + 1] <- max(dp[i, w + 1], dp[i, w - x$w[i] + 1] + x$v[i])
      } else {
        dp[i + 1, w + 1] <- dp[i, w + 1]
      }
    }
  }
  
  # The maximum value that can be carried in the knapsack of weight W
  max_value <- dp[n + 1, W + 1]
  
  # To find which items were selected, we can trace back from dp[n+1, W+1]
  selected_items <- c()
  remaining_capacity <- W
  for (i in n:1) {
    if (dp[i + 1, remaining_capacity + 1] != dp[i, remaining_capacity + 1]) {
      selected_items <- c(selected_items, i)
      remaining_capacity <- remaining_capacity - x$w[i]
    }
  }
  
  # Return result as a list containing the max value and the selected items
  return(list(max_value = max_value, selected_items = selected_items))
}

result <- knapsack_dynamic_programming(knapsack_objects, 3300)
