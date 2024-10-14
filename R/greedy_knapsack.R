greedy_knapsack <- function(x, W) {
  # Add a column to keep track of original indices
  x$original_index <- 1:nrow(x)
  
  # Calculate the value-to-weight ratio
  x$ratio <- x$v / x$w
  
  # Sort items by the ratio in decreasing order
  x <- x[order(-x$ratio), ]
  
  # Initialize total value and current weight
  total_value <- 0
  current_weight <- 0
  elements <- c()
  
  # Iterate through the sorted items
  for (i in 1:nrow(x)) {
    if (current_weight + x$w[i] <= W) {
      # Add item to the knapsack
      current_weight <- current_weight + x$w[i]
      total_value <- total_value + x$v[i]
      elements <- c(elements, x$original_index[i])  # Keep track of the original item indices
    } else {
      break
    }
  }
  
  # Return a list with the total value and selected elements
  return(list(value = total_value, elements = elements))
}

# Example usage:
RNGversion("3.5.3")
suppressWarnings({
  set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
})
n <- 2000
knapsack_objects <- data.frame(
  w = sample(1:4000, size = n, replace = TRUE),
  v = runif(n = n, 0, 10000)
)


