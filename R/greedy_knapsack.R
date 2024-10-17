greedy_knapsack <- function(x, W) {
  # Check for invalid input: W must be non-negative
  if (W < 0) {
    stop("The knapsack capacity must be non-negative")
  }
  
  # Check if x contains the necessary columns 'v' (value) and 'w' (weight)
  if (!all(c("v", "w") %in% colnames(x))) {
    stop("Input data must contain 'v' for values and 'w' for weights")
  }
  
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


# Measure the time to run the algorithm using system.time for n <- 1000000 objects
# user  system elapsed 
# 0.38    0.13    0.49




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

# Generating 1,000,000 objects
n <- 1000000
suppressWarnings({
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
})
knapsack_objects_large <- data.frame(
  w = sample(1:4000, size = n, replace = TRUE),
  v = runif(n = n, 0, 10000)
)

# Measure the time to run the algorithm
system.time({
  result_large <- greedy_knapsack(knapsack_objects_large, W = 500000)
})

print(result_large$value)
print(result_large$elements)

