profvis({
  set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
  n <- 1000000
  knapsack_objects <- data.frame(
    w = sample(1:4000, size = n, replace = TRUE),
    v = runif(n = n, 0, 10000)
  )
  
  # Run the greedy knapsack function
  result <- greedy_knapsack(knapsack_objects, W = 500000)
})




#Optimized versions of "greedy_knapsack":


#OPIMIZATION 1: Preallocate Vectors:
greedy_knapsack_optimized <- function(x, W) {
  # Add a column to keep track of original indices
  x$original_index <- 1:nrow(x)
  
  # Calculate the value-to-weight ratio
  x$ratio <- x$v / x$w
  
  # Sort items by the ratio in decreasing order
  x <- x[order(-x$ratio), ]
  
  # Initialize total value and current weight
  total_value <- 0
  current_weight <- 0
  elements <- integer(nrow(x))  # Preallocate space for elements
  count <- 0  # Track the number of elements added
  
  # Iterate through the sorted items
  for (i in 1:nrow(x)) {
    if (current_weight + x$w[i] <= W) {
      # Add item to the knapsack
      current_weight <- current_weight + x$w[i]
      total_value <- total_value + x$v[i]
      count <- count + 1
      elements[count] <- x$original_index[i]  # Assign the original index
    } else {
      break
    }
  }
  
  # Return a list with the total value and selected elements
  return(list(value = total_value, elements = elements[1:count]))  # Only return filled elements
}




#OPIMIZATION 2: Using data.table for faster sorting in the algorithm
# Install and load the data.table package
install.packages("data.table")
library(data.table)

greedy_knapsack_optimized_dt <- function(x, W) {
  # Convert to data.table for faster operations
  x <- as.data.table(x)
  
  # Add a column to keep track of original indices
  x[, original_index := .I]
  
  # Calculate the value-to-weight ratio
  x[, ratio := v / w]
  
  # Sort by the ratio
  setorder(x, -ratio)
  
  # Initialize total value and current weight
  total_value <- 0
  current_weight <- 0
  elements <- integer(nrow(x))  # Preallocate space for elements
  count <- 0  # Track the number of elements added
  
  # Iterate through the sorted items
  for (i in 1:nrow(x)) {
    if (current_weight + x$w[i] <= W) {
      # Add item to the knapsack
      current_weight <- current_weight + x$w[i]
      total_value <- total_value + x$v[i]
      count <- count + 1
      elements[count] <- x$original_index[i]
    } else {
      break
    }
  }
  
  # Return a list with the total value and selected elements
  return(list(value = total_value, elements = elements[1:count]))  # Only return filled elements
}

#Measuring the time/performance for greedy_knapsack_optimized
# Original function
system.time({
  result <- greedy_knapsack(knapsack_objects, W = 500000)
})

# Optimized function
system.time({
  result_optimized <- greedy_knapsack_optimized(knapsack_objects, W = 500000)
})

# Optimized function with data.table
system.time({
  result_optimized_dt <- greedy_knapsack_optimized_dt(knapsack_objects, W = 500000)
})

profvis({
  result_optimized <- greedy_knapsack_optimized(knapsack_objects, W = 500000)
})
