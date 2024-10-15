


#Optimized versions of "greedy_knapsack":


#OPIMIZATION: Preallocate Vectors:
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

profvis({
  result_optimized <- greedy_knapsack_optimized_with_sleep(knapsack_objects, W = 500000)
})


#Question: What performance gain could you get by trying to improving your code?
  # the optimized function lowered the execution time - function became vary fast