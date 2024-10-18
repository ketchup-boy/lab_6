#' Brute Force Knapsack Solver (Optimized)
#'
#' Solves the knapsack problem using a brute force approach with pruning to exclude items that exceed the capacity.
#' This version uses binary representation to explore all possible item subsets.
#'
#' @param x A data frame containing two columns: v (values) and w (weights).
#' @param W A numeric value representing the maximum weight capacity of the knapsack.
#'
#' @return A list with the following components:
#'   \item{value}{The maximum value obtained.}
#'   \item{selected_items}{The indices of the selected items that maximize the value.}
#'   \item{weight}{The total weight of the selected items.}
#' @export
brute_force_knapsack_optimized <- function(x, W){
  stopifnot("v" %in% names(x) & "w" %in% names(x) & all(x$v > 0) & all(x$w > 0))
  #pruning away items that have a weight above the capacity mainly works 
  #when you have a lot of items above max capacity. Otherwise
  #it might even take longer time
  #my approach takes a little bit more memory since i now added a column of the original indexes of the list. This 
  #could have been made in a better way but it seemed like the simpelest for now
  x$original_index <- 1:length(x$v)
  x <- subset(x, w <= W)
  n <- length(x$v)
  current_max_weight <- 0
  current_best_items <- 0
  current_best_value <- 0
  loop_time <- (2^n)-1
  for(i in 1:loop_time){
    convertion <- intToBits(i)
    indexes <- which(convertion == 01)
    current_weight <- sum(x$w[indexes])
    if((current_weight <= W)){
      current_value <- sum(x$v[indexes])
      if(current_value > current_best_value){
        current_best_value <- current_value
        current_best_items <- indexes
        current_max_weight <- current_weight
      }
    }
  }
  
  return(list(value = current_best_value, selected_items = x$original_index[current_best_items], weight = current_max_weight))
}

RNGversion(min(as.character(getRversion()),"3.5.3"))

set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 16
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

#system.time({
 # result_optimized <- brute_force_knapsack_optimized(knapsack_objects, 3300)
#})

#o <- profvis({brute_force_knapsack_optimized(knapsack_objects, 4033)})
#o

