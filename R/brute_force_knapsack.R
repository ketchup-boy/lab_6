#' Brute Force Knapsack Solver
#'
#' Solves the 0/1 knapsack problem using a brute force approach, where every
#' possible subset of items is checked to determine the optimal selection.
#'
#' @param x A data frame containing two columns: `v` (values) and `w` (weights).
#' @param W A numeric value representing the maximum capacity (weight) of the knapsack.
#'
#' @return A list with the following components:
#'   \item{max_value}{The maximum value that can be obtained.}
#'   \item{selected_items}{The indices of the items selected to obtain the maximum value.}
#' @export
brute_force_knapsack <- function(x, W){
  stopifnot("v" %in% names(x) & "w" %in% names(x) & all(x$v > 0) & all(x$w > 0) & (W >= 0))
  n <- length(x$v)
  current_best_items <- 0
  current_best_value <- 0
  for(i in 1:(2^n)-1){
    convertion <- intToBits(i)
    indexes <- which(convertion == 01)
    current_weight <- sum(x$w[indexes])
    current_value <- sum(x$v[indexes])
    if((current_weight <= W) & (current_value > current_best_value)){
        current_best_value <- current_value
        current_best_items <- indexes
    }
  }
  return(list(value = current_best_value, elements = current_best_items))
}

RNGversion(min(as.character(getRversion()),"3.5.3"))

set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 18
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )
# 
#  library(profvis)
#  l <- profvis({brute_force_knapsack(knapsack_objects, 4033)})
#  l

system.time({
 result <- brute_force_knapsack_general(knapsack_objects, 3300, FALSE)
})

system.time({
result_parallel <- brute_force_knapsack_general(knapsack_objects, 3000, TRUE)
})
 #result <- brute_force_knapsack(knapsack_objects, 3000)
