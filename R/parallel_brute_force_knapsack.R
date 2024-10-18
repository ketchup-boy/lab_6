library(parallel)

#' Parallelized Brute Force Knapsack Solver
#'
#' Solves the 0/1 knapsack problem using a parallelized brute force approach.
#' This function divides the search space across multiple processors to improve performance,
#' especially for large numbers of items, by evaluating chunks of possible combinations in parallel.
#'
#' @param x A data frame containing two columns: \code{v} (values) and \code{w} (weights) for each item.
#' @param W A numeric value representing the maximum capacity (weight) of the knapsack.
#'
#' @return A list with the following components:
#'   \item{value}{The maximum value that can be obtained.}
#'   \item{elements}{The indices of the items selected to obtain the maximum value.}
#'   \item{weight}{The total weight of the selected items.}
#' @import parallel
#' @details
#' This function evaluates all possible combinations of items using brute force, but to improve 
#' performance, it evaluates chunks of combinations in parallel using the \code{parallel} package. 
#' Each chunk of combinations is processed separately, and the best combination is selected.
#'
#' @export

brute_force_knapsack_parallel <- function(x, W) {
  stopifnot("v" %in% names(x) & "w" %in% names(x) & all(x$v > 0) & all(x$w > 0))
  n <- length(x$v)
  current_max_weight <- 0
  current_best_items <- 0
  current_best_value <- 0
  
  evaluate_chunk <- function(indices) {
    best_value <- 0
    best_items <- NULL
    best_weight <- 0
    
    for (i in indices) {
      convertion <- intToBits(i)
      indexes <- which(convertion == 01)
      current_weight <- sum(x$w[indexes])
      current_value <- sum(x$v[indexes])
      
      if (current_weight <= W && current_value > best_value) {
        best_value <- current_value
        best_items <- indexes
        best_weight <- current_weight
      }
    }
    
    return(list(value = best_value, elements = best_items, weight = best_weight))
  }

  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  results <- parLapply(cl, 1:(2^n - 1), evaluate_chunk)

  stopCluster(cl)
  overall_best <- Reduce(function(a, b) if (a$value > b$value) a else b, results)
  return(overall_best)
}


#' generlized Brute Force Knapsack Solver
#'
#' Solves the knapsack problem using a parallelized brute force approach or with non paralellalized brute force approach depending on inpur.
#' @param x A data frame containing two columns: \code{v} (values) and \code{w} (weights) for each item.
#' @param W A numeric value representing the maximum capacity (weight) of the knapsack.
#'
#' @return A list with the following components:
#'   \item{value}{The maximum value that can be obtained.}
#'   \item{elements}{The indices of the items selected to obtain the maximum value.}
#'   \item{weight}{The total weight of the selected items.}
#'   \item{parallel}{True if user wants to run brute force with parallel, false if user wants regular brute force}
#'
#' @details
#' This function evaluates calls brute_force_knapsack or brute_force_knapsack_optimized depending on boolean input for parameter parallel
#'
#' @export

brute_force_knapsack_general <- function(x, W, parallel){
  if(parallel){
    return(brute_force_knapsack_parallel(x, W))
  }
  else{
    return(brute_force_knapsack(x, W))
  }
}

result_parallel <- brute_force_knapsack_parallel(knapsack_objects, 3000)

