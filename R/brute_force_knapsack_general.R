#' generlized Brute Force Knapsack Solver
#'
#' Solves the knapsack problem using a parallelized brute force approach or with non paralellalized brute force approach depending on inpur.
#' @param x A data frame containing two columns: \code{v} (values) and \code{w} (weights) for each item.
#' @param W A numeric value representing the maximum capacity (weight) of the knapsack.
#' @param parallel A logical value indicating whether to enable parallel processing.
#'
#' @return A list with the following components:
#'   \item{value}{The maximum value that can be obtained.}
#'   \item{elements}{The indices of the items selected to obtain the maximum value.}
#'   \item{weight}{The total weight of the selected items.}
#'   \item{parallel}{True if user wants to run brute force with parallel, false if user wants regular brute force}
#'
#' @import parallel
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
