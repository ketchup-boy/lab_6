knapsack_brute_force <- function(x, W){
  stopifnot("v" %in% names(x) & "w" %in% names(x) & all(x$v > 0) & all(x$w > 0))
  n <- length(x$v)
  current_max_weight <- 0
  current_best_items <- 0
  current_best_value <- 0
  for(i in 1:((2^n)-1)){
    convertion <- intToBits(i)
    indexes <- which(convertion == 01)
    current_weight <- sum(x$w[indexes])
    current_value <- sum(x$v[indexes])
    #browser()
    if((current_weight <= W) & (current_value > current_best_value)){
      current_best_value <- current_value
      current_best_items <- indexes
      current_max_weight <- current_weight
    }
  }
  return(list(value = current_best_value, elements = current_best_items, weight = current_max_weight))
}

result <- knapsack_brute_force(knapsack_objects, 6033)



