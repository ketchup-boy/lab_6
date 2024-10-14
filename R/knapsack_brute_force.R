knapsack_brute_force <- function(x, W){
  n <- length(x$v)
  current_max_weight <- 0
  current_best_items <- 0
  for(i in 1:((2^n)-1)){
    convertion <- intToBits(i)
    indexes <- which(convertion == 01)
    current_weight <- sum(x$w[indexes])
    #browser()
    if((current_weight <= W) & (current_weight > current_max_weight)){
      current_max_weight <- current_weight
      current_best_items <- indexes
    }
  }
  return(data.frame(items = current_best_items, weight = current_max_weight))
}

result <- knapsack_brute_force(knapsack_objects, 8033)



