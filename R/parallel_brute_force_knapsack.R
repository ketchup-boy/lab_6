library(parallel)

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


brute_force_knapsack_general <- function(x, W, parallel){
  if(parallel){
    return(brute_force_knapsack_parallel(x, W))
  }
  else{
    return(brute_force_knapsack(x, W))
  }
}

result_parallel <- brute_force_knapsack_parallel(knapsack_objects, 3000)

