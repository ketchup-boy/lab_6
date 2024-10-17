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
n <- 16
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )
# 
#  library(profvis)
#  l <- profvis({brute_force_knapsack(knapsack_objects, 4033)})
#  l

#system.time({
  result_bf <- brute_force_knapsack(knapsack_objects, 3300)
#})

#system.time({
 # result_parallel <- brute_force_knapsack_general(knapsack_objects, 3000, TRUE)
#})
 #result <- brute_force_knapsack(knapsack_objects, 3000)
