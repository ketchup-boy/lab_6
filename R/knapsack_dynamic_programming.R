

knapsack_dynamic_programming <- function(x, W){
  n <- length(x$v)
  m <- matrix(0, nrow = (n+1), ncol = (W+1))
  for(i in 2:(n)){
    for(j in 2:(W)){
      if(x$w[i] > j){
        m[i,j] <- m[i-1,j]
      }
      else{
        m[i,j] <- max(m[i-1, j], m[i-1, j-x$w[i]] + x$v[i])
      }
    }
  }
  return(m[n, W])
}


result <- knapsack_dynamic_programming(knapsack_objects, 3300)
