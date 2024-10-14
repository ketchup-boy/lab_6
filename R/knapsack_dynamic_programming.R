knapsack_dynamic_programming <- function(x, W){
  n <- length(x$v)
  m <- matrix(0, nrows = n+1, ncols = W+1)
  
  for(i in 2:(n+1)){
    for(j in 2:(W+1)){
      if(x$W[i] > j){
        m[i,j] <- m[i-1,j]
      }
      else{
        m[i,j] <- max(m[i-1, j], m[i-1, j-w[i]] + v[i])
      }
    }
  }
  return(m[n+1, W+1])
}