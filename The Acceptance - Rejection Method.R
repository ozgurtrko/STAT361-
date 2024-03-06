## The Acceptance - Rejection Method

n <- 1000
k <- 0
j <- 0
y <- numeric(n)

f1 <- while (k < n) {
  u <- runif(1)
  j <- j + 1
  x <- runif(1)
  if(x * (1 - x) > u){
    k <- k+1
    y[k] <- x
  }
  
}

f1 <- function(k , n){
  while (k < n) {
    u <- runif(1)
    j <- j + 1
    x <- runif(1)
    if(x * (1 - x) > u){
      k <- k+1
      y[k] <- x
    }
    
  }
}

