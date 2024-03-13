## The Acceptance - Rejection Method


betagen <- function(n, a, b){
  #init
  k<-0
  j<-0
  #acceptance rate
  y<-numeric(n)
  while (k < n) {
    u <- runif(1)
    j <- j + 1
    x <- runif(1)
    if(x *4* (1 - x) > u){
      k <- k+1
      y[k] <- x
    }
    
  }
  print(n/j) #acceptance rate
  return(y)
}
n <- 10000
z <- betagen(n,2,2) 
z  
  
p <- seq(0.1,0.9,0.1)
qhat <- quantile(z, p) #sample quantiles
q <- qbeta(p,2,2)  #theorichal quantiles
se <- sqrt(p*(1-p)/n*dbeta(q,2,2)) #standard error
round(rbind(qhat, q, se),3)

?qqplot
qqplot(q,z)
plot(q,qhat)

abline(0,1)










