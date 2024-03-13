n <- 10000
a <- 3
b <- 2

betagen1 <- function(n,a,b){
  u <- rgamma(n, shape = a, rate = 1)
  v <- rgamma(n, shape = b, rate = 1)
  x <- u/(u+v)
  return(x)
}

z <- betagen1(n, a, b)
q<- qbeta(ppoints(n), a,b)
qqplot(z, x, cex = 0.25, xlab = "Beta(3,2)", ylab = "sample")
abline(0,1)

x
?ppoints

theta <- 0.5
log_gen <- function(n, theta){
  u <- runif(n)
  v <- runif(n)
  x <- floor((1 + log(v))/log(1-(1-theta)^u))
  k <- 1:max(x)
  p <- -1/log(1-theta)*(theta^k)/k
  se <- sqrt(p*(1-p)/n)
  phat <- tabulate(x)/n
  print(round(rbind(phat, p, se),3))
}
z <- log_gen(n, theta)
qqplot(p,z)
abline(0,1)
