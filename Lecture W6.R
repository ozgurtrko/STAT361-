## MONTE CARLO INTEGRATION
set.seed(361)

f1 <- function(m){
  # compare with estimate with exact value
  x <- runif(m) 
  theta_hat <- mean(exp(-x))
  print(theta_hat) #mc estimate
  print(1 - exp(-1)) # exact value
}
f1(10000)


# MC Integration on bounded interval

f2 <- function(a, b, n){
  ## e^x mc integration from a to b.
  x <- runif(n, max = b, min = a) 
  theta_hat <- mean(g)*(b-a) #mc estimate.
  theta <- exp(-a) - exp(-b) #exact value.
  return(list(theta_hat, theta))
  
}
theta <- f2(2,4,10000)
theta


f3 <- function(n, a, b){
  y <- runif(n)
  t <- y*(b-a)+a
  
  theta_hat <- mean(exp(-t))*(b-a)
  x <- runif(n,min=a,max=b)
  
  theta_hat2 <- mean(exp(-x))*(b-a)
  return(list(theta_hat, theta_hat2))
}
f3(10000, 2, 4)
theta <- f3(10000, 2, 4)
t1 <- theta[1]
t2 <- theta[2]

fun <- function(x){exp(-x)}
exact <- integrate(fun, 2, 4)
exact

#MC INtegration on unbounded interval

fun1 <- function(m){
  x <- seq(.1, 2.5, length = 10) 
  u <- runif(m)
  cdf <- numeric(length(x))
  for (i in 1:length(x)) {
    g <- x[i] * exp(-(u * x[i])^2 / 2) 
    cdf[i] <- mean(g) / sqrt(2 * pi) + 0.5
  }
  
  
  Phi <- pnorm(x)
  print(round(rbind(x, cdf, Phi), 3))
  
}
fun1(10000)


fun1_plus <- function(n,x){
  #x is a vector containing several x values. 
  u <- runif(n)
  cdf <- numeric(length(x))
  for (i in 1:length(x)){
    g <- x[i]*exp(-(u*x[i])^2/2)
    cdf[i] <- mean(g) / sqrt(2*pi)+0.5
  }
  return(cdf)
}
x <- seq(0.1, 2.5)
estimate <- fun1_plus(1000, x)

Phi <- pnorm(x)

print(round(rbind(Phi, estimate),2)) #compare exact val. and estiamte val.















