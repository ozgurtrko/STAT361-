n <- 1000
p <- 0.25
u <- runif(n)
k <- ceiling(log(1-u)/log(1-p)) - 1
k

k <- floor(log(u)/log(1-p))
k

f <- function(n, p){
  u <- runif(n)
  k <- floor(log(u)/log(1-p)) 
  return(k)
}

a <- f(100000, 0.25)
mean(a)
var(a)

## teorik ortalama ve varyans
mt <- (1-0.25)/0.25
mt

vt <- (0.75/0.25^2)
vt
## teorik varyans ve ortalama ile yazdığım fonksiyonun varyans ve ortalaması birbirine yakın olmalı

