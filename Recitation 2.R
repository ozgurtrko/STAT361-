## Q1.
abs_val <- function(n){
  if(n < 0){
    n <- abs(n)
  }
  return(n)
}

## Q2
# A.
?round
round(37.6532, digits = 1)

# B.
format_temperature <- function(temps, decimal){
  temps <- round(temps, decimal)
  return(temps)
}

## Q3
# A.
signif(0.00012345, 3)

# B.
adjust_precision <- function(num, desire){
  desired <- signif(num, desire)
  return(desired)
}
adjust_precision(0.00012345, 2)

## Q4
# A.

response <- ("Great service; friendly staff; clean environment; will visit again")

ind_com <- strsplit(response, split = ";")

# C

split_feedbacks <- function(feedback, char){
  splited <- lapply(feedback, function(f1){
    strsplit(f1,char)
  })
  return(splited)
}
split_feedbacks(response, ";")



## Q5.
ID <- "PR123US1"
category <- substr(ID, 1,2)
prod_ID <- substr(ID, 3,5)
region_code <- substr(ID, 6, 8)

decode_product_info <- function(code){
  category <- substr(code, 1, 2)
  prod_ID <- substr(code, 3,5)
  region_code <- substr(code, 6, 8)
  return(c(category, prod_ID, region_code))
}

decode_product_info(ID)

## Q6.

satisfaction_score <- c(8, 7, 9, 6, 7, 8, 9, 5, 6, 7, 8, 9, 7, 6, 8, 9, 5, 7, 8, 6)

# A.
mean(satisfaction_score)

# B.
mad(satisfaction_score)

# C.
quantile(satisfaction_score)


## Q7.

temp <- c(25, 27, 28, 26, 29, 30, 31)

# A.
mean(temp)

calculate_weekly_mean <- function(temps){
  ort <- mean(temps)
  return(ort)
}

calculate_weekly_mean(temp)

## Q8. 

cortisol_levels <- c(-0.5, -0.1, 0, -0.2, -0.4, 0.1, -0.3, 0, -0.2, -0.1)

mad(cortisol_levels)

# B.
calculate_mad <- function(cortisol){
  cort_level <- mad(cortisol)
  return(cort_level)
}

calculate_mad(cortisol_levels)


## Q9.

yield <- c(2.1, 2.5, 2.3, 2.7, 2.2, 2.6, 2.4, 2.8, 3.0, 2.9, 3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7, 3.8, 3.9, 4.0)

quantile(yield)

calculate_quantiles <- function(y){
  quantiles <- quantile(y)
  return(quantiles)
}

calculate_quantiles(yield)

## Q10.
sqft_living <- c(650,800,850,900,1000)
price <- c(300000,350000,355000,365000,400000)
data <- data.frame(sqft_living,price)
data

calculate_mean <- function(x){
  ort <- mean(x)
  return(ort)
}

calculate_mean(sqft_living)

# B.

calculate_beta_1 <- function(x, y){
  x_mean <- mean(x)
  y_mean <- mean(y)
  
  numerator <- sum((x - x_mean)*(y - y_mean))
  dominator <- sum((x - x_mean)^2)
  
  return(numerator / dominator)
}

calculate_beta_0 <- function(x, y){
  x_mean <- mean(x)
  y_mean <- mean(y)
  
  beta_1_hat <- calculate_beta_1(x, y)
  beta_0_hat <- y_mean - beta_1_hat * x_mean
  return(beta_0_hat)
  
}

# C.

beta_0 <- calculate_beta_0(sqft_living, price)

beta_1 <- calculate_beta_1(sqft_living, price)

x <- 950
predict_price <- beta_0 + beta_1 * x
predict_price

# E.

age <- c(5,3,10,8,2)
data2 <- data.frame(sqft_living,age,price)
data2

calculate_multiple_betas <- function(size, age, price){
  n <- length(size)
  x <- cbind(rep(1, n), size, age)
  y <- age
  
  
  betas <- solve(t(x) %*% x) %*% t(x) %*% y
  
  return(betas)
}

betas <- calculate_multiple_betas(sqft_living, age, price)
betas


## Q11.

scores <- c(8, 7, 9, 7, 8, 7, 9, 6, 7, 8, 9, 7, 6, 8, 9, 7, 8, 9, 5, 7, 8, 9, 7, 6, 8, 9, 7, 8, 6, 7)
scores

#A

mu_0 <- 7
h1 <- "greater"

t_test_result <- t.test(scores, mu = mu_0, alternative = h1)
t_test_result

# B.
calculate_t_stat <- function(sample_data, hyp_mean){
  n <- length(sample_data)
  sample_mean <- mean(sample_data)
  sample_std <- sd(sample_data)
  
  t_stat <- (sample_mean - hyp_mean) / (sample_std / sqrt(n))
  return(t_stat)
}

calculate_t_stat(scores, 7)
alpha <- 0.05
df <- length(scores)-1
critical_value <- qt(1-alpha, df)

if(calculate_t_stat(scores, 7) > critical_value){
  print("Reject null hypothesis: Results are statistically significant")
}else{
  print("Do not reject null hypothesis: Results are not significant")
}