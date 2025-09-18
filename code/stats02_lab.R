library(tidyverse)
library(patchwork)


#read data 
df_h <- read_csv(here::here("data_raw/data_plant_height.csv"))

# true mean and variance
# true variance is divided by N because this is not an estimate, the truth
mu <- mean(df_h$height)
sigma2 <- sum((df_h$height - mu)^2) / nrow(df_h)

#8.3 Question 1
#use var() to estimate unbiased variance
#create 100 datasets with 100 plant measures
#create 100 datasets with 50 plant measures
#use var() to estimate unbiased variance 

mu100_i <- var100_i <- NULL

for(i in 1:100) {
  df_i <- df_h %>%
    sample_n(size = 100)
  
  (mu100_i[i] <- mean(df_i$height))
  (var100_i[i] <- var(df_i$height))
}

mu50_i <- var50_i <- NULL

for(i in 1:100) {
  df_i <- df_h %>%
    sample_n(size = 50)
  
  (mu50_i[i] <- mean(df_i$height))
  (var50_i[i] <- var(df_i$height))
}

## create tibbles
df_est <- tibble(mu100 = mu100_i,
       var100 = var100_i,
       mu50 = mu50_i,
       var50 = var50_i)
#histogram
##mean 
g100_mu <- df_est %>%
  ggplot(aes(x = mu100)) +
  geom_histogram()+
  geom_vline(xintercept = mu) +
  scale_x_continuous(limits = c(18, 22))

g50_mu <- df_est %>%
ggplot(aes(x = mu50)) +
  geom_histogram()+
  geom_vline(xintercept = mu)+
  scale_x_continuous(limits = c(18, 22))

g100_mu / g50_mu

##variance
g100_var <- df_est %>%
  ggplot(aes(x = var100)) +
  geom_histogram()+
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits = c(10,40))

g50_var <- df_est %>%
  ggplot(aes(x = var50)) +
  geom_histogram()+
  geom_vline(xintercept = sigma2)+
  scale_x_continuous(limits = c(10,40))

g100_var / g50_var

