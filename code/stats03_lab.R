library(tidyverse)
library(patchwork)


# normal distribution -----------------------------------------------------

#use rnorm() function to generate a vector of values 
v <- rnorm(n = 50, mean = 100, sd = 5)

#create a vector of bins
bin <- seq(floor(min(v)), #integer part of the minimum value 
    ceiling(max(v)), #round up of the maximum value 
    by = 1)

#calculate the probability in each bin with pnorm()
#this is just for bin 1, but need to be looped
pnorm(bin[2], mean = mean(v), sd = sd(v)) - pnorm(bin[1], mean = mean(v), sd = sd(v))


#looped version 
p <- NULL
for(i in 1:(length(bin)- 1)) {
p[i] <- pnorm(bin[i + 1], mean = mean(v), sd = sd(v)) - pnorm(bin[i], mean = mean(v), sd = sd(v))
}

#create a tibble to organize the produced data
df_prob <- tibble(bin = bin[-length(bin)] + 0.5,
       prob = p) %>%
  mutate(freq = length(v) * prob)

#figure
df_v <- tibble(v = v)

df_v %>%
  ggplot(aes(x = v)) +
  geom_histogram() +
  geom_point(data = df_prob,
             aes(x = bin,
                 y = freq),
             color = "darkgreen")+
  geom_line(data = df_prob,
            aes(x = bin,
                y = freq),
            color = "darkgreen")


# poisson distribution ----------------------------------------------------
# poisson distribution is for a discrete random variable
#such as number of ind, specie richness, etc.
#the realization of a poission distribution is always 0 or positive integer
#no decimal points in the output 

#get data for 1000 samples
x <- rpois(n = 1000, lambda = 10)
bin <- seq(0,
           max(x) + 5, #max +5 is just to cover a wider range of values 
           by = 1)

#calculate the probability of observation 
p <- dpois(bin, lambda = mean(x))

#data frame
df_prob <- tibble(bin = bin,
                  prob = p) %>%
  mutate(freq = length(x) * p)

df_x <- tibble(x =x)

#figure 
df_x %>%
  ggplot(aes(x = x)) +
  geom_histogram(binwidth = 0.5) +
  geom_point (data = df_prob,
              aes(y = freq,
                  x = bin),
              color = "steelblue")+
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "steelblue")

