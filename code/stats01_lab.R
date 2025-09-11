#7.3.1

z <- exp(rnorm(n = 1000, mean = 0, sd = 0.1))

mean(z)
median(z)
prod(z)^ (1 / length(z))

library(tidyverse)
df_z <- tibble(z)

z_hist <- df_z %>% 
  ggplot(aes(x = z)) +
  geom_histogram() 
  

g1 <- z_hist+geom_vline(xintercept = median (z),
                  color = "green") +
  geom_vline(xintercept = mean(z),
             color = "salmon") +
  geom_vline(xintercept = prod(z)^ (1 / length(z)),
             color = "blue")
  
z_rev <- -z + max(z) + 0.1
mean(z_rev)
median(z_rev)
prod(z_rev)^ (1 / length(z_rev))

library(tidyverse)
df_zrev <- tibble(z_rev)

zrev_hist <- df_zrev %>% 
  ggplot(aes(x = z_rev)) +
  geom_histogram() 
  
  
g2 <- zrev_hist+geom_vline(xintercept = median (z_rev),
                       color = "green") +
  geom_vline(xintercept = mean(z_rev),
             color = "pink") +
  geom_vline(xintercept = prod(z_rev)^ (1 / length(z_rev)),
            color = "blue" )

#7.3.2
  
w <- rnorm(100, mean = 10, sd = 1)
head(w) # show first 10 elements in w

m <- 1000 * w #unit: milligram 
s2_w <- sum((w - mean(w))^2) / length (w)
s_w <- sqrt(s2_w)

s2_m <- sum((m - mean(m))^2) / length (m)
s_m <- sqrt(s2_m)

mad_w <- median(abs(w - median(w)))
mad_m <- median(abs(m - median(m)))

cv_w <- s_w / mean(w) 
cv_m <- s_m / mean(m) 

madr_w <- mad_w / median(w)
madr_m <- mad_m / median(m)


