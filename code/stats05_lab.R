pacman::p_load(tidyverse,
               patchwork,
               here)

# anova practice with plant growth 

## draw a figure 
df_pg <- as_tibble(PlantGrowth)

df_pg %>%
  ggplot(aes(x = group, 
             y = weight)) +
  geom_violin(draw_quantiles = 0.5, 
              alpha = 0.8) +
  geom_jitter(width = 0.1)

## perform anova
m <- aov(weight ~ group,
    data = df_pg)

summary(m)

## pwr package
#install.packages("pwr")

pwr::pwr.anova.test(k = 3, 
                    f = 0.5,
                    sig.level = 0.05,
                    power = 0.8)


## change k, f, power, respectively - one at a time
## see how these changes affect the number of samples you may need 

##increase in k 
pwr::pwr.anova.test(k = 5, 
                    f = 0.5,
                    sig.level = 0.05,
                    power = 0.8)

## decrease in f
pwr::pwr.anova.test(k = 3, 
                    f = 0.3,
                    sig.level = 0.05,
                    power = 0.8)

## increase in power 
pwr::pwr.anova.test(k = 3, 
                    f = 0.5,
                    sig.level = 0.05,
                    power = 0.9)

## - leave power blank 
pwr::pwr.anova.test(k = 3,
                    n = 5,
                    f = 0.5, 
                    sig.level = 0.05)

## - different levels of k, n, f

## decrease in k 
pwr::pwr.anova.test(k = 2,
                    n = 5,
                    f = 0.5, 
                    sig.level = 0.05)

## increase in n 
pwr::pwr.anova.test(k = 3,
                    n = 7,
                    f = 0.5, 
                    sig.level = 0.05)

## increase in f 
pwr::pwr.anova.test(k = 3,
                    n = 5,
                    f = 0.8, 
                    sig.level = 0.05)
