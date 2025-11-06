pacman::p_load(tidyverse,
               patchwork,
               here)


# glm exercise  -----------------------------------------------------------

# fish data:
## - response variable: n_sp
## - predictors: distance, cat_area, hull_area

url <- "https://raw.githubusercontent.com/aterui/public-proj_fish-richness-shubuto/master/data/data_vpart.csv"
df_fish <- read_csv(url)

m_fish <- glm(n_sp ~ distance + cat_area + hull_area,
    data = df_fish,
    family = "poisson")

summary(m_fish)

## mtcars data
## - response variable: am (automatic vs. manual)
## - predictors: mpg, hp, wt

### with appropriate distribution 

m_am <- glm(cbind(am, 1 - am) ~ mpg + hp + wt,
            data = mtcars,
            family = "binomial")

m_am <- glm(am ~ mpg + hp + wt,
            data = mtcars,
            family = "binomial")

summary(m_am)

plot(am ~ wt,
     mtcars)

### this will get different results 

m_am_gau <- glm(am ~ mpg + hp + wt,
    data = mtcars,
    family = "gaussian") %>%
  summary()

# effect size  ------------------------------------------------------------

summary(m_fish)

df_fish <- df_fish %>%
  mutate(std_dist = scale(distance), 
         std_cat = scale(cat_area),
         std_hull = scale(hull_area))

m_fish_std <- glm(n_sp ~ std_dist + std_cat + std_hull, 
    data = df_fish,
    family = "poisson")

#### compare coefs
coef(m_fish)
coef(m_fish_std)


# offset ------------------------------------------------------------------

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_offset.csv"
df_offset <- read_csv(url)

## visualization
g1 <- df_offset %>%
  ggplot(aes(x = nitrate,
          y = count)) +
  geom_point()

g2 <- df_offset %>%
  ggplot(aes(x = area,
             y = count)) +
  geom_point()

g3 <- df_offset %>%
  mutate(density = count/area) %>%
  ggplot(aes(x = nitrate,
             y = density)) +
  geom_point()
  
g1 + g2 + g3

## gaussian trial 

df_offset <- df_offset %>% 
  mutate(density = count / area)

glm(density ~ nitrate,
    data = df_offset,
    family = "poisson")

## use offset term with Poisson distribution 

m_count_wo_offset <- glm(count ~ nitrate,
              data = df_offset,
              family = "poisson")
summary(m_count_wo_offset)

m_count_offset <- glm(count ~ nitrate + offset(log(area)),
                data = df_offset,
                family = "poisson")
summary(m_count_offset)


# overdispersion ----------------------------------------------------------

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_tadpole.csv"
df_tadpole <- read_csv(url)

g_v <- df_tadpole %>%
  ggplot(aes(x = aqveg,
             y = tadpole)) +
  geom_point()

g_p <- df_tadpole %>%
  ggplot(aes(x = permanence,
             y = tadpole)) +
  geom_point()

g_v + g_p

m_tad <- glm(tadpole ~ aqveg + permanence,
    data = df_tadpole,
    family = "poisson")

mean(df_tadpole$tadpole)
var(df_tadpole$tadpole)

## negative binomial regression

MASS::glm.nb(tadpole ~ aqveg + permanence,
             data = df_tadpole)





