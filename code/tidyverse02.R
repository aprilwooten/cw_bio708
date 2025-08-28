library(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)

# Group Operation ---------------------------------------------------------

## combine group_by() with summarize()
df_m_sd <- iris_sub %>%
    group_by(Species) %>%
    summarize(mean_sl = mean(Sepal.Length),
    sd = sd(Sepal.Length))

# when you summarize, original data is gone. to keep original
#data, use the mutate function 

## combine group_by() with mutate()
df_eps <- iris_sub %>%
    group_by(Species) %>%
    mutate(mean_sl = mean(Sepal.Length)) %>%
    ungroup() %>%
    mutate(eps = abs(Sepal.Length - mean_sl))


# Reshape -----------------------------------------------------------------

##reshape to wide format 
iris_w <- iris_sub %>% 
  mutate(id = rep(1:3, 3)) %>% # add an ID column
  select(id, Sepal.Length, Species) %>%
  pivot_wider(id_cols = "id", # unique row ID based on
              values_from = "Sepal.Length", # values in each cell from
              names_from = "Species") # new column names from

## reshape to long format
iris_l <- iris_w %>%
    pivot_longer(cols = c("setosa",
                          "versicolor",
                          "virginica"),
                 names_to = "Species", 
                 values_to = "Sepal.Length")



# Join --------------------------------------------------------------------

# matching by a single column
## left join by "Species": one to one
(df1 <- tibble(Species = c("A", "B", "C"),
              x = c(1, 2, 3)))

(df2 <- tibble(Species = c("A", "B", "C"),
              y = c(4, 5, 6)))

(df1.2 <- left_join(x = df1,
          y = df2,
          by = "Species"))

## what happens if df2 does not contain Species B 

(df_2minus_B <- tibble(Species = c("A", "C"),
                      y = c(4,6)))

left_join(x = df1,
          y = df_2minus_B,
          by = "Species")

left_join(x = df_2minus_B,
          y = df1,
          by = "Species")


# Additional Resource -----------------------------------------------------

#library(swirl)
#install_course_github("sysilviakim", "swirl-tidy")



