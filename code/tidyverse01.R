install.packages("tidyverse")
library(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)

#single match ==
filter(iris_sub, Species == "virginica")

# multiple match %in%
filter(iris_sub, Species %in% c("virginica", "versicolor"))

#except != 
filter(iris_sub, Species != "virginica")

#except multiple ! and %in%
filter(iris_sub, !(Species %in% c("virginica", "versicolor")))

#greater than >
filter(iris_sub, Sepal.Length > 5)

#greater than or equal to >=
filter(iris_sub, Sepal.Length >= 5)

#less than <
filter(iris_sub, Sepal.Length < 5)

#less than or equal to <=
filter(iris_sub, Sepal.Length <= 5)

# Sepal.Length is less than 5 AND Species equals "setosa"
filter(iris_sub,
       Sepal.Length < 5 & Species == "setosa")

# same; "," works like "&"
filter(iris_sub,
       Sepal.Length < 5, Species == "setosa")

# Either Sepal.Length is less than 5 OR Species equals "setosa"
filter(iris_sub,
       Sepal.Length < 5 | Species == "setosa")

#ascending order
arrange(iris_sub, Sepal.Length)

#descending order
arrange(iris_sub, desc(Sepal.Length))

#select one column
select(iris_sub, Sepal.Length)

#select multiple columns 
select(iris_sub, c(Sepal.Length, Sepal.Width))

#remove one column 
select(iris_sub, -Sepal.Length)

#remove multiple columns 
select(iris_sub, -c(Sepal.Length, Sepal.Width))

# select columns starting with "Sepal"
select(iris_sub, starts_with("Sepal"))

# remove columns starting with "Sepal"
select(iris_sub, -starts_with("Sepal"))

# select columns ending with "Sepal"
select(iris_sub, ends_with("Width"))

# remove columns ending with "Sepal"
select(iris_sub, -ends_with("Width"))

#add a new column
# nrow() returns the number of rows of the dataframe
(x_max <- nrow(iris_sub))
# create a vector from 1 to x_max
x <- 1:x_max

# add as a new column
# named `x` as `row_id` when added
mutate(iris_sub, row_id = x)

#modifying an existing column
# twice `Sepal.Length` and add as a new column
mutate(iris_sub, sl_two_times = 2 * Sepal.Length)

# piping ------------------------------------------------------------------

df_sl <- select(iris_sub, Sepal.Length)
df_sl_2times <- mutate(df_sl, 2 * Sepal.Length)
#if df_sl_2times is what you need, and not the intermediate function, you can use pipe

df_tw <- iris_sub %>%
  select(Sepal.Length) %>%
  mutate(twice = 2 * Sepal.Length)




