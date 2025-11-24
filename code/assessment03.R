# Submitting this assessment by the due date is worth 50 points.
# Each question is worth 5 points.
# Call suitable packages as needed.

library(tidyverse)

# CO2 dataset -------------------------------------------------------------

## DATA DESCRIPTION #######################################################

# The CO2 dataset in R records how grass plants from two origins, Quebec and
# Mississippi, respond to varying CO₂ concentrations and temperature treatments.

head(CO2)

# Each plant is identified by the `Plant` factor, with `Type` indicating its origin
# and `Treatment` showing whether it was chilled or nonchilled. `conc` gives the
# ambient CO₂ concentration (mL/L), and `uptake` measures the rate of CO₂
# assimilation (µmol/m²/sec). 

## DATA DESCRIPTION END ###################################################
# Q1
# CO2 dataframe is a base dataframe. Convert this to a class `tibble`
# then assign to `df_co2`

df_co2 <- as_tibble(CO2)

# Q2
# Convert column names to lowercase and reassign to `df_co2`

df_co2 <- df_co2 %>%
  rename_with(tolower)

view(df_co2)

# Q3
# Create scatter plots of CO₂ uptake versus ambient CO₂ concentration using `df_co2`.
# - The x-axis should represent ambient CO₂ concentration
# - The y-axis should represent CO₂ assimilation rate
# - Color the points by treatment.
# - Create separate panels for each plant type (Quebec vs Mississippi) and combine the plots.

ggplot(df_co2,
       aes(x = conc,
           y = uptake,
           color = treatment)) +
  geom_point() +
  facet_wrap(~ type) +
  labs(x = "Ambient CO2 Concentration",
       y = "CO2 Assimilation Rate",
       color = "Treatment") +
  theme_minimal()


# Q4
# The df_co2 dataset contains the following variables:
# - CO₂ assimilation rate
# - ambient CO₂ concentration
# - treatment: chilled vs nonchilled
# - type: plant origin (Quebec vs Mississippi)
# 
# Develop suitable statistical models to examine:
#   
# - The main effect of ambient CO₂ concentration (conc)
# - The main effect of treatment
# - The interaction between concentration and treatment
# 
# Fit these models separately for each plant origin.

df_quebec <- df_co2 %>%
  filter(type == "Quebec")

df_mississippi <- df_co2 %>%
  filter(type == "Mississippi")

mod_quebec <- lm(uptake ~ conc * treatment, data = df_quebec)
mod_mississippi <- lm(uptake ~ conc * treatment, data = df_mississippi)

summary(mod_quebec)
summary(mod_mississippi)

# Q5
# Based on the models fitted in Q4 for Quebec and Mississippi plants,
# describe how CO2 assimilation rate responded to ambient CO2
# concentration under different treatments (chilled vs non-chilled) 
# for each plant origin. Highlight the differences between Quebec and 
# Mississippi plants, and use the model results to support your answers.

# ENTER YOUR ANSWER HERE as COMMENT:
# (no coding required for this question)

## Answer: CO2 assimilation rate generally increases with ambient 
## CO2 concentration for both Quebec and Mississippi plants. The
## coefficient for both concentrations are positive, indicating 
## higher ambient CO2 is associated with higher uptake, on average.
## Treatment changes the level of uptake at a given CO2 conc.
## For Quebec plants, nonchilled plants tend to have a higher 
## uptake than chilled plants across most CO2 conc., but the 
## treatment effect and the interaction between the conc & treatment
## are relatively moderate. This suggests that chilling slightly
## reduces overall uptake in Quebec plants, but their response to 
## increasing CO2 is relatively similar between treatments. 
## For Mississippi plants, the treatment effect is stronger.
## There is lower uptake for chilled plants compared to nonchilled
## plants at the same conc., and the conc. & treatment interaction 
## indicates that chilled Mississippi plants gain less uptake
## per unit increase of CO2 than nonchilled plants. 
## When comparing the two, both Quebec and Mississippi plants show
## increasing uptake as CO2 conc. rises, but Quebec plants reach
## the overall highest uptake values. Chilling has a small effect
## on Quebec plants, but a larger negative effect on Mississippi plants.
## Quebec plants are more tolerant to chilling than Mississippi plants. 


# BCI data ----------------------------------------------------------------

## DATA DESCRIPTION #######################################################

# BCI dataset:
# run the following code to get data.
if(!require(vegan)) install.packages("vegan")
library(vegan)
data("BCI")
data("BCI.env")

# BCI dataset:
# The BCI dataset contains tree species abundance data from 50 1-hectare plots 
# on Barro Colorado Island (Panama). Each row represents a plot, and each 
# column represents a tree species. Entries are counts of individuals of each 
# species in that plot. This dataset is often used to study species richness, 
# community composition, and diversity patterns in tropical forests.

print(BCI)

# The following code transforms the BCI dataset from wide to long format. 
# Originally, each plot was a row and each species a column. 
# After the transformation, each row represents a single species in a 
# single plot, with columns indicating the plot (plot), species, and 
# the corresponding count.

cnm <- colnames(BCI)

df_bci <- BCI %>% 
  mutate(plot = paste0("p", str_pad(row_number(),
                                    width = 2, 
                                    pad = 0))) %>% 
  pivot_longer(cols = cnm[1]:cnm[length(cnm)], 
               names_to = "species", 
               values_to = "count")

# BCI.env dataset:
# This dataset contains environmental variables for the 50 plots in the BCI dataset.
# Key columns include:
# - UTM.EW, UTM.NS: spatial coordinates of each plot
# - Precipitation: mean annual rainfall (mm)
# - Elevation: plot elevation (m)
# - Age.cat: categorical forest age class
# - Geology: underlying geological formation type
# - Habitat: dominant habitat type in the plot
# - Stream: indicates presence of streamside (riparian) habitat
# - EnvHet: environmental heterogeneity (Simpson diversity of habitat subcells)
# These variables help explain variation in species composition and abundance 
# across plots and allow exploration of species–environment relationships.

print(BCI.env)

# The following code adds a new "plot" column.

df_env <- BCI.env %>% 
  mutate(plot = paste0("p", str_pad(row_number(),
                                    width = 2, 
                                    pad = 0))) %>% 
  relocate(plot)

## DATA DESCRIPTION END ####################################################

# Q6
# Convert column names of `df_env` to lowercase and reassign to `df_env`

df_env <- df_env %>%
  rename_with(tolower) 

# Q7
# In `df_env`, some environmental variables have no variation between plots
# (i.e., the same value for all plots). Identify these columns and remove them
# from the dataframe. Assign the resulting dataframe to `df_env_sub`.

constant_cols <- df_env %>%
  summarize(across(everything(), ~ dplyr::n_distinct(.x))) %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "n_levels") %>%
  filter(n_levels == 1) %>%
  pull(variable)

df_env_sub <- df_env %>%
  select(-all_of(constant_cols))


# Q8
# Calculate summary statistics for each plot using `df_bci`.
# For each plot, compute:
# - n_sum: total count of all individuals across species
# - n1: count of the most dominant species (maximum count among species)
# - p: proportion of the most abundant species (n1 / n_sum)
# Assign the resulting dataframe to `df_n`.

df_n <- df_bci %>%
  group_by(plot) %>%
  summarize(n_sum = sum(count),
            n1 = max(count),
            p = n1 / n_sum,
            .groups = "drop")

# Q9
# Combine the summary data (`df_n`) with the environmental variables
# (`df_env_sub`) for each plot. Assign the resulting dataframe to `df_m`.

df_m <- df_n %>%
  left_join(df_env_sub, by = "plot")

# Q10
# Develop a statistical model to explain variation in the proportion of the dominant
# species in each plot. Use `EnvHet`, `Stream`, and `Habitat` as predictors.
# Fit a suitable statistical model to the data.
# Use model selection based on predictability (i.e., out-of-sample prediction) 
# rather than the goodness of fit, and report which variables are included in 
# the best predictive model as a comment.

set.seed(123)

df_m_mod <- df_m %>%
  drop_na(p, envhet, stream, habitat) %>%
  mutate(stream = factor(stream),
         habitat = factor(habitat))

m1 <- lm(p ~ envhet, data = df_m_mod)
m2 <- lm(p ~ envhet + stream, data = df_m_mod)
m3 <- lm(p ~ envhet + habitat, data = df_m_mod)
m4 <- lm(p ~ envhet + stream + habitat, data = df_m_mod)

AIC(m1, m2, m3, m4)

## Answer: The model with the lowest AIC was m4. This indicates that
## model 4, including the variables envhet, stream, and habitat
## is the best model. Therefore, the variation in the proportion of the dominant 
## species among plots is best explained by AIC model 4. 