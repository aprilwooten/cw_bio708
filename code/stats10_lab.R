pacman::p_load(tidyverse,
               patchwork,
               janitor,
               palmerpenguins,
               here)


# data manipulation -------------------------------------------------------

colnames(penguins_raw)

## clean column names 
## - use clean_names()

?clean_names()

penguins_clean <- clean_names(penguins_raw)
colnames(penguins_clean)

## change input in clutch_completion
## use ifelse()
## combine with mutate()
unique(penguins_clean$clutch_completion)

?ifelse()

penguins_clean <- penguins_clean %>%
  mutate(clutch_completion = ifelse(clutch_completion == "Yes",
                                   yes = 1, 
                                    no = 0))
unique(penguins_clean$clutch_completion)

## change species name input
## use case_when() to change input 
## combine with mutate()
unique(penguins_clean$species)

penguins_clean %>%
  mutate(species = case_when(species == "Adelie Penguin (Pygoscelis adeliae)"  ~ "adelie",
                             species == "Gentoo penguin (Pygoscelis papua)"  ~ "gentoo",
                             species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "chinstrap"))
unique(penguins_clean$species)

view(penguins_clean)

penguins_clean %>%
  drop_na(culmen_length_mm,
          culmen_depth_mm,
          flipper_length_mm,
          body_mass_g,
          sex) 


# model selection ---------------------------------------------------------

## develop a full model first 
m_full <- glm(clutch_completion ~ species +
      culmen_length_mm +
      culmen_depth_mm +
      flipper_length_mm +
      body_mass_g + 
      sex,
    data = penguins_clean,
    family = "binomial")

## model selection with dredge()
library(MuMIn)
options(na.action = "na.fail")
m_set <- dredge(m_full, rank = "AIC")
subset(m_set, delta < 2)





  





