pacman::p_load(tidyverse,
               patchwork,
               janitor,
               stringdist,
               here)

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_messy.csv"
df_messy <- read_csv(url)

view(df_messy)


# check data entries before you analyze data ------------------------------

## check class of each column
## sapply and class


sapply(df_messy, 
       FUN = class)


sapply(df_messy, 
       FUN = unique)

collector <- unique(df_messy$collector)

stringdistmatrix(collector)

a <- c(" a ", "a  b", "b ", "a", "b")

str_squish(a)

b <- c("A", "a", "bB", "BB")

str_to_lower(b)
str_to_upper(b)

v <- c("a b", "a.b", "a b.c")

str_replace(v, "\\s", "_") %>%
  str_replace("\\.", "_")

str_replace(v, "\\s|\\.", "_")


# remove text
x <- c("abc", "dd", "abd")
str_remove_all(x, "ab")

z <- c("abc", "dd", "abd")
str_extract(z, "ab")


str_detect(z, "a")
ifelse(str_detect(z, "a"),
       yes = 1,
       no = 0)

y <- c("ab", "Ab")
str_detect(y, "^[Aa]")


# cleanup data ------------------------------------------------------------

d <- c("2024/06/01", "June 4, 2024", "2024.06.07")

lubridate::parse_date_time(d, 
                           tz = "EST",
                           orders = c("Y/m/d",
                                      "B d Y",
                                      "Y.m.d"))

df_messy %>% 
    mutate(collector = str_to_lower(collector),
           species = str_squish(species) %>%
             str_to_lower() %>%
             str_replace_all("\\s|\\.", "_") %>%
             str_remove("_$"),
           length_mm = str_replace(length_mm,
                                  ",",
                                  ".") %>%
             str_remove("\\smm") %>%
             as.numeric(),
           sample_date = parse_date_time(sample_date,
                                         tz = "EST",
                                         order = c("Y/m/d",
                                                   "B d Y",
                                                  "d B Y",
                                                  "Y.m.d")) %>%
  as.Date(),
recaptured = ifelse(str_detect(recaptured, "^[Yy]"),
                    yes = 1,
                    no = 0))















