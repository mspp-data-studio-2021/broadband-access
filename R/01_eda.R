# renata gerecke
# 24 june 2021

library(tibble)
library(readr)
library(dplyr)
library(tidyr)
library(skimr)
library(ggplot2)
library(stringr)
library(usmap)


# read data ----------

df_county <- read_csv("data/fcc/Area_Table_June_2020_County.csv")

df_ms <- read_csv("data/microsoft/broadband_data_2020October.csv", skip = 18, na = "-")

# chart county data, all techs, speed = 25+, has at least 1 ------------

df_has1plus <- filter(df_county, tech == "acfosw") %>%
    group_by(id, speed, tech) %>%
    summarise(across(where(is.numeric), sum),
              .groups = "drop") %>%
    rowwise() %>%
    mutate(has_1more = sum(c_across(has_1:has_3more))) %>%
    ungroup() %>%
    mutate(pct_has = has_1more / (has_1more + has_0)) %>%
    filter(complete.cases(.)) %>%
    group_split(speed)

qplot(data = df_has1plus, x = pct_has, geom = "histogram", bins = 100, fill = speed)

plot_usmap(data = rename(df_has1plus[[6]], fips = id), values = "pct_has")
