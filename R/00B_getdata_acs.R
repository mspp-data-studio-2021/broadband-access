# renata gerecke
# 2021-07-01
# pull acs data using tidycensus

# libraries ------------

library(tidyverse)
library(tidycensus)


# variables of interest ------

acs_vars <- map_dfr(2016:2019,
                    ~ load_variables(.x,
                                     "acs1",
                                     cache = TRUE),
                    .id = "year")

acs_vars_5 <- load_variables(2019, "acs5", cache = TRUE)

acs_keep <- filter(acs_vars, year == 1) %>%
    filter(
        str_detect(name, "B03002") | # race
            str_detect(name, "B01001_") | # sex & age
            str_detect(name, "B28001_") | # type of computer
            str_detect(name, "B28003_") | # type of computer x internet
            str_detect(name, "B28002_") | # type of internet
            name == "B05010_002" | # income below poverty line
            name == "B05010_010" | # income above & under double poverty line
            name == "B05010_018"  # income above double poverty line
    ) %>%
    pull(name)

# confirm variables are the same across years -----

filter(acs_vars, name %in% acs_keep) %>%
    pivot_wider(names_from = "name",
                values_from = "label",
                id_cols = "year") %>%
    View()

filter(acs_vars_5, name %in% acs_keep) %>%
    View()

# get data ------

acs_1yr <- map_dfr(
    2016:2019,
    ~ get_acs(
        geography = "county",
        variables = acs_keep,
        year = .x,
        survey = "acs1"
    ),
    .id = "year"
)

write_rds(acs_1yr, "data/acs/acs_1yr.rds")

acs_5yr <- get_acs(
    geography = "county",
    variables = acs_keep,
    year = 2019,
    survey = "acs5"
)

write_rds(acs_5yr, "data/acs/acs_5yr.rds")
