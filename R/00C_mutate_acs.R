# Munging census data
# renata gerecke
# 2021-07-05

# setup -----------------

library(tidyverse)
library(tidycensus)

df_raw_1yr <- read_rds("data/acs/acs_1yr.rds")
df_raw_5yr <- read_rds("data/acs/acs_5yr.rds")


acs_vars <- load_variables(2019, "acs1", cache = TRUE)

acs_keep <- filter(
    acs_vars,
        str_detect(name, "B03002") | # race
            str_detect(name, "B01001_") | # sex & age
            str_detect(name, "B28001_") | # type of computer
            str_detect(name, "B28003_") | # type of computer x internet
            str_detect(name, "B28002_") | # type of internet
            name == "B05010_002" | # income below poverty line
            name == "B05010_010" | # income above & under double poverty line
            name == "B05010_018"  # income above double poverty line
    )

# munge 1 year -------------

## WARNING - many missing values for race data & income data -- best to stick
## with 5 year data since this already is restricted to high pop counties

df_mut_1yr <- df_raw_1yr %>%
    pivot_wider(
        id_cols = c(year, GEOID, NAME),
        names_from = variable,
        values_from = estimate
    ) %>%
    rowwise() %>%
    transmute(
        year = as.numeric(year) + 2015,
        fips = GEOID,
        name = NAME,
        tot_pop = B01001_001,
        tot_comp = B28001_001,
        male = B01001_002,
        race_whitenh = B03002_003,
        race_blacknh = B03002_004,
        race_asiannh = B03002_006,
        race_othernh = sum(c_across(c(B03002_005, B03002_007:B03002_009))),
        race_hispanic = B03002_012,
        age_18under = sum(c_across(c(B01001_003:B01001_006,
                                   B01001_027:B01001_030))),
        age_18to64 = sum(c_across(c(B01001_007:B01001_019,
                                  B01001_031:B01001_043))),
        age_65up = sum(c_across(c(B01001_020:B01001_025,
                                B01001_044:B01001_049))),
        comp_pc = B28001_003,
        comp_oth = tot_comp - comp_pc - B28001_011,
        int_sub = B28002_002,
        int_none = B28002_013
    ) %>%
    ungroup() %>%
    mutate(
        across(c(male:age_65up), ~ . / tot_pop),
        across(c(comp_pc:int_none), ~ . / tot_comp)
    )

write_rds(df_mut_1yr, "data/acs/acs_1yr_ed.rds")

# munge 5 year -----------------

# warning -- have 0 values for poverty level in some counties...not great!!

df_mut_5yr <- df_raw_5yr %>%
    pivot_wider(
        id_cols = c(GEOID, NAME),
        names_from = variable,
        values_from = estimate
    ) %>%
    rowwise() %>%
    transmute(
        fips = GEOID,
        name = NAME,
        tot_pop = B01001_001,
        tot_comp = B28001_001,
        male = B01001_002,
        race_whitenh = B03002_003,
        race_blacknh = B03002_004,
        race_asiannh = B03002_006,
        race_othernh = sum(c_across(c(B03002_005, B03002_007:B03002_009))),
        race_hispanic = B03002_012,
        age_18under = sum(c_across(c(B01001_003:B01001_006,
                                     B01001_027:B01001_030))),
        age_18to64 = sum(c_across(c(B01001_007:B01001_019,
                                    B01001_031:B01001_043))),
        age_65up = sum(c_across(c(B01001_020:B01001_025,
                                  B01001_044:B01001_049))),
        inc_vlow = B05010_002,
        inc_low = B05010_010,
        comp_pc = B28001_003,
        comp_oth = tot_comp - comp_pc - B28001_011,
        int_sub = B28002_002,
        int_none = B28002_013
    ) %>%
    ungroup() %>%
    mutate(
        across(c(male:inc_low), ~ . / tot_pop),
        across(c(comp_pc:int_none), ~ . / tot_comp)
    )

# confirm race calculated correctly

df_mut_5yr %>% select(starts_with("race")) %>% rowSums() %>% summary



write_rds(df_mut_5yr, "data/acs/acs_5yr_ed.rds")
