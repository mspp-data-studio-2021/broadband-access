# animate graphs
# renata gerecke
# 2021-07-14

# libraries -----------
library(tidyverse)
library(sf)
library(gganimate)
library(scales)

# data ----------------

df_clean <- read_rds("../data/fcc/data_cleaned.rds")
us_county <- st_read("../data/tl_2019_us_county/tl_2019_us_county.shp",
                     quiet = TRUE)

# munge ---------------

us_simp <- st_simplify(us_county, dTolerance = 500) %>%
    mutate(state = parse_number(STATEFP)) %>%
    filter(state < 60, state != 2, state != 15)


# animate ------------

test <- filter(df_clean, speed == 25, date %in% c("jun16", "jun20")) %>%
    inner_join(us_simp, by = c("fips" = "GEOID")) %>%
    st_as_sf()

gga_25 <- ggplot(test) +
    aes(fill = pct_2more) +
    geom_sf(color = "transparent") +
    scale_fill_viridis_c(label = scales::percent) +
    labs(title = "Percent Eligible for 2+ ISPs at 25mbps",
         subtitle = "{closest_state}",
         fill = NULL,
         x = NULL,
         y = NULL,
         caption = "Source: FCC Form 477 Aggregate Area Tables") +
    transition_states(date, transition_length = 10, state_length = 5) +
    theme_classic() +
    theme(
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
    )
