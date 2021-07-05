# get fcc data from socrata
# renata gerecke
# 2021-06-26

library(tidyverse)
library(httr)
library(jsonlite)
library(RSocrata)
library(tictoc)

app_token <- "2sC55bQkSqnsDPTcPqIyhD4Eb"
paths <- c(
    jun20 = "https://opendata.fcc.gov/resource/ktav-pdj7.json",
    dec19 = "https://opendata.fcc.gov/resource/ws2a-amik.json",
    jun19 = "https://opendata.fcc.gov/resource/udcw-naqn.json",
    dec18 = "https://opendata.fcc.gov/resource/wucg-w9k9.json",
    jun18 = "https://opendata.fcc.gov/resource/cekp-f8tj.json",
    dec17 = "https://opendata.fcc.gov/resource/fnid-qg8r.json",
    jun17 = "https://opendata.fcc.gov/resource/yikn-7er3.json",
    dec16 = "https://opendata.fcc.gov/resource/xv2f-wqqz.json",
    jun16 = "https://opendata.fcc.gov/resource/nb5q-gkcn.json"
)

tic()
dfs <- map_dfr(
    paths,
    ~read.socrata(str_c(.x, "?type=county&tech=acfosw"),
                  app_token = app_token),
    .id = "date"
)
toc()

write_rds(dfs, "data/fcc/data_export.rds")
