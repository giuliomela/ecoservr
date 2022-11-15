## code to prepare `DATASET` dataset goes here

library(tidyverse)
library(rdbnomics)
library(lubridate)
library(rsdmx)

ref_yr <- 2021

h <- 3

# loading NUTS2 codes to download

nuts2_codes <- readxl::read_xlsx(here::here("data-raw/data_raw.xlsx"),
                                 sheet = "nuts2_codes")

# loading master table

master_table <- readxl::read_xlsx(here::here("data-raw/data_raw.xlsx"),
                                  sheet = "master_table")

usethis::use_data(nuts2_codes, master_table, overwrite = TRUE, internal = TRUE)
