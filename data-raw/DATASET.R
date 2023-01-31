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

master_table_agr <- readxl::read_xlsx(here::here("data-raw/data_raw.xlsx"),
                                  sheet = "master_table_agr")

# loading corine - maes conversion table

maes_corine <- readxl::read_xlsx(here::here("data-raw/data_raw.xlsx"),
                                 sheet = "maes_corine")

# loading crop label conversion table

crop_labels <- readxl::read_excel(here::here("data-raw/data_raw.xlsx"),
                                  sheet = "crop_names_it_en")

##### Forestry data

forest_data <- readRDS(here::here("data-raw/forest_data_istat.rds"))

# istat.fl <- readSDMX(providerId = "ISTAT", resource = "dataflow") %>%
#   as.data.frame()
#
# istat.fl %>%
#   filter(str_detect(Name.it, "silvi"))
#
# forest_data <- readSDMX(providerId = "ISTAT", resource = "data",
#                          flowRef  = "93_48",
#                          # Data Structure Definition (metadati)
#                          dsd = TRUE) %>%
#   # dataframe con le etichette, importate con dsd
#   as.data.frame(labels = TRUE)

val_prod_forest <- forest_data %>%
  filter(TIPO_AGGR_MIGLEURO_label.it == "produzione" &
           BRANCA_ATTIV_REV2_label.it == "silvicoltura e utilizzo di aree forestali" &
           VAL_label.it == "prezzi correnti" &
           EDI == "2022M5") %>%
  select(label = ITTER107_label.it, year = obsTime, value = obsValue) %>%
  mutate(label = case_when(
    str_detect(label, "Bolzano") ~ "Bolzano",
    str_detect(label, "Trento") ~ "Trento",
    str_detect(label, "Valle") ~ "Valle d'Aosta",
    TRUE ~ label
  )) %>%
  left_join(nuts2_codes) %>%
  filter(!is.na(code)) %>%
  mutate(value = value / 1000)  # expressing values in million EUR

# Downloading roundwood import data to compute unit value (computed using Italy import data)

# roundwood_price <- rdbnomics::rdb(ids = c("Eurostat/for_basic/A.RW.TOTAL.IMP.THS_EUR.IT",
#                "Eurostat/for_basic/A.RW.TOTAL.IMP.THS_M3.IT")
#                ) %>%
#   select(original_period, unit, value) %>%
#   pivot_wider(names_from = unit, values_from = value) %>%
#   mutate(unit_value = THS_EUR / THS_M3, .keep = "unused")

# loading other forestry data

# source: https://www.inventarioforestale.org/it/category/statistiche-infc2015/principali-caratteristiche-dei-boschi-italiani/

forestry_data <- readxl::read_excel(here::here("data-raw/forest_data.xlsx")) %>%
  left_join(nuts2_codes) %>%
  mutate(nai_m3_faws = nai_m3 / forest_area * faws)

#### Disaggregated production and area data for pollination services

# istat.fl <- readSDMX(providerId = "ISTAT", resource = "dataflow") %>%
#   as.data.frame()
# #
# istat.fl %>%
#   filter(str_detect(Name.it, "conti"))
# #
#
#
# crops_data_istat <- readSDMX(providerId = "ISTAT", resource = "data",
#                              flowRef  = "101_1015",
#                              # Data Structure Definition (metadati)
#                              dsd = TRUE) %>%
#   # dataframe con le etichette, importate con dsd
#   as.data.frame(labels = TRUE)
#
# saveRDS(crops_data_istat, here::here("data-raw/crops_data_istat.rds"))
#
# agr_accounts_istat <- readSDMX(providerId = "ISTAT", resource = "data",
#                                flowRef  = "93_48",
#                                # Data Structure Definition (metadati)
#                                dsd = TRUE) %>%
#   # dataframe con le etichette, importate con dsd
#   as.data.frame(labels = TRUE)
#
# saveRDS(agr_accounts_istat, here::here("data-raw/agr_accounts_istat.rds"))

# loading key

pollination_key <- readxl::read_xlsx(here::here("data-raw/data_raw.xlsx"),
                                     sheet = "pollination_key")

crops_data_istat <- readRDS(here::here("data-raw/crops_data_istat.rds"))

agr_accounts_istat <- readRDS(here::here("data-raw/agr_accounts_istat.rds"))

# tidying crops data

crops_data_istat <- crops_data_istat %>%
  filter(TIPO_DATO == "ART") %>%
  select(label = ITTER107_label.it, year = obsTime, value = obsValue, crop_label = TIPCOLTIV_label.it,
         crop_code = TIPCOLTIV) %>%
  mutate(label = case_when(
    str_detect(label, "Bolzano") ~ "Bolzano",
    str_detect(label, "Trento") ~ "Trento",
    str_detect(label, "Valle") ~ "Valle d'Aosta",
    TRUE ~ label
  )
  ) %>%
  filter(label %in% nuts2_codes$label) %>%
  arrange(label, crop_label, year)

crop_area_istat <- crops_data_istat %>%
  left_join(pollination_key) %>%
  filter(!is.na(istat_area_class)) %>%
  group_by(label, year, rse_class) %>%
  summarise(area = sum(value, na.rm = TRUE)) %>%
  ungroup()


# downloading fiber crop areas from Eurostat

fiber_codes <- lapply(nuts2_codes$code, function(x) {

  flax <- paste0("Eurostat/apro_cpshr/A.I2100.AR.", x)

  cotton <- paste0("Eurostat/apro_cpshr/A.I2300.AR.", x)

  c(flax, cotton)

}) %>% unlist()

fiber_raw <- rdbnomics::rdb(ids = fiber_codes)[, c("geo", "original_period", "value")]

fiber_tidy <- fiber_raw %>%
  group_by(geo, original_period) %>%
  summarise(area = sum(value, na.rm = TRUE) * 1000, value = NULL) %>% #expressing areas in hectares
  ungroup() %>%
  left_join(nuts2_codes, by = c("geo" = "code")) %>%
  select(year = original_period, area, label) %>%
  mutate(rse_class = "tessili")

crop_area_istat <- crop_area_istat %>%
  bind_rows(fiber_tidy)


# tidying crop production value data

crop_value_istat <- agr_accounts_istat %>%
  filter( VAL_label.it == "prezzi correnti" &
           TIPO_AGGR_MIGLEURO_label.it == "produzione di beni e servizi per prodotto" &
           EDI == "2022M5") %>%
  select(label = ITTER107_label.it, year = obsTime, value = obsValue, crop_label = GPROD_PPROD_label.it,
         crop_code = GPROD_PPROD) %>%
  mutate(label = case_when(
    str_detect(label, "Bolzano") ~ "Bolzano",
    str_detect(label, "Trento") ~ "Trento",
    str_detect(label, "Valle") ~ "Valle d'Aosta",
    TRUE ~ label
  ),
  crop_label = ifelse(crop_label == "pomodori",
                      "pomodoro",
                      crop_label)
  ) %>%
  filter(label %in% nuts2_codes$label)

# loading pollination dependence ratios

# IMPORTANT: pollination dependence coefficients are adjusted for the amount of total
# pollination demand that is actually met (see pag 44 of Ecosystem services accounting Part I Outdoor recreation and crop pollination)
# Data are country-specific

pollination_dependence <- readxl::read_xlsx(here::here("data-raw/data_raw.xlsx"),
                                          sheet = "pollination_dependence")

# Loading raw data on landscape values

landscape_values_raw <- readxl::read_xlsx(here::here("data-raw/data_raw.xlsx"),
                                 sheet = "landscape_values")

#Loading EcoConCoefficients

eco_con_coeff <- readxl::read_xlsx(here::here("data-raw/data_raw.xlsx"),
                                   sheet = "eco_con_coeff")

# Loading land use cover data (Data regarding Trento and Bolzano are "simulated" using aggregate data about
# Trentino Altoadige (date divided by 2))

corine_area <- readxl::read_xlsx(here::here("data-raw/data_raw.xlsx"),
                                 sheet = "corine_area")

corine_area <- corine_area %>%
  group_by(corine3_code) %>%
  summarise(area_corine = sum(area_corine)) %>% # add Italy's value
  ungroup() %>%
  mutate(code = "IT", label = "Italia", um = "ha") %>%
  bind_rows(corine_area)

usethis::use_data(val_prod_forest, forestry_data,
                  crop_area_istat, crop_value_istat, pollination_dependence,
                  landscape_values_raw, eco_con_coeff, corine_area,
                  overwrite = TRUE, internal = TRUE)

usethis::use_data(nuts2_codes, master_table_agr,
                  maes_corine, overwrite = TRUE, internal = FALSE)



