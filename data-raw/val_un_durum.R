# Calcolo valori unitari grano duro

library(tidyverse)
library(rdbnomics)
library(readxl)

ref_yr <- 2021

years <- 2017:2021

# scarico deflatore pil, area e valore produzione

codes <- c(
  "Eurostat/apro_cpsh1/A.C1120.AR.IT", # area (1000 ha)
  "Eurostat/nama_10_gdp/A.PD15_NAC.B1GQ.IT", # deflatore
  "Eurostat/aact_eaa01/A.01120.PROD_BP.MIO_EUR.IT" # valore milioni di euro correnti
)

data_raw <- map2(codes,
                 c("area", "defl", "value"),
                function(x, y) {
                  db <- rdbnomics::rdb(x)
                  db <- db[, c("original_period", "value")]
                  names(db) <- c("year", y)
                  db
                  }
                )

val_un_durum <- Reduce(function(x, y) merge(x, y, all=TRUE), data_raw)

xlsx::write.xlsx(val_un_durum,
                 here::here("data-raw", "val_un_durum.xlsx"),
                 row.names = F)
