#' Computes the share of single crop classes on total agricultural area
#'
#' This function computes the ratio of the average areas of crops that benefit from insect pollination on total
#' utilized agricultural area in Italian NUTS2.
#'
#' @inheritParams most_common_crop
#' @return A tibble with NUTS labels and codes and the share of insect pollinated crops on total UAA.
compute_crop_shares <- function (nuts = "Italia", last_yr = 2019, h = 3){

  # computing average areas in the chosen period

  period <- c((last_yr - h + 1):last_yr)

  areas <- crop_area_istat %>%
    dplyr::filter(year %in% period & label %in% nuts & rse_class != "tessili") %>%
    dplyr::group_by(label, rse_class) %>%
    dplyr::summarise(area = mean(area, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(nuts2_codes)

  nuts_code <- nuts2_codes[nuts2_codes$label %in% nuts, ]$code

  estat_codes <- lapply(nuts_code, function (x) {
    paste0("Eurostat/apro_cpshr/A.UAA.MA.",
           x)

  }) %>% unlist()

  uaa_data_raw <- rdbnomics::rdb(ids = estat_codes) %>%
    dplyr::select(geo, original_period, value) %>%
    tidyr::as_tibble()

  uaa <- uaa_data_raw %>%
    dplyr::filter(is.element(original_period, as.character(period))) %>%
    dplyr::mutate(area = value * 1000, .keep = "unused") %>%
    dplyr::group_by(geo) %>%
    dplyr::summarise(uaa = mean(area, na.rm = TRUE)) %>%
    dplyr::ungroup()

  # computing area shares on total uaa

  data_raw <- areas %>%
    dplyr::left_join(uaa, by = c("code" = "geo")) %>%
    dplyr::mutate(area_share = area / uaa) %>%
    dplyr::select(label, rse_class, code, area_share)

  data_raw


}
