#' Identifying the most common crop grown on arable land
#'
#' This function identifies - on the basis of Eurostat official data - the most
#' common crop grown on arable land in Italian NUTS2 regions. The most common crop
#' is identified on the basis of the average crop areas of a user-defined time window.
#'
#' @param nuts A character vector. The names of the the Italian NUTS2 regions of interest. Names
#'     must be in Italian and chosen among the folloqing list: `r knitr::combine_words(ecoservr::nuts2_codes$label)`.
#'     `Bolzano` and `Trento` are treated as separate regions. Data on `Italia` as a whole can be retrieved
#'     as well.
#' @param h An integer. A number specifying the number of years over which computing average areas. Default value
#'     is `3`.
#' @param last_yr An integer. The latest (meaning most recent) year from which computing average areas.
#' @param lang character string. Either `en` or `it`, to specify in which language Corine classes and crop labels must
#'     be returned. Default value is `it`.
#' @return A tibble with the average area occupied by crops grown on arable land.
#' @export
#'
#' @examples
#'
#' most_common_crop(last_yr = 2019)
most_common_crop <- function(nuts = "Italia", h = 3, last_yr, lang = "it") {

  original_period <- value_label <- value <- NULL

  if (!is.element(lang, c("en", "it"))) stop("Please provide a valid 'lang' value: either 'it' or 'en")

  if (any(!is.element(nuts, ecoservr::nuts2_codes$label)) == TRUE)
    stop(paste0("Please provide a valid NUTS2 name. Pick a name among: ",
                knitr::combine_words(ecoservr::nuts2_codes$label, and = "or ")))

  if (last_yr < 2015)
    stop("Data are from 2015 onwards")

  # filtering codes referring to arable land only

    metadata <- ecoservr::master_table_agr[ecoservr::master_table_agr$corine3_code == 211, c(paste0("value_label_" , "it"),
                                                                                             "area_code", "strucpro")]

    metadata$value_label <- metadata[[paste0("value_label_", lang)]]

    metadata[[paste0("value_label_", lang)]] <- NULL

  # dowloading area data

  geo <- ecoservr::nuts2_codes[ecoservr::nuts2_codes$label %in% nuts, ]$code

  data_raw <- rdbnomics::rdb(provider_code = "Eurostat",
      dataset_code = "apro_cpshr",
      dimensions = list(geo = geo,
                        FREQ = "A",
                        crops = metadata$area_code,
                        strucpro = "AR"
      )
  )

  # summarizing data

  time_period <- c((last_yr - h + 1):last_yr)

  data_raw$original_period <- as.numeric(data_raw$original_period)

  data_raw <- data_raw %>%
    dplyr::filter(original_period %in% time_period) %>%
    dplyr::left_join(metadata, by = c("crops" = "area_code")) %>%
    dplyr::group_by(geo, value_label, original_period) %>%
    dplyr::summarize(value = sum(value, na.rm = TRUE)) %>%
    dplyr::group_by(geo, value_label) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
    dplyr::ungroup()

  output <- data_raw %>%
    dplyr::group_by(geo) %>%
    dplyr::filter(value == max(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(code = geo)

  output


}
