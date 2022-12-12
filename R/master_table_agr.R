#' Master table with data needed for the analysis
#'
#' A data frame with Eurostat codes needed to download production and cropped areas from Eurostat,
#' ecosystem contribution coefficients from the literature, Corine codes and labels, MAES ecosystems.
#'
#' @format ## `master_table_agr`
#' A dataframe with 12 variables
#'
#' \describe{
#'     \item{value_label_en}{Crop label in English}
#'     \item{value_label_it}{Crop label in Italian}
#'     \item{value_code}{Eurostat code to download data on value of production}
#'     \item{unit}{Eurostat code to select the monetary unit to download from Eurostat}
#'     \item{area_code}{Eurostat code to download data on cropped areas}
#'     \item{strucpro}{Supplementary Eurostat code to download data on cropped areas}
#'     \item{eco_con_coeff}{Ecosystem contribution coefficient from the literature}
#'     \item{corine3_code}{Corine class code}
#'     \item{corine3_label_en}{Corine class label in English}
#'     \item{corine3_label_it}{Corine class label in Italian}
#'     \item{maes}{Ecosystem label in English}
#'     \item{fct}{Weight to adjust the value of production for areas with mixed natural and agricultural cover}
#' }
"master_table_agr"
