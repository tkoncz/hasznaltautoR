#' \code{hasznaltautoR} package
#'
#' Easy scraping of car ads from \href{https://www.hasznaltauto.hu/}{hasznaltauto.hu}
#'
#' See the README on
#' \href{https://github.com/tomiaJO/hasznaltautoR#readme}{GitHub}
#'
#' @docType package
#' @name hasznaltautoR
#' @importFrom dplyr %>%
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))