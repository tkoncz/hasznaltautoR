#' Get car ad details from ad URL
#'
#' @param ad_url String, URL to the specific car ad page
#'
#' @return data_frame, with one raw, and columns containing the ad details
#'
#' @importFrom magrittr %>%
#'
#' @examples \dontrun{
#'   ad_url <- paste0(
#'       "https://www.hasznaltauto.hu/szemelyauto/volvo/xc60/",
#'       "volvo_xc60_2_0_t8_twin_engine_inscription_geartronic-13821432"
#'   )
#'
#'   getAdDetailsFromUrl(ad_url)
#' }
#'
#' @export
getAdDetailsFromUrl <- function(ad_url) {
    ad_html <- xml2::read_html(ad_url)

    ad_code            <- getAdCode(ad_url)
    ad_title           <- getAdTitle(ad_html)
    ad_description     <- getAdDescription(ad_html)
    ad_main_attributes <- getAdMainAttributes(ad_html)
    ad_accessories     <- getAdAccessories(ad_html)
    ad_location        <- getAdLocation(ad_html)

    ad_table <- purrr::flatten(list(
        "Hirdet\U00E9skod"   = ad_code,
        "C\U00EDm"           = ad_title,
        "Le\U00EDr\U00E1s"   = ad_description,
        "URL"                = ad_url,
        ad_main_attributes,
        "Felszerelts\U00E9g" = ad_accessories,
        "Helysz\U00EDn"      = ad_location
    )) %>% dplyr::as_tibble()

    ad_table
}

getAdCode <- function(ad_url) {
    gsub(".*-(\\d{1,15})$", "\\1", ad_url)
}

getAdTitle <- function(ad_html) {
    ad_html %>%
        rvest::html_nodes("h1") %>%
        rvest::html_text() %>%
        {if(length(.) == 0) NA_character_ else .}
}

getAdDescription <- function(ad_html) {
    ad_html %>%
        rvest::html_nodes(xpath = "//div[@class='leiras']/div") %>%
        rvest::html_text() %>%
        {if(length(.) == 0) NA_character_ else .}
}

getAdMainAttributes <- function(ad_html) {
    ad_main_attribute_values <- ad_html %>%
        rvest::html_nodes(
            xpath = "//table[@class='hirdetesadatok']//td[not (@class='bal pontos')]//strong"
        ) %>%
        rvest::html_text()

    ad_main_attributes <- ad_html %>%
        rvest::html_nodes(
            xpath = "//table[@class='hirdetesadatok']//td[@class='bal pontos']"
        ) %>%
        rvest::html_text()

    ad_main_attributes <- gsub(":", "", ad_main_attributes)
    ad_main_attribute_values <- purrr::set_names(
        ad_main_attribute_values, ad_main_attributes
    )

    ad_main_attribute_values
}

getAdAccessories <- function(ad_html) {
    ad_accessories <- ad_html %>%
        rvest::html_nodes(
            xpath = "//div[@class='row felszereltseg']//li"
        ) %>% rvest::html_text() %>%
        paste(collapse = ", ")

    ad_accessories
}

getAdLocation <- function(ad_html) {
    ad_html %>%
        rvest::html_node(
            xpath = "//div[span/@id='adatlap-terkep-mutat']"
        ) %>%
        rvest::html_text() %>%
        gsub("T\U00E9rk\U00E9p megjelen\U00EDt\U00E9se", "", .)
}
