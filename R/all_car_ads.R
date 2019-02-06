#' Get all car ads from search landing page URL.
#' Results are automatically saved in the folder defined by `save_path`.
#'
#' @param search_landing_url String, URL for search results.
#' @param save_path String, folder to which the results in .csv should be saved to.
#' @param wait_between_requests Integer, how many seconds to wait between sending request
#'   to get specific details from car advertisements.
#'
#' @return data_frame, containing details for all ads found from the starting URL.
#'   In addition, results will be save to the folder specified in `save_path`.
#'
#' @importFrom magrittr %>%
#'
#' @examples \dontrun{
#'   getAllCarAds("https://www.hasznaltauto.hu/szemelyauto/volvo/xc60")
#' }
#'
#' @export
getAllCarAds <- function(search_landing_url, save_path = "data", 
                         wait_between_requests = 0) {
    all_ad_urls <- getAllCarAdUrls(search_landing_url) 
    
    ads <- purrr::map_df(all_ad_urls, ~{
        Sys.sleep(wait_between_requests)
        getAdDetailsFromUrl(.x)
    })

    saveAdsToCSV(ads, search_landing_url, save_path)

    ads
}

#' Get all car ad URLs from search landing page URL
#'
#' @param search_landing_url String, URL for search results.
#'
#' @return character vector, containing the url to all car ads relevant to search
#'
#' @importFrom magrittr %>%
getAllCarAdUrls <- function(search_landing_url) {
    last_page_number <- getLastPageNumber(search_landing_url)

    urls_for_cars_on_page <- purrr::map(1:last_page_number, ~{
        getCarAdUrlsOnPage(page_url = paste0(search_landing_url, "/page", .x))
    }) %>% purrr::flatten_chr()

    message(paste0("ads found: ", length(urls_for_cars_on_page)))

    urls_for_cars_on_page
}

getLastPageNumber <- function(url_of_page_with_ad_list) {
    last_page_number <- xml2::read_html(url_of_page_with_ad_list) %>%
        rvest::html_node(xpath = "//li[@class='last']//a") %>% # enough to find the first match, hence "node" is used
        rvest::html_attr("data-page")

    if(!is.na(last_page_number)) {
        as.integer(last_page_number) + 1
    } else {
        1
    }
}

getCarAdUrlsOnPage <- function(page_url) {
    xml2::read_html(page_url) %>%
        rvest::html_nodes(xpath = "//div[@class='col-xs-28']//h3/a") %>%
        rvest::html_attr("href")
}

#' Save ads resulting for scraping to a .csv file
#'
#' @param ads String
#' @param search_landing_url String
#' @param save_path String
#'
#' @return Nothing, saves file to location.
#'
#' @importFrom magrittr %>%
saveAdsToCSV <- function(ads, search_landing_url, save_path) {
    if(!dir.exists(save_path)) dir.create(save_path, recursive = TRUE)
    
    file_name <- fileNameFromUrl(search_landing_url)
    readr::write_csv(ads, path = file.path(save_path, file_name))
}

#' Hash URL (for naming saved files)
#'
#' @param url String
#'
#' @return character
#'
#' @importFrom magrittr %>%
fileNameFromUrl <- function(url) {
    paste0("all_cars_from_url_", digest::digest(url, "md5", FALSE),
        "_", Sys.Date(), ".csv")
}
