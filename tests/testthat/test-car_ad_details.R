context("getAdDetailsFromUrl")

test_that("Advertisement code is returned correctly", {
    ad_url <- paste0(
        "https://www.hasznaltauto.hu/szemelyauto/volvo/xc60/",
        "volvo_xc60_2_0_t8_twin_engine_inscription_geartronic-13821432"
    )
    ad_code <- "13821432"

    expect_equal(getAdDetailsFromUrl(ad_url)[["Hirdetéskod"]], ad_code)
})

# clean-up ----
if(dir.exists("data")) unlink("data", recursive = TRUE)