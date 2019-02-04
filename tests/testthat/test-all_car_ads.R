context("getAllCarAds")

test_that("if save_path folder does not exist, it will be created", {
    if(dir.exists("data")) unlink("data", recursive = TRUE)

    mockery::stub(getAllCarAds, "getAllCarAdUrls", "test_url")
    mockery::stub(getAllCarAds, "getAdDetailsFromUrl", dplyr::tibble(test = "test"))

    getAllCarAds("test_search", "data")

    expect_true(dir.exists("data"))
})

# clean-up ----
if(dir.exists("data")) unlink("data", recursive = TRUE)