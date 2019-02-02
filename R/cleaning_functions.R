# CURRENTLY NOT IN USE - TO BE UPDATED WITH ERROR HANDLING
cleanNumberColumns <- function(ad_df) {
    number_columns <- list(
        "Vételár", "Akciós ár", "Vételár EUR",
        "Kilométeróra állása",
        "Saját tömeg", "Teljes tömeg", "Csomagtartó"
    )

    columns_to_rename <- list(
        "Saját tömeg (kg)"    = "Saját tömeg",
        "Teljes tömeg (kg)"   = "Teljes tömeg",
        "Csomagtartó (liter)" = "Csomagtartó",
        "Vételár HUF"         = "Vételár",
        "Akciós ár HUF"       = "Akciós ár"
    )
    ## TODO: map with if!
    # ad_df %>%
    #     dplyr::mutate_at(
    #         number_columns, gsub,
    #         pattern = "Ft|\\s|€|km|kg|liter", replacement = ""
    #     ) %>%
    #     dplyr::mutate_at(number_columns, as.numeric) %>%
    #     dplyr::rename(!!columns_to_rename)
}