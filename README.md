# hasznaltautoR

A friendly package to help you pull car ad details from [hasznaltauto.hu]https://www.hasznaltauto.hu/).  

## Installation
```{r eval=F}
devtools::install_github("https://github.com/tomiaJO/hasznaltautoR")
```
## Examples
```{r eval=F}
library(hasznalautoR)

ad_url <- paste0(
    "https://www.hasznaltauto.hu/szemelyauto/volvo/xc60/",
    "volvo_xc60_2_0_t8_twin_engine_inscription_geartronic-13821432"
)
getAdDetailsFromUrl(ad_url)

getAllCarAds("https://www.hasznaltauto.hu/szemelyauto/volvo/xc60")
```