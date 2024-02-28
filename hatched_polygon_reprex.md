``` r
library(sf)
#> Linking to GEOS 3.11.2, GDAL 3.7.2, PROJ 9.3.0; sf_use_s2() is TRUE
library(gg.layers)
#> Registered S3 methods overwritten by 'ggpp':
#>   method                  from   
#>   heightDetails.titleGrob ggplot2
#>   widthDetails.titleGrob  ggplot2
#> Registered S3 method overwritten by 'gg.layers':
#>   method       from  
#>   print.gtable gtable
library(mapview)
franconia <- mapview::franconia

hatch_franconia <-franconia %>% 
  gg.layers::st_hatched_polygon(density = 2, angle = 45)
#> Error in x[, 2]: incorrect number of dimensions

# opening up the st_hatched_polygon func for inspection

density <- 2
angle <- 45
geoms = sf::st_geometry(franconia)
n = length(geoms)
x <-franconia

function (x, density = 2, angle = 45, fillOddEven = FALSE) 
{
  geoms = sf::st_geometry(x)
  n = length(geoms)
  if (length(density) != n) 
    density <- rep(density, n, n)
  if (length(angle) != n) 
    angle <- rep(angle, n, n)
  sf_lines <- list()
  for (j in 1:n) {
    sf_lines[[j]] <- polygonRingHolesLines(geoms[[j]], density = density[j], #this function is not found
                                           angle = angle[j], ID = j, fillOddEven = fillOddEven)
  }
  do.call(rbind, sf_lines) %>% sf::st_set_crs(sf::st_crs(x)) %>% 
    sf::st_make_valid()
}
#> function (x, density = 2, angle = 45, fillOddEven = FALSE) 
#> {
#>   geoms = sf::st_geometry(x)
#>   n = length(geoms)
#>   if (length(density) != n) 
#>     density <- rep(density, n, n)
#>   if (length(angle) != n) 
#>     angle <- rep(angle, n, n)
#>   sf_lines <- list()
#>   for (j in 1:n) {
#>     sf_lines[[j]] <- polygonRingHolesLines(geoms[[j]], density = density[j], #this function is not found
#>                                            angle = angle[j], ID = j, fillOddEven = fillOddEven)
#>   }
#>   do.call(rbind, sf_lines) %>% sf::st_set_crs(sf::st_crs(x)) %>% 
#>     sf::st_make_valid()
#> }
```

<sup>Created on 2024-01-10 with [reprex v2.0.2](https://reprex.tidyverse.org)</sup>

<details style="margin-bottom:10px;">
<summary>
Session info
</summary>

``` r
sessioninfo::session_info()
#> ─ Session info ───────────────────────────────────────────────────────────────
#>  setting  value
#>  version  R version 4.3.2 (2023-10-31 ucrt)
#>  os       Windows 10 x64 (build 19045)
#>  system   x86_64, mingw32
#>  ui       RTerm
#>  language (EN)
#>  collate  English_United States.utf8
#>  ctype    English_United States.utf8
#>  tz       America/Los_Angeles
#>  date     2024-01-10
#>  pandoc   3.1.1 @ C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools/ (via rmarkdown)
#> 
#> ─ Packages ───────────────────────────────────────────────────────────────────
#>  package      * version  date (UTC) lib source
#>  backports      1.4.1    2021-12-13 [1] CRAN (R 4.3.1)
#>  base64enc      0.1-3    2015-07-28 [1] CRAN (R 4.3.1)
#>  boot           1.3-28.1 2022-11-22 [2] CRAN (R 4.3.2)
#>  broom          1.0.5    2023-06-09 [1] CRAN (R 4.3.2)
#>  cachem         1.0.8    2023-05-01 [1] CRAN (R 4.3.2)
#>  class          7.3-22   2023-05-03 [2] CRAN (R 4.3.2)
#>  classInt       0.4-10   2023-09-05 [1] CRAN (R 4.3.2)
#>  cli            3.6.2    2023-12-11 [1] CRAN (R 4.3.2)
#>  codetools      0.2-19   2023-02-01 [2] CRAN (R 4.3.2)
#>  colorspace     2.1-0    2023-01-23 [1] CRAN (R 4.3.2)
#>  crosstalk      1.2.1    2023-11-23 [1] CRAN (R 4.3.2)
#>  data.table     1.14.10  2023-12-08 [1] CRAN (R 4.3.2)
#>  DBI            1.2.0    2023-12-21 [1] CRAN (R 4.3.2)
#>  digest         0.6.33   2023-07-07 [1] CRAN (R 4.3.2)
#>  dplyr          1.1.4    2023-11-17 [1] CRAN (R 4.3.2)
#>  e1071          1.7-14   2023-12-06 [1] CRAN (R 4.3.2)
#>  evaluate       0.23     2023-11-01 [1] CRAN (R 4.3.2)
#>  fansi          1.0.6    2023-12-08 [1] CRAN (R 4.3.2)
#>  fastmap        1.1.1    2023-02-24 [1] CRAN (R 4.3.2)
#>  fftwtools      0.9-11   2021-03-01 [1] CRAN (R 4.3.1)
#>  fs             1.6.3    2023-07-20 [1] CRAN (R 4.3.2)
#>  generics       0.1.3    2022-07-05 [1] CRAN (R 4.3.2)
#>  gg.layers    * 0.1.1    2024-01-08 [1] Github (rpkgs/gg.layers@3211fea)
#>  gggrid         0.2-0    2022-01-11 [1] CRAN (R 4.3.2)
#>  ggh4x          0.2.7    2023-12-22 [1] CRAN (R 4.3.2)
#>  ggpattern      1.0.1    2022-11-09 [1] CRAN (R 4.3.2)
#>  ggplot2        3.4.4    2023-10-12 [1] CRAN (R 4.3.2)
#>  ggplotify      0.1.2    2023-08-09 [1] CRAN (R 4.3.2)
#>  ggpp           0.5.5    2023-11-08 [1] CRAN (R 4.3.2)
#>  ggtext         0.1.2    2022-09-16 [1] CRAN (R 4.3.2)
#>  glue           1.6.2    2022-02-24 [1] CRAN (R 4.3.2)
#>  gridGraphics   0.5-1    2020-12-13 [1] CRAN (R 4.3.2)
#>  gridtext       0.1.5    2022-09-16 [1] CRAN (R 4.3.2)
#>  gtable         0.3.4    2023-08-21 [1] CRAN (R 4.3.2)
#>  htmltools      0.5.7    2023-11-03 [1] CRAN (R 4.3.2)
#>  htmlwidgets    1.6.4    2023-12-06 [1] CRAN (R 4.3.2)
#>  KernSmooth     2.23-22  2023-07-10 [2] CRAN (R 4.3.2)
#>  knitr          1.45     2023-10-30 [1] CRAN (R 4.3.2)
#>  lattice        0.22-5   2023-10-24 [1] CRAN (R 4.3.2)
#>  leafem         0.2.3    2023-09-17 [1] CRAN (R 4.3.2)
#>  leaflet        2.2.1    2023-11-13 [1] CRAN (R 4.3.2)
#>  lifecycle      1.0.4    2023-11-07 [1] CRAN (R 4.3.2)
#>  lubridate      1.9.3    2023-09-27 [1] CRAN (R 4.3.2)
#>  magrittr       2.0.3    2022-03-30 [1] CRAN (R 4.3.2)
#>  mapview      * 2.11.2   2023-10-13 [1] CRAN (R 4.3.2)
#>  MASS           7.3-60   2023-05-04 [2] CRAN (R 4.3.2)
#>  matrixStats    1.2.0    2023-12-11 [1] CRAN (R 4.3.2)
#>  memoise        2.0.1    2021-11-26 [1] CRAN (R 4.3.2)
#>  munsell        0.5.0    2018-06-12 [1] CRAN (R 4.3.2)
#>  pillar         1.9.0    2023-03-22 [1] CRAN (R 4.3.2)
#>  pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.3.2)
#>  png            0.1-8    2022-11-29 [1] CRAN (R 4.3.1)
#>  polynom        1.4-1    2022-04-11 [1] CRAN (R 4.3.2)
#>  proxy          0.4-27   2022-06-09 [1] CRAN (R 4.3.2)
#>  purrr          1.0.2    2023-08-10 [1] CRAN (R 4.3.2)
#>  R.cache        0.16.0   2022-07-21 [1] CRAN (R 4.3.2)
#>  R.methodsS3    1.8.2    2022-06-13 [1] CRAN (R 4.3.1)
#>  R.oo           1.25.0   2022-06-12 [1] CRAN (R 4.3.1)
#>  R.utils        2.12.3   2023-11-18 [1] CRAN (R 4.3.2)
#>  R6             2.5.1    2021-08-19 [1] CRAN (R 4.3.2)
#>  raster         3.6-26   2023-10-14 [1] CRAN (R 4.3.2)
#>  Rcpp           1.0.11   2023-07-06 [1] CRAN (R 4.3.2)
#>  reprex         2.0.2    2022-08-17 [1] CRAN (R 4.3.2)
#>  rlang          1.1.2    2023-11-04 [1] CRAN (R 4.3.2)
#>  rmarkdown      2.25     2023-09-18 [1] CRAN (R 4.3.2)
#>  rstudioapi     0.15.0   2023-07-07 [1] CRAN (R 4.3.2)
#>  rtrend         0.1.4    2022-11-07 [1] CRAN (R 4.3.2)
#>  satellite      1.0.4    2021-10-12 [1] CRAN (R 4.3.2)
#>  scales         1.3.0    2023-11-28 [1] CRAN (R 4.3.2)
#>  sessioninfo    1.2.2    2021-12-06 [1] CRAN (R 4.3.2)
#>  sf           * 1.0-15   2023-12-18 [1] CRAN (R 4.3.2)
#>  sp             2.1-2    2023-11-26 [1] CRAN (R 4.3.2)
#>  stringi        1.8.3    2023-12-11 [1] CRAN (R 4.3.2)
#>  stringr        1.5.1    2023-11-14 [1] CRAN (R 4.3.2)
#>  styler         1.10.2   2023-08-29 [1] CRAN (R 4.3.2)
#>  terra          1.7-65   2023-12-15 [1] CRAN (R 4.3.2)
#>  tibble         3.2.1    2023-03-20 [1] CRAN (R 4.3.2)
#>  tidyr          1.3.0    2023-01-24 [1] CRAN (R 4.3.2)
#>  tidyselect     1.2.0    2022-10-10 [1] CRAN (R 4.3.2)
#>  timechange     0.2.0    2023-01-11 [1] CRAN (R 4.3.2)
#>  units          0.8-5    2023-11-28 [1] CRAN (R 4.3.2)
#>  utf8           1.2.4    2023-10-22 [1] CRAN (R 4.3.2)
#>  vctrs          0.6.5    2023-12-01 [1] CRAN (R 4.3.2)
#>  withr          2.5.2    2023-10-30 [1] CRAN (R 4.3.2)
#>  xfun           0.41     2023-11-01 [1] CRAN (R 4.3.2)
#>  xml2           1.3.6    2023-12-04 [1] CRAN (R 4.3.2)
#>  yaml           2.3.8    2023-12-11 [1] CRAN (R 4.3.2)
#>  yulab.utils    0.1.3    2024-01-08 [1] CRAN (R 4.3.2)
#> 
#>  [1] C:/Users/mgaughan/AppData/Local/R/win-library/4.3
#>  [2] C:/Program Files/R/R-4.3.2/library
#> 
#> ──────────────────────────────────────────────────────────────────────────────
```

</details>
