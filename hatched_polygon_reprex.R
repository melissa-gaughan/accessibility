#' ---
#' output:
#'   reprex::reprex_document:
#'     session_info: TRUE
#' ---

#' ---
#' output:
#'   reprex::reprex_document:
#'     session_info: TRUE
#' ---

#' ---
#' output:
#'   reprex::reprex_document:
#'     session_info: TRUE
#' ---


library(sf)
library(gg.layers)
library(mapview)
library(ggplot2)
franconia <- mapview::franconia

hatch_franconia <-franconia %>% 
  gg.layers::st_hatched_polygon(density = 2, angle = 45)

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
