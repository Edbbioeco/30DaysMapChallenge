# Desafio ---

# linhas

# Fonte ----

# https://datarepository.movebank.org/entities/datapackage/8fed4a87-14d3-4f9d-b630-e1d406b15c68

# https://doi.org/10.1002/ecy.2379

# Pacotes ----

library(tidyverse)

library(sf)

library(geobr)

library(maptiles)

library(ggmagnify)

library(ggmagnify)

library(ggtext)

# Dados ----

## Movimento de onça ----

### Importando ----

mov <- read.csv("Movement ecology of the jaguar in the largest floodplain of the world, the Brazilian Pantanal.csv")

### Visualizando ----

mov

mov |> dplyr::glimpse()

mov |>
  ggplot(aes(location.long, location.lat)) +
  geom_path()

### Transformando em shapefile ----

shp_mov <- mov |>
  dplyr::arrange(timestamp) |>
  sf::st_as_sf(coords = c("location.long", "location.lat"),
               crs = 4674) |>
  dplyr::summarise(do_union = TRUE) |>
  sf::st_cast("LINESTRING")

shp_mov

ggplot() +
  geom_sf(data = shp_mov) +
  geom_point(data = mov |>
               dplyr::slice_head(),
             aes(location.long, location.lat),
             color = "red") +
  geom_point(data = mov |>
               dplyr::slice_tail(),
             aes(location.long, location.lat),
             color = "blue")

## Estados do Brasil ----

### Importando ----

estados <- geobr::read_state()

### Visualizando ----

estados

ggplot() +
  geom_sf(data = estados) +
  geom_sf(data = shp_mov)

## Imagem de satélite ----

### Importando ----

sat <- maptiles::get_tiles(shp_mov,
                           provider = "Esri.WorldImagery",
                           zoom = 13)

### Visualizando ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = sat) +
  geom_sf(data = shp_mov, color = "gold") +
  coord_sf(expand = FALSE)
