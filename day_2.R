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

library(ggspatial)

library(ggtext)

library(ggview)

library(patchwork)

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

# Mapa ----

## Mapa principal ----

mapa_principal <- ggplot() +
  geom_sf(data = estados,
          aes(fill = "Brazil"),
          color = "black",
          linewidth = 1) +
  tidyterra::geom_spatraster_rgb(data = sat) +
  geom_sf(data = shp_mov,
          aes(color = "Jaguar traject")) +
  coord_sf(label_graticule = "SWE") +
  scale_x_continuous(breaks = seq(-57.6, -57.3, 0.15),
                     limits = c(-57.65625, -57.30471),
                     expand = FALSE) +
  scale_y_continuous(limits = c(-17.26667, -16.80458),
                     expand = FALSE) +
  scale_fill_manual(values = "white") +
  scale_color_manual(values = "gold4") +
  labs(fill = NULL,
       color = NULL,
       title = "Jaguar (<i>Panthera onca</i>) moviment deslocation on Brazilian Pantanal",
       subtitle = "Source: Morato <i>et al.</i> (2018). Jaguar movement database: a GPS-based <br> movement dataset of an apex predator in the Neotropics. <b>Ecology</b>") +
  ggspatial::annotation_scale(location = "bl",
                              text_face = "bold",
                              text_cex = 2,
                              text_col = "black",
                              unit_category = "metric",
                              bar_cols = c("black", "gold")) +
  theme_bw() +
  theme(axis.text = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.position = "bottom",
        strip.text = element_text(size = 15, color = "black", face = "italic"),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(color = "black", linewidth = 1),
        plot.title = ggtext::element_markdown(size = 15, color = "black", hjust = 0.5),
        plot.subtitle = ggtext::element_markdown(size = 12.5,
                                                 color = "black", hjust = 0.5)) +
  ggview::canvas(height = 10, width = 12)

mapa_principal

## Inset map ----

escala_x <- (57.65625 - 57.3047) * 25

escala_x

escala_y <- (17.26667 - 16.80458) * 25

escala_y

inset_map <- ggplot(data = estados) +
  geom_sf(fill = "white",
          color = "black",
          linewidth = 0.5) +
  tidyterra::geom_spatraster_rgb(data = sat) +
  ggmagnify::geom_magnify(from = c(-57.65625, -57.30471,
                                   -17.26667, -16.80458),
                          to = c(-72, -72 + escala_x,
                                 -30, -30 + escala_y),
                          linewidth = 1,
                          shadow = TRUE,
                          colour = "gold4",
                          proj.fill = alpha("gold", 0.5),
                          expand = FALSE) +
  theme_void() +
  ggview::canvas(height = 10, width = 12)

inset_map

## Mapa final ---

cowplot::ggdraw(mapa_principal) +
  cowplot::draw_plot(inset_map,
                     x = 0.3,
                     y = 0.2,
                     height = 0.35,
                     width = 0.35) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapas/map_day2.png", height = 10, width = 12)
