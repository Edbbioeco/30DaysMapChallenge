# Desfios ----

# meus dados

# Pacotes ----

library(tidyverse)

library(magrittr)

library(parzer)

library(sf)

library(terra)

library(tidyterra)

library(adehabitatHR)

library(ggnewscale)

library(ggspatial)

library(sp)

# Dados ----

## Coordenadas ----

### Importando ----

dados <- read.csv("dados_gps.csv",
                  sep = ";")

### Visualizando ----

dados

dados |> dplyr::glimpse()

### Tratando ----

dados %<>%
  dplyr::mutate(long = long |> parzer::parse_lon(),
                lat = lat |> parzer::parse_lat(),
                datetime = paste0(date, time) |>
                  lubridate::mdy_hm()) |>
  tidyr::drop_na()

dados |> dplyr::glimpse()

### Criando shapefile ----

sf_dados <- dados |>
  sf::st_as_sf(coords = c("long", "lat"),
               crs = 4674) |>
  sf::st_transform(crs = 32725)

sf_dados

ggplot() +
  geom_sf(data = sf_dados |>
            sf::st_transform(crs = 4674)) +
  geom_path(data = dados, aes(long, lat)) +
  geom_sf(data = sf_dados |>
            dplyr::slice_head(),
          color = "blue") +
  geom_sf(data = sf_dados |>
            dplyr::slice_tail(),
          color = "red") +
  theme_bw()

## Imagem de satélite ----

### Importando ----

tapacura_sat <- terra::rast("tapacura_rec.tif")

### Visualizando ----

tapacura_sat

ggplot() +
  tidyterra::geom_spatraster_rgb(data = tapacura_sat) +
  geom_sf(data = sf_dados |>
            sf::st_transform(crs = 4674),
          color = "gold") +
  geom_path(data = dados, aes(long, lat),
            color = "gold") +
  coord_sf(expand = FALSE) +
  theme_minimal()

### Tratando ----

terra::crs(tapacura_sat) <- "EPSG:4674"

# BBMM ----

## Transformar em um objeto de trajetória ----

traj <- adehabitatLT::as.ltraj(xy = sf_dados |>
                                 sf::st_coordinates() |>
                                 as.data.frame(),
                               date = dados$datetime,
                               id = dados$id,
                               proj4string = sp::CRS("+init=epsg:32725"))

traj

plot(traj)

## Cálculo para a variância do movimento animal (sig 1) dado pelo ponto de inflexão - likelihood ----

adehabitatHR::liker(traj,
                    rangesig1 = c(1, 1000),
                    sig2 = 5,
                    byburst = FALSE,
                    plotit = TRUE)

## Calcular o modelo de utilização da área ----

bb_traj <- adehabitatHR::kernelbb(traj,
                                  sig1 = 4,
                                  sig2 = 5,
                                  grid = 1000,
                                  extent = 5,
                                  nalpha = 25)

bb_traj

bb_traj_rast <- bb_traj |>
  terra::rast()

terra::crs(bb_traj_rast) <- "EPSG:4674"

ggplot() +
  tidyterra::geom_spatraster(data = bb_traj_rast) +
  scale_fill_viridis_c() +
  coord_sf(expand = FALSE) +
  theme_bw()

## Polígonos de contorno (95% e 50%) ----

### 95% ----

bb_contour_95 <- bb_traj |>
  adehabitatHR::getverticeshr(percent = 95, unout = "km2")

bb_contour_95

### 50% ----

bb_contour_50 <- bb_traj |>
  adehabitatHR::getverticeshr(percent = 50, unout = "km2")

bb_contour_50

## Unindo ----

unido_bb <- dplyr::bind_rows(bb_contour_95 |>
                               sf::st_as_sf(crs = 32725) |>
                               sf::st_set_crs(32725) |>
                               sf::st_transform(crs = 4674) |>
                               dplyr::mutate(`Brownian Bridge Moviment Model` = "95%"),
                             bb_contour_50 |>
                               sf::st_as_sf(crs = 32725) |>
                               sf::st_set_crs(32725) |>
                               sf::st_transform(crs = 4674) |>
                               dplyr::mutate(`Brownian Bridge Moviment Model` = "50%")) |>
  dplyr::arrange(`Brownian Bridge Moviment Model` |> dplyr::desc())

unido_bb

## Visualizando ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = tapacura_sat) +
  geom_sf(data = unido_bb[1, ],
          aes(color = "95%"),
          fill = "orange",
          alpha = 0.3,
          linewidth = 1) +
  geom_sf(data = unido_bb[2, ],
          aes(color = "50%"),
          fill = "royalblue",
          alpha = 0.3,
          linewidth = 1) +
  labs(color = "Brownian Bridge Moviment Model") +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5)) +
  scale_color_manual(values = c("royalblue",
                                "orange")) +
  ggnewscale::new_scale_color() +
  geom_path(data = dados,
            aes(long, lat, color = "Trajetória")) +
  geom_sf(data = sf_dados,
          aes(color = "Pontos de registro")) +
  scale_color_manual(values = c("red",
                                "black")) +
  coord_sf(expand = FALSE,
           label_graticule = "SWE",
           xlim = c(-35.203, -35.19),
           ylim = c(-8.045, -8.031)) +
  labs(color = NULL,
       x = NULL,
       y = NULL,
       title = "Map made on traject coordinates taken on Pernambuco Atlantic Forest",
       subtitle = "Location: Tapacurá Ecological Reserve") +
  scale_x_continuous(breaks = seq(-35.2, -35.19, 0.004)) +
  ggspatial::annotation_scale(location = "bl",
                              text_face = "bold",
                              text_cex = 2,
                              text_col = "black",
                              unit_category = "metric",
                              bar_cols = c("black", "gold")) +
  theme_bw() +
  theme(axis.text = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 15, color = "black"),
        legend.position = "bottom",
        strip.text = element_text(size = 15, color = "black", face = "italic"),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(color = "black", linewidth = 1),
        plot.title = element_text(size = 15, color = "black", hjust = 0.5),
        plot.subtitle = element_text(size = 15, color = "black", hjust = 0.5)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapas/map_day4.png", height = 10, width = 12)
