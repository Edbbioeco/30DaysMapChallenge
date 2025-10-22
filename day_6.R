# Desafios ----

# Mapa 3d

# Pacotes ----

library(geobr)

library(tidyverse)

library(sf)

library(elevatr)

library(terra)

library(tidyterra)

library(magrittr)

library(rayshader)

# Dados ----

### Importando ----

br <- geobr::read_state(year = 2018)

### Visualizando ----

br

ggplot() +
  geom_sf(data = br)

## Nordeste ----

### Importando ----

ne <- geobr::read_region(year = 2019) |>
  dplyr::filter(code_region == 2)

### Visualizando ----

ne

ggplot() +
  geom_sf(data = ne)

## Raster de EVI ----

### Importando ----

alt <- elevatr::get_aws_terrain(locations = ne,
                                z = 8,
                                prj = 4674)

### Visualizando ----

alt

ggplot() +
  tidyterra::geom_spatraster(data = alt) +
  tidyterra::scale_fill_hypso_c(palette = "colombia_hypso")

### Recortando ----

terra::crs(alt) <- "EPSG:4674"

alt %<>%
  terra::crop(ne) %<>%
  terra::mask(ne)

alt

ggplot() +
  tidyterra::geom_spatraster(data = alt) +
  tidyterra::scale_fill_hypso_c(palette = "colombia_hypso")

# Mapa ----

## Transformando o raster em dataframe ----

alt_df <- alt |>
  tidyterra::filter(file21e4a157688 >= 0) |>
  as.data.frame(xy = TRUE) |>
  tidyr::drop_na() |>
  dplyr::rename("Altitude" = 3)

alt_df

## Mapa de base -----

ggplt <- ggplot() +
  geom_raster(data = alt_df,
              aes(x, y, fill = Altitude)) +
  tidyterra::scale_fill_hypso_c(palette = "colombia_hypso",
                                   na.value = "transparent",
                                   name = "Altitude",
                                   guide = guide_colorbar(order = 1,
                                                          title.position = "top",
                                                          title.hjust = 0.5,
                                                          barwidth = 15,
                                                          frame.colour = "black",
                                                          frame.linewidth = 1,
                                                          ticks.colour = "black",
                                                          ticks.linewidth = 1)) +
  coord_sf(label_graticule = "SWE") +
  labs(title = "Brazilian Northeastern Region Altitude",
       subtitle = "Source: AppEEARS, Nasa",
       x = NULL,
       y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.position = "none",
        strip.text = element_text(size = 15, color = "black", face = "italic"),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(color = "black", linewidth = 1),
        plot.title = element_text(size = 15, color = "black", hjust = 0.5),
        plot.subtitle = element_text(size = 12.5, color = "black", hjust = 0.5))

ggplt

## Mapa 3D ----

rayshader::plot_gg(ggplt,
                   multicore = TRUE,
                   width = 6,
                   height = 6,
                   scale = 300,
                   zoom = 0.5,
                   theta = 30,
                   phi = 30,
                   windowsize = c(1000, 800))

## Exportando ----

render_snapshot()

rayshader::render_snapshot(filename = "mapas/map_day6.png")

rayshader::render_movie(filename = "mapas/map_day6.gif",
                        type = "orbit",
                        frames = 180,
                        fps = 30,
                        phi = 40,
                        theta = 0,
                        zoom = 0.7,
                        fov = 70)

rayshader::render_movie(filename = "mapas/map_day6.mp4",
                        type = "orbit",
                        frames = 180,
                        fps = 30,
                        phi = 40,
                        theta = 0,
                        zoom = 0.7,
                        fov = 70)
