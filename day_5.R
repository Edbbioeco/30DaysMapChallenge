# Desafios ----

# Terra

# Pacotes ----

library(geobr)

library(tidyverse)

library(sf)

library(terra)

library(tidyterra)

library(magrittr)

library(ggmagnify)

library(ggview)

# Dados ----

## Estados do Brasil ----

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

evi <- terra::rast("evi_indice.tif")

### Visualizando ----

evi

ggplot() +
  tidyterra::geom_spatraster(data = evi)

### Recortando ----

terra::crs(evi) <- "EPSG:4674"

evi %<>%
  terra::crop(ne) %<>%
  terra::mask(ne)

evi

ggplot() +
  tidyterra::geom_spatraster(data = evi) +
  scale_fill_viridis_c()

# Mapa ----

ggplot() +
  geom_sf(data = br,
          aes(color = "brazil"),
          fill = "gray90",
          linewidth = 1) +
  tidyterra::geom_spatraster(data = evi) +
  tidyterra::scale_fill_whitebox_c(palette = "atlas",
                                direction = -1,
                                na.value = "transparent",
                                name = "EVI",
                                guide = guide_colorbar(order = 1,
                                                       title.position = "top",
                                                       title.hjust = 0.5,
                                                       barwidth = 15,
                                                       frame.colour = "black",
                                                       frame.linewidth = 1,
                                                       ticks.colour = "black",
                                                       ticks.linewidth = 1)) +
  geom_sf(data = ne,
          aes(color = "Northeastern Region"),
          fill = "transparent",
          linewidth = 1) +
  scale_color_manual(values = c("black",
                                "orangered"),
                     name = NULL) +
  coord_sf(xlim = c(-48.75515, -34.9),
           ylim = c(-18.34849, -1.049328),
           label_graticule = "SWE") +
  labs(title = "Brazilian Northeastern Regeion Enhenced Evegetation Index (EVI) values",
       subtitle = "Source: AppEEARS, Nasa") +
  theme_bw() +
  theme(axis.text = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.position = "bottom",
        strip.text = element_text(size = 15, color = "black", face = "italic"),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(color = "black", linewidth = 1),
        plot.title = element_text(size = 15, color = "black", hjust = 0.5),
        plot.subtitle = element_text(size = 12.5, color = "black", hjust = 0.5)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapas/map_day5.png", height = 10, width = 12)
