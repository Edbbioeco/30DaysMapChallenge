# Desafio ----

# Acessibilidade

# Fonte ----

# https://doi.org/10.1111/2041-210X.13815

# Pacotes ----

library(tidyverse)

library(sf)

library(maptiles)

library(tidyterra)

library(geobr)

library(ggimage)

library(ggspatial)

library(ggmagnify)

library(ggview)

library(cowplot)

# Dados ----

## Deslocamento ----

### Importando ----

desl <- read.csv("Lowland tapirs, Tapirus terrestris, in Southern Brazil.csv")

### Visualizando ----

desl

desl |> dplyr::glimpse()

### Criando um shapefile ----

desl_sf <- desl |>
  sf::st_as_sf(coords = c("location.long", "location.lat"),
               crs = 4674) |>
  dplyr::summarise(do_union = TRUE) |>
  sf::st_cast("LINESTRING")

desl_sf

ggplot() +
  geom_sf(data = desl_sf)

## Imagem de satélite ----

### Importando ----

sat <- maptiles::get_tiles(desl_sf,
                           provider = "Esri.WorldStreetMap",
                           zoom = 10)

### Visualizandom ----

sat

ggplot() +
  tidyterra::geom_spatraster_rgb(data = sat) +
  geom_sf(data = desl_sf, color = "darkred")

## Shapefile do Brasil ----

### Importando ----

br <- geobr::read_state(year = 2019)

### Visualizando ----

br

ggplot() +
  geom_sf(data = br)

# Mapa ----

## Dataframe de mapeamento da imagem ----

tapirus <- tibble::tibble(x = -55.15,
                          y = -22.25,
                          image = "tapirus-terrestris.png")

tapirus

## Mapa principal ----

mapa_principal <- ggplot() +
  geom_sf(data = br,
          aes(color = "Brazil"),
          fill = "gray90",
          linewidth = 1) +
  tidyterra::geom_spatraster_rgb(data = sat) +
  geom_sf(data = desl_sf,
          aes(color = "<i>Tapirus terrestris</i> deslocation"),
          linewidth = 0.5) +
  geom_sf(data = br,
          color = "black",
          fill = "transparent",
          linewidth = 1) +
  scale_color_manual(values = c("darkred",
                                "black")) +
  coord_sf(xlim = c(-55.89844, -52.03084),
           ylim = c(-22.91829, -18.97941),
           expand = FALSE,
           label_graticule = "SWE") +
  ggimage::geom_image(data = tapirus,
                      aes(x, y, image = image),
                      size = 0.3) +
  labs(title = "<i>Tapirus terrestris</i> population deslocation along Brazlian Southwestern Region",
       subtitle = "Data Source: Fleming <i>et al.</i> (2022). Population-level inference for home-range<br>areas. <b>Methods in Ecology and Evolution</b><br>Image Source: IUCN Red List: <i>Tapirus terrestris</i> page",
       color = NULL,
       x = NULL,
       y = NULL) +
  ggspatial::annotation_scale(location = "bl",
                              text_face = "bold",
                              text_cex = 2,
                              text_col = "black",
                              unit_category = "metric",
                              bar_cols = c("black", "gold")) +
  theme_bw() +
  theme(axis.text = element_text(size = 15, color = "black"),
        legend.text = ggtext::element_markdown(size = 15, color = "black"),
        legend.position = "bottom",
        strip.text = element_text(size = 15, color = "black", face = "italic"),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(color = "black", linewidth = 1),
        plot.title = ggtext::element_markdown(size = 15, color = "black", hjust = 0.5),
        plot.subtitle = ggtext::element_markdown(size = 15,
                                                 color = "black", hjust = 0.5),
        plot.caption = ggtext::element_markdown(size = 15,
                                                 color = "black", hjust = 0.5)) +
  ggview::canvas(height = 10, width = 12)

mapa_principal

## Inset map ----

inset_map <- ggplot() +
  geom_sf(data = br,
          color = "black",
          fill = "gray90",
          linewidth = 0.5) +
  geom_rect(aes(xmin = -55.89844, xmax = -52.03084,
                ymin = -22.91829, ymax = -18.97941),
            color = "darkred",
            fill = "red",
            alpha = 0.3) +
  theme_void() +
  theme(legend.position = "none") +
  ggview::canvas(height = 10, width = 12)

inset_map

## Mapa final ----

cowplot::ggdraw(mapa_principal) +
  cowplot::draw_plot(inset_map,
                     x = 0.5,
                     y = 0.55,
                     height = 0.35,
                     width = 0.35) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapas/map_day7.png", height = 10, width = 12)
