# Desafio ----

# points

# Pacotes ----

library(rnaturalearth)

library(tidyverse)

library(dismo)

library(magrittr)

# Dados ----

## Shapefile dos países ----

### Importando ----

paises <- rnaturalearth::countries110 |>
  dplyr::filter(CONTINENT |> stringr::str_detect("America"))

### Visualizando ----

paises

paises |> dplyr::glimpse()

ggplot() +
  geom_sf(data = paises)

### Lista dos continentes e subregiões ----

paises$SUBUNIT |> unique()

### Filtrando para a América Latina ----

america_latina <- paises |>
  dplyr::filter(SUBREGION %in% c("South America", "Central America", "Caribbean"))

america_latina

ggplot() +
  geom_sf(data = paises, color = "black", fill = "royalblue") +
  geom_sf(data = america_latina, color = "black", fill = "gold")

## Distribuição ----

### Lista de espécies ----

especies <- c(paste0("Caiman ", c("latirostris", "yacare", "crocodilus")),
              paste0("Paleosuchus ", c("palpebrosus", "trigonatus")),
              "Melanosuchus niger")

especies

### Loop para baixar os dados de ocorrência ----

baixar_correncias <- function(especies){

  ocorrencias <- dismo::gbif(genus = especies |> stringr::word(1),
                             species = especies,
                             removeZeros = TRUE)

  ocorrencias %<>%
    dplyr::mutate(Espécie = especies)

  assign(paste0("ocorrencias_", especies |> stringr::word(2)),
         ocorrencias,
         envir = globalenv())

}

purrr::map(especies, baixar_correncias)
