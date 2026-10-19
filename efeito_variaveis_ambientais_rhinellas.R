# Pacotes ----

library(googledrive)

library(readxl)

library(tidyverse)

library(parzer)

library(geosphere)

library(sf)

library(adehabitatHR)

library(janitor)

library(reshape2)

library(ggview)

library(lmerTest)

library(performance)

library(lme4)

library(ggeffects)

library(DHARMa)

# Dados ----

## Dados de movimentação ----

### Baixar do Google Drive ----

googledrive::drive_auth("edson.nilton@ufpe.br")

Sys.getenv("pasta_artigo_rhinella") |>
  googledrive::drive_get() |>
  googledrive::drive_ls() |>
  dplyr::filter(name |> stringr::str_detect("Dados Rhinella")) |>
  dplyr::pull(name) |>
  purrr::map(
    \(arquivo){

      googledrive::drive_download(file = arquivo,
                                  path = paste0("./", arquivo),
                                  overwrite = TRUE)

    },
    .progress = TRUE)

### Importar ----

mov <- purrr::map_dfr(
  c("Rhinella crucifer",
    "Rhinella icterica"),
  \(sps){

    readxl::read_xlsx(paste0("Dados ",
                             sps,
                             ".xlsx")) |>
      dplyr::mutate(Espécie = sps)

    },
  .progress = TRUE)

mov

### Visualizar ----

mov

mov |> dplyr::glimpse()

### Completar coordenadas ----

mov <- mov |>
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat())

mov

mov |> dplyr::glimpse()

### Completar as coordenadas ----

mov_trat <- purrr::reduce(
  .x = 1:nrow(mov),
  .f = \(dados, linha){

    if(is.na(dados$Longitude[linha]) || is.na(dados$Longitude[linha])){

      coord <- geosphere::destPoint(
        p = c(dados$Longitude[linha - 1], dados$Latitude[linha - 1]),
        b = dados$`Angulo de virada`[linha - 1],
        d = dados$`Distância (m)`[linha - 1]
      )

      dados$Longitude[linha] <- coord[1]

      dados$Latitude[linha] <- coord[2]

    }

    dados

    },
  .init = mov)

mov_trat

### Transformar em shapefile ----

dados_rhinellas_sf <- mov_trat |>
  sf::st_as_sf(coords = c(2:3),
               crs = 4674)

dados_rhinellas_sf

## Dados ambientais ----

### Baixar do Google Drive ----

Sys.getenv("pasta_artigo_rhinella") |>
  googledrive::drive_get() |>
  googledrive::drive_ls() |>
  dplyr::filter(name |> stringr::str_detect("AMB")) |>
  dplyr::pull(name) |>
  purrr::map(
    \(arquivo){

      googledrive::drive_download(file = arquivo,
                                  path = paste0("./", arquivo),
                                  overwrite = TRUE)

    },
    .progress = TRUE)

### Importar ----

amb <- readxl::read_xlsx("./DADOS AMBIENTAIS COM AS 6 ESPÉCIES INDIVIDUAIS.xlsx")


### Visualizar ----

amb

amb |> dplyr::glimpse()

# Modelos lineares ----

## Calcular valores médios das variáveis ambientais para as espécies de Rhinella ----

amb_trat <- amb |>
  dplyr::summarise(dplyr::across(.cols = dplyr::where(is.numeric),
                                 .fns = ~.x |> mean()),
                   .by = INDIVIDUO) |>
  dplyr::rename("ID" = 1) |>
  dplyr::filter(ID |> stringr::str_detect("^R")) |>
  dplyr::mutate(Espécie = dplyr::case_when(

    ID |> stringr::str_detect("RI") ~ "Rhinella icterica",
    .default = "Rhinella crucifer"
    ))

amb_trat

## Calculara área de vida das espécies ----

area_rhinella <- purrr::map_dfr(
  c("Rhinella icterica", "Rhinella crucifer"),
  \(rhinella){

    id <- dados_rhinellas_sf |>
      dplyr::filter(Espécie == rhinella) |>
      dplyr::pull(ID) |>
      unique()

    purrr::map(
      id,
      \(ind){

        KDE95 <- dados_rhinellas_sf |>
          dplyr::filter(Espécie == rhinella & ID == ind) |>
          sf::as_Spatial() |>
          adehabitatHR::kernelUD(h = "href",
                                 extent = 3) |>
          adehabitatHR::getverticeshr(percent = 90) |>
          sf::st_as_sf() |>
          dplyr::mutate(Tipo = "KDE 95%",
                        Espécie = rhinella,
                        Indivíduo = ind)

        kde50 <- dados_rhinellas_sf |>
          dplyr::filter(Espécie == rhinella & ID == ind) |>
          sf::as_Spatial() |>
          adehabitatHR::kernelUD(h = "href",
                                 extent = 3) |>
          adehabitatHR::getverticeshr(percent = 50) |>
          sf::st_as_sf() |>
          dplyr::mutate(Tipo = "KDE 50%",
                        Espécie = rhinella,
                        Indivíduo = ind)

        dplyr::bind_rows(kde50, KDE95)

      }
    )

  },
  .progress = TRUE) |>
  dplyr::mutate(Tipo = Tipo |> forcats::fct_relevel(c("KDE 95%",
                                                      "KDE 50%")),
                area = geometry |>
                  sf::st_transform(crs = 31982) |>
                  sf::st_area() |>
                  as.numeric()) |>
  dplyr::rename("ID" = Indivíduo)

area_rhinella

## Multicolinearidade ----

cor_rhinellas <- purrr::map_dfr(
  c("Rhinella crucifer",
    "Rhinella icterica"),
  \(sps){

    cor_mat <- amb_trat |>
      dplyr::filter(Espécie == sps) |>
      dplyr::select_if(is.numeric) |>
      cor(method = "spearman") |>
      as.matrix()

    cor_mat[upper.tri(cor_mat)] <- NA

    cor_mat |>
      reshape2::melt() |>
      tidyr::drop_na() |>
      dplyr::filter(Var1 != Var2) |>
      dplyr::mutate(value = value |> round(2),
                    Espécie = sps)

  },
  .progress = TRUE)

cor_rhinellas

cor_rhinellas |>
  ggplot(aes(Var1, Var2, fill = value, label = value)) +
  geom_tile(color = "black", linewidth = 1) +
  geom_text(size = 8) +
  labs(x = NULL,
       y = NULL,
       fill = "Spearman correlation") +
  coord_equal() +
  facet_wrap(~Espécie) +
  scale_fill_gradientn(colours = c(viridis::viridis(n = 10) |> rev(),
                                   viridis::viridis(n = 10)),
                       limits = c(-1, 1),
                       guide = guide_colourbar(

                         title.position = "top",
                         title.hjust = 0.5,
                         frame.colour = "black",
                         ticks.colour = "black",
                         ticks.linewidth = 0.75,
                         barwidth = 30,
                         barheight = 1.5

                       )) +
  theme_bw() +
  theme(aspect.ratio = 1,
        axis.text = element_text(color = "black", size = 20),
        axis.text.x = element_text(angle = 90,
                                   hjust = 1, vjust = 0.5),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        legend.position = "bottom",
        strip.text = element_text(color = "black", size = 30,
                                  face = "italic"),
        strip.background = element_rect(color = "black",
                                        linewidth = 1.5),
        panel.border = element_rect(color = "black", linewidth = 1.5))

ggsave(filename = "multicolinearidade_amb_rhinella.png",
       height = 12,
       width = 16)

## Unir os dados ----

area_rhinella_amb <- area_rhinella |>
  dplyr::left_join(amb_trat |>
                     dplyr::select(-Espécie),
                   by = "ID")

area_rhinella_amb

## Modelos LM ----

### Criar modelos ----

modelos_lm <- purrr::pmap(
  tidyr::expand_grid(sps = c("Rhinella crucifer",
                             "Rhinella icterica"),
                     kde = c("KDE 50%", "KDE 95%"),
                     var = c("umidade_relativa_maxima",
                             "temperatura_maxima",
                             "temperatura_minima")),
  \(sps, kde, var){

    formula <- reformulate(var,
                           response = "area")

    lm(formula,
       data = area_rhinella_amb |>
         janitor::clean_names() |>
         dplyr::filter(especie == sps &
                         tipo == kde))

  },
  .progress = TRUE) |>
  setNames(tidyr::expand_grid(sps = c("Rhinella crucifer",
                                      "Rhinella icterica"),
                              kde = c("KDE 50%", "KDE 95%"),
                              var = c("humidade_relativa_maxima",
                                      "temperatura_maxima",
                                      "temperatura_minima")) |>
             dplyr::mutate(mod = paste(sps, kde, var, sep = "-")) |>
             dplyr::pull(mod))

modelos_lm

### Avaliar modelos ----

purrr::map(
  modelos_lm,
  \(modelo){

    modelo |>
      performance::check_normality() |>
      print()

    modelo |>
      performance::check_heteroscedasticity() |>
      print()

    modelo |>
      performance::check_model(check = c("qq",
                                         "normality",
                                         "homogeneity"))

  },
  .progress = TRUE)

## Modelos GLM ----

### Criar modelos ----

modelos_glm <- purrr::pmap(
  tidyr::expand_grid(sps = c("Rhinella crucifer",
                             "Rhinella icterica"),
                     kde = c("KDE 50%", "KDE 95%"),
                     var = c("umidade_relativa_maxima",
                             "temperatura_maxima",
                             "temperatura_minima")),
  \(sps, kde, var){

    formula <- reformulate(var,
                           response = "area")

    glm(formula,
        data = area_rhinella_amb |>
          janitor::clean_names() |>
          dplyr::filter(especie == sps &
                          tipo == kde),
        family = Gamma(link = "log"))

  },
  .progress = TRUE) |>
  setNames(tidyr::expand_grid(sps = c("Rhinella crucifer",
                                      "Rhinella icterica"),
                              kde = c("KDE 50%", "KDE 95%"),
                              var = c("humidade_relativa_maxima",
                                      "temperatura_maxima",
                                      "temperatura_minima")) |>
             dplyr::mutate(mod = paste(sps, kde, var, sep = "-")) |>
             dplyr::pull(mod))

modelos_glm

### Avaliar modelos ----

purrr::map(
  modelos_lm,
  \(modelo){

    modelo |>
      DHARMa::simulateResiduals(plot = TRUE)

  },
  .progress = TRUE)

## Estatísticas dos modelos ----

### Modelos LMs ----

purrr::imap_dfr(
  modelos_lm,
  \(modelo, id){

    modelo |>
      broom::tidy() |>
      dplyr::mutate(modelo = id)

  },
  .progress = TRUE) |>
  dplyr::filter(!term |> stringr::str_detect("Intercept")) |>
  dplyr::mutate(p.value = dplyr::case_when(

    p.value < 0.01 ~ "< 0.01",
    .default = p.value |> round(2) |> as.character()

  )) |>
  as.data.frame()

qt(p = 0.05, df = 9, lower.tail = FALSE)

purrr::map(
  modelos_lm,
  \(modelo){

    modelo |> summary() |> print()

  },
  .progress = TRUE)

### Modelos GLMs ----

purrr::imap_dfr(
  modelos_glm,
  \(modelo, id){

    modelo |>
      broom::tidy() |>
      dplyr::mutate(modelo = id)

  },
  .progress = TRUE) |>
  dplyr::filter(!term |> stringr::str_detect("Intercept")) |>
  dplyr::mutate(p.value = dplyr::case_when(

    p.value < 0.01 ~ "< 0.01",
    .default = p.value |> round(2) |> as.character()

  )) |>
  as.data.frame()

qt(p = 0.05, df = 9, lower.tail = FALSE)

purrr::map(
  modelos_glm,
  \(modelo){

    modelo |> summary() |> print()

  },
  .progress = TRUE)
