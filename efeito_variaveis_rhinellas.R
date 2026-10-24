# Pacotes ----

library(googledrive)

library(readxl)

library(tidyverse)

library(parzer)

library(geosphere)

library(sf)

library(adehabitatHR)

library(ggbeeswarm)

library(janitor)

library(reshape2)

library(performance)

library(DHARMa)

library(marginaleffects)

library(flextable)

library(ggh4x)

library(glmmTMB)

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

### Converter coordenadas ----

mov <- mov |>
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat())

mov

mov |> dplyr::glimpse()

### Completar as coordenadas ----

mov_trat <- purrr::reduce(
  .init = mov,
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

  })

mov_trat

### Transformar em shapefile ----

dados_rhinellas_sf <- mov_trat |>
  sf::st_as_sf(coords = c(2:3),
               crs = 4674)

dados_rhinellas_sf

ggplot() +
  geom_sf(data = dados_rhinellas_sf)

## Dados morfológicos ----

### Baixar do Google Drive ----

Sys.getenv("pasta_artigo_rhinella") |>
  googledrive::drive_get() |>
  googledrive::drive_ls() |>
  dplyr::filter(name |> stringr::str_detect("médias")) |>
  dplyr::pull(name) |>
  purrr::map(
    \(arquivo){

      googledrive::drive_download(file = arquivo,
                                  path = paste0("./", arquivo),
                                  overwrite = TRUE)

    },
    .progress = TRUE)

### Importar ----

morf <- purrr::map_dfr(
  c("Rhinella crucifer",
    "Rhinella icterica"),
  \(sps){

    readxl::read_xlsx("./médias das espécies.xlsx",
                      sheet = sps) |>
      dplyr::mutate(Espécie = sps)

    },
  .progress = TRUE)

### Visualizar ----

morf

morf |> dplyr::glimpse()

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

### Calcular valores médios das variáveis ambientais para as espécies de Rhinella ----

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

# Modelos lineares ----

## Calcular área de vida ----

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
                                                      "KDE 50%"))) |>
  dplyr::mutate(area = geometry |>
                  sf::st_transform(crs = 31982) |>
                  sf::st_area() |>
                  as.numeric())

area_rhinella

## Calcular tortuosidade e trajeto percorrido ----

tort_traj_rhinella <- purrr::map_dfr(
  c("Rhinella icterica", "Rhinella crucifer"),
  \(sps){

    id <- dados_rhinellas_sf |>
      dplyr::filter(Espécie == sps) |>
      dplyr::pull(ID) |>
      unique()

    purrr::map(
      id,
      \(id){

        traj <- dados_rhinellas_sf |>
          dplyr::filter(ID == id) |>
          dplyr::summarise(do.union = FALSE) |>
          sf::st_cast("LINESTRING") |>
          sf::st_transform(crs = 31985) |>
          sf::st_length() |>
          as.numeric()

        min_traj <- dados_rhinellas_sf |>
          dplyr::filter(ID == id) |>
          dplyr::slice(1, dplyr::n()) |>
          dplyr::summarise(do.union = FALSE) |>
          sf::st_cast("LINESTRING") |>
          sf::st_transform(crs = 31985) |>
          sf::st_length() |>
          as.numeric()

        tibble::tibble(ID = id,
                       trajeto_percorrido = traj,
                       tortuosidade = (min_traj / traj))

        }
      )

    },
  .progress = )

tort_traj_rhinella

## Multicolinaridade ----

### Dados morfológicos ----

cor_rhinellas_morf <- purrr::map_dfr(
  c("Rhinella crucifer",
    "Rhinella icterica"),
  \(sps){

    cor_mat <- morf |>
      dplyr::filter(Espécie == sps) |>
      dplyr::select_if(is.numeric) |>
      dplyr::rename("Body mass (g)" = "peso",
                    "Hindlimb proportion" = "Proporção CMA",
                    "Forelimb proportion" = "Proporção CMP",
                    "Hindlimb length" = "CMA",
                    "Forelimb length" = "CMP",
                    "SVL" = "crc") |>
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

cor_rhinellas_morf

cor_rhinellas_morf |>
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

ggsave(filename = "multicolinearidade_morf_rhinella.png",
       height = 12,
       width = 16)

"multicolinearidade_morf_rhinella.png" |>
  googledrive::drive_upload(path = Sys.getenv("pasta_artigo_rhinella"),
                            name = "multicolinearidade_morf_rhinella.png",
                            overwrite = TRUE)

### Dados ambientais ----

cor_rhinellas_amb <- purrr::map_dfr(
  c("Rhinella crucifer",
    "Rhinella icterica"),
  \(sps){

    cor_mat <- amb_trat |>
      dplyr::filter(Espécie == sps) |>
      dplyr::select_if(is.numeric) |>
      dplyr::rename("Max temperature (°C)" = "Temperatura máxima",
                    "Min temperature (°C)" = "Temperatura mínima",
                    "Mean relative humidity" = "Umidade Relativa -média",
                    "Max relative humidity" = "Umidade Relativa -máxima",
                    "Min relative humidity" = "Umidade Relativa - mínima",
                    "Precipitation (mm)" = "Precipitação",
                    "Mean temperature (°C)" = "Temperatura média") |>
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

cor_rhinellas_amb

cor_rhinellas_amb |>
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

"multicolinearidade_amb_rhinella.png" |>
  googledrive::drive_upload(path = Sys.getenv("pasta_artigo_rhinella"),
                            name = "multicolinearidade_amb_rhinella.png",
                            overwrite = TRUE)

## unir os dados ----

vars_rhinella <- area_rhinella |>
  sf::st_drop_geometry() |>
  dplyr::rename("ID" = Indivíduo) |>
  dplyr::select(ID, Tipo, area) |>
  dplyr::left_join(amb_trat,
                   by = "ID") |>
  dplyr::left_join(morf |>
                     dplyr::select(-Espécie),
                   by = "ID") |>
  dplyr::left_join(tort_traj_rhinella,
                   by = "ID")

vars_rhinella

## Áreas de KDE ----

### LM ----

#### Criar modelos ----

modelos_lm <- purrr::pmap(
  tidyr::expand_grid(
    sps = c("Rhinella crucifer",
            "Rhinella icterica"),
    kde = c("KDE 50%",
            "KDE 95%"),
    var = c("peso",
            "proporcao_cmp",
            "umidade_relativa_maxima",
            "temperatura_maxima",
            "temperatura_minima")),
  \(sps, kde, var){

    formula <- paste0("area ~ ", var) |>
          as.formula()

    lm(formula,
       data = vars_rhinella |>
         dplyr::filter(Espécie == sps &
                         Tipo == kde) |>
         janitor::clean_names())

    },
  .progress = TRUE) |>
  setNames(tidyr::expand_grid(sps = c("Rhinella crucifer",
                                      "Rhinella icterica"),
                              kde = c("KDE 50%",
                                      "KDE 95%"),
                              var = c("peso",
                                      "proporcao_cmp",
                                      "umidade_relativa_maxima",
                                      "temperatura_maxima",
                                      "temperatura_minima")) |>
             dplyr::mutate(mod = paste(sps, kde, var, sep = "_")) |>
             dplyr::pull(mod))

modelos_lm

#### Avaliar modelos ----

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

### GLM ----

#### Criar modelos GLM ----

modelos_glm <- purrr::pmap(
  tidyr::expand_grid(
    sps = c("Rhinella crucifer",
            "Rhinella icterica"),
    kde = c("KDE 50%",
            "KDE 95%"),
    var = c("peso",
            "proporcao_cmp",
            "umidade_relativa_maxima",
            "temperatura_maxima",
            "temperatura_minima")),
  \(sps, kde, var){

    formula <- paste0("area ~ ", var) |>
          as.formula()

    tryCatch(
      glm(formula,
          data = vars_rhinella |>
            dplyr::filter(Espécie == sps &
                            Tipo == kde) |>
            janitor::clean_names(),
          family = Gamma(link = "log")),
      error = \(e){
        glm(formula,
            data = vars_rhinella |>
              dplyr::filter(Espécie == sps &
                              Tipo == kde) |>
              janitor::clean_names(),
            family = Gamma(link = "log"),
            start = lm(formula,
                       data = vars_rhinella |>
                         dplyr::filter(Espécie == sps &
                                         Tipo == kde) |>
                         janitor::clean_names()) |>
              coef())
        }
      )

    },
  .progress = TRUE) |>
  setNames(tidyr::expand_grid(sps = c("Rhinella crucifer",
                                      "Rhinella icterica"),
                              kde = c("KDE 50%",
                                      "KDE 95%"),
                              var = c("peso",
                                      "proporcao_cmp",
                                      "umidade_relativa_maxima",
                                      "temperatura_maxima",
                                      "temperatura_minima")) |>
             dplyr::mutate(mod = paste(sps, kde, var, sep = "_")) |>
             dplyr::pull(mod))

modelos_glm

#### Avaliar modelos ----

purrr::map(
  modelos_glm,
  \(modelo){

    modelo |>
      DHARMa::simulateResiduals(plot = TRUE)

    },
  .progress = TRUE)

### Estatísticas dos modelos ----

#### Modelos LMs ----

purrr::imap_dfr(
  modelos_lm,
  \(modelo, id){

    modelo |>
      marginaleffects::avg_comparisons() |>
      tibble::as_tibble() |>
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

#### Modelos GLMs ----

purrr::imap_dfr(
  modelos_glm,
  \(modelo, id){

    modelo |>
      marginaleffects::avg_comparisons() |>
      tibble::as_tibble() |>
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

### Tabela dos valores ----

tabela_flex <- purrr::map_dfr(
  modelos_glm,
  \(modelo){

    modelo |>
      marginaleffects::avg_comparisons() |>
      tibble::as_tibble()|>
      dplyr::mutate(R2 = modelo |>
                      performance::r2_kullback(adjust = TRUE) |>
                      as.numeric() |>
                      round(3))

  },
  .progress = TRUE) |>
  dplyr::select(-c(2, 7:9)) |>
  dplyr::filter(!term |> stringr::str_detect("Intercept")) |>
  dplyr::mutate(p.value = dplyr::case_when(

    p.value < 0.01 ~ "< 0.01",
    .default = p.value |> round(2) |> as.character()

  )) |>
  dplyr::mutate(Species = c("Rhinella crucifer",
                          "Rhinella icterica") |>
                  rep(each = 10),
                KDE = c("50%", "95%") |>
                  rep(2) |>
                  rep(5),
                .before = 1) |>
  as.data.frame() |>
  dplyr::mutate(term = dplyr::case_when(

    term == "peso" ~ "Body mass (g)",
    term == "proporcao_cmp" ~ "Forelimb proportion",
    term == "umidade_relativa_maxima" ~ "Max relative precipitation (mm)",
    term == "temperatura_maxima" ~ "Max temperature (°C)",
    term == "temperatura_minima" ~ "Min temperature (°C)"

    ),
                estimate = estimate |> round(2),
                std.error = std.error |> round(2),
                statistic = statistic |> round(2)) |>
  dplyr::rename("Predictor" = 3,
                "t" = 6,
                "p" = 7,
                "pseudo-R²" = 8) |>
  tidyr::unite(4:5,
               sep = " ± ",
               col = "β1 ± SE") |>
  flextable::flextable() |>
  flextable::align(align = "center",
                   part = "all") |>
  flextable::width(j = 1, width = 1.5) |>
  flextable::width(j = 3, width = 1.25) |>
  flextable::width(j = 4, width = 1.5) |>
  flextable::width(j = 7, width = 1.5) |>
  flextable::italic(j = 1, part = "body") |>
  flextable::bg(i = ~abs(t) > 1.833113,
                bg = "gray")

tabela_flex

tabela_flex |>
  flextable::save_as_docx(path = "tabela_sts_morf_rhinella.docx")

"tabela_sts_morf_rhinella.docx" |>
  googledrive::drive_upload(path = Sys.getenv("pasta_artigo_rhinella"),
                            name = "tabela_sts_morf_rhinella.docx",
                            overwrite = TRUE)

### Gráfico ----

#### Rhinella crucifer ----

vars_rhinella |>
  dplyr::filter(Espécie == "Rhinella crucifer") |>
  tidyr::pivot_longer(cols = c(peso,
                               `Proporção CMP`,
                               `Umidade Relativa -máxima`,
                               `Temperatura máxima`,
                               `Temperatura mínima`
                               ),
                      names_to = "Preditor",
                      values_to = "Predictor value") |>
  dplyr::mutate(Preditor  = dplyr::case_when(

    Preditor == "peso" ~ "Body mass (g)",
    Preditor == "Proporção CMP" ~ "Forelimb proportion",
    Preditor == "Umidade Relativa -máxima" ~ "Max relative precipitation (mm)",
    Preditor == "Temperatura máxima" ~ "Max temperature (°C)",
    Preditor == "Temperatura mínima" ~ "Min temperature (°C)"

    )) |>
  ggplot(aes(area, `Predictor value`)) +
  geom_point(size = 5) +
  geom_smooth(data = . %>%
                dplyr::filter(Preditor %in% c("Body mass (g)",
                                              "Min temperature (°C)")),
              method = "lm",
              se = FALSE) +
  ggh4x::facet_grid2(Tipo ~ Preditor,
                     scales = "free",
                     independent = "all") +
  labs(x = "Area (m²)") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(color = "black", size = 30),
        strip.text.x.top = element_text(color = "black", size = 11.5))

ggsave(filename = "efeito_morf_rhinella_crucifer.png",
       height = 12,
       width = 16)

"efeito_morf_rhinella_crucifer.png" |>
  googledrive::drive_upload(path = Sys.getenv("pasta_artigo_rhinella"),
                            name = "efeito_morf_rhinella_crucifer.png",
                            overwrite = TRUE)

#### Rhinella icterica ----

vars_rhinella |>
  dplyr::filter(Espécie == "Rhinella icterica") |>
  tidyr::pivot_longer(cols = c(peso,
                               `Proporção CMP`,
                               `Umidade Relativa -máxima`,
                               `Temperatura máxima`,
                               `Temperatura mínima`
  ),
  names_to = "Preditor",
  values_to = "Predictor value") |>
  dplyr::mutate(Preditor  = dplyr::case_when(

    Preditor == "peso" ~ "Body mass (g)",
    Preditor == "Proporção CMP" ~ "Forelimb proportion",
    Preditor == "Umidade Relativa -máxima" ~ "Max relative precipitation (mm)",
    Preditor == "Temperatura máxima" ~ "Max temperature (°C)",
    Preditor == "Temperatura mínima" ~ "Min temperature (°C)"

  )) |>
  ggplot(aes(area, `Predictor value`)) +
  geom_point(size = 5) +
  geom_smooth(data = . %>%
                dplyr::filter(Preditor %in% c("Body mass (g)",
                                              "Min temperature (°C)")),
              method = "lm",
              se = FALSE) +
  ggh4x::facet_grid2(Tipo ~ Preditor,
                     scales = "free",
                     independent = "all") +
  labs(x = "Area (m²)") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(color = "black", size = 30),
        strip.text.x.top = element_text(color = "black", size = 11.5))

ggsave(filename = "efeito_morf_rhinella_icterica.png",
       height = 12,
       width = 16)

"efeito_morf_rhinella_icterica.png" |>
  googledrive::drive_upload(path = Sys.getenv("pasta_artigo_rhinella"),
                            name = "efeito_morf_rhinella_icterica.png",
                            overwrite = TRUE)

### Modelo Rhinella icterica sem os outliers ----

#### Identifcar os outliers ----

medias_areas <- vars_rhinella |>
  dplyr::summarise(sd = area |> sd(),
                   area = area |> mean(),
                   .by = c(Espécie, Tipo))

medias_areas

vars_rhinella |>
  ggplot(aes(Espécie, area, fill = Tipo)) +
  ggbeeswarm::geom_quasirandom(shape = 21, color = "black", size = 7,
                               dodge.width = 0.7,
                               stroke = 1.5) +
  geom_errorbar(data = medias_areas,
                aes(x = Espécie, y = area,
                    ymin = area - sd, ymax = area + sd,
                    group = Tipo),
                linewidth = 1.25,
                width = 0.35,
                position = position_dodge(width = 0.7)) +
  geom_point(data = medias_areas,
             aes(x = Espécie, y = area),
             position = position_dodge(width = 0.7),
             size = 5,
             show.legend = FALSE) +
  scale_fill_manual(values = c("orange", "royalblue")) +
  labs(x = "Species",
       y = "Area (m²)",
       fill = NULL) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.text.x = element_text(face = "italic"),
        axis.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", linewidth = 1))

ggsave(filename = "dispersao_area_de_vida.png",
       height = 12,
       width = 12)

"dispersao_area_de_vida.png" |>
  googledrive::drive_upload(path = Sys.getenv("pasta_artigo_rhinella"),
                            name = "dispersao_area_de_vida.png",
                            overwrite = TRUE)

id_ri_rem <- vars_rhinella |>
  dplyr::filter(Espécie == "Rhinella icterica") |>
  dplyr::slice_max(area, n = 2, by = Tipo) |>
  dplyr::select(ID, Tipo, Espécie) |>
  dplyr::pull(ID) |>
  unique()

id_ri_rem

#### Criar modelos GLM para o dataset sem os indivíduos ----

modelos_glm_rm <- purrr::pmap(
  tidyr::expand_grid(sps = c("Rhinella crucifer",
                              "Rhinella icterica"),
                     kde = c("KDE 50%",
                             "KDE 95%"),
                     var = c("peso",
                             "proporcao_cmp",
                             "umidade_relativa_maxima",
                             "temperatura_maxima",
                             "temperatura_minima")),
  \(sps, kde, var){

    formula <- paste0("area ~ ", var) |>
      as.formula()

    tryCatch(
      glm(formula,
          data = vars_rhinella |>
            dplyr::filter(Espécie == sps &
                            Tipo == kde &
                            ID %notin% id_ri_rem) |>
            janitor::clean_names(),
          family = Gamma(link = "log")),
      error = \(e){

        glm(formula,
            data = vars_rhinella |>
              dplyr::filter(Espécie == "Rhinella icterica" &
                              Tipo == kde &
                              ID %notin% id) |>
              janitor::clean_names(),
            family = Gamma(link = "log"),
            start = lm(formula,
                       data = vars_rhinella |>
                         dplyr::filter(Espécie == "Rhinella icterica" &
                                         Tipo == kde &
                                         ID %notin% id) |>
                         janitor::clean_names()) |>
              coef())

      }
    )

  },
  .progress = TRUE) |>
  setNames(tidyr::expand_grid(sps = c("Rhinella crucifer",
                                      "Rhinella icterica"),
                              kde = c("KDE 50%",
                                      "KDE 95%"),
                              var = c("peso",
                                      "proporcao_cmp",
                                      "umidade_relativa_maxima",
                                      "temperatura_maxima",
                                      "temperatura_minima")) |>
             dplyr::mutate(mod = paste(sps, kde, var, sep = "_")) |>
             dplyr::pull(mod))

modelos_glm_rm

#### Pressupostos dos modelos ----

purrr::map(
  modelos_glm_rm,
  \(modelo){

    modelo |>
      DHARMa::simulateResiduals(plot = TRUE)

  },
  .progress = TRUE)

#### Estatísticas dos modelos ----

purrr::map(
  modelos_glm_rm,
  \(modelo){

    modelo |> summary()

    },
  .progress = TRUE)

purrr::imap_dfr(
  modelos_glm_rm,
  \(modelo, id){

    modelo |>
      marginaleffects::avg_comparisons() |>
      tibble::as_tibble() |>
      dplyr::mutate(statistic = modelo |>
                      broom::tidy() |>
                      dplyr::pull(statistic) |>
                      purrr::pluck(2),
                    p.value = modelo |>
                      broom::tidy() |>
                      dplyr::pull(p.value) |>
                      purrr::pluck(2),
                    modelo = id)

  },
  .progress = TRUE) |>
  dplyr::mutate(p.value = dplyr::case_when(

    p.value < 0.01 ~ "< 0.01",
    .default = p.value |> round(2) |> as.character()

  )) |>
  dplyr::select(-c(2, 7:9)) |>
  as.data.frame()

qt(p = 0.05, df = 12, lower.tail = FALSE)

r2_icterica <- purrr::imap_dfr(
  modelos_glm_rm,
  \(modelo, id){

    tibble::tibble(Model = id,
                   R2 = modelo |>
                     performance::r2_mcfadden() |>
                     as.numeric() %>%
                     .[2] |>
                     round(3))

    },
  .progress = TRUE) |>
  dplyr::rename("R²" = 2)

r2_icterica

#### Tabela das estatatísticas ----

tabela_flex_rm <- purrr::map_dfr(
  modelos_glm_rm,
  \(modelo){

    modelo |>
      marginaleffects::avg_comparisons() |>
      tibble::as_tibble() |>
      dplyr::mutate(statistic = modelo |>
                      broom::tidy() |>
                      dplyr::pull(statistic) |>
                      purrr::pluck(2),
                    p.value = modelo |>
                      broom::tidy() |>
                      dplyr::pull(p.value) |>
                      purrr::pluck(2),
                    R2 = modelo |>
                      performance::r2_kullback() |>
                      round(3))

  },
  .progress = TRUE) |>
  dplyr::mutate(p.value = dplyr::case_when(

    p.value < 0.01 ~ "< 0.01",
    .default = p.value |> round(2) |> as.character()

  )) |>
  dplyr::select(-c(2, 7:9)) |>
  dplyr::mutate(Species = c("Rhinella crucifer",
                            "Rhinella icterica") |>
                  rep(each = 10),
                KDE = c("50%", "95%") |>
                  rep(2) |>
                  rep(5),
                .before = 1) |>
  as.data.frame() |>
  dplyr::mutate(term = dplyr::case_when(

    term == "peso" ~ "Body mass (g)",
    term == "proporcao_cmp" ~ "Forelimb proportion",
    term == "umidade_relativa_maxima" ~ "Max relative precipitation (mm)",
    term == "temperatura_maxima" ~ "Max temperature (°C)",
    term == "temperatura_minima" ~ "Min temperature (°C)"

  ),
  estimate = estimate |> round(2),
  std.error = std.error |> round(2),
  statistic = statistic |> round(2)) |>
  dplyr::rename("Predictor" = 3,
                "t" = 6,
                "p" = 7,
                "pseudo-R²" = 8) |>
  tidyr::unite(4:5,
               sep = " ± ",
               col = "ME ± SE") |>
  flextable::flextable() |>
  flextable::align(align = "center",
                   part = "all") |>
  flextable::width(j = 1, width = 1.5) |>
  flextable::width(j = 3, width = 1.25) |>
  flextable::width(j = 4, width = 1.5) |>
  flextable::width(j = 7, width = 1.5) |>
  flextable::italic(j = 1, part = "body") |>
  flextable::bg(i = ~abs(t) > 1.833113 &
                  readr::parse_number(p) < 0.05,
                bg = "gray")

tabela_flex_rm

tabela_flex_rm |>
  flextable::save_as_docx(path = "tabela_area_rhinella_rm.docx")

"tabela_sts_rhinella_rm.docx" |>
  googledrive::drive_upload(path = Sys.getenv("pasta_artigo_rhinella"),
                            name = "tabela_area_rhinella_rm.docx",
                            overwrite = TRUE)

#### Gráfico ----

vars_rhinella |>
  dplyr::filter(ID %notin% id_ri_rem) |>
  tidyr::pivot_longer(cols = c(peso,
                               `Proporção CMP`,
                               `Umidade Relativa -máxima`,
                               `Temperatura máxima`,
                               `Temperatura mínima`),
                      names_to = "Preditor",
                      values_to = "Predictor value") |>
  dplyr::mutate(Preditor  = dplyr::case_when(

    Preditor == "peso" ~ "Body mass (g)",
    Preditor == "Proporção CMP" ~ "Forelimb proportion",
    Preditor == "Umidade Relativa -máxima" ~ "Max relative precipitation (mm)",
    Preditor == "Temperatura máxima" ~ "Max temperature (°C)",
    Preditor == "Temperatura mínima" ~ "Min temperature (°C)"

    ),
    area = area |> log()) |>
  dplyr::left_join(tabela_flex_rm$body$dataset |>
                     dplyr::rename("Espécie" = 1,
                                   "Tipo" = 2,
                                   "Preditor" = 3) |>
                     dplyr::mutate(Tipo = paste0("KDE ", Tipo),
                                   p = p |> readr::parse_number()) |>
                     dplyr::select(1, 2, 3, 5, 6),
                   by = c("Espécie", "Tipo", "Preditor")) |>
  ggplot(aes(`Predictor value`, area, color = Tipo, fill = Tipo)) +
  geom_point(size = 5,
             shape = 21,
             color = "black",
             stroke = 1.5) +
  geom_smooth(data = . %>%
                dplyr::filter(abs(t) > 1.883 &
                                p < 0.05),
              method = "lm",
              se = FALSE) +
  ggh4x::facet_grid2(Preditor ~ Espécie,
                     scales = "free",
                     independent = "all") +
  scale_color_manual(values = c("orange4", "royalblue4")) +
  scale_fill_manual(values = c("orange", "royalblue")) +
  labs(y = "Log 10 Area (m²)",
       color = NULL,
       fill = NULL) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(color = "black", size = 30),
        strip.text.x.top = element_text(face = "italic"),
        strip.text.y.right = element_text(size = 16.5))

ggsave(filename = "efeito_area_rhinella_rm.png",
       height = 20,
       width = 14)

"efeito_area_rhinella_rm.png" |>
  googledrive::drive_upload(path = Sys.getenv("pasta_artigo_rhinella"),
                            name = "efeito_area_rhinella_rm.png",
                            overwrite = TRUE)

## Comprimento do trajeto ----

### Identificar outliers ----

medias_trajeto <- vars_rhinella |>
  dplyr::distinct(ID, .keep_all = TRUE) |>
  dplyr::summarise(sd = trajeto_percorrido |> sd(),
                   trajeto_percorrido = trajeto_percorrido |> mean(),
                   .by = c(Espécie, Tipo))

medias_trajeto

vars_rhinella |>
  dplyr::distinct(ID, .keep_all = TRUE) |>
  ggplot(aes(Espécie, trajeto_percorrido)) +
  ggbeeswarm::geom_quasirandom(color = "black",
                               size = 7,
                               width = 0.25,
                               stroke = 1.5) +
  geom_errorbar(data = medias_trajeto,
                aes(x = Espécie, y = trajeto_percorrido,
                    ymin = trajeto_percorrido - sd, ymax = trajeto_percorrido + sd,
                    group = Tipo),
                linewidth = 1.25,
                width = 0.35,
                color = "blue") +
  geom_point(data = medias_trajeto,
             aes(x = Espécie, y = trajeto_percorrido),
             size = 7,
             show.legend = FALSE,
             color = "red") +
  labs(x = "Species",
       y = "Runned track (m)",
       fill = NULL) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.text.x = element_text(face = "italic"),
        axis.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", linewidth = 1))

ggsave(filename = "dispersao_trajeto.png",
       height = 12,
       width = 12)

"dispersao_trajeto.png" |>
  googledrive::drive_upload(path = Sys.getenv("pasta_artigo_rhinella"),
                            name = "dispersao_trajeto.png",
                            overwrite = TRUE)

id_ri_rem_traj <- vars_rhinella |>
  dplyr::filter(Espécie == "Rhinella icterica") |>
  dplyr::slice_max(trajeto_percorrido, n = 2, by = Tipo) |>
  dplyr::select(ID, Tipo, Espécie) |>
  dplyr::pull(ID) |>
  unique()

id_ri_rem_traj

### LM ----

#### Criar modelos ----

modelos_lm_traj <- purrr::pmap(
  tidyr::expand_grid(
    sps = c("Rhinella crucifer",
            "Rhinella icterica"),
    var = c("peso",
            "proporcao_cmp",
            "umidade_relativa_maxima",
            "temperatura_maxima",
            "temperatura_minima")),
  \(sps, var){

    formula <- paste0("trajeto_percorrido ~ ", var) |>
      as.formula()

    lm(formula,
       data = vars_rhinella |>
         dplyr::filter(Espécie == sps &
                         ID %notin% id_ri_rem_traj) |>
         janitor::clean_names())

    },
  .progress = TRUE) |>
  setNames(tidyr::expand_grid(sps = c("Rhinella crucifer",
                                      "Rhinella icterica"),
                              var = c("peso",
                                      "proporcao_cmp",
                                      "umidade_relativa_maxima",
                                      "temperatura_maxima",
                                      "temperatura_minima")) |>
             dplyr::mutate(mod = paste(sps, var, sep = "_")) |>
             dplyr::pull(mod))

modelos_lm_traj

#### Avaliar modelos ----

purrr::map(
  modelos_lm_traj,
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

### GLM ----

#### Criar modelos ----

modelos_glm_traj <- purrr::pmap(
  tidyr::expand_grid(
    sps = c("Rhinella crucifer",
            "Rhinella icterica"),
    var = c("peso",
            "proporcao_cmp",
            "umidade_relativa_maxima",
            "temperatura_maxima",
            "temperatura_minima")),
  \(sps, var){

    formula <- paste0("trajeto_percorrido ~ ", var) |>
      as.formula()

    glm(formula,
        data = vars_rhinella |>
          dplyr::filter(Espécie == sps &
                          ID %notin% id_ri_rem_traj) |>
          janitor::clean_names(),
        family = Gamma(link = "log"))

    },
  .progress = TRUE) |>
  setNames(tidyr::expand_grid(sps = c("Rhinella crucifer",
                                      "Rhinella icterica"),
                              var = c("peso",
                                      "proporcao_cmp",
                                      "umidade_relativa_maxima",
                                      "temperatura_maxima",
                                      "temperatura_minima")) |>
             dplyr::mutate(mod = paste(sps, var, sep = "_")) |>
             dplyr::pull(mod))

modelos_glm_traj

#### Avaliar modelos ----

purrr::map(
  modelos_glm_traj,
  \(modelo){

    modelo |>
      DHARMa::simulateResiduals(plot = TRUE)

  },
  .progress = TRUE)

### Estatísticas dos modelos ----

#### Modelos LMs ----

purrr::imap_dfr(
  modelos_lm_traj,
  \(modelo, id){

    modelo |>
      marginaleffects::avg_comparisons() |>
      tibble::as_tibble() |>
      dplyr::mutate(statistic = modelo |>
                      broom::tidy() |>
                      dplyr::pull(statistic) |>
                      purrr::pluck(2),
                    p.value = modelo |>
                      broom::tidy() |>
                      dplyr::pull(p.value) |>
                      purrr::pluck(2),
                    modelo = id)

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
  modelos_lm_traj,
  \(modelo){

    modelo |> summary() |> print()

  },
  .progress = TRUE)

#### Modelos GLMs ----

purrr::imap_dfr(
  modelos_glm_traj,
  \(modelo, id){

    modelo |>
      marginaleffects::avg_comparisons() |>
      tibble::as_tibble() |>
      dplyr::mutate(statistic = modelo |>
                      broom::tidy() |>
                      dplyr::pull(statistic) |>
                      purrr::pluck(2),
                    p.value = modelo |>
                      broom::tidy() |>
                      dplyr::pull(p.value) |>
                      purrr::pluck(2),
                    modelo = id)

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
  modelos_glm_traj,
  \(modelo){

    modelo |> summary() |> print()

  },
  .progress = TRUE)

### Tabela dos valores ----

tabela_flex_traj <- purrr::map_dfr(
  modelos_glm_traj,
  \(modelo){

    modelo |>
      marginaleffects::avg_comparisons() |>
      tibble::as_tibble() |>
      dplyr::mutate(statistic = modelo |>
                      broom::tidy() |>
                      dplyr::pull(statistic) |>
                      purrr::pluck(2),
                    p.value = modelo |>
                      broom::tidy() |>
                      dplyr::pull(p.value) |>
                      purrr::pluck(2),
                    R2 = modelo |>
                      performance::r2_kullback() |>
                      round(3))

  },
  .progress = TRUE) |>
  dplyr::mutate(p.value = dplyr::case_when(

    p.value < 0.01 ~ "< 0.01",
    .default = p.value |> round(2) |> as.character()

  )) |>
  dplyr::select(-c(2, 7:9)) |>
  dplyr::mutate(Species = c("Rhinella crucifer",
                            "Rhinella icterica") |>
                  rep(each = 5),
                .before = 1) |>
  as.data.frame() |>
  dplyr::mutate(term = dplyr::case_when(

    term == "peso" ~ "Body mass (g)",
    term == "proporcao_cmp" ~ "Forelimb proportion",
    term == "umidade_relativa_maxima" ~ "Max relative precipitation (mm)",
    term == "temperatura_maxima" ~ "Max temperature (°C)",
    term == "temperatura_minima" ~ "Min temperature (°C)"

  ),
  estimate = estimate |> round(2),
  std.error = std.error |> round(2),
  statistic = statistic |> round(2)) |>
  dplyr::rename("Predictor" = 2,
                "t" = 5,
                "p" = 6,
                "pseudo-R²" = 7) |>
  tidyr::unite(3:4,
               sep = " ± ",
               col = "ME ± SE") |>
  flextable::flextable() |>
  flextable::align(align = "center",
                   part = "all") |>
  flextable::width(j = 1, width = 1.5) |>
  flextable::width(j = 2, width = 1.25) |>
  flextable::width(j = 3, width = 1.5) |>
  flextable::width(j = 6, width = 1.5) |>
  flextable::italic(j = 1, part = "body") |>
  flextable::bg(i = ~abs(t) > 1.833113 &
                  readr::parse_number(p) < 0.05,
                bg = "gray")

tabela_flex_traj

tabela_flex_traj |>
  flextable::save_as_docx(path = "tabela_sts_traj_rhinella.docx")

"tabela_sts_traj_rhinella.docx" |>
  googledrive::drive_upload(path = Sys.getenv("pasta_artigo_rhinella"),
                            name = "tabela_sts_traj_rhinella.docx",
                            overwrite = TRUE)

### Gráfico ----

vars_rhinella |>
  dplyr::filter(ID %notin% id_ri_rem_traj) |>
  tidyr::pivot_longer(cols = c(peso,
                               `Proporção CMP`,
                               `Umidade Relativa -máxima`,
                               `Temperatura máxima`,
                               `Temperatura mínima`),
                      names_to = "Preditor",
                      values_to = "Predictor value") |>
  dplyr::mutate(Preditor  = dplyr::case_when(

    Preditor == "peso" ~ "Body mass (g)",
    Preditor == "Proporção CMP" ~ "Forelimb proportion",
    Preditor == "Umidade Relativa -máxima" ~ "Max relative precipitation (mm)",
    Preditor == "Temperatura máxima" ~ "Max temperature (°C)",
    Preditor == "Temperatura mínima" ~ "Min temperature (°C)"
    )) |>
  dplyr::left_join(tabela_flex_traj$body$dataset |>
                     dplyr::rename("Espécie" = 1,
                                   "Preditor" = 2) |>
                     dplyr::mutate(p = p |> readr::parse_number()) |>
                     dplyr::select(1, 2, 4, 5),
                   by = c("Espécie", "Preditor")) |>
  ggplot(aes(`Predictor value`, trajeto_percorrido)) +
  geom_point(size = 5) +
  geom_smooth(data = . %>%
                dplyr::filter(abs(t) > 1.833113 &
                                p < 0.05),
              method = "glm",
              se = FALSE) +
  ggh4x::facet_grid2(Preditor ~ Espécie,
                     scales = "free",
                     independent = "all") +
  labs(y = "Runned track (m)") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(color = "black", size = 30),
        strip.text.y.right = element_text(color = "black", size = 16),
        strip.text.x.top = element_text(face = "italic"))

ggsave(filename = "efeito_traj_rhinella.png",
       height = 20,
       width = 14)

"efeito_traj_rhinella.png" |>
  googledrive::drive_upload(path = Sys.getenv("pasta_artigo_rhinella"),
                            name = "efeito_traj_rhinella.png",
                            overwrite = TRUE)

## Tortuosidade ----

### Identificar outliers ----

medias_tortuosidade <- vars_rhinella |>
  dplyr::distinct(ID, .keep_all = TRUE) |>
  dplyr::summarise(sd = tortuosidade |> sd(),
                   tortuosidade = tortuosidade |> mean(),
                   .by = c(Espécie, Tipo))

medias_tortuosidade

vars_rhinella |>
  dplyr::distinct(ID, .keep_all = TRUE) |>
  ggplot(aes(Espécie, tortuosidade)) +
  ggbeeswarm::geom_quasirandom(color = "black",
                               size = 7,
                               width = 0.25,
                               stroke = 1.5) +
  geom_errorbar(data = medias_tortuosidade,
                aes(x = Espécie, y = tortuosidade,
                    ymin = tortuosidade - sd, ymax = tortuosidade + sd,
                    group = Tipo),
                linewidth = 1.25,
                width = 0.35,
                color = "blue") +
  geom_point(data = medias_tortuosidade,
             aes(x = Espécie, y = tortuosidade),
             size = 7,
             show.legend = FALSE,
             color = "red") +
  labs(x = "Species",
       y = "Tortuosity index",
       fill = NULL) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.text.x = element_text(face = "italic"),
        axis.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", linewidth = 1),
        plot.margin = ggplot2::margin(t = 15, r = 10, b = 10, l = 10))

ggsave(filename = "dispersao_tortuosidade.png",
       height = 12,
       width = 12)

"dispersao_tortuosidade.png" |>
  googledrive::drive_upload(path = Sys.getenv("pasta_artigo_rhinella"),
                            name = "dispersao_tortuosidade.png",
                            overwrite = TRUE)

id_ri_rem_tort <- vars_rhinella |>
  dplyr::filter(Espécie == "Rhinella icterica") |>
  dplyr::slice_max(tortuosidade, n = 1, by = Tipo) |>
  dplyr::select(ID, Tipo, Espécie) |>
  dplyr::pull(ID) |>
  unique()

id_ri_rem_tort

### Criar os modelos ----

modelos_glm_tort <- purrr::pmap(
  tidyr::expand_grid(
    sps = c("Rhinella crucifer",
            "Rhinella icterica"),
    var = c("peso",
            "proporcao_cmp",
            "umidade_relativa_maxima",
            "temperatura_maxima",
            "temperatura_minima")),
  \(sps, var){

    formula <- paste0("tortuosidade ~ ", var) |>
      as.formula()

    glmmTMB::glmmTMB(formula,
                     data = vars_rhinella |>
                       dplyr::filter(Espécie == sps &
                                       ID %notin% id_ri_rem_traj) |>
                       janitor::clean_names(),
                     family = glmmTMB::beta_family())

  },
  .progress = TRUE) |>
  setNames(tidyr::expand_grid(sps = c("Rhinella crucifer",
                                      "Rhinella icterica"),
                              var = c("peso",
                                      "proporcao_cmp",
                                      "umidade_relativa_maxima",
                                      "temperatura_maxima",
                                      "temperatura_minima")) |>
             dplyr::mutate(mod = paste(sps, var, sep = "_")) |>
             dplyr::pull(mod))

modelos_glm_tort

### Avaliar os modelos ----

purrr::map(
  modelos_glm_tort,
  \(modelo){

    modelo |>
      DHARMa::simulateResiduals(plot = TRUE)

  },
  .progress = TRUE)

## Estatísticas dos modelos ----

purrr::imap_dfr(
  modelos_glm_tort,
  \(modelo, id){

    modelo  |>
      marginaleffects::avg_comparisons() |>
      tibble::as_tibble() |>
      dplyr::mutate(statistic = modelo |>
                      summary() %>%
                      .$coefficients %>%
                      .$cond |>
                      as.data.frame() |>
                      dplyr::pull(`z value`) |>
                      purrr::pluck(2),
                    p.value = modelo |>
                      summary() %>%
                      .$coefficients %>%
                      .$cond |>
                      as.data.frame() |>
                      dplyr::pull(`Pr(>|z|)`)|>
                      purrr::pluck(2),
                    modelo = id)

  },
  .progress = TRUE)

### Tabela dos valores ----

tabela_flex_tort <- purrr::map_dfr(
  modelos_glm_tort,
  \(modelo){

    modelo  |>
      marginaleffects::avg_comparisons() |>
      tibble::as_tibble() |>
      dplyr::mutate(statistic = modelo |>
                      summary() %>%
                      .$coefficients %>%
                      .$cond |>
                      as.data.frame() |>
                      dplyr::pull(`z value`) |>
                      purrr::pluck(2),
                    p.value = modelo |>
                      summary() %>%
                      .$coefficients %>%
                      .$cond |>
                      as.data.frame() |>
                      dplyr::pull(`Pr(>|z|)`)|>
                      purrr::pluck(2),
                    R2 = modelo |>
                      performance::r2_ferrari(adjust = TRUE) |>
                      as.numeric() |>
                      round(3))

    },
  .progress = TRUE) |>
  dplyr::select(-c(2, 7:9)) |>
  dplyr::mutate(p.value = dplyr::case_when(

    p.value < 0.01 ~ "< 0.01",
    .default = p.value |> round(2) |> as.character()

  )) |>
  dplyr::mutate(Species = c("Rhinella crucifer",
                            "Rhinella icterica") |>
                  rep(each = 5),
                .before = 1) |>
  as.data.frame() |>
  dplyr::mutate(term = dplyr::case_when(

    term == "peso" ~ "Body mass (g)",
    term == "proporcao_cmp" ~ "Forelimb proportion",
    term == "umidade_relativa_maxima" ~ "Max relative precipitation (mm)",
    term == "temperatura_maxima" ~ "Max temperature (°C)",
    term == "temperatura_minima" ~ "Min temperature (°C)"

  ),
  estimate = estimate |> round(4),
  std.error = std.error |> round(3),
  statistic = statistic |> round(2)) |>
  dplyr::rename("Predictor" = 2,
                "z" = 5,
                "p" = 6,
                "pseudo-R²" = 7) |>
  tidyr::unite(3:4,
               sep = " ± ",
               col = "ME ± SE") |>
  flextable::flextable() |>
  flextable::align(align = "center",
                   part = "all") |>
  flextable::width(j = 1, width = 1.5) |>
  flextable::width(j = 3, width = 1.25) |>
  flextable::italic(j = 1, part = "body") |>
  flextable::bg(i = ~abs(z) > 1.833113 &
                  readr::parse_number(p) < 0.05,
                bg = "gray")

tabela_flex_tort

tabela_flex_tort |>
  flextable::save_as_docx(path = "tabela_tort_rhinella.docx")

"tabela_tort_rhinella.docx" |>
  googledrive::drive_upload(path = Sys.getenv("pasta_artigo_rhinella"),
                            name = "tabela_tort_rhinella.docx",
                            overwrite = TRUE)

### Gráfico ----

vars_rhinella |>
  dplyr::filter(ID %notin% id_ri_rem_tort) |>
  tidyr::pivot_longer(cols = c(peso,
                               `Proporção CMP`,
                               `Umidade Relativa -máxima`,
                               `Temperatura máxima`,
                               `Temperatura mínima`),
                      names_to = "Preditor",
                      values_to = "Predictor value") |>
  dplyr::mutate(Preditor  = dplyr::case_when(

    Preditor == "peso" ~ "Body mass (g)",
    Preditor == "Proporção CMP" ~ "Forelimb proportion",
    Preditor == "Umidade Relativa -máxima" ~ "Max relative precipitation (mm)",
    Preditor == "Temperatura máxima" ~ "Max temperature (°C)",
    Preditor == "Temperatura mínima" ~ "Min temperature (°C)"
  )) |>
  dplyr::left_join(tabela_flex_tort$body$dataset |>
                     dplyr::rename("Espécie" = 1,
                                   "Preditor" = 2) |>
                     dplyr::mutate(p = p |> readr::parse_number()) |>
                     dplyr::select(1, 2, 4, 5),
                   by = c("Espécie", "Preditor")) |>
  ggplot(aes(`Predictor value`, tortuosidade)) +
  geom_point(size = 5) +
  geom_smooth(data = . %>%
                dplyr::filter(abs(z) > 1.833113 &
                                p < 0.05),
              method = "glm",
              se = FALSE) +
  ggh4x::facet_grid2(Preditor ~ Espécie,
                     scales = "free",
                     independent = "all") +
  labs(y = "Tortuosity index") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(color = "black", size = 30),
        strip.text.y.right = element_text(color = "black", size = 16),
        strip.text.x.top = element_text(face = "italic"))

ggsave(filename = "efeito_tort_rhinella.png",
       height = 20,
       width = 14)

"efeito_tort_rhinella.png" |>
  googledrive::drive_upload(path = Sys.getenv("pasta_artigo_rhinella"),
                            name = "efeito_tort_rhinella.png",
                            overwrite = TRUE)
