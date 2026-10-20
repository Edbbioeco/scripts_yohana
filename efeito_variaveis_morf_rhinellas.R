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

library(performance)

library(DHARMa)

library(ggh4x)

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

cor_rhinellas <- purrr::map_dfr(
  c("Rhinella crucifer",
    "Rhinella icterica"),
  \(sps){

    cor_mat <- morf |>
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

ggsave(filename = "multicolinearidade_rhinella.png",
       height = 12,
       width = 16)

## unir os dados ----

area_mof_rhinella <- area_rhinella |>
  sf::st_drop_geometry() |>
  dplyr::rename("ID" = Indivíduo) |>
  dplyr::select(ID, Tipo, area) |>
  dplyr::left_join(morf,
                   by = "ID") |>
  dplyr::left_join(tort_traj_rhinella,
                   by = "ID")

area_mof_rhinella

## Áreas de KDE ----

### LM ----

#### Criar modelos ----

modelos_lm <- purrr::pmap(
  tidyr::expand_grid(
    sps = c("Rhinella crucifer",
            "Rhinella icterica"),
    kde = c("KDE 50%",
            "KDE 95%"),
    var = c("peso", "proporcao_cmp")),
  \(sps, kde, var){

    formula <- paste0("area ~ ", var) |>
          as.formula()

    lm(formula,
       data = area_mof_rhinella |>
         dplyr::filter(Espécie == sps &
                         Tipo == kde) |>
         janitor::clean_names())

    },
  .progress = TRUE) |>
  purrr::flatten() |>
  setNames(tidyr::expand_grid(sps = c("Rhinella crucifer",
                                      "Rhinella icterica"),
                              kde = c("KDE 50%",
                                      "KDE 95%"),
                              var = c("peso", "proporcao_cmp")) |>
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
    var = c("peso", "proporcao_cmp")),
  \(sps, kde, var){

    formula <- paste0("area ~ ", var) |>
          as.formula()

    tryCatch(
      glm(formula,
          data = area_mof_rhinella |>
            dplyr::filter(Espécie == sps &
                            Tipo == kde) |>
            janitor::clean_names(),
          family = Gamma(link = "log")),
      error = \(e){
        glm(formula,
            data = area_mof_rhinella |>
              dplyr::filter(Espécie == sps &
                              Tipo == kde) |>
              janitor::clean_names(),
            family = Gamma(link = "log"),
            start = lm(formula,
                       data = area_mof_rhinella |>
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
                              var = c("peso", "proporcao_cmp")) |>
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

#### Modelos GLMs ----

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

### Tabela dos valores ----

tabela_flex <- purrr::map_dfr(
  modelos_glm,
  \(modelo){

    modelo |>
      broom::tidy() |>
      dplyr::mutate(R2 = modelo |>
                      performance::r2_kullback(adjust = TRUE) |>
                      as.numeric() |>
                      round(3))

  },
  .progress = TRUE) |>
  dplyr::filter(!term |> stringr::str_detect("Intercept")) |>
  dplyr::mutate(p.value = dplyr::case_when(

    p.value < 0.01 ~ "< 0.01",
    .default = p.value |> round(2) |> as.character()

  )) |>
  dplyr::mutate(Species = c("Rhinella crucifer",
                          "Rhinella icterica") |>
                  rep(each = 4),
                KDE = c("50%", "95%") |>
                  rep(each = 2) |>
                  rep(2),
                .before = 1) |>
  as.data.frame() |>
  dplyr::mutate(term = dplyr::case_when(

    term == "peso" ~ "Body mass (g)",
    .default = "Posterior Limb proportion"

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
  flextable::italic(j = 1, part = "body")

tabela_flex

tabela_flex |>
  flextable::save_as_docx(path = "tabela_sts_morf_rhinella.docx")

"tabela_sts_morf_rhinella.docx" |>
  googledrive::drive_upload(path = Sys.getenv("pasta_artigo_rhinella"),
                            name = "tabela_sts_morf_rhinella.docx")

### Gráfico ----

#### Rhinella crucifer ----

area_mof_rhinella |>
  dplyr::filter(Espécie == "Rhinella crucifer") |>
  tidyr::pivot_longer(cols = c("peso", "Proporção CMP"),
                      names_to = "Preditor",
                      values_to = "Predictor value") |>
  dplyr::mutate(Preditor  = dplyr::case_when(

    Preditor == "peso" ~ "Body mass (g)",
    .default = "Posterior Limb proportion")) |>
  ggplot(aes(area, `Predictor value`)) +
  geom_point(size = 5) +
  geom_smooth(data = . %>%
                dplyr::filter(Preditor == "Body mass (g)"),
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
        strip.text = element_text(color = "black", size = 30))

ggsave(filename = "efeito_morf_rhinella_crucifer.png",
       height = 12,
       width = 16)

"efeito_morf_rhinella_crucifer.png" |>
  googledrive::drive_upload(path = Sys.getenv("pasta_artigo_rhinella"),
                            name = "efeito_morf_rhinella_crucifer.png")

#### Rhinella icterica ----

area_mof_rhinella |>
  dplyr::filter(Espécie == "Rhinella icterica") |>
  tidyr::pivot_longer(cols = c("peso", "Proporção CMP"),
                      names_to = "Preditor",
                      values_to = "Predictor value") |>
  dplyr::mutate(Preditor  = dplyr::case_when(

    Preditor == "peso" ~ "Body mass (g)",
    .default = "Posterior Limb proportion")) |>
  ggplot(aes(area, `Predictor value`)) +
  geom_point(size = 5) +
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
        strip.text = element_text(color = "black", size = 30))

ggsave(filename = "efeito_morf_rhinella_icterica.png",
       height = 12,
       width = 16)

"efeito_morf_rhinella_icterica.png" |>
  googledrive::drive_upload(path = Sys.getenv("pasta_artigo_rhinella"),
                            name = "efeito_morf_rhinella_icterica.png")

### Modelo Rhinella icterica sem os outliers ----

#### Identifcar os outliers ----

id_ri_rem <- area_mof_rhinella |>
  dplyr::filter(Espécie == "Rhinella icterica") |>
  dplyr::slice_max(area, n = 2, by = Tipo) |>
  dplyr::select(ID, Tipo, Espécie) |>
  dplyr::pull(ID) |>
  unique()

id_ri_rem

#### Criar modelos GLM para o dataset sem os indivíduos ----

modelos_glm_rm <- purrr::pmap(
  tidyr::expand_grid(kde = c("KDE 50%",
                             "KDE 95%"),
                     var = c("peso",
                             "proporcao_cmp")),
  \(kde, var){

    formula <- paste0("area ~ ", var) |>
      as.formula()

    tryCatch(
      glm(formula,
          data = area_mof_rhinella |>
            dplyr::filter(Espécie == "Rhinella icterica" &
                            Tipo == kde &
                            ID %notin% id_ri_rem) |>
            janitor::clean_names(),
          family = Gamma(link = "log")),
      error = \(e){

        glm(formula,
            data = area_mof_rhinella |>
              dplyr::filter(Espécie == "Rhinella icterica" &
                              Tipo == kde &
                              ID %notin% id) |>
              janitor::clean_names(),
            family = Gamma(link = "log"),
            start = lm(formula,
                       data = area_mof_rhinella |>
                         dplyr::filter(Espécie == "Rhinella icterica" &
                                         Tipo == kde &
                                         ID %notin% id) |>
                         janitor::clean_names()) |>
              coef())

      }
    )

  },
  .progress = TRUE) |>
  setNames(tidyr::expand_grid(kde = c("KDE 50%",
                                      "KDE 95%"),
                              var = c("peso",
                                      "proporcao_cmp")) |>
             dplyr::mutate(mod = paste(kde, var, sep = "_")) |>
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

tabela_flex_icterica <- purrr::map_dfr(
  modelos_glm_rm,
  \(modelo){

    modelo |>
      broom::tidy() |>
      dplyr::mutate(R2 = modelo |>
                      performance::r2_kullback(adjust = TRUE) |>
                      as.numeric() |>
                      round(3))

  },
  .progress = TRUE) |>
  dplyr::filter(!term |> stringr::str_detect("Intercept")) |>
  dplyr::mutate(p.value = dplyr::case_when(

    p.value < 0.01 ~ "< 0.01",
    .default = p.value |> round(2) |> as.character()

  )) |>
  dplyr::mutate(Species = "Rhinella icterica",
                KDE = c("50%", "95%") |>
                  rep(each = 2),
                .before = 1) |>
  as.data.frame() |>
  dplyr::mutate(term = dplyr::case_when(

    term == "peso" ~ "Body mass (g)",
    .default = "Posterior Limb proportion"

  ),
  estimate = estimate |> round(5),
  std.error = std.error |> round(5),
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
  flextable::italic(j = 1, part = "body")

tabela_flex_icterica

tabela_flex_icterica |>
  flextable::save_as_docx(path = "tabela_sts_morf_rhinella_cterica.docx")

"tabela_sts_morf_rhinella_cterica.docx" |>
  googledrive::drive_upload(path = Sys.getenv("pasta_artigo_rhinella"),
                            name = "tabela_sts_morf_rhinella_cterica.docx")

#### Gráfico ----

area_mof_rhinella |>
  dplyr::filter(Espécie == "Rhinella icterica" &
                  ID %notin% id_ri_rem$ID) |>
  tidyr::pivot_longer(cols = c("peso", "Proporção CMP"),
                      names_to = "Preditor",
                      values_to = "Predictor value") |>
  dplyr::mutate(Preditor  = dplyr::case_when(

    Preditor == "peso" ~ "Body mass (g)",
    .default = "Posterior Limb proportion")) |>
  ggplot(aes(area, `Predictor value`)) +
  geom_point(size = 5) +
  geom_smooth(data = . %>%
                dplyr::filter(Preditor == "Body mass (g)" &
                                Tipo == "KDE 95%"),
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
        strip.text = element_text(color = "black", size = 30))

ggsave(filename = "efeito_morf_rhinella_icterica_rm.png",
       height = 12,
       width = 16)

"efeito_morf_rhinella_icterica_rm.png" |>
  googledrive::drive_upload(path = Sys.getenv("pasta_artigo_rhinella"),
                            name = "efeito_morf_rhinella_icterica_rm.png")

## Comprimento do trajeto ----

### Identificar outliers ----

id_ri_rem_traj <- area_mof_rhinella |>
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
    var = c("peso", "proporcao_cmp")),
  \(sps, var){

    formula <- paste0("trajeto_percorrido ~ ", var) |>
      as.formula()

    lm(formula,
       data = area_mof_rhinella |>
         dplyr::filter(Espécie == sps &
                         ID %notin% id_ri_rem_traj) |>
         janitor::clean_names())

    },
  .progress = TRUE) |>
  setNames(tidyr::expand_grid(sps = c("Rhinella crucifer",
                                      "Rhinella icterica"),
                              var = c("peso", "proporcao_cmp")) |>
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

## GLM ----

### Criar modelos ----

modelos_glm_traj <- purrr::pmap(
  tidyr::expand_grid(
    sps = c("Rhinella crucifer",
            "Rhinella icterica"),
    var = c("peso", "proporcao_cmp")),
  \(sps, var){

    formula <- paste0("trajeto_percorrido ~ ", var) |>
      as.formula()

    glm(formula,
        data = area_mof_rhinella |>
          dplyr::filter(Espécie == sps &
                          ID %notin% c("RI2", "RI3")) |>
          janitor::clean_names(),
        family = Gamma(link = "log"))

    },
  .progress = TRUE) |>
  setNames(tidyr::expand_grid(sps = c("Rhinella crucifer",
                                      "Rhinella icterica"),
                              var = c("peso", "proporcao_cmp")) |>
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
      broom::tidy() |>
      dplyr::mutate(R2 = modelo |>
                      performance::r2_kullback(adjust = TRUE) |>
                      as.numeric() |>
                      round(3))

  },
  .progress = TRUE) |>
  dplyr::filter(!term |> stringr::str_detect("Intercept")) |>
  dplyr::mutate(p.value = dplyr::case_when(

    p.value < 0.01 ~ "< 0.01",
    .default = p.value |> round(2) |> as.character()

  )) |>
  dplyr::mutate(Species = c("Rhinella crucifer",
                            "Rhinella icterica") |>
                  rep(each = 2),
                .before = 1) |>
  as.data.frame() |>
  dplyr::mutate(term = dplyr::case_when(

    term == "peso" ~ "Body mass (g)",
    .default = "Posterior Limb proportion"

  ),
  estimate = estimate |> round(4),
  std.error = std.error |> round(5),
  statistic = statistic |> round(2)) |>
  dplyr::rename("Predictor" = 2,
                "t" = 5,
                "p" = 6,
                "pseudo-R²" = 7) |>
  tidyr::unite(3:4,
               sep = " ± ",
               col = "β1 ± SE") |>
  flextable::flextable() |>
  flextable::align(align = "center",
                   part = "all") |>
  flextable::width(j = 1, width = 1.5) |>
  flextable::width(j = 3, width = 1.25) |>
  flextable::italic(j = 1, part = "body")

tabela_flex_traj

tabela_flex_traj |>
  flextable::save_as_docx(path = "tabela_sts_morf_traj_rhinella.docx")

"tabela_sts_morf_traj_rhinella.docx" |>
  googledrive::drive_upload(path = Sys.getenv("pasta_artigo_rhinella"),
                            name = "tabela_sts_morf_traj_rhinella.docx")

### Gráfico ----

#### Rhinella crucifer ----

area_mof_rhinella |>
  dplyr::filter(Espécie == "Rhinella crucifer") |>
  tidyr::pivot_longer(cols = c("peso", "Proporção CMP"),
                      names_to = "Preditor",
                      values_to = "Predictor value") |>
  dplyr::mutate(Preditor  = dplyr::case_when(

    Preditor == "peso" ~ "Body mass (g)",
    .default = "Posterior Limb proportion")) |>
  ggplot(aes(`Predictor value`, trajeto_percorrido)) +
  geom_point(size = 5) +
  geom_smooth(data = . %>%
                dplyr::filter(Preditor == "Body mass (g)"),
              method = "glm",
              se = FALSE) +
  facet_wrap(~ Preditor,
             scales = "free_x") +
  labs(y = "Runned traject (m²)") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(color = "black", size = 30))

ggsave(filename = "efeito_morf_rhinella_crucifer_traj.png",
       height = 12,
       width = 16)

"efeito_morf_rhinella_crucifer_traj.png" |>
  googledrive::drive_upload(path = Sys.getenv("pasta_artigo_rhinella"),
                            name = "efeito_morf_rhinella_crucifer_traj.png")

#### Rhinella icterica ----

area_mof_rhinella |>
  dplyr::filter(Espécie == "Rhinella icterica" &
                  ID %notin% c("RI2", "RI3")) |>
  tidyr::pivot_longer(cols = c("peso", "Proporção CMP"),
                      names_to = "Preditor",
                      values_to = "Predictor value") |>
  dplyr::mutate(Preditor  = dplyr::case_when(

    Preditor == "peso" ~ "Body mass (g)",
    .default = "Posterior Limb proportion")) |>
  ggplot(aes(`Predictor value`, trajeto_percorrido)) +
  geom_point(size = 5) +
  geom_smooth(data = . %>%
                dplyr::filter(Preditor == "Body mass (g)"),
              method = "glm",
              se = FALSE) +
  facet_wrap(~ Preditor,
             scales = "free_x") +
  labs(y = "Runned traject (m²)") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", linewidth = 1),
        strip.text = element_text(color = "black", size = 30))

ggsave(filename = "efeito_morf_rhinella_icterica_traj.png",
       height = 12,
       width = 16)

"efeito_morf_rhinella_icterica_traj.png" |>
  googledrive::drive_upload(path = Sys.getenv("pasta_artigo_rhinella"),
                            name = "efeito_morf_rhinella_icterica_traj.png")
