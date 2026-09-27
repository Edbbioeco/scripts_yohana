# Pacotes ----

library(tidyverse)

library(readxl)

library(parzer)

library(sf)

library(geosphere)

library(flextable)

library(lmerTest)

library(janitor)

library(performance)

library(lme4)

library(DHARMa)

library(adehabitatHR)

library(ggview)

# Dados ----

## Registros de espécies ----

### Importar ----

reg <- purrr::map_dfr(
  c("Boana faber",
    "Phyllomedusa tetraploidea",
    "Rhinella crucifer",
    "Rhinella icterica",
    "Odontophrynus americanus",
    "Leptodactylus mystacinus"),
  \(sps){

    readxl::read_xlsx("dados de campo.xlsx",
                      sheet = sps) |>
      dplyr::select(ID,
                    Data,
                    `Coordenada inicial`,
                    Ponto:`Ângulo de virada (°)`) |>
      tidyr::separate(col = `Coordenada inicial`,
                      into = c("Latitude", "Longitude"),
                      sep = " ") |>
      dplyr::mutate(Espécie = sps,
                    .before = 1) |>
      dplyr::mutate(ID = dplyr::case_when(

        Espécie == "Boana faber" ~ ID |> stringr::str_replace("B", "BF"),
        .default = ID

        ))

    }

  )

### Visualizar ----

reg

reg |> dplyr::glimpse()

### Converter dados de longitude e latitude ----

reg <- reg |>
  dplyr::mutate(Longitude = parzer::parse_lon(Longitude),
                Latitude  = parzer::parse_lat(Latitude))

reg

### Completar as coordenadas ----

reg_trat <- purrr::reduce(
  .x = 1:nrow(reg),
  .f = \(dados, x){

    if(is.na(dados$Longitude[x]) || is.na(dados$Latitude[x])){

      dest <- geosphere::destPoint(
        c(dados$Longitude[x - 1], dados$Latitude[x - 1]),
        dados$`Ângulo de virada (°)`[x - 1],
        dados$`Distancia percorrida (m)`[x - 1])

      dados$Longitude[x] <- dest[1]

      dados$Latitude[x]  <- dest[2]

      }

    dados

    },

  .init = reg)

reg_trat

### Transformar em shapefile ----

reg_sf <- reg_trat |>
  dplyr::filter(!Longitude |> is.na()) |>
  sf::st_as_sf(coords = c(4:5),
               crs = 4674)

reg_sf

ggplot() +
  geom_sf(data = reg_sf)

## Variáveis ambientais ----

### Importar -----

amb <- readxl::read_xlsx("DADOS AMBIENTAIS COM AS 6 ESPÉCIES INDIVIDUAIS.xlsx")

### Visualizar ----

amb

amb |> dplyr::glimpse()

### Tratar ----

amb <- amb |>
  dplyr::rename("ID" =  1) |>
  dplyr::mutate(ID = dplyr::case_when(

    ID |> stringr::str_detect("^B.*[0-9]+$") ~ ID |> stringr::str_replace("B", "BF"),
    ID |> stringr::str_detect("^BF") ~ ID,
    .default = ID

  ))

amb

# Distância percorrida ----

## Por indivíduo ----

### Calcular trajeto ----

trajeto <- purrr::map_dfr(
  reg_sf$ID |> unique(),
  \(ind){

    percurso <- reg_sf |>
          dplyr::filter(ID == ind) |>
          dplyr::summarise(geometry = geometry |>
                             sf::st_combine()) |>
          sf::st_cast(to = "LINESTRING") |>
          sf::st_length()

    sps <- reg_sf |>
      dplyr::filter(ID == ind) |>
      dplyr::pull(Espécie) |>
      unique()

    tibble::tibble(Espécie = sps,
                   ID = ind,
                   `Distância percorrida (m)` = percurso |>
                     as.numeric() |>
                     round(2))

        },
  .progress = TRUE)

trajeto

### Tabela flextable ----

trajeto_flex <- trajeto |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(j = c(1, 3),
                   width = 2) |>
  flextable::italic(j = 1,
                    part = "body")

trajeto_flex

trajeto_flex |> flextable::save_as_docx(path = "tabela_ind_trajeto.docx")

## Modelos lineares ----

### Calcular média ----

media_amb <- amb |>
  dplyr::summarise(dplyr::across(.cols = dplyr::where(is.numeric),
                                 .fns = ~.x |> mean()),
                   .by = ID)

media_amb

### Unir os dados ----

trajeto_amb <- trajeto |>
  dplyr::left_join(media_amb,
                   by = "ID")

trajeto_amb

### Calcular a multicolinearidade ----

cor_trajeto_amb <- trajeto_amb |>
  dplyr::select(dplyr::where(is.numeric)) |>
  cor(method = "spearman") |>
  as.matrix()

cor_trajeto_amb

cor_trajeto_amb[cor_trajeto_amb |> upper.tri()] <- NA

cor_trajeto_amb |>
  reshape2::melt() |>
  dplyr::filter(Var1 != Var2) |>
  tidyr::drop_na() |>
  ggplot(aes(Var1, Var2, fill = value, label = value |> round(2))) +
  geom_tile(color = "black", linewidth = 1) +
  geom_text(size = 7.5) +
  coord_equal() +
  scale_fill_gradientn(
    colours = c(viridis::viridis(n = 10) |>
                  rev(),
                viridis::viridis(n = 10)),
    guide = guide_colourbar(
      title = "Spearman correlation",
      title.hjust = 0.5,
      barheight = 15,
      frame.colour = "black",
      ticks.colour = "black",
      ticks.linewidth = 0.5
      ),
    limits = c(-1, 1)) +
  labs(x = NULL,
       y = NULL) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  ggview::canvas(height = 10, width = 12)

### Criar modelos lineares ----

modelos_trajeto <- purrr::map(
  c("temperatura_maxima",
    "umidade_relativa_maxima",
    "precipitacao"),
  \(var){

    formula <- paste0("distancia_percorrida_m ~ ",
                      var,
                      "+ (1 | especie)") |>
      as.formula()

    lmerTest::lmer(formula,
                   data = trajeto_amb |>
                     janitor::clean_names() |>
                     dplyr::select(distancia_percorrida_m,
                                   especie,
                                   var))

    },
  .progress = TRUE) |>
  setNames(c("temperatura_maxima",
             "umidade_relativa_maxima",
             "precipitacao"))

modelos_trajeto

### Avliar modelos ----

purrr::map(
  modelos_trajeto,
  \(modelo){

    modelo |> performance::check_normality() |> print()

    modelo |> performance::check_heteroscedasticity() |> print()

    },
  .progress = TRUE)

### Criar GLM ----

modelos_trajeto_glm <- purrr::map(
  c("temperatura_maxima",
    "umidade_relativa_maxima",
    "precipitacao"),
  \(var){

    formula <- paste0("distancia_percorrida_m ~ ",
                      var,
                      "+ (1 | especie)") |>
      as.formula()

    lme4::glmer(formula,
                family = Gamma(link = "log"),
                data = trajeto_amb |>
                  janitor::clean_names() |>
                  dplyr::select(distancia_percorrida_m,
                                especie,
                                var))

    },
  .progress = TRUE) |>
  setNames(c("temperatura_maxima",
             "umidade_relativa_maxima",
             "precipitacao"))

modelos_trajeto_glm

### Avaliar GLMs ----

purrr::map(
  modelos_trajeto_glm,
  \(modelo){

    modelo |> DHARMa::simulateResiduals(plot = TRUE)

  },
  .progress = TRUE)

### Estatísticas dos modelos GLM ----

sts_trajeto <- purrr::imap_dfr(
  modelos_trajeto_glm,
  \(modelo, var){

    modelo |>
      summary() %>%
      .$coefficients |>
      as.data.frame() |>
      tibble::rownames_to_column() |>
      dplyr::rename("Predictor" = rowname,
                    "t" = 4,
                    "p" = 5) |>
      dplyr::filter(Predictor != "(Intercept)") |>
      dplyr::mutate(dplyr::across(.cols = 2:3,
                                  .fns = ~.x |> round(3)),
                    t = t |> round(2),
                    p = dplyr::case_when(

                      p < 0.01 ~ "< 0.01",
                      .default = p |> round(2) |> as.character()
                    )) |>
      tidyr::unite(c(2 |> round(2),
                     3 |> round(3)),
                   col = "β1 ± SE",
                   sep = " ± ")

    },
  .progress = TRUE) |>
  dplyr::mutate(Predictor = c("Temperatura máxima (°C)",
                              "Umidade Relativa máxima (mm)",
                              "Precipitação (mm)"))

sts_trajeto

### Tabela flextable ----

sts_trajeto_flex <- sts_trajeto |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(j = 1,
                   width = 2.5) |>
  flextable::width(j = 2,
                   width = 1.2)

sts_trajeto_flex

sts_trajeto_flex |> flextable::save_as_docx(path = "tabela_estatisticas_trajeto.docx")

## Por indivíduo e data ----

### Calcular trajeto ----

trajeto_data <- purrr::map_dfr(
  reg_sf$ID |> unique(),
  \(ind){

    data <- reg_sf |>
      dplyr::filter(ID == ind) |>
      dplyr::pull(Data) |>
      unique()

    sps <- reg_sf |>
      dplyr::filter(ID == ind) |>
      dplyr::pull(Espécie) |>
      unique()

    purrr::map(
      data,
      \(data){

        percurso <- reg_sf |>
          dplyr::filter(ID == ind & Data == data) |>
          dplyr::summarise(geometry = geometry |>
                             sf::st_combine()) |>
          sf::st_cast(to = "LINESTRING") |>
          sf::st_length()

        tibble::tibble(Espécie = sps,
                       ID = ind,
                       Data = data |> format("%d-%m-%Y"),
                       `Distância percorrida (m)` = percurso |>
                         as.numeric() |>
                         round(2))

        }
      )

    },
  .progress = TRUE)

trajeto_data

### Tabela flextable ----

trajeto_data_flex <- trajeto_data |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(j = c(1, 3:4),
                   width = 2) |>
  flextable::italic(j = 1,
                    part = "body")

trajeto_data_flex

trajeto_flex |> flextable::save_as_docx(path = "tabela_ind_data_trajeto.docx")

# Índice de tortuosidade ----

## Por indivíduo ----

### Calcular índice de tortuosidade ----

tortuosidade <- purrr::map_dfr(
  reg_sf$ID |> unique(),
  \(ind){

    percurso <- reg_sf |>
      dplyr::filter(ID == ind) |>
      dplyr::summarise(geometry = geometry |>
                         sf::st_combine()) |>
      sf::st_cast(to = "LINESTRING") |>
      sf::st_length()

    percurso_min <- reg_sf |>
      dplyr::filter(ID == ind) |>
      dplyr::slice(c(1, dplyr::n())) |>
      dplyr::summarise(geometry = geometry |>
                         sf::st_combine()) |>
      sf::st_cast(to = "LINESTRING") |>
      sf::st_length()

    sps <- reg_sf |>
      dplyr::filter(ID == ind) |>
      dplyr::pull(Espécie) |>
      unique()

    tibble::tibble(Espécie = sps,
                   ID = ind,
                   `Índice de tortuosidade` = (percurso_min / percurso) |>
                     as.numeric() |>
                     round(2))

    },
  .progress = TRUE)

tortuosidade

## Modelos lineares ----

### nor dados ----

tortuosidade_amb <- tortuosidade |>
  dplyr::left_join(media_amb,
                   by = "ID")

tortuosidade_amb

### Criar GLM ----

modelos_tortuosidade_glm <- purrr::map(
  c("temperatura_maxima",
    "umidade_relativa_maxima",
    "precipitacao"),
  \(var){

    formula <- paste0("indice_de_tortuosidade ~ ",
                      var,
                      "+ (1 | especie)") |>
      as.formula()

    glmmTMB::glmmTMB(formula,
                     family = glmmTMB::beta_family(),
                     data = tortuosidade_amb |>
                       janitor::clean_names() |>
                       dplyr::select(indice_de_tortuosidade,
                                     especie,
                                     var))

  },
  .progress = TRUE) |>
  setNames(c("temperatura_maxima",
             "umidade_relativa_maxima",
             "precipitacao"))

modelos_tortuosidade_glm

### Avaliar GLMs ----

purrr::map(
  modelos_tortuosidade_glm,
  \(modelo){

    modelo |> DHARMa::simulateResiduals(plot = TRUE)

  },
  .progress = TRUE)
