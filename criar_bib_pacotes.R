# Pacotes ----

library(knitr)

library(tidyverse)

# Loop da criação dos bibs dos pacotes ----

criar_bib <- function(pacote){

  knitr::write_bib(pacote, paste0(pacote, ".bib"))

}

pacote <- c("geosphere", "sf", "adehabitatHR", "performance")

purrr::map(pacote, criar_bib)
