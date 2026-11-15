# Pacotes ----

library(googledrive)

library(tidyverse)

library(terra)

library(exifr)

library(tidyterra)

library(patchwork)

# Dados ----

## Autenticar Google Drive ----

googledrive::drive_auth("edson.nilton@ufpe.br")
