# Pacotes ----

library(ape)

library(ggtree)

library(phytools)

# Dados ----

## Importando ----

arvore <- ape::read.nexus("output.nex")

## Visualizando ----

arvore

arvore |>
  ggtree::ggtree() +
  ggtree::geom_tiplab(size = 4,
                      color = "black",
                      fontface = "bold.italic",
                      offset = 14.5)

# Consenso ----

## Calculando ----

consenso <- arvore |> phytools::consensus.edges(if.absent="ignore")

## Visualizando ----

consenso

consenso$edge.length

consenso |> ape::plot.phylo()

# Alterando o nome da espécie Hypsiboas faber ----

consenso$tip.label[consenso$tip.label == "Hypsiboas_faber"] <- "Boana_faber"

consenso$tip.label[consenso$tip.label == "Odontophrynus_americanus"] <- "Odontophrynus_asper"

consenso |> ape::plot.phylo()

# Exportando ----

consenso |> ape::write.tree("consenso.phy")
