# NMA - Efetividade de Exercicio Aquatico (PAIFIT - Luana Paixao)
# Autor: Thiago Viana Camata  Data: 18/06/2024
#

# if (!require("remotes")) {
#    install.packages("remotes")
#  }
# remotes::install_github("MathiasHarrer/dmetar")


library(netmeta)
library(dmetar)
library(dplyr)

tug <- read.csv("./Data/Tug.csv")

# Tug Scale ----

# visualizando a rede
tug.net <- netconnection(data = tug,
                         treat1,
                         treat2,
                         studlab = Study.label,
                         sep.trts = ":¨"
)
# Checando se existem sub-redes
print(tug.net)
netgraph(tug.net)

# removendo dois estudos que "quebram" a rede
tug <- filter(tug, treat2 != 'PNF' & treat2 != 'Marcha so.' & treat2 != 'Est. Solo')

# checando os resultados
tug.net <- netconnection(data = tug,
                         treat1,
                         treat2,
                         studlab = Study.label
)
# Checando se existem sub-redes
print(tug.net)
netgraph(tug.net)

nma_tug <- netmeta(TE = Effect.size,
                   seTE = Std.error,
                   treat1 = treat1,
                   treat2 = treat2,
                   studlab = Study.label,
                   data = tug,
                   sm = "SMD",
                   reference.group = "Solo",
                   sep.trts = " vs "
) 

summary(nma_tug)

# decompoe inconsistência total
decomp.design(nma_tug)

# Grafico da rede Tug ----
netgraph(nma_tug,
         plastic = FALSE,
         col = "darkgray",
         points = TRUE,
         col.points = "black",
         cex.points = 3,
         seq = "optimal",
         number.of.studies = TRUE
)

# Evidencia direta e indireta ----
# d.evidence <- direct.evidence.plot(nma_ea)
# #d.evidence <- direct.evidence.plot(nma_ea, random = TRUE)
# plot(d.evidence)

# ranking de tratamentos - fiexed-effects model
#netrank(nma_tug, small.values = "bad", random = FALSE) # confirmar se maior eh melhor mesmo

# random-effects model
netrank(nma_tug, small.values = "bad", common = FALSE, method = "SUCRA")

# forest plot ----
forest(nma_tug,
       reference.group = "Solo",
       sortvar = TE,
       smlab = "Exerc. Aquatico vs. Solo",
       label.left = "Solo",
       label.right = "E. Aqua.",
       drop.reference.group = TRUE,
)

netheat(nma_tug)
