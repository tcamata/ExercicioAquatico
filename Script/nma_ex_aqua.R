# NMA - Efetividade de Exercicio Aquatico (PAIFIT - Luana Paixao)
# Autor: Thiago Viana Camata  Data: 18/06/2024
#

# if (!require("remotes")) {
#   install.packages("remotes")
# }
# remotes::install_github("MathiasHarrer/dmetar")


library(netmeta)
library(dmetar)
library(dplyr)

dados <- read.csv("./Data/Berg.csv")

# removendo dois estudos que "quebram" a rede
#dados_berg <- filter(dados, treat2 != 'PNF' & treat2 != 'Est.solo')

nma_ea <- netmeta(TE = TE,
                  seTE = seTE,
                  treat1 = treat1,
                  treat2 = treat2,
                  studlab = studlab,
                  data = dados_berg,
                  sm = "SMD",
                  reference.group = "Solo",
                  sep.trts = " vs "
                  )

summary(nma_ea)

# decompoe inconsistência total
decomp.design(nma_ea)

# visualizando a rede ----
netgraph(nma_ea,
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
netrank(nma_ea, small.values = "bad", random = FALSE) # confirmar se maior eh melhor mesmo

# random-effects model
netrank(nma_ea, small.values = "bad", common = FALSE)

# forest plot ----
forest(nma_ea,
       reference.group = "Solo",
       sortvar = TE,
       smlab = "Exerc. Aquatico vs. Solo",
       label.left = "Solo",
       label.right = "E. Aqua.",
       drop.reference.group = TRUE,
       )

netheat(nma_ea)

#netsplit(nma_ea) #%>% forest(show = "all")

# funnel(nma_ea,
#        order = c("EA","EAb", "EA.solo", "Halli.", "Bad + Solo", "Solo"),
#        col = c("blue", "red", "forestgreen","purple", "orange"),
#        linreg = TRUE)
