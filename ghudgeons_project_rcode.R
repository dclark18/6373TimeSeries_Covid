library(tswge)

az = read.csv(file.choose(), header=TRUE)

plotts.sample.wge(az$Cases)
plotts.sample.wge(az$Deaths)
plotts.sample.wge(az$Hospitalized)
plotts.sample.wge(az$posPer[2:136])