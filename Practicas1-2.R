# PRACTICAS 1-2

Dataset <- 
  read.table("C:/Users/Usuario/Dropbox/UGR/TEACHING/Informatica/Practicas/ETSIIT-STAT/Data/ejemplodatos4.csv", 
  header=TRUE, stringsAsFactors=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
Dataset <- readXL("C:/Users/Usuario/Dropbox/UGR/TEACHING/Informatica/Practicas/ETSIIT-STAT/Data/Ejemplodatos3.xls",
   rownames=FALSE, header=TRUE, na="", sheet="Respuestas", stringsAsFactors=TRUE)
editDataset(Dataset)
str(Dataset)

library(lattice, pos=16)
xyplot(Peso ~ Altura, type="p", pch=16, auto.key=list(border=TRUE), par.settings=simpleTheme(pch=16), 
  scales=list(x=list(relation='same'), y=list(relation='same')), data=Dataset)

Boxplot(Peso ~ Sexo, data=Dataset, id=list(method="y"))

with(Dataset, qqPlot(Peso, dist="norm", id=list(method="y", n=2, labels=rownames(Dataset))))

with(Dataset, discretePlot(n_herm, scale="frequency"))

with(Dataset, Barplot(Red, xlab="Red", ylab="Frequency", label.bars=TRUE, col=rainbow(10)))

library(abind, pos=17)
local({
  .Table <- xtabs(~Grupo+Sexo, data=Dataset)
  cat("\nFrequency table:\n")
  print(.Table)
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})

library(e1071, pos=18)
numSummary(Dataset[,"Altura", drop=FALSE], statistics=c("mean", "sd", "IQR", "quantiles", "cv", "skewness", 
  "kurtosis"), quantiles=c(0,.25,.5,.75,1), type="2")
binnedCounts(Dataset[,"Altura", drop=FALSE])

library(colorspace, pos=19)
with(Dataset, piechart(Plataf, xlab="", ylab="", main="Plataf", col=rainbow_hcl(4), scale="percent"))

