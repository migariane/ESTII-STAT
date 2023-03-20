### TABLAS DE DOBLE ENTRADA: % MARGINALES Y CONDICIONALES

Empleados <- 
  readXL("C:/Users/Usuario/Dropbox/UGR/TEACHING/Informatica/Practicas/ETSIIT-STAT/ETSIIT-STAT/Data/Ejemplodatos3.xls",
   rownames=FALSE, header=TRUE, na="", sheet="Respuestas", stringsAsFactors=TRUE)

library(abind, pos=16)
local({
  .Table <- xtabs(~Red+Sexo, data=Empleados)
  cat("\nFrequency table:\n")
  print(.Table)
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})
local({
  .Table <- xtabs(~Red+Sexo, data=Empleados)
  cat("\nFrequency table:\n")
  print(.Table)
  cat("\nTotal percentages:\n")
  print(totPercents(.Table))
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})
local({
  .Table <- xtabs(~Red+Sexo, data=Empleados)
  cat("\nFrequency table:\n")
  print(.Table)
  cat("\nRow percentages:\n")
  print(rowPercents(.Table))
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})
local({
  .Table <- xtabs(~Red+Sexo, data=Empleados)
  cat("\nFrequency table:\n")
  print(.Table)
  cat("\nColumn percentages:\n")
  print(colPercents(.Table))
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})

### MEDIAS POR GRUPO
Tapply(Peso ~ Sexo, mean, na.action=na.omit, data=Empleados) # mean by groups
with(Empleados, plotMeans(Peso, Sexo, error.bars="se", connect=TRUE))



###PRACTICA TRES MODELOS REGRESION

library(lattice, pos=16)
xyplot(Peso ~ Altura, type="p", pch=16, auto.key=list(border=TRUE), par.settings=simpleTheme(pch=16), 
  scales=list(x=list(relation='same'), y=list(relation='same')), data=Empleados)
RegModel.1 <- lm(Peso~Altura, data=Empleados)
summary(RegModel.1)

library(leaps, pos=17)

predict(RegModel.1, data.frame(Altura=173))
predict(RegModel.1, data.frame(Altura=173),interval=”confidence”)

plot(Empleados$Altura, Empleados$Peso, main="Nube de puntos y ajuste", xlab="Altura", ylab="Peso")
abline(lm(Empleados$Peso ~ Empleados$Altura), col="red")
lines(lowess(Empleados$Altura, Empleados$Peso), col="blue")

scatterplot(Peso ~ Altura | Sexo, xlab="Altura", ylab="Peso", data=Empleados)

LinearModel.2 <- lm(Peso ~ Altura + Edad + Edad^2, data=Empleados)
summary(LinearModel.2)
plot(allEffects(LinearModel.2, residuals=TRUE), partial.residuals=list(span=0.5))

LinearModel.3 <- lm(log(Peso) ~ Altura, data=Empleados)
summary(LinearModel.3)
scatterplot(log(Peso)~Altura, reg.line=lm, xlab="Altura", ylab="log(Peso)", data=Empleados)

