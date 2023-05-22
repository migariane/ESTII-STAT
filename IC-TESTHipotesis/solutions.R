# LEER DATOS
Data <- 
  readXL("C:/Users/Usuario/Dropbox/UGR/TEACHING/Informatica/Practicas/IC-TESTHipotesis/Prácticas parte IV_solucion.xlsx",
   rownames=FALSE, header=TRUE, na="", sheet="Sheet1", stringsAsFactors=TRUE)
editDataset(Data)
# FACTOR SEXO
Data <- within(Data, {
  Sexo_F <- as.factor(sexo)
})
# IC95% NIVEL MEDIO ESTRES ANTES EXAMEN ES DE 0
with(Data, (t.test(antes, alternative='two.sided', mu=0.0, conf.level=.95)))
# IC95% NIVEL MEDIO ESTRES ANTES EXAMEN ES DE 0
with(Data, (t.test(antes, alternative='two.sided', mu=32, conf.level=.95)))
# IC95% NIVEL MEDIO ESTRES ANTES Y DESPUES EXAMEN POR SEXO Y T TEST INDEPENDENCIA ANTES Y DESPUES POR SEXO
t.test(antes~Sexo_F, alternative='two.sided', conf.level=.95, var.equal=TRUE, data=Data)
t.test(despues~Sexo_F, alternative='two.sided', conf.level=.95, var.equal=TRUE, data=Data)
# F TEST HOMOGENIDAD DE VARIANZAS POR SEXO
Tapply(antes ~ Sexo_F, var, na.action=na.omit, data=Data) # variances by group
Tapply(despues ~ Sexo_F, var, na.action=na.omit, data=Data) # variances by group
var.test(antes ~ Sexo_F, alternative='two.sided', conf.level=.95, data=Data)
var.test(despues ~ Sexo_F, alternative='two.sided', conf.level=.95, data=Data)

# DESCRIBIR ANTES DESPUES POR SEXO
library(abind, pos=16)
library(e1071, pos=17)
numSummary(Data[,c("antes", "despues"), drop=FALSE], groups=Data$Sexo_F, statistics=c("mean", "sd", "IQR"), 
  quantiles=c(0,.25,.5,.75,1))
# TTEST DATOS EMPAREJADOS ANTES DESPUES 
with(Data, (t.test(antes, despues, alternative='two.sided', conf.level=.95, paired=TRUE)))
with(Data, (t.test(antes, despues, alternative='greater', conf.level=.95, paired=TRUE)))
with(Data, (t.test(antes, despues, alternative='greater', conf.level=.95, paired=TRUE)))
# TEST SHAPIRO WILK DE NORMALIDAD 
normalityTest(~antes, test="shapiro.test", data=Data)
normalityTest(~despues, test="shapiro.test", data=Data)

