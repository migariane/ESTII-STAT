# QUANTILES DISTRIBUCION NORMAL
qnorm(c(0.975), mean=0, sd=1, lower.tail=TRUE)
# NC 90
qnorm(0.95)
# NC 95% 
qnorm(0.975)
# NC 98%
qnorm(0.99)
# NC 99%
qnorm(0.995)

# INTERVALO CONFIANZA DISTRIBUCION NORMAL
1012 + qnorm(0.975)*(25/sqrt(64))
1012 - qnorm(0.975)*(25/sqrt(64))

# QUANTILES DISTRIBUCION T-STUDENT 
qt(c(0.975), df=8, lower.tail=TRUE)


# QUANTILES DISTRIBUCION CHI-CUADRADO
qchisq(c(0.9), df=15, lower.tail=TRUE)
qchisq(c(0.1), df=15, lower.tail=TRUE)
qchisq(c(0.9,0.1), df=15, lower.tail=TRUE)

# QUANTILES DISTRIBUCION F SNEDECOR 
qf(c(0.975), df1=24, df2=24, lower.tail=TRUE)



