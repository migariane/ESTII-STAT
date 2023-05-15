# CUANTILES DISTRIBUCION NORMAL
# NC 95%
qnorm(c(0.975), mean=0, sd=1, lower.tail=TRUE)
# NC 90%
qnorm(0.95)
# NC 95% 
qnorm(0.975)
# NC 98%
qnorm(0.99)
# NC 99%
qnorm(0.995)

# INTERVALO CONFIANZA DISTRIBUCION NORMAL
n <- 64
s <- 25
alfa <- 0.05
qn <-1-(alfa/2)

1012 + qnorm(0.975)*(25/sqrt(64))
1012 - qnorm(0.975)*(25/sqrt(64))
IC95 <- c(1012 - qnorm(qn)*(s/sqrt(n)),1012 + qnorm(qn)*(s/sqrt(n))); IC95

# CUANTILES DISTRIBUCION T-STUDENT 
qt(c(0.975), df=100, lower.tail=TRUE)

# CUANTILES DISTRIBUCION CHI-CUADRADO IC90%
qchisq(c(0.9), df=15, lower.tail=TRUE)
qchisq(c(0.1), df=15, lower.tail=TRUE)
qchisq(c(0.9,0.1), df=15, lower.tail=TRUE)

# CUANTILES DISTRIBUCION CHI-CUADRADO IC95%
qchisq(c(0.975), df=15, lower.tail=TRUE)
qchisq(c(0.025), df=15, lower.tail=TRUE)
qchisq(c(0.975,0.025), df=15, lower.tail=TRUE)

# CUANTILES DISTRIBUCION F SNEDECOR IC95%
qf(c(0.975), df1=24, df2=24, lower.tail=TRUE)
1/qf(c(0.975), df1=24, df2=24, lower.tail=TRUE)



