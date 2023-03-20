x <- c(3.5,8.6,9.9,4.5,6.5,9.9,8.9,9.8,8.8)
y <- c(4.5,6.5,6.3,5.6,2.4,8.9,4.3,3.5,5.5)
n <- length(x)

## Modelo lineal
mod.lin <- lm(y ~ x); mod.lin;
summary(mod.lin)
## Coeficiente "a" del modelo lineal Y=a+b*x;
a <- mod.lin$coefficients[[1]];a;
## Coeficiente "b" del modelo lineal Y=a+b*x;
b <- mod.lin$coefficients[[2]];b;
cov(x,y) ## Covarianza;
cor <- cor(x,y) ## Coeficiente de correlacion lineal;
R2 <- cor^2; R2
DesStaRes <- sqrt(sum((y - (a+b*x))^2)/(n-2)); DesStaRes;# Residual var
plot(x, y, col="blue");
abline(a, b, col="red", lwd="3");

## Modelo Exponencial ln(y)=a+b*x
logy <- log(y)
mod.exp <- lm(logy ~ x); mod.exp;
summary(mod.exp)
## Coeficiente "a" del modelo ln(y)=a+b*x;
a <- mod.exp$coefficients[[1]];a;
## Coeficiente "b" del modelo ln(y)=a+b*x;;
b <- mod.exp$coefficients[[2]];b;
cov(x,logy) ## Covarianza;
cor <- cor(x,logy) ## Coeficiente de correlacion lineal;
R2 <- cor^2; R2
DesStaRes <- sqrt(sum((logy - (a+b*x))^2)/(n-2)); DesStaRes;# Residual var
plot(x,logy, col="blue");
abline(a, b, col="red", add=TRUE, lwd="3");

## Modelo parabolico o quadratic, y = a+bx+cX2
mod.par <- lm(y ~ x + I(x^2)); mod.par;
summary(mod.par)
## Coeficiente "a" del modelo y = a+bx+cX2;
a <- mod.par$coefficients[[1]];a;
## Coeficiente "b" del modelo y = a+bx+cX2;
b <- mod.par$coefficients[[2]];b;
## Coeficiente "c" del modelo y = a+bx+cX2
c <- mod.par$coefficients[[3]];c;
DesStaRes <- sqrt(sum((y - (a+b*x+c*x^2))^2)/(n-3)); DesStaRes;# Residual var
plot(x,y, col="blue");
curve(a+b*x+c*x^2, col="red", add=TRUE, lwd="3");

## Modelo hiperbolico
inv.x <- 1/x
mod.hip <- lm(y ~ inv.x); mod.hip;
summary(mod.hip)
## Coeficiente "a" del modelo lineal Y=a+b/x;
a <- mod.hip$coefficients[[1]];a;
## Coeficiente "b" del modelo lineal Y=a+b/x;
b <- mod.hip$coefficients[[2]];b;
cov(inv.x,y) ## Covarianza;
cor <- cor(inv.x,y) ## Coeficiente de correlacion lineal;
R2 <- cor^2; R2
DesStaRes <- sqrt(sum((y - (a+b/x))^2)/(n-2)); DesStaRes;# Residual var
plot(x, y, col="blue");
curve(a+b/x, add=TRUE, col="red", lwd="3");
library(car)
scatterplot(y~logx, reg.line=lm, xlab="x", ylab="y")

# Modelo potenical o multiplicativo 
logy <- log(y)
logx <- log(x)
mod.pot <- lm(logy ~ logx); mod.pot;
summary(mod.pot)
## Coeficiente "a" del modelo ln(y)=a+b*x;
a <- mod.pot$coefficients[[1]];a;
## Coeficiente "b" del modelo ln(y)=a+b*x;;
b <- mod.pot$coefficients[[2]];b;
cov(logx,logy) ## Covarianza;
cor <- cor(logx,logy) ## Coeficiente de correlacion lineal;
R2 <- cor^2; R2
DesStaRes <- sqrt(sum((logy - (a+b*logx))^2)/(n-2)); DesStaRes;# Residual var
plot(logx,logy, col="blue");
curve(a+b*log(x), col="red", add=TRUE, lwd="3");

# Modelo logaritmico
logx <- log(x)
mod.log <- lm(y ~ logx); mod.log;
summary(mod.log)
## Coeficiente "a" del modelo ln(y)=a+b*x;
a <- mod.log$coefficients[[1]];a;
## Coeficiente "b" del modelo ln(y)=a+b*x;;
b <- mod.log$coefficients[[2]];b;
cov(logx,y) ## Covarianza;
cor <- cor(logx,y) ## Coeficiente de correlacion lineal;
R2 <- cor^2; R2
DesStaRes <- sqrt(sum((y - (a+b*logx))^2)/(n-2)); DesStaRes;# Residual var
plot(logx,y, col="blue");
curve(a+b*log(x), col="red", add=TRUE, lwd="3");
