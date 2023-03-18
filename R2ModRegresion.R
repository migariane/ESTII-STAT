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

## Modelo Exponencial y = a*(b^X)
logy <- log(y)
mod.exp <- lm(logy ~ x); mod.exp;
summary(mod.exp)
## Coeficiente "a" del modelo exp Y=a*(b^X);
a <- exp(mod.exp$coefficients[[1]]);a;
## Coeficiente "b" del modelo exp Y=a*(b^X);;
b <- exp(mod.exp$coefficients[[2]]);b;
cov(x,logy) ## Covarianza;
cor <- cor(x,logy) ## Coeficiente de correlacion lineal;
R2 <- cor^2; R2
DesStaRes <- sqrt(sum((y - (a*(b^x)))^2)/(n-2)); DesStaRes;# Residual var
plot(x,y, col="blue");
curve(a*(b^x), col="red", add=TRUE, lwd="3");

## Modelo parabolico y = a+bx+cX2
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

