# Optimizacion sin restricciones bivariada: Ejemplo clase
f <- function(x) ((x[1])^2 - (x[2])^2)
r <- optim(c(1, 1), f);r
# Optimo (0,0)

# Derivadas en R
f <- expression((x)^2 - (y)^2)
# Hessian
fprimax <- D(f,"x")
fprimax
fprimay <- D(f,"y")
fprimay
fprimaxy <- D(f,"yx")
fprimaxy
fprimaxy <- D(f,"xy")
fprimaxy

# Grafica de la funcion
library(plotly)
my_function <- function(x,y) {
  final_value = x^2 - y^2
}

input_1 <- seq(-1.5, 1.5,0.1)
input_2 <- seq(-1.5, 1.5,0.1)
z <- outer(input_1, input_2, my_function)
plot_ly(x = input_1, y = input_2, z = z) %>% add_surface()


# Otro ejemplo
# Minimizacion
f <- function(x) (x[1]^3 + 3*x[1]*x[2]^2 -15*x[1] -12*x[2])
r <- optim(c(1, 1), f, hessian=T);r
# Maximizacion (-)
f <- function(x) -(x[1]^3 + 3*x[1]*x[2]^2 -15*x[1] -12*x[2])
r <- optim(c(1, 1), f, hessian=T);r

# Optimo (2,1): Minimo
# Derivadas parciales en R y Hessian
f <- expression(x^3 + 3*x*y^2 -15*x -12*y)

# Derivadas Parciales
fprimax <- D(f,"x")
fprimax
fprimay <- D(f,"y")
fprimay

# Hessian
# Partial derivatives
f2x <- expression(3 * x^2 + 3 * y^2 - 15)
f2xprimax <- D(f2x,"x")
f2xprimax

f2y <- expression(6 * x*y - 12)
f2yprimax <- D(f2y,"y")
f2yprimax

#Partial derivatives xy
f2x <- expression(3 * x^2 + 3 * y^2 - 15)
f2xprimax <- D(f2xy,"y")
f2xprimax

f2y <- expression(6 * x*y - 12)
f2yprimax <- D(f2y,"x")
f2yprimax

my_function <- function(x,y) {
  final_value = x^3 + 3*x*y^2 -15*x -12*y
}

input_1 <- seq(-3, 3,0.1)
input_2 <- seq(-3, 3,0.1)
z <- outer(input_1, input_2, my_function)
plot_ly(x = input_1, y = input_2, z = z) %>% add_surface()
