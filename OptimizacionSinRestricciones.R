# Optimizacion sin restricciones bivariada
f <- function(x) ((x[1])^2 + (x[2])^2)
r <- optim(c(1, 1), f);r
# Optimo (0,0)

# Derivadas en R
f <- expression((x)^2 - (y)^2)
# Hessian
fprimaxy <- D(f,"x")
fprimax
fprimaxy <- D(f,"y")
fprimaxy
fprimaxy <- D(f,"yx")
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
f <- function(x) (x[1]^3 + 3*x[1]*x[2]^2 -15*x[1] -12*x[2])
r <- optim(c(5, 5), f);r
# Optimo (2,1): Minimo
# Derivadas parciales en R y Hessian
f <- expression(x^3 + 3*x*y^2 -15*x -12*y)

# Derivadas Parciales
fprimax <- D(f,"x")
fprimax
fprimay <- D(f,"y")
fprimay

# Hessian
f2x <- expression(3 * x^2 + 3 * y^2 - 15)
f2primax <- D(f2x,"x")
f2primax

f2y <- expression(6 * x*y - 12)
f2yprimax <- D(f2y,"y")
f2yprimax

f2xy <- expression(3 * x^2 + 3 * y^2 - 15)
f2xyprimax <- D(f2x,"y")
f2xyprimax

f2yx <- expression(6 * x*y - 12)
f2yxprimax <- D(f2xy,"x")
f2yxprimax

my_function <- function(x,y) {
  final_value = x^3 + 3*x*y^2 -15*x -12*y
}

input_1 <- seq(-3, 3,0.1)
input_2 <- seq(-3, 3,0.1)
z <- outer(input_1, input_2, my_function)
plot_ly(x = input_1, y = input_2, z = z) %>% add_surface()
