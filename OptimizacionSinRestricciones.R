# Optimizacion sin restricciones bivariada: Ejemplo clase
f <- function(x) ((x[1])^2 - (x[2])^2)
r <- optim(c(2, 2), f, hessian=T);r
# Optimo (0,0)

# Derivadas en R
f <- expression((x)^2 - (y)^2)
# Hessian
fprimax <- D(f,"x")
fprimax
fprimay <- D(f,"y")
fprimay
fprimayx <- D(f,"yx")
fprimayx
fprimaxy <- D(f,"xy")
fprimaxy

# Grafica de la funcion
# install.packages("plotly")
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

# Optimizacion con restriciones (ejemplo clase)

install.packages("lpSolve")
library(lpSolve)

# Objective function
objective.in <- c(3, 6)
# Variables and restrictions
const.mat <- matrix(c(20, 50, 4, 3), nrow=2,
                    byrow=TRUE)
const.rhs <- c(3300, 380)
const.dir <- c("<=", "<=")

# Optimization function
optimum <- lp(direction="max", objective.in, const.mat,
              const.dir, const.rhs)
optimum$solution 
optimum$objval