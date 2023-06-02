# Optimizacion con restriciones (ejemplo clase)
# install.packages("lpSolve")
library(lpSolve)

# Primer ejemplo: solucion en vertice

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
print(optimum)

# Segundo ejemplo: solucion en arista (in R only one point)

# Objective function
objective.in <- c(400, 500)

# Variables and restrictions
const.mat <- matrix(c(4000, 5000, 4, 3, 3, 2, 2,1), nrow=4,
                    byrow=TRUE)
const.rhs <- c(32000, 24, 20, 16)
const.dir <- c("<=", "<=")

# Optimization function
optimum <- lp(direction="max", objective.in, const.mat,
              const.dir, const.rhs)
optimum$solution 
optimum$objval
print(optimum)

