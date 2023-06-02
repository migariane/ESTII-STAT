# Optimizacion con restriciones (ejemplo clase)
# install.packages("lpSolve")
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
print(optimum)



