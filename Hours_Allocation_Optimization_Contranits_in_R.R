library(lpSolve)
obj.fun <- c(80,129)
constr.lhs <- matrix(c(5,6,1,2,1,0,0,1,1,0,0,1),ncol=2, byrow=TRUE)
constr.dir <- c("<=","<=","<=","<=",">=",">=")
constr.rhs <- c(10000,3000,600,1200,0,0)
prod.solve <- lp("max",obj.fun,constr.lhs,constr.dir,constr.rhs,compute.sens = TRUE)
prod.solve$solution
sum(prod.solve$objective * prod.solve$solution)

AllSolutions = NULL
for (o1 in c(75:85)) {
  for (o2 in c(125:135)) {
    obj.fun <- c(o1,o2)
    prod.solve <- lp("max",obj.fun, constr.lhs,constr.dir,constr.rhs,compute.sens = TRUE)
    AllSolutions = rbind(AllSolutions, c(prod.solve$solution[1],prod.solve$solution[2],sum(prod.solve$objective * prod.solve$solution)))
  }
}
AllSolutions 



AllSolutions = NULL
for (rhs1 in seq(9000, 11000, by = 100)) {
  for (rhs2 in seq(2700,3300,by = 100)) {
    obj.fun <- c(80,129)
    rhs <- c(rhs1,rhs2,600,1200,0,0)
    prod.solve <- lp("max",obj.fun,constr.lhs,constr.dir,constr.rhs,compute.sens = TRUE)
    AllSolutions = rbind(AllSolutions, c(rhs1,rhs2,prod.solve$solution[1],prod.solve$solution[2],sum(prod.solve$objective * prod.solve$solution)))
  }
}
AllSolutions 


AllSolutions=NULL
for (i in c(1:100)) {
  o1 = rnorm(1,80,8)
  o2 = rnorm(1,129,12.9)
  obj.fun = c(o1,o2)
  
  rhs1 = rnorm(1,10000,1000)
  rhs2 = rnorm(1,3000,300)
  mx1 = rnorm(1,600,60)
  mx2 = rnorm(1,1200,120)
  rhs <- c(rhs1, rhs2, mx1, mx2,0,0)
  prod.solve <- lp("max",obj.fun,constr.lhs,constr.dir,rhs,compute.sens = TRUE)
  AllSolutions = rbind(AllSolutions, c(prod.solve$solution[1],prod.solve$solution[2],sum(prod.solve$objective * prod.solve$solution)))
}
AllSolutions
