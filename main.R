# Objective Function
eval_f <- function(x)
{
  return (x[1]*x[4]*(x[1] +x[2] + x[3] ) + x[3] )
}
# Inequality constraints
eval_g_ineq <- function(x)
{
  return (25 - x[1]*x[2]*x[3]*x[4])
}
# Equality constraints
eval_g_eq <- function(x)
{
  return ( x[1]^2 + x[2]^2 + x[3]^2 + x[4]^2 - 40 )
}
# Lower and upper bounds
lb <- c(1,1,1,1)
ub <- c(5,5,5,5)