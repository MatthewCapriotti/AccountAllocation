library('nloptr')

# Objective Function
eval_f <- function(x)
{
  return ( -(.85*(x[1]*(1 + (.04*.7) + .02)^40 - x[1]) + x[1]) -
             (.85*(x[2]*(1 + (.02*.85) + .08)^40 - x[2]) + x[2]) -
             (.85*(x[3]*(1 + (.04*.7) + .04)^40 - x[3]) + x[3]) -  
             (x[4]*(1.06)^40) -
             (x[5]*(1.1)^40) -
             (x[6]*(1.08)^40)
               )
}
# Inequality constraints
eval_g_ineq <- function(x)
{
  return (x[6] + x[5] + x[4] - 6000)
}
# Equality constraints
eval_g_eq <- function(x)
{
  constr <- c( x[1] + x[2] + x[3] + x[4]+ x[5] + x[6] - 20000,
           x[1] + x[4] - 4000,
           x[2] + x[5] - 14000,
           x[3] + x[6] - 2000)
  return (constr)
}
# Lower and upper bounds
lb <- c(0,0,0,0,0,0)
ub <- c(4000,14000,2000,4000,14000,2000)
#initial values
x0 <- c(0,0,0,0,0,0)
# Set optimization options.
local_opts <- list( "algorithm" = "NLOPT_LD_MMA", "xtol_rel" = 1.0e-15 )
opts <- list( "algorithm"= "NLOPT_GN_ISRES",
              "xtol_rel"= 1.0e-15,
              "maxeval"= 16000000,
             "local_opts" = local_opts,
              "print_level" = 0 )
res <- nloptr ( x0 = x0,
                eval_f = eval_f,
                lb = lb,
                ub = ub,
                eval_g_ineq = eval_g_ineq,
                eval_g_eq = eval_g_eq,
                opts = opts)
print(res)
