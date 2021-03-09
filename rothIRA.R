library('nloptr')

bndYield = .04
bndAppr = .02
stckYield = .02
stckAppr = .08
reitYield = .04
reitAppr = .04
stockPercent = .7
bondPercent = .2
reitPercent = .1
totalInvestment = 20000
capitalGainTax = .15
incomeTax = .3
dividendTax = .15
iraLimit = 6000
years = 40


# Objective Function
eval_f <- function(x)
{
  return ( -( (1-capitalGainTax) *(x[1]*(1 + (bndYield*(1-incomeTax)) + bndAppr)^years - x[1]) + x[1]) -
             ((1-capitalGainTax)*(x[2]*(1 + (stckYield*(1-dividendTax)) + stckAppr)^years - x[2]) + x[2]) -
             ((1-capitalGainTax)*(x[3]*(1 + (reitYield*(1-incomeTax)) + reitAppr)^years - x[3]) + x[3]) -  
             (x[4]*(1 + bndAppr + bndYield)^years) -
             (x[5]*(1 + stckAppr + stckYield)^years) -
             (x[6]*(1 + reitAppr + reitYield)^years)
               )
}
# Inequality constraints
eval_g_ineq <- function(x)
{
  return (x[6] + x[5] + x[4] - iraLimit)
}
# Equality constraints
eval_g_eq <- function(x)
{
  constr <- c( x[1] + x[2] + x[3] + x[4]+ x[5] + x[6] - totalInvestment,
           x[1] + x[4] - (totalInvestment * bondPercent),
           x[2] + x[5] - (totalInvestment * stockPercent),
           x[3] + x[6] - (totalInvestment * reitPercent))
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
