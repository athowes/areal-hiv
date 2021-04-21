# Debugging INLA crash on DIDEHPC
# INLA_21.01.08-1 is the first build which breaks
# https://github.com/hrue/r-inla/blob/devel/rinla/inst/NEWS.Rd#L69

haakon_seeds(verbose = TRUE)

q <- obj$enqueue(haakon_seeds(verbose = TRUE))

q$status()
q$result()
q$log()

# Note: for cluster workarounds
# Use expand.grid on each parameter vector to create pars
# Then mapply(FUN = fn, pars$Var1, pars$Var2, ...)