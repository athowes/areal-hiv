# Function for debugging INLA on the cluster
haakon_seeds <- function(verbose = FALSE){
  data(Seeds)
  df <- data.frame(y = Seeds$r, Ntrials = Seeds$n, Seeds[, 3:5])
  family <- "binomial"
  control.family <- list(control.link = list(model = "logit"))
  hyper <- list(theta = list(prior = "pc.prec", param = c(1, 0.01)))
  formula <- y ~ x1 + x2 + f(plate, model="iid", hyper = hyper)
  res <- inla(formula = formula, 
              data = df, 
              family = family,
              Ntrials=Ntrials, 
              control.family = control.family,
              verbose = verbose)
  return(res)
}
