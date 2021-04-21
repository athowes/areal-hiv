intervals <- function(fit, ...) {
  UseMethod("intervals")
}
  
intervals.stanfit <- function(fit, parameter_name) {
  l <- nchar(parameter_name)
  data.frame(summary(fit)$summary) %>% 
    tibble::rownames_to_column("parameter") %>% 
    filter(substr(parameter, 1, l) == parameter_name) %>%
    dplyr::select(mean, sd, lower = X2.5., upper = X97.5.) %>%
    dplyr::mutate(id = row_number()) %>%
    tibble::remove_rownames()
}
  
intervals.inla <- function(fit, parameter_name){
  # Assumes parameter_name = "rho"
  fit$summary.fitted.values %>%
    dplyr::select(mean, sd, lower = "0.025quant", upper = "0.975quant") %>%
    dplyr::mutate(id = row_number()) %>%
    tibble::remove_rownames()
}
