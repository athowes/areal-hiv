ctx_ver <- "19-04-2021"

fit_files <- list.files(
  path = paste0("data/", ctx_ver, "/fits"),
  recursive = TRUE
)

meta_data <- strsplit(fit_files, '[/_.]')
index <- sapply(meta_data, function(list) sum(list %in% c("ck", "ik")))
stan_fits <- fit_files[as.logical(index)]

for(i in seq_along(stan_fits)) {
  print(which(rstan::summary(fit)[["summary"]][, "Rhat"] > 1.025))
}

for(i in seq_along(stan_fits)) {
  print(which(rstan::summary(fit)[["summary"]][, "Rhat"] > 1.05))
}
