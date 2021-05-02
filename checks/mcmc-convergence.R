ctx_ver <- "19-04-2021"

fit_files <- list.files(
  path = paste0("data/", ctx_ver, "/fits"),
  recursive = TRUE
)

meta_data <- strsplit(fit_files, '[/_.]')
index <- sapply(meta_data, function(list) sum(list %in% c("ck", "ik")))
stan_fit_files <- fit_files[as.logical(index)]
fits <- lapply(stan_fit_files, function(file) readRDS(file = paste0("data/", ctx_ver, "/fits/", file)))

rhat_check <- function(threshold) {
  for(i in seq_along(fits)) {
    print(paste0("For model ", i, " the following parameters have Rhat greater than ", threshold, ": "))
    print(which(rstan::summary(fits[[i]])[["summary"]][, "Rhat"] > threshold))
  }
}

rhat_check(1.025)
rhat_check(1.05)
