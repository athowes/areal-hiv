# Checking the BYM2 proportion posterior
do.call("rbind", lapply(fit_files[str_detect(fit_files, "bym")], FUN = function(file) {
  fit <- readRDS(file = paste0("data/", ctx_ver, "/fits/", file))
  meta_data <- strsplit(file, '[/_.]')
  df <- data.frame(name = meta_data[[1]][1], fit$summary.hyperpar["Phi for id", ])
  rownames(df) <- NULL
  return(df)
}))

# Comparison of model fit to Tanzania with and without the islands
tanzania_only <- full_cv_df %>%
  filter(geometry %in% c("Tanzania 2012 AIS (no islands)", "Tanzania 2012 AIS"))

# First look
boxplot(tanzania_only, metric = "crps", title = "CRPS")

# Ridges plots to look at the distributions of CRPS
tanzania_only %>%
  filter(inf_model == "Besag") %>%
  ggplot(aes(y = geometry, x = crps, fill = type)) +
    ggridges::geom_density_ridges(jittered_points = TRUE, position = "raincloud", 
                                  point_alpha = 0.3, scale = 0.5) +
    facet_wrap(~type) +
    labs(x = "CRPS", y = "", fill = "CV type") +
    scale_fill_manual(values = cbpalette)

# What is Besag doing that is different to the others on the islands?
tz <- readRDS("data/hiv_surveys.rds") %>%
  filter(survey_id == "TZ2012AIS")

comp <- sf_to_nb(tz) %>%
  spdep::n.comp.nb() # Connected components

islands <- which(comp$comp.id != 1)

# What do the islands look like?
ggplot(tz[islands, ]) +
  geom_sf(aes(fill = y)) +
  viridis::scale_fill_viridis()  

# The Besag predictions do look very different
# y_bar is mean(s$y_samples)
# The constant model doesn't have identical values as sample sizes varies by region
# The light blue dashed line is the largest value of y in the data
tanzania_only %>%
  filter(geometry == "Tanzania 2012 AIS", id %in% islands) %>%
  ggplot() +
    geom_jitter(aes(x = inf_model, y = y_bar), width = 0.1, alpha = 0.5) +
    facet_wrap(~type) +
    geom_hline(yintercept = max(filter(tanzania_only, geometry == "Tanzania 2012 AIS", id %in% islands)$y),
               col = lightblue, linetype = "dashed") +
    labs(x = "Inferential model", y = "Mean prediction")
