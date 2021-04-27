ctx_ver <- "19-04-2021"

fit_files <- list.files(
  path = paste0("data/", ctx_ver, "/fits"),
  recursive = TRUE
)

# DIC, WAIC from fits
ic_df <- do.call("rbind", lapply(fit_files, FUN = function(file) {
  fit <- readRDS(file = paste0("data/", ctx_ver, "/fits/", file))
  meta_data <- strsplit(file, '[/_.]')
  dic <- bsae::dic(fit)
  waic <- bsae::waic(fit)
  
  df <- data.frame(
    geometry = meta_data[[1]][1],
    inf_model = meta_data[[1]][2],
    dic = dic$est,
    dic_se = dic$se,
    waic = waic$est,
    waic_se = waic$se
  )
  
  return(df)
}))

# LOO, SLOO from cv
cv_files <- list.files(
  path = paste0("data/", ctx_ver, "/cv"),
  recursive = TRUE
)

full_cv_df <- do.call("rbind", lapply(cv_files, FUN = function(file) {
  cv <- readRDS(file = paste0("data/", ctx_ver, "/cv/", file))
  meta_data <- strsplit(file, '[/_.]')
  
  df <- cbind(
    geometry = meta_data[[1]][1],
    inf_model = meta_data[[1]][2],
    type = meta_data[[1]][3],
    tidyr::unnest(
      as.data.frame(cv$scores),
      cols = everything()
    )
  )
  
  if(length(meta_data[[1]]) > 4) {
    index_string <- strsplit(meta_data[[1]][4], "-")
    index <- as.numeric(index_string[[1]])
    out <- cbind(cbind(id = index[1]:index[2], df))
  } else {
    out <- cbind(id = 1:nrow(df), df)
  }

  return(out)
}))

cv_df <- full_cv_df %>%
  group_by(geometry, inf_model, type) %>%
  summarise(n = n(), across(mse:lds, list(mean = mean, se = ~ sd(.x) / sqrt(length(.x))))) %>%
  mutate(type = recode_factor(type, "loo" = "LOO", "sloo" = "SLOO")) %>%
  rename_df()

cv_id_df <- full_cv_df %>%
  group_by(geometry, inf_model, type, id) %>%
  summarise(n = n(), across(mse:lds, list(mean = mean, se = ~ sd(.x) / sqrt(length(.x))))) %>%
  mutate(type = recode_factor(type, "loo" = "LOO", "sloo" = "SLOO")) %>%
  rename_df()

# This for tab_spanner_delim later on
cv_df_wider <-  cv_df %>%
  pivot_wider(names_from = type, values_from = mse_mean:lds_se, names_sep = ".")

# Merging the two and tidying up
df <- merge(rename_df(ic_df), cv_df_wider)

# Full table format
df %>%
  arrange(match(inf_model, c("Constant", "IID", "Besag", "BYM2", "FCK", "CK", "FIK", "IK"))) %>%
  arrange_at(1) %>%
  gt(rowname_col = "inf_model", groupname_col = "geometry") %>%
  tab_spanner_delim(delim = ".") %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups()) %>%
  as_latex() %>%
  as.character() %>%
  cat()

# Posterior means and 95% credible intervals from fits
fit_df <- do.call("rbind", lapply(fit_files, FUN = function(file) {
  fit <- readRDS(file = paste0("data/", ctx_ver, "/fits/", file))
  meta_data <- strsplit(file, '[/_.]')
  
  df <- data.frame(
    geometry = meta_data[[1]][1],
    inf_model = meta_data[[1]][2],
    intervals(fit, parameter = "rho")
  )
  return(df)
}))

# Supplementary material
metric_table(df, "crps", latex = TRUE)
