# Prevalence ladder plots
prev_ladder <- function(df, id, level) {
  hiv_surveys <- readRDS("data/hiv_surveys.rds")
  
  sf <- subset(hiv_surveys, survey_id == id) %>%
    dplyr::mutate(id = row_number())
  
  names(sf)[names(sf) == paste0("name_", level)] <- "est_level"
  
  df_id <- fit_df[which(fit_df$geometry == id), ] %>% # Strange behaviour here from subset and filter
    rename_df()
  
  inner_join(df_id, sf, by = "id") %>%
    ggplot(aes(x = reorder(est_level, mean), y = mean, ymin = lower, ymax = upper, 
               group = inf_model, color = inf_model)) +
    geom_pointrange(position = position_dodge(width = 0.6), alpha = 0.8) +
    labs(x = "District", y = "Posterior prevalence estimate", col = "Inferential model") +
    theme_minimal() +
    scale_color_manual(values = cbpalette) +
    coord_flip()
}

# Dynamite plots
dynamite_plot <- function(df, metric, title) {
  metric_mean <- paste0(metric, "_mean")
  metric_se <- paste0(metric, "_se")
  
  # .data see https://adv-r.hadley.nz/evaluation.html#data-masks
  ggplot(df, aes(x = inf_model, y = .data[[metric_mean]], fill = type)) +
    geom_col(position = position_dodge(width = 0.9), alpha = 0.8) +
    geom_errorbar(aes(ymin = .data[[metric_mean]] - .data[[metric_se]], 
                      ymax = .data[[metric_mean]] + .data[[metric_se]]), 
                  position = position_dodge(width = 0.9), alpha = 0.8, width = 0.75) +
    facet_wrap(~geometry, scales = "free") +
    labs(x = "Inferential model", y = title, fill = "CV method") +
    scale_fill_manual(values = cbpalette) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}

# Boxplots
boxplot <- function(df, metric, title) {
  
  .calc_boxplot_stat <- function(x) {
    coef <- 1.5
    n <- sum(!is.na(x))
    stats <- quantile(x, probs = c(0.0, 0.25, 0.5, 0.75, 1.0))
    names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
    iqr <- diff(stats[c(2, 4)])
    outliers <- x < (stats[2] - coef * iqr) | x > (stats[4] + coef * iqr)
    if (any(outliers)) {
      stats[c(1, 5)] <- range(c(stats[2:4], x[!outliers]), na.rm = TRUE)
    }
    return(stats)
  }
  
  ggplot(df, aes(x = inf_model, y = .data[[metric]], fill = type)) +
    stat_summary(fun.data = .calc_boxplot_stat, geom = "boxplot", position = position_dodge(), width = 0.5, alpha = 0.9, show.legend = FALSE) +
    stat_summary(fun = "mean", geom = "point", position = position_dodge(width = 0.5), alpha = 0.7, show.legend = FALSE) +
    facet_wrap(~geometry, scales = "free", nrow = 2) +
    labs(x = "Inferential model", y = title, fill = "CV method") +
    scale_fill_manual(values = cbpalette) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "bottom")
}

# Scoropleths
scoropleth <- function(df_id, metric, g, t, sf) {
  metric_mean <- paste0(metric, "_mean")
  
  df <- df_id %>% filter(geometry == g, type == t)
  
  # How many simulation models here?
  n_inf <- length(unique(df$inf_model))
  
  df %>%
    cbind(rep(sf$geometry, n_inf)) %>%
    st_as_sf() %>%
    ggplot() + 
    geom_sf(aes(fill = .data[[metric_mean]])) +
    facet_wrap(~inf_model) +
    scale_fill_viridis() +
    labs(fill = toupper(metric)) + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
}

# Largest five districts by population according to Wikipedia
big5 <- list(
  # https://en.wikipedia.org/wiki/Regions_of_Ivory_Coast
  "2012 DHS Cote d'Ivoire" = c("Abidjan", "Lagunes", "Woroba", "Savanes", "Lacs"),
  # https://en.wikipedia.org/wiki/Districts_of_Malawi
  "2015 DHS Malawi" = c("Lilongwe", "Blantyre", "Mzimba", "Mangochi", "Zomba"),
  # https://en.wikipedia.org/wiki/Districts_of_Tanzania
  "2012 AIS Tanzania" = c("Temeke", "Kinondoni", "Ilala", "Geita", "Sengerema"),
  # https://en.wikipedia.org/wiki/Districts_of_Zimbabwe
  "2015 DHS Zimbabwe" = c("Harare", "Shurugwi", "Bulawayo", "Mutare", "Gokwe South")
)

# Score ladder plots
score_ladder <- function(df_id, metric, g, t, sf, level) {
  metric_mean <- paste0(metric, "_mean")
  
  names(sf)[names(sf) == paste0("name_", level)] <- "est_level"
  
  df <- df_id %>%
    filter(geometry == g, type == t) %>%
    inner_join(dplyr::mutate(sf, id = row_number()), by = "id") %>%
    mutate(city = ifelse(est_level %in% big5[[g]], yes = TRUE, no = FALSE))
  
  ggplot(df, aes(x = reorder(est_level, .data[[metric_mean]]), y = .data[[metric_mean]], col = inf_model)) +
    geom_point(shape = 4, size = 1.5) +
    scale_color_manual(values = cbpalette) +
    coord_flip() + 
    labs(y = paste0("$\\mathrm{", t, "-", toupper(metric), "}_i$"), x = "District", col = "Inferential model")
}
