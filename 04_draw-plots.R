theme_adam <- theme(
  panel.grid.major = element_line(colour = "grey90", size = 0.2),
  panel.grid.minor = element_line(colour = "grey98", size = 0.5),
  panel.background = element_rect()
)

theme_adam_minimal <- theme_adam + theme(
  axis.text = element_text(size = rel(0.8)), 
  axis.ticks = element_line(colour = "black"), 
  panel.background = element_rect(fill = "white", colour = NA)
)

# fig61 -------------------------------------------------------------------

training_sets <- create_folds(zw, remove_cols = c("y", "est"), type = "SLOO")

df <- training_sets[[2]]$data %>%
  mutate(left_out = as.numeric(is.na(est)))

df$left_out[training_sets[[2]]$predict_on] <- 2

tikz(file = "fig61.tex", width = 6, height = 2.5)
ggplot(df, aes(fill = as.factor(left_out))) +
  geom_sf(aes(geometry = geometry)) +
  coord_sf() +
  theme_minimal() +
  scale_fill_manual(values = c(sf_lightgrey, lightblue, lightgreen),
                    name = "",
                    labels = c("$A_{-(i, \\delta i)}$", "$A_{\\delta i}$", "$A_i$")) +
  theme_void()
dev.off()

# fig62 -------------------------------------------------------------------

tikz(file = "plots/fig62.tex", width = 6, height = 5.5)
full_cv_df %>%
  filter(geometry != "Tanzania 2012 AIS") %>%
  boxplot(metric = "crps", title = "CRPS", facet_type = TRUE) +
    theme_adam
dev.off()

system("cd plots && lualatex compile_fig62.tex")

# figB1 -------------------------------------------------------------------

tikz(file = "plots/figB1.tex", width = 6.25, height = 8.5)
scoropleth(cv_id_df, metric = "crps", g = "Cote d'Ivoire 2012 DHS", t = "LOO", sf = ci) + theme_adam_minimal
dev.off()

system("cd plots && lualatex compile_figB1.tex")

# figB2 -------------------------------------------------------------------

tikz(file = "plots/figB2.tex", width = 6.25, height = 8.5)
scoropleth(cv_id_df, metric = "crps", g = "Malawi 2015 DHS", t = "LOO", sf = mw) + theme_adam_minimal
dev.off()

system("cd plots && lualatex compile_figB2.tex")

# figB3 -------------------------------------------------------------------

comp <- sf_to_nb(tz) %>%
  spdep::n.comp.nb() # Connected components

mainland <- which(comp$comp.id == 1)

tikz(file = "plots/figB3.tex", width = 6.25, height = 8.5)
scoropleth(cv_id_df, metric = "crps", g = "Tanzania 2012 AIS (no islands)", t = "LOO", sf = tz[mainland, ]) + theme_adam_minimal
dev.off()

system("cd plots && lualatex compile_figB3.tex")

# figB4 -------------------------------------------------------------------

tikz(file = "plots/figB4.tex", width = 6.25, height = 8.5)
scoropleth(cv_id_df, metric = "crps", g = "Zimbabwe 2015 DHS", t = "LOO", sf = zw) + theme_adam_minimal
dev.off()

system("cd plots && lualatex compile_figB4.tex")

# figB5 -------------------------------------------------------------------

tikz(file = "plots/figB5.tex", width = 6.25, height = 8.5)
prev_ladder(fit_df, id = "CI2012DHS", level = 2) + theme_adam_minimal
dev.off()

system("cd plots && lualatex compile_figB5.tex")

# figB6 -------------------------------------------------------------------

tikz(file = "plots/figB6.tex", width = 6.25, height = 8.5)
prev_ladder(fit_df, id = "MW2015DHS", level = 1) + theme_adam_minimal
dev.off()

system("cd plots && lualatex compile_figB6.tex")

# figB7 -------------------------------------------------------------------

tikz(file = "plots/figB7.tex", width = 6.25, height = 8.5)
prev_ladder(fit_df, id = "TZ2012AIS-no-islands", level = 2) + theme_adam_minimal
dev.off()

system("cd plots && lualatex compile_figB7.tex")

# figB8 -------------------------------------------------------------------

tikz(file = "plots/figB8.tex", width = 6.25, height = 8.5)
prev_ladder(fit_df, id = "ZW2015DHS", level = 2) + theme_adam_minimal
dev.off()

system("cd plots && lualatex compile_figB8.tex")


# Compression -------------------------------------------------------------

system("sh scripts/compress_phd.sh")

# Experimental ------------------------------------------------------------

score_ladder(cv_id_df, metric = "crps", g = "2012 DHS Cote d'Ivoire", t = "LOO", sf = ci, level = 2)
score_ladder(cv_id_df, metric = "crps", g = "2015 DHS Malawi", t = "LOO", sf = mw, level = 1)
score_ladder(cv_id_df, metric = "crps", g = "2012 AIS Tanzania (no islands)", t = "LOO", sf = tz, level = 2)
score_ladder(cv_id_df, metric = "crps", g = "2015 DHS Zimbabwe", t = "LOO", sf = zw, level = 2)
