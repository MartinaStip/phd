
# Data
mcdata






# Plots
walk2(mcplots, mcnames, ~ ggsave(.x, file = paste0("figs/", .y, ".png"),
                                   width = w, height = h + 0.075 * nrow(batdata[[.y]])))