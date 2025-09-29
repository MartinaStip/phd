# single vars
singlevars = c("form", "solution", "finance", "gender", "fak")

singledata = map(singlevars, ~ prep_single(data, !!sym(.x))) %>% 
  set_names(singlevars)

singleplots = map(singledata, ~ plot_bar(.x)) %>% 
  set_names(singlevars)
  
walk2(singleplots, names(singleplots), ~ ggsave(.x, file = paste0("figs/", .y, ".png"), 
                                           width = w, height = h))


