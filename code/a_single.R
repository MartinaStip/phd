# single vars
singlevars = c("form", "solution", "finance", "gender", "fak")

singledata = map(singlevars, ~ prep_single(data, !!sym(.x))) %>% 
  set_names(singlevars)

singleplots = map(singledata, ~ plot_bar(.x)) %>% 
  set_names(singlevars)
  
walk2(singleplots, names(singleplots), ~ ggsave(.x, file = paste0("figs/", .y, ".png"), 
                                           width = w, height = h))





# library(grid)
# library(gridtext)
# 
# grid.newpage()
# grid.draw(richtext_grob("Hello<br><span style='color:red'>Red</span>"))
# 
# 
# # This works 
# #windows()   # opens a new plot window
# ggplot(mtcars, aes(mpg, wt)) +
#   geom_point() +
#   scale_x_continuous(
#     breaks = c(10, 20, 30),  # specify numeric positions where labels appear
#     labels = c("A", "B", "C<br><span style='color:red'>Red</span>")
#   ) +
#   theme(axis.text.x = ggtext::element_markdown())
