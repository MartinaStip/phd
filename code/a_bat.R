# Data 
batdata = map(batvars, ~ prep_bat(data, vars = .x, x_nsize = TRUE) %>%
                 #arrange(desc(zvar), yvar) %>%
                 mutate(ord = lab %>% as_factor(),
                        caption = case_when(str_detect(name,"study") ~ cap_prez, 
                                            TRUE ~ "")
                 ))
names(batdata) = batnames

batdata$study$xvar |> levels()

xx = batdata$study 

# Plots
batscales = list(
  study = c(uwb_palettes(name = "bi", type = 'continuous', n = 5, add_na = T)),
  condition = c(uwb_palettes(name = "bi", type = 'continuous', n = 5, add_na = T)),
  competence = c(uwb_palettes(name = "bi", type = 'continuous', n = 5, add_na = T)),
  research = c(uwb_palettes(name = "bi", type = 'continuous', n = 5, add_na = T))
)

batplots = map2(batdata, batnames, ~ plot_stack(.x) + 
                  scale_fill_manual(values = batscales[[.y]]) +
                  #guides(fill = guide_legend(nrow = batrows[[.y]])) +
                  theme(legend.margin = margin(l = -.3, unit = "npc"))
                ) 

#walk(batdata, ~ print(nrow(.x)))

walk2(batplots, batnames, ~ ggsave(.x, file = paste0("figs/", .y, ".png"),
                                   width = w, height = h + 0.075 * nrow(batdata[[.y]])))


# ##############################check
# ggplot(batdata$research, aes(y = yvar, x = xvar, fill = zvar )) +
#   geom_bar(stat = "identity", width = 0.85, colour = uwb_vals$barcol,
#              position = position_stack(reverse = TRUE)) +
#     scale_fill_uwb("quali") +
#     ylim(0, 100.1) +
#     geom_text(aes(x = fct_rev(xvar), y = pos, label = labvar),
#               #family = uwb_vals$font,
#               colour = "white", size = uwb_vals$labsize) +
#     labs(y = "%", x = "", fill = "",
#          title = dat$title[1],
#          subtitle = dat$subtitle[1],
#          #caption = dat$caption[1]
#          ) +
#     coord_flip() +
#     theme_uwb_horiz() 
  
