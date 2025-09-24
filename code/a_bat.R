# Identify bat items

batnames = c("study", "research")

batvars = map(batnames, ~ codebook %>% 
                filter(!str_detect(name, "en_|dupli_")) %>% 
                filter(str_detect(name, paste0(.x, "_"))) %>% 
                pull(name)) %>% 
  set_names(batnames)

codebook

# # Remove R with all misssings in a the battery question
# allmissing = map(batvars, ~ zcu %>% 
#                    select(rid, .x) %>% 
#                    rowwise() %>% 
#                    mutate(nr_missing = sum(is.na(across(everything())))) %>% 
#                    select(-.x) %>% 
#                    filter(nr_missing < length(.x)) %>% 
#                    pull(rid)
# )
# walk(allmissing, ~ zcu %>% 
#        filter(rid %in% .x) %>%
#        nrow() %>% print()
# )

# Data 
batdata = map(batvars, ~ prep_bat(data, vars = .x) %>%
                 #arrange(desc(zvar), yvar) %>%
                 mutate(ord = lab %>% as_factor(),
                        xvar = str_wrap(xvar, uwb_vals$chrnum) %>% as_factor, 
                        #xvar = fct_reorder(str_wrap(xvar, uwb_vals$chrnum), as.numeric(ord)),
                        #subtitle = case_when(subtitle == "" ~ paste0("N=", nsize),
                         #                    TRUE ~ paste0(subtitle, ", N=", nsize)),
                 )
)
names(batdata) = batnames

# Plots
batscales = list(
  study = c(uwb_palettes(name = "bi", type='continuous', n = 5, add_na = T)),
  research = c(uwb_palettes(name = "bi", type='continuous', n = 5, add_na = T))
)


batplots = map2(batdata, batnames, ~ plot_stack(.x) + 
                  scale_fill_manual(values = batscales[[.y]]) 
                  #guides(fill = guide_legend(nrow = batrows[[.y]])) +
                  #theme(legend.margin = margin(l = -.3, unit = "npc"))
                ) 
batplots$study

#walk(batdata, ~ print(nrow(.x)))

walk2(batplots, batnames, ~ ggsave(.x, file = paste0("figs/", .y, ".png"),
                                   width = w, height = h + 0.075 * nrow(batdata[[.y]])))
