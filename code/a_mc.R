

# Remove R with all misssings in a MC question
allmissing = map(mcvars, ~ data %>% 
                   select(Id.respondenta, all_of(.x)) %>% 
                   rowwise() %>% 
                   mutate(nr_missing = sum(is.na(across(everything())))) %>% 
                   select(-.x) %>% 
                   filter(nr_missing < length(.x)) %>% 
                   pull(Id.respondenta)
                 )

walk(allmissing, ~ data %>% 
       filter(Id.respondenta %in% .x) %>%
       nrow() %>% print()
     )

# Situation needs another treatment, because very few Rs responded properly by choosing 
# "none" opetion. I will use all Rs who completed questionnaire as a basis for percentages.
allmissing$situation = data |> 
  filter(Status.odpovědi == "dokončená") |> 
  pull(Id.respondenta)

# Data
mcdata = map2(allmissing, mcvars, ~ 
                prep_mc(data %>% filter(Id.respondenta %in% .x), 
                        vars = .y, chosen = 1) %>% 
                mutate(xvar = fct_reorder(str_wrap(xvar, uwb_vals$chrnum), desc(yvar)),
                       #xvar = fct_relevel(xvar, "Jiné", after = Inf),
                       cvar = case_when(str_detect(xvar, "ic z uveden") ~ c_nor,
                                        #str_detect(xvar, "žádné") ~ uwb_scales$quali[2],
                                        TRUE ~ cvar),
                       subtitle =paste0("N=", nsize)
                       )
)
names(mcdata) = mcnames

mcdata$situation = mcdata$situation |> 
  filter(!str_detect(xvar, "Nic"))


# Plots
mcplots = map2(mcdata, mcnames, ~ plot_lolli(.x)) 

mcplots[[1]]
mcplots[[2]]

# Plots
walk2(mcplots, mcnames, ~ ggsave(.x, file = paste0("figs/", .y, ".png"),
                                   width = w, height = h + 0.075 * nrow(mcdata[[.y]])))
