# Identify bat items

batnames = c("study", "condition", "competence", "research")

batvars = map(batnames, ~ codebook %>% 
                filter(!str_detect(name, "en_|dupli_")) %>% 
                filter(str_detect(name, paste0(.x, "_"))) %>% 
                pull(name)) %>% 
  set_names(batnames)

# Data 
batdata = map(batvars, ~ prep_bat(data, vars = .x, x_nsize = TRUE) %>%
                 #arrange(desc(zvar), yvar) %>%
                 mutate(ord = lab %>% as_factor(),
                        #xvar = str_wrap(xvar, uwb_vals$chrnum) %>% as_factor, 
                        caption = case_when(str_detect(name,"study") ~ cap_prez, 
                                            TRUE ~ NA)
                        #xvar = fct_reorder(str_wrap(xvar, uwb_vals$chrnum), as.numeric(ord)),
                        #subtitle = case_when(subtitle == "" ~ paste0("N=", nsize),
                         #                    TRUE ~ paste0(subtitle, ", N=", nsize)),
                 )
)
names(batdata) = batnames

# Plots
batscales = list(
  study = c(uwb_palettes(name = "bi", type='continuous', n = 5, add_na = T)),
  condition = c(uwb_palettes(name = "bi", type='continuous', n = 5, add_na = T)),
  competence = c(uwb_palettes(name = "bi", type='continuous', n = 5, add_na = T)),
  research = c(uwb_palettes(name = "bi", type='continuous', n = 5, add_na = T))
)


batplots = map2(batdata, batnames, ~ plot_stack(.x) + 
                  scale_fill_manual(values = batscales[[.y]]) 
                  #guides(fill = guide_legend(nrow = batrows[[.y]])) +
                  #theme(legend.margin = margin(l = -.3, unit = "npc"))
                ) 
batplots$study + labs(title = "Mám všechny potřebné informace ke studiu <br><span style = 'color:blue;'>n=73</span>")


#walk(batdata, ~ print(nrow(.x)))

walk2(batplots, batnames, ~ ggsave(.x, file = paste0("figs/", .y, ".png"),
                                   width = w, height = h + 0.075 * nrow(batdata[[.y]])))


xx = batdata$study |> 
  mutate(xvarno = str_sub(xvarno, 1, 10),
         xvar = glue("{xvarno} <span style='color:grey50'>n = {nsize}</span>")
)

xx$xvar

xx = xx |> 
  mutate(xvarf = as_factor(xvarno),
xvar_nr = as.numeric(xvarf))

ggplot(xx, aes(y = yvar, x = xvar, fill = zvar )) +
  geom_bar(stat = "identity", width = 0.85, colour = uwb_vals$barcol,
             position = position_stack(reverse = TRUE)) +
    scale_fill_uwb("quali") +
    coord_flip() +
    theme_uwb_horiz() 
  theme(axis.text = element_markdown())





plot_stack = function(dat, horiz=TRUE, wrap = FALSE){
  if (horiz){
    dat = dat %>% mutate(xvar = fct_rev(xvar))
  }
  p = ggplot(dat,aes(y = yvar,x = xvar, fill = zvar )) +
    geom_bar(stat = "identity", width = 0.85,colour=uwb_vals$barcol,
             position=position_stack(reverse = TRUE)) +
    scale_fill_uwb("quali") + #default palette is qualitative, change after plotting if necessary
    geom_text(aes(x = fct_rev(xvar), y = pos, label = labvar),
              #family = uwb_vals$font,
              colour = "white", size = uwb_vals$labsize) +
    ylim(0, 100.1) + # Slightly more than 100 to make sure all values are shown
    labs(y = "%", x = "", fill = "",
         title = dat$title[1],
         subtitle = dat$subtitle[1],
         caption = dat$caption[1]) +
        theme_uwb() 
  
  if (horiz) {
    p = p + coord_flip() +
      theme_uwb_horiz() 
  }
  
  if (wrap){
    p = p + scale_x_discrete(labels = label_wrap(uwb_vals$chrnum)) 
  }
  
  print(p)
}