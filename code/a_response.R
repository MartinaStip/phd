# Cube data (STAG) -----------------------------------------------------------------

# Total N
nrow(cube_phd)
length(cube_phd$os_cislo |> unique())
length(cube_phd$osobidno |> unique())

ex1(cube_phd, gender)
ex1(cube_phd, fak)

# Response thermometers
resp_data = prep_single(cube_phd, fak)

# Missing answers for each var -------------------------------------------------
mnames = names(missing)

# MC vars with all items missing
allmissing_mc = map(
  mcvars,
  ~ data %>%
    select(Id.respondenta, all_of(.x)) %>%
    rowwise() %>%
    mutate(nr_missing = sum(is.na(across(everything())))) %>%
    select(-all_of(.x)) %>%
    filter(nr_missing == length(.x)) %>%
    pull(Id.respondenta)
)

n_miss_mc = map(allmissing_mc, ~ length(.x))


missing_sum = missing %>%
  select(form:prog) %>%
  summarise(across(everything(), ~ sum((.)))) %>%
  pivot_longer(cols = everything(), values_to = "n_missing") %>%
  impute_labs %>%
  mutate(
    yvar = case_when(
      str_detect(name, "info_") ~ n_miss_mc$info,
      str_detect(name, "situation_") ~ n_miss_mc$situation,
      TRUE ~ n_missing / nrow(missing) * 100
    ),
    xvar = case_when(
      is.na(lab) ~ label,
      str_detect(name, "info_|situation_") ~ label,
      TRUE ~ lab
    ) %>%
      as_factor(),
    labvar_single = round(yvar),
    cvar = uwb_scales$quali[1],
    title = glue(
      "Procento odpovídajících, kteří na danou otázku <span style = 'color:{uwb_scales$quali[1]}'>NEODPOVĚDĚLI</span>"
    ),
    #title = paste0("Procento odpovídajících, kteří na danou otázku <span style='color:blue' NEODPOVĚDĚLI</span>"),
    subtitle = "Pořadí odpovídá pořadí otázek v dotazníku"
  ) %>%
  distinct()


# Closed vars
missing_closed = missing_sum %>%
  filter(!str_detect(name, "open_")) %>%
  mutate(
    caption = cap_prez,
    row_nr = row_number(),
    facetvar = ifelse(row_nr < 20, 1, 2)
  )

# Open comments
missing_open = missing_sum %>%
  filter(str_detect(name, "open_")) %>%
  mutate(
    xvar = name %>% as_factor(),
    title = paste0(title, ": otevřené otázky")
  )

# Plots
nonresponse = plot_lolli(missing_closed) +
  ylim(0, 100) +
  facet_wrap(~facetvar, ncol = 2, scales = "free")
nonresponse
ggsave(nonresponse, file = "figs/nonresponse.png", width = w, height = h + 8)

nonresponse_open = plot_lolli(missing_open) +
  theme(plot.title = element_markdown())
nonresponse_open
ggsave(
  nonresponse_open,
  file = "figs/nonresponse_open.png",
  width = w,
  height = h
)
