# Total response rate-----------------------------------------------------------------

# Total N
nrow(cube_phd)
length(cube_phd$os_cislo |> unique())
length(cube_phd$osobidno |> unique())

ex1(cube_phd, gender)
ex1(cube_phd, fak)

# Response thermometer
tmdata = tibble(y_cube = nrow(cube_phd), y_survey = nrow(data)) |>
  mutate(
    p = nrow(data) / nrow(cube_phd) * 100 |> round(),
    x_cube = glue("Celkem {y_cube}<br>Ph.D. studujících"),
    lab_survey = glue("{y_survey} vyplněných dotazníků\nnávratnost {round(p)} %")
  )

tmplot = ggplot(tmdata, aes(y = y_cube, x = x_cube)) +
  geom_bar(
    stat = "identity",
    width = 0.85,
    color = "black",
    fill = "white"
  ) +
  geom_bar(
    aes(y = y_survey, x = x_cube),
    stat = "identity",
    width = 0.85,
    fill = uwb_scales$quali[1]
  ) +

  geom_text(
    aes(y = y_survey + 100, x = x_cube, label = lab_survey),
    color = uwb_scales$quali[1],
    size = uwb_vals$labsize
  ) +
  labs(
    x = "",
    y = "",
    title = "Návratnost šetření doktorských studujících",
    #subtitle = paste0(sub_agg3, ", N=", nrow(eras_start %>% filter(ar %in% ars)))
  ) +
  coord_flip() +
  theme_uwb_horiz()

tmplot

ggsave(tmplot, file = "figs/response.png", width = w, height = h - 4) 

# Gender -------------------------------------------------------------------------------
gender_data = prep_single(cube_phd |> 
    mutate(gender = str_to_title(gender)), 
  gender
) |> 
  mutate(zvar = xvar, 
    xvar = glue("Všichni<br><span style = 'color:{uwb_vals$c_nsize3}' >n={nsize}</span>" )) |> 
  bind_rows(
    prep_single(data, gender) |>
      mutate(zvar = xvar, 
        xvar = glue("Dotázaní<br><span style = 'color:{uwb_vals$c_nsize3}' >n={nsize}</span>"))
  )

plot_stack(gender_data) + 
  labs(title = "Gender -- srovnání všech studujících a respondentů/respondentek šetření",
    subtitle = "") +
  scale_fill_manual(values = c(uwb_scales$quali[1:3], c_nor))

ggsave(file = "figs/response_gender.png", width = w, height = h - 3)

# Fak -------------------------------------------------------------------------------
fak_data = prep_single(cube_phd, fak) |> 
  mutate(zvar = xvar, 
    xvar = glue("Všichni<br><span style = 'color:{uwb_vals$c_nsize3}' >n={nsize}</span>" )) |> 
  bind_rows(
    prep_single(data, fak) |>
      mutate(zvar = xvar, 
        xvar = glue("Dotázaní<br><span style = 'color:{uwb_vals$c_nsize3}' >n={nsize}</span>"))
  )

plot_stack(fak_data) + 
  labs(title = "Fakulta studia -- srovnání všech studujících a respondentů/respondentek šetření",
    subtitle = "") +
  scale_fill_manual(values = c(uwb_scales$faks[1:8], c_nor))

ggsave(file = "figs/response_fak.png", width = w, height = h - 3)

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
