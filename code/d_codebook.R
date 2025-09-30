# 2 bateries are duplicated but differ in details. 
# Different versions for full=time and combined students
# But the situations battery is identical in both versions
# The shorter version is identified by dupli == 1

codebook = tibble(orig = names(data_raw),
                  nr = 1:ncol(data_raw)
) %>% 
  mutate(orig_low = tolower(orig),
         #orig = str_remove(orig, "(optional)"),
         orig = str_replace(orig, "Prostor pro komentář:", "Prostor pro komentář"),
         orig = str_replace(orig, "Comment section:", "Comment section "),
         name = case_when(nr < 11 ~ make.names(orig),
                          nr == 11 ~ "lang",
                          nr %in% c(78:82, 96:100) ~ "research",
                          str_detect(orig, "hodnotíte tyto výroky|evaluate the following statements") ~ "study",
                          nr == 82 ~ "study", 
                          str_detect(orig, "přiměřenost studijních podmínek|appropriateness of the study") ~ "condition",
                          str_detect(orig, "vaše kompetence|competencies") ~ "competence",
                          str_detect(orig, "poskytuje informace| obtain information") ~ "info",
                          str_detect(orig, "následujícími situacemi|following situations") ~ "situation",
                          nr %in% c(12, 13) ~ "form",
                          nr %in% c(21, 29, 39, 49) ~ "open_study",
                          nr %in% c(56, 63) ~ "open_condition",
                          nr %in% c(70, 77) ~ "open_competence",
                          nr %in% c(95, 111) ~ "open_info",
                          nr %in% c(127, 144, 162, 180) ~ "open_situation",
                          nr %in% c(128, 145, 163, 181) ~ "solution",
                          nr %in% c(146, 164) ~ "finance",
                          nr %in% c(182, 183) ~ "open_general",
                          nr %in% c(184, 188) ~ "gender",
                          nr %in% c(185, 189)  ~ "fak",
                          nr %in% c(186, 190)  ~ "prog",
                          nr %in% c(187, 191)  ~ "consent",),
         label = str_remove(orig, ".*:"),
         label = str_remove(label, "\\.{3}.*"),
         label = str_remove(label, "Lze vybrat více možností."),
         label = case_when(nr == 82 ~ "Jak hodnotíte tyto výroky?", 
                           nr == 164 ~ "Je podle Vás výše finanční podpory (doktorská či jiná stipendia, granty, odměny za výuku) dostatečná?",  
                           str_detect(name, "situation") ~ "Setkal/a jste se na vašem pracovišti během studia s následujícími situacemi?",
                           TRUE ~ label),
         lab = case_when(str_detect(orig, ":") ~ str_extract(orig, "^[^:]+")),
         en = case_when(name =="lang" ~ 0,
                        str_detect(orig_low, paste(c("the", "from", "you", "faculty", "study", "section", "gender"), collapse = "|")) ~ 1, 
                        TRUE ~ 0),
         # name = case_when(
         #                  TRUE ~ name),
         dupli = case_when(nr %in% c(14:29, 147:163, 165:181) ~ 1,
                           TRUE ~ 0)
  ) %>% 
  group_by(name, en, dupli) %>% 
  mutate(nr_item = row_number(),
         name = case_when(nr %in% c(189, 190) ~ name,
                          is.na(lab) == 0 ~ paste0(name, "_", nr_item),
                          TRUE ~ name),
         name = case_when(dupli == 1 ~ paste0("dupli_", name), 
                          TRUE ~ name),
         name = case_when(en == 1 ~ paste0("en_", name), 
                          TRUE ~ name)
  ) %>% 
  ungroup() %>% 
  relocate(name, label, lab) %>% 
  select(-c("orig_low", "en", "dupli", "nr_item"))

codebook = codebook %>% 
  mutate(title = case_when(name == "fak" ~ "Fakulta doktorského studia",
                           name == "gender" ~ "Gender",
                           name == "form" ~ "Forma studia",
                           TRUE ~ label),
        subtitle = "",
        caption = "",
        lab_orig = lab,
        lab = str_remove(lab, "\\([^)]*\\)") %>% str_remove("\\."),
        lab = case_when(name %in% c("study_8", "study_9") ~ paste0(lab, "*"),
                         TRUE ~ lab),
        lab_short = lab)



