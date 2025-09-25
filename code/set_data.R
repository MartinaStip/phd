# Encoding UTF-8

rm(list = ls())
source("uwb/set_uwb.R")


library(readxl)
library(writexl)
#library(janitor)
#library(stringi)

data_raw = read_xlsx("data/PhD_data_2025.xlsx")
names(data_raw)

# Codebook ---------------------------------------------------------------------
source("code/d_codebook.R", encoding = "UTF-8")

# Clean data -------------------------------------------------------------------
data_cz_en = data_raw %>% 
  set_names(codebook$name)
nrow(data_cz_en)

# Nr of missings
ncol(data_cz_en)

data_cz_en = data_cz_en %>% 
  mutate(na_count = rowSums(is.na(.)))

ex1(data_cz_en, lang)
ex1(data_cz_en, form)
ex1(data_cz_en, na_count)
ex1(data_cz_en, Status.odpovědi)

xx = data_cz_en %>% 
  arrange(desc(na_count)) %>% 
  relocate(na_count)

# Drop empty questionnaires
data_cz_en = data_cz_en %>% 
  filter(na_count < 182)
  
nrow(data_cz_en)

# Split + bind cz and en version ------------------------------------------------------
data_cz = data_cz_en %>% 
  filter(lang == "česky") %>% 
  select(!starts_with("en_"))
names(data_cz)
nrow(data_cz)

data_en = data_cz_en %>% 
  filter(lang == "english") %>% 
  select(all_of(names(data_cz)[1:11]), starts_with("en_")) 
names(data_en) = str_remove(names(data_en), "en_")
nrow(data_en)


nrow(data_cz_en)
data = data_cz %>% 
  bind_rows(data_en) 

# Harmonize cz and en version of form and fak ----------------------------------
data = data %>% 
  mutate(form = case_when(str_detect(form, "Full|Prez") ~ "Prezenční",
                          str_detect(form, "Combi|Kombi") ~ "Kombinovaná") %>% 
           fct_rev(),
         fak = case_when(str_detect(fak, "Applied") ~ "FAV",
                         str_detect(fak, "of Arts") ~ "FDU",
                         str_detect(fak, "Economics") ~ "FEK",
                         str_detect(fak, "Electrical") ~ "FEL",
                         TRUE ~ fak) %>%  
           as.factor())
nrow(data)

# Split + bind prez and kombi form ---------------------------------------------
ex1(data, form)
ex2(data, form, dupli_study_1)
ex2(data, form, study_1)

dupli_cols = data %>% 
  select(starts_with("dupli")) %>% 
  names() %>% 
  str_remove("dupli_")

data = data %>% 
  mutate(across(all_of(dupli_cols), ~ case_when(form == "Kombinovaná" ~ get(sub("study_", "dupli_study_", cur_column())), 
                                                TRUE ~ .))) %>% 
  select(!starts_with("dupli"))

ex2(data, form, study_1)
ex2(data, form, study_7)

# Dataset to analyse nonresponse
missing = data %>% 
  mutate(across(everything(), ~ case_when(is.na(.) ~ 1, 
                                        TRUE ~ 0)))


# Harmonize cz and en version of vars + factor levels --------------------------
source("code/d_harmon.R", encoding = "UTF-8")

# Explo ------------------------------------------------------------------------
ex1(data, fak)

data %>% 
  filter(!is.na(prog)) %>% 
  pull(prog)


ex1(data, study_1)

# Open comments ---------------------------------------------------------
open = data %>%
  select(starts_with("open"), gender, form, prog, fak) %>%
  select(!open_situation) %>%
  mutate(id = row_number(),
         na_open_count = rowSums(across(starts_with("open"), ~ is.na(.))),
         across(everything(), ~ na_if(as.character(.), "Bez odpovědi")), 
         across(everything(), ~ replace_na(., "")),
         person_info = paste0(gender, " ", form, " ", fak, " ", prog)) %>%
  filter(na_open_count < 5) %>%
  select(id, person_info, starts_with("open")) %>% 
  pivot_longer(cols = starts_with("open"), names_to = "item", values_to = "answer") %>% 
  filter(answer != "") %>% 
  group_by(id) %>% 
  mutate(row_nr = row_number(),
         person_info = case_when(row_nr == 1 ~ person_info,
                                 TRUE ~ "")) %>% 
  ungroup() %>% 
  select(-c(row_nr))
  
writexl::write_xlsx(list("open answers" = open), "data/phd_quali_data.xlsx") 

# Save -------------------------------------------------------------------------
save(data, missing, codebook, open,
     file = "data/data_phd.RData")

# write.xlsx(open, "data/phd_quali.xlsx", sheetName = "open answers") 
# 
# open2 = open %>% 
#   select(-tag) %>% 
#   t() %>% 
#   as.data.frame() %>%
#   select(where(~ !all(is.na(.)))) %>% 
#   set_names(open$tag)


