# Harmonize cz and en version of vars ------------------------------------------
# # Single
# ex1(data, solution)
# ex1(data, finance)
# ex1(data, gender)
# ex1(data, fak)
# 
# # Batteries
# ex1(data, study_1)
# ex1(data, condition_1)
# ex1(data, competence_1)
# ex1(data, research_1)
# 
# # MC
# ex1(data, info_1)
# ex1(data, situation_1)

# Replace NA -------------------------------------------------------------------
data = data %>% 
  mutate(across(all_of(c("gender", "fak")), ~replace_na(as.character(.x), "Bez odpovědi")))

# Factor levels ----------------------------------------------------------------

# This is necessary to get the factor levels
data_cz = data_cz %>% 
  mutate(across(all_of(c("gender", "fak")), ~ replace_na(as.character(.x), "Bez odpovědi")))

l_gender = unique(data_cz$gender)[order(c(4, 1, 2, 3))]
l_study = unique(data_cz$study_1)[order(c(2, 1, 6, 3, 4, 5))]
l_condition = unique(data_cz$condition_1)[order(c(1, 6, 3, 2, 4, 5))]
l_competence = unique(data$competence_1)[order(c(7, 4, 2, 6, 5, 1, 3))]
l_finance = unique(data$finance)[order(c(6, 4, 3, 1, 2, 5))] 

# Harmonise cz and en values ---------------------------------------------------
data = data %>% 
  mutate(across(matches("^(study_|research_)"), ~ case_match(., "Strongly Agree" ~ "Zcela souhlasím",
                                                    "Somewhat Agree" ~ "Spíše souhlasím",
                                                    "Strongly Disagree" ~ "Zcela nesouhlasím",
                                                    "Somewhat Disagree" ~ "Spíše nesouhlasím",
                                                    "Neutral" ~ "Tak napůl",
                                                    .default = . ) %>% 
                  factor(levels = l_study)),
         across(starts_with("condition_"), ~ case_match(., "Completely Adequate" ~ "Zcela přiměřené",
                                                    "Somewhat Adequate" ~ "Spíše přiměřené",
                                                    "Somewhat Unadequate" ~ "Spíše nepřiměřené",
                                                    "Completely Unadequate" ~ "Zcela nepřiměřené",
                                                    "Neutral" ~ "Tak napůl",
                                                    .default = . ) %>% 
                  factor(levels = l_condition)),
         across(starts_with("competence_"), ~ case_match(., "Highly Developed" ~ "Velmi rozvíjeno",
                                                        "Somewhat Developed" ~ "Spíše rozvíjeno",
                                                        #"Somewhat Unadequate" ~ "Spíše nerozvíjeno",
                                                        #"Completely Unadequate" ~ "Vůbec nerozvíjeno",
                                                        "Neutral" ~ "Tak napůl",
                                                        "Not Applicable" ~ "Netýká se",
                                                        .default = . ) %>% 
                  factor(levels = l_competence)),
         gender = case_match(gender, "Female" ~ "Žena",
                             "Male" ~ "Muž",
                             .default = gender) %>% 
           factor(levels = l_gender),
         finance = case_match(finance, 
                              "Definitely yes" ~ "Určitě ano",
                              "Somewhat yes" ~ "Spíše ano",
                              "Neutral" ~ "Tak napůl", 
                              "Somewhat no" ~ "Spíše ne",
                              "Definitely no" ~ "Určitě ne",
                              .default = finance) %>% 
           factor(levels = l_finance),
         fak = fct_relevel(fak, "Bez odpovědi", after = Inf)
  )
  

