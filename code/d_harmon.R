# Harmonize cz and en version of vars ------------------------------------------
# Single
ex1(data, solution)
ex1(data, finance)
ex1(data, gender)

# Batteries
ex1(data, study_1)
ex1(data, condition_1)
ex1(data, competence_1)
ex1(data, research_1)

# MC
ex1(data, info_1)
ex1(data, situation_1)



names(data)
data = data %>% 
  mutate(across(starts_with("study_"), ~ case_match(., "Strongly Agree" ~ "Zcela souhlasím",
                                                    "Somewhat Agree" ~ "Spíše souhlasím",
                                                    "Strongly Disagree" ~ "Zcela nesouhlasím",
                                                    "Somewhat Disagree" ~ "Spíše nesouhlasím",
                                                    "Neutral" ~ "Tak napůl",
                                                    .default = . )),
         gender = case_match(gender, "Female" ~ "Žena",
                             "Male" ~ "Muž",
                             .default = gender ),
         finance = case_match
         )



