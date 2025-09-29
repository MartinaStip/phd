# Encoding UTF-8

rm(list = ls())
source("uwb/set_uwb.R")
load("data/data_phd.RData")

sizeadj = 0.8
w = 15 * sizeadj
h = 9 * sizeadj

# Texts for plots -------------------------------------------------------------
cap_prez = "* Tuto položku hodnotili pouze studující v prezenční formě"


# Identify sets of vars -------------------------------------------------------
# MC items 
mcnames = c("info", "situation")

mcvars = map(mcnames, ~ codebook %>% 
               filter(!str_detect(name, "en_|dupli_")) %>% 
               filter(str_detect(name, paste0(.x, "_"))) %>% 
               pull(name)) %>% 
  set_names(mcnames)

# Analysis --------------------------------------------------------------------
# Response rate + analysis of nonresponse by item
source("code/a_response.R", encoding = "UTF-8")

# Single vars 
source("code/a_single.R", encoding = "UTF-8")

# MC
source("code/a_mc.R", encoding = "UTF-8")

# Batteries 
source("code/a_bat.R", encoding = "UTF-8")

