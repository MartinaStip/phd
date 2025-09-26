# Encoding UTF-8

rm(list = ls())
source("uwb/set_uwb.R")

load("data/data_phd.RData")

sizeadj = 0.8
w = 15 * sizeadj
h = 9 * sizeadj


# Texts for plots
cap_prez = "* Tuto položku hodnotili pouze studující v prezenční formě"





# Identify sets of vars
# MC items ---------------------------------------------------------------------
mcnames = c("info", "situation")

mcvars = map(mcnames, ~ codebook %>% 
               filter(!str_detect(name, "en_|dupli_")) %>% 
               filter(str_detect(name, paste0(.x, "_"))) %>% 
               pull(name)) %>% 
  set_names(mcnames)


# Response rate + analysis of nonresponse by item
source("code/a_response.R", encoding = "UTF-8")

# Single vars 
source("code/a_single.R", encoding = "UTF-8")

# MC
source("code/a_mc.R", encoding = "UTF-8")

# Batteries 
source("code/a_bat.R", encoding = "UTF-8")


# library(ggplot2)
# library(ggtext)
# # 
# options(device = function(...) ragg::agg_png(..., res = 72))
# df <- data.frame(x = 1:2, y = c(3, 5))
# ggplot(df, aes(x, y)) +
#   geom_point(size = 5) +
#   scale_x_continuous(
#     breaks = 1:2,
#     labels = c("FAV<br><span style='color:#944e4e'>n=29</span>",
#                "FST<br><span style='color:#2e6f9e'>n=42</span>")
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = ggtext::element_markdown())
# 
# ggsave("test_plot.png", width = 6, height = 4, dpi = 300)
# dev.new(type = "cairo")  



