# Encoding UTF-8

rm(list = ls())
source("uwb/set_uwb.R")

load("data/data_phd.RData")

sizeadj = 0.8
w = 15 * sizeadj
h = 9 * sizeadj


# Response rate - cube

# Single vars ------------------------------------------------------------------
source("code/a_single.R", encoding = "UTF-8")

# MC


# Batteries
source("code/a_bat.R", encoding = "UTF-8")



# library(ggplot2)
# library(ggtext)
# 
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
