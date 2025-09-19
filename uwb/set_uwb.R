# This should be called in the beginning of the project until the uwb package is ready.
# Encoding UTF-8

# This script does 3 things:
  # loads all libraries,
  # loads the uwb data (colors, uwbvals for themes and plot functions, codebooks for names of faculties and programs)
  # sources scripts for theme, color palletes, and functions. 

library(tidyverse)
library(ggtext) # Allows automatic line breaks in titles
library(scales) # Allows automatic line breaks in axis labels
library(sysfonts)
library(showtext)
library(ggsankey)

font_add_google(name = "Open Sans", family = "open sans")

#font_add("Franklin Gothic", "C:/Windows/Fonts/Franklin Gothic/framd.ttf")
#showtext_auto()

load("uwb/uwb.RData") 
uwb_vals$font = "Franklin Gothic"


# uwb.RData is created in a_uwb_objects.R in the kvalita-manual repo
# uwbvals may be customized here to alter the visual style

source("uwb/set_theme.R", encoding = "UTF-8") 
source("uwb/set_colors.R", encoding = "UTF-8")
source("uwb/set_functions.R", encoding = "UTF-8")


# Notes about text sizes -------------------------------------------------------
# https://ggplot2.tidyverse.org/articles/ggplot2-specs.html
# Size in theme is in points (.pt) as defined in the grid package. 1 pt = 0.35mm
# Size of geom_text in in mm.

# Web ZCU - fonts:
# Headlines: Roboto condensed
# Text: Open Sans

#library(systemfonts)
#print(systemfonts::system_fonts(), n = Inf)
