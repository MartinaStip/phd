library(tidyverse)
library(patchwork)
library(extrafont)
library(extrafontdb)
library(plotwidgets)

# load("data/uwb.RData") 

# Don't know answers
c_nor = hsl2col(col2hsl('grey50'))

####################################uwb color scales

#list of color scales #ADD FURTHER SCALES HERE IF NECESSARY
# uwb_scales = list(
#   quali=c("#3670BC", "#BF4059", "#82A329", "#5C3399", "#E0B406", "#326754", 
#           "#E68319", "#8F5682", "#707CA9", "#93261F", rep(c_nor,10)), #qualitative palette; 10 greys at the end are to fill enough slots for long lists of categories
#   faks=c(c_fav, c_fdu, c_fek, c_fel, c_ff, c_fpe, c_fpr, c_fst, c_fzs, c_zcu_grey),
#   mono=c(c_light,c_single,c_dark),
#   mono_rev=c(c_dark,c_single,c_light),
#   long=c(c_long1,c_single,c_long2),
#   long_rev=c(c_long2,c_single,c_long1),
#   #bi=c(c_green,c_mid,c_red),
#   bi=c(c_blue,c_mid,c_red),
#   #bi_rev=c(c_red,c_mid,c_green)
#   bi_rev=c(c_red,c_mid,c_blue)
# )

# Function to create a palette from the provided colors (one of the  items in the list uwb_scales) 
# 2 steps:
  #1. uwb_palettes to generate the palette with the required number of items
  #2. scale_fill/color_uwb to be called from ggplot

uwb_scales$faks[10] = "grey50"


#ADD MORE ARGUMENTS? ALPHA, DIRECTION
uwb_palettes = function(name = 'quali', n, all_palettes = uwb_scales, type = c("discrete", "continuous"), 
                        add_na = FALSE) {
  palette = all_palettes[[name]]
  if (missing(n)) {
    # n = length(palette)
    n = ifelse(add_na == TRUE, length(palette) - 1, length(palette))
  }
  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  if (add_na == TRUE) {
    out = c(out,"grey50")
  }
  structure(out, name = name, class = "palette")
}

#check
uwb_palettes(name = 'quali', type = 'discrete')
# uwb_palettes(type='discrete')
uwb_palettes(type = 'discrete', add_na = T)
uwb_palettes(name = 'long',type = 'continuous', n = 3)
uwb_palettes(name = 'long',type = 'continuous', n = 3, add_na = T)

######################################## color/fill for discrete vars (nominal or ordinal, no decimal places)
scale_fill_uwb = function(name='quali',n,add_na=FALSE) {
  if (missing(n)) {
    ggplot2::scale_fill_manual(values = uwb_palettes(name,type = "discrete", add_na = add_na))
  } else {
    ggplot2::scale_fill_manual(values = uwb_palettes(name,n,type = "continuous", add_na = add_na))
  }
}

scale_color_uwb = function(name,n,add_na=FALSE) {
  if (missing(n)) {
    ggplot2::scale_color_manual(values = uwb_palettes(name,type = "discrete"))
  } else {
    ggplot2::scale_color_manual(values = uwb_palettes(name,n,type = "continuous"))
  }
}

#make sure it works with both versions of spelling
scale_colour_uwb = scale_color_uwb 

#check
#cplot + scale_fill_uwb("mono", n=10) 
#cplot_short + scale_fill_uwb("faks") 

######################################## color/fill for continuous vars 
#TO BE DONE
#scale_colour_ksspk_conti = function(name) {
#    ggplot2::scale_colour_gradientn(colours = ksspk_palettes(name = name,type = "continuous"))
#  }
 
"#24528f"

"#8F2439"