
# University colors ------------------------------------------------------------
c_zcu = "#31539D" # hsl 221,52,40
c_zcu_grey = "grey30"

# Faculties
c_fav = "#CEAA1B" #hsl(48, 77%, 46%)
c_fdu = "#C7362D" #hsl 4,63,48
c_fek = "#D67C1C" #hsl(31, 77%, 47%)
c_fel = "#293D83" #hsl(227, 52%, 34%)
c_ff = "#5DB3DA" #hsl 199,63,61
c_fpe = "#99BC39" #hsl(76, 53%, 48%)
c_fpr = "#8A172E" #348, 71%, 32%
c_fst = "#4C8CCB" #hsl(210, 55%, 55%)
c_fzs = "#1C966A" #hsl(158, 69%, 35%)

c_ntc = "#80217E" #hsl(301, 59%, 32%)

# Faculties + parts - color vectors
c_faks = c(c_fav, c_fdu, c_fek, c_fel, c_ff, c_fpe, c_fpr, c_fst, c_fzs) 
names(c_faks) = c('FAV','FDU','FEK','FEL','FF','FPE','FPR','FST','FZS') 

c_parts = c(c_ntc, c_zcu, c_zcu, c_fav, c_zcu)
names(c_parts) = c("NTC", "UJP", "RTI", "RICE", "NTIS")

# Single colors to define the uwb palettes -------------------------------------
# Single color 
c_single='#3670BC' #mix of the 4 zcu blues hsl(214, 55%, 47%)

# Light and dark ends of monochromatic scale
c_light= "#82AFBD"
c_dark="#2A2666"

# Ends of blue-red scale (bipolar, positive-negative)
c_red = "#8F2439" # hsl(348, 60, 35)
#c_green = "#1A6166" 
c_blue = "#24528f" # darker c_single hsl(214, 60%, 35%)

# Midpoint of blue-red scale
#c_mid = '#c2b170'
c_mid = "#84738c"

# Long scale (alternative of  monochromatic for longer range of values)
c_long1 = "#82BDB7" 
c_long2 = '#651a64' 

# Don't know answers
c_nor = hsl2col(col2hsl('grey50'))


# uwb color scales--------------------------------------------------------------
#list of color scales #ADD FURTHER SCALES HERE IF NECESSARY
uwb_scales = list(
  quali=c("#3670BC", "#BF4059", "#82A329", "#5C3399", "#E0B406", "#326754", 
          "#E68319", "#8F5682", "#707CA9", "#93261F", rep(c_nor,10)), #qualitative palette; 10 greys at the end are to fill enough slots for long lists of categories
  faks=c(c_fav, c_fdu, c_fek, c_fel, c_ff, c_fpe, c_fpr, c_fst, c_fzs, c_zcu_grey),
  mono=c(c_light,c_single,c_dark),
  mono_rev=c(c_dark,c_single,c_light),
  long=c(c_long1,c_single,c_long2),
  long_rev=c(c_long2,c_single,c_long1),
  bi=c(c_blue,c_mid,c_red),
  bi_rev=c(c_red,c_mid,c_blue)
)


# Keep selected objects, remove the rest ---------------------------------------
rm(list = setdiff(ls(), c("c_zcu", "c_faks", "c_nor", "c_parts", "uwb_scales")))


# Color functions --------------------------------------------------------------

# Function to create a palette from the provided colors (from the list uwb_scales) 
# 2 steps:
  # 1. uwb_palettes to generate the palette with the required number of items
  # 2. scale_fill/color_uwb to be called from ggplot

# ADD MORE ARGUMENTS? ALPHA, DIRECTION
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
    out = c(out, c_nor)
  }
  structure(out, name = name, class = "palette")
}

# Check
# uwb_palettes(name = 'quali', type = 'discrete')
# uwb_palettes(type='discrete')
# uwb_palettes(type = 'discrete', add_na = T)
# uwb_palettes(name = 'long',type = 'continuous', n = 3)
# uwb_palettes(name = 'long',type = 'continuous', n = 3, add_na = T)

# Color/fill for discrete vars (nominal or ordinal, no decimal places)
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

# To make it work with both versions of spelling
scale_colour_uwb = scale_color_uwb 

#check
#cplot + scale_fill_uwb("mono", n=10) 
#cplot_short + scale_fill_uwb("faks") 

# color/fill for continuous vars 
#TO BE DONE
#scale_colour_ksspk_conti = function(name) {
#    ggplot2::scale_colour_gradientn(colours = ksspk_palettes(name = name,type = "continuous"))
#  }
 
#"#24528f"

#"#8F2439"
