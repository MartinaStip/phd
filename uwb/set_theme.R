# THEMES FOR FIGURES 
# created by Martina, 8/2024

library(ggtext) #allows automatic line breaks in titles
library(scales) #allows automatic line breaks in axis labels

# Font and sizes of text elements in theme and plots are set in set_all.R

p_wid=15
p_hei=9

# rests ------------------------------------------------------------------------
# width of figures in inches 
#p_wid=7.5 #to fit A4 page
#p_hei=4.5 #cca proportional to ppt slide with width 7.5 (dimensions of ppt slide are 10 x 5.6)

# Font for plots DORESIT, JESTLI TO CHCEME--------------------------------------
#library(showtext)
#font_add_google("Roboto", family = "Roboto")
#showtext_auto()

##################themes
theme_uwb <- function(){ 
  #font
  font = uwb_vals$font 
  theme_minimal() %+replace%    #replace elements we want to change
    theme(
      # Grid elements
      panel.grid.major = element_blank(),    
      panel.grid.minor = element_blank(),   
      axis.ticks = element_blank(),        
      # Axis lines + ticks
      axis.ticks.y = element_line(color = uwb_vals$axiscol, linetype = "solid", linewidth = uwb_vals$linesize),
      axis.line.y = element_line(color = uwb_vals$axiscol, linetype = "solid", linewidth = uwb_vals$linesize),
      # Legend
      legend.position="bottom",
      legend.text = element_text(size = uwb_vals$tsize - 3, family = font),
      # Text elements
      plot.title = element_textbox_simple(size = uwb_vals$tsize + 3, family = uwb_vals$font, lineheight = uwb_vals$lineheight_tit),
      plot.title.position = "plot",
      plot.subtitle = element_textbox_simple(size = uwb_vals$tsize - 3, family = uwb_vals$font, hjust = 0),
      #plot.subtitle.position = "plot",
      plot.caption=element_text(size=uwb_vals$tsize - 3, family = font, hjust = 0, lineheight = uwb_vals$lineheight),
      plot.caption.position = "plot",
      strip.text = element_text(size = uwb_vals$tsize, family = uwb_vals$font, margin = margin(b = 1.5)),
      #strip.text = element_textbox_simple(size = uwb_vals$tsize, family = uwb_vals$font, hjust = 0.5, lineheight = uwb_vals$lineheight), # This does not increase space for facet labels, use labeller = label_wrap_gen(multi_line = TRUE, width = 30) inside the facet_wrap()
      axis.title = element_text(color = uwb_vals$axiscol, size = uwb_vals$tsize, family = font),
      axis.text = element_text(size = uwb_vals$tsize, family = uwb_vals$font, lineheight = uwb_vals$lineheight),
      axis.text.y = element_text(color = uwb_vals$axiscol, lineheight = uwb_vals$lineheight)
    )
}

theme_uwb_horiz=function() {
  theme_uwb() %+replace% 
    theme(
      #panel.grid.major.y = element_blank(),
      #panel.grid.major.x = element_blank(), 
      axis.line.x = element_line(color = uwb_vals$axiscol, linetype = "solid", linewidth = uwb_vals$linesize),
      axis.line.y = element_blank(),
      axis.text.y = element_text(color = "black"),
      axis.text.x = element_text(color = uwb_vals$axiscol),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(color = uwb_vals$axiscol, linetype = "solid", size = uwb_vals$linesize)
    )
} 
