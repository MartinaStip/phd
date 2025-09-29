# THEMES FOR FIGURES 
# created by Martina, 8/2024

#p_wid=15
#p_hei=9

# Colors are defined in set_colors

# Fonts and sizes --------------------------------------------------------------

# Set uwbvals for theme and plot functions -------------------------------------
# font_add("Franklin Gothic", "C:/Windows/Fonts/Franklin Gothic/framd.ttf")
tsize = 20

uwb_vals = list(
  # Font
  font = "Franklin Gothic",
  #font = "open sans",
  # Text sizes
  tsize = tsize, # main text size; influences theme and labsizes
  labsize = (tsize) / .pt, # size of text labels in plots 
  chrnum = 30, # number of characters before line break in axis labels
  lineheight_tit = 1, # space between lines in titlea and subtitle labels
  lineheight = 0.75, # space between lines in axis labels
  # Position of text labels
  lim_single_pos = 5, # Position of text label inside vs. outside bar in barplots
  lim_stack_no = 5, # Whether text labels is printed in a stacked barplot
  # Line and point sizes
  axissize = 0.25, # Width of axes
  linesize = 1, # Width of lines in a line plot
  pointsize = 15, # size of circles in lollipop, scatter and trend plots
  # Line colors (except palettes that go into aes)
  barcol = "white", # Outline of bars
  gridcol = "gray85", # Plot grid
  lollistick = "gray35",
  axiscol = 'grey35', # Color for axis is muted because values are labelled 
  # color of N sizes: very small, small, enough
  c_nsize1 = "#c73a3a",
  c_nsize2 = "#944e4e",
  c_nsize3 = "grey35"
  )

# Default theme ----------------------------------------------------------------
# I used to base it on theme_minimal, but it did not work together with element_markdown, theme_void is better as a starting point.
theme_uwb = function(){ 
  # Font
  font = uwb_vals$font 
  theme_void() %+replace%    #replace elements we want to change
    theme(
      # Grid elements
      #panel.grid.major = element_blank(),    
      #panel.grid.minor = element_blank(),   
      #axis.ticks = element_blank(),        
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
      plot.caption=element_textbox_simple(size=uwb_vals$tsize - 3, family = font, hjust = 0, lineheight = uwb_vals$lineheight),
      plot.caption.position = "plot",
      strip.text = element_text(size = uwb_vals$tsize, family = uwb_vals$font, margin = margin(b = 1.5)),
      #strip.text = element_textbox_simple(size = uwb_vals$tsize, family = uwb_vals$font, hjust = 0.5, lineheight = uwb_vals$lineheight), # This does not increase space for facet labels, use labeller = label_wrap_gen(multi_line = TRUE, width = 30) inside the facet_wrap()
      axis.title = element_markdown(color = uwb_vals$axiscol, size = uwb_vals$tsize, family = font),
      axis.text =  element_markdown(size = uwb_vals$tsize, family = uwb_vals$font, lineheight = uwb_vals$lineheight),
      #axis.text.x =  ggtext::element_markdown(),  # the markdown only works when .x and .y are set explicitly here, the generic axis.text doesnt work.
      axis.text.y = ggtext::element_markdown(color = uwb_vals$axiscol)
    )
}


 #old theme based on theme_minimal
# theme_uwb = function(){ 
#   # Font
#   font = uwb_vals$font 
#   theme_minimal() %+replace%    #replace elements we want to change
#     theme(
#       # Grid elements
#       panel.grid.major = element_blank(),    
#       panel.grid.minor = element_blank(),   
#       axis.ticks = element_blank(),        
#       # Axis lines + ticks
#       axis.ticks.y = element_line(color = uwb_vals$axiscol, linetype = "solid", linewidth = uwb_vals$linesize),
#       axis.line.y = element_line(color = uwb_vals$axiscol, linetype = "solid", linewidth = uwb_vals$linesize),
#       # Legend
#       legend.position="bottom",
#       legend.text = element_text(size = uwb_vals$tsize - 3, family = font),
#       # Text elements
#       plot.title = element_textbox_simple(size = uwb_vals$tsize + 3, family = uwb_vals$font, lineheight = uwb_vals$lineheight_tit),
#       plot.title.position = "plot",
#       plot.subtitle = element_textbox_simple(size = uwb_vals$tsize - 3, family = uwb_vals$font, hjust = 0),
#       #plot.subtitle.position = "plot",
#       plot.caption=element_text(size=uwb_vals$tsize - 3, family = font, hjust = 0, lineheight = uwb_vals$lineheight),
#       plot.caption.position = "plot",
#       strip.text = element_text(size = uwb_vals$tsize, family = uwb_vals$font, margin = margin(b = 1.5)),
#       #strip.text = element_textbox_simple(size = uwb_vals$tsize, family = uwb_vals$font, hjust = 0.5, lineheight = uwb_vals$lineheight), # This does not increase space for facet labels, use labeller = label_wrap_gen(multi_line = TRUE, width = 30) inside the facet_wrap()
#       axis.title = element_text(color = uwb_vals$axiscol, size = uwb_vals$tsize, family = font),
#       axis.text =  element_text(size = uwb_vals$tsize, family = uwb_vals$font, lineheight = uwb_vals$lineheight),
#       #axis.text.x =  ggtext::element_markdown(),  # the markdown only works when .x and .y are set explicitly here, the generic axis.text doesnt work.
#       axis.text.y = ggtext::element_markdown(color = uwb_vals$axiscol)
#     )
# }


# Horizontal theme (flipped axes) ----------------------------------------------
theme_uwb_horiz = function() {
  theme_uwb() %+replace% 
    theme(
      #panel.grid.major.y = element_blank(),
      #panel.grid.major.x = element_blank(), 
      axis.line.x = element_line(color = uwb_vals$axiscol, linetype = "solid", linewidth = uwb_vals$linesize),
      axis.line.y = element_blank(),
      axis.text.y = element_markdown(color = "black"),
      axis.text.x = element_markdown(color = uwb_vals$axiscol),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(color = uwb_vals$axiscol, linetype = "solid", linewidth = uwb_vals$linesize)
    )
} 
