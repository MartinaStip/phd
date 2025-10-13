# Functions for easy data visualisation. Two kinds of functions: to prepare data and to plot them.
# reference to variable names in dplyr works differently in functions
# https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html

# Values are defined in uwb_vals

################NOTES FOR PACKAGE/IMPROVEMENTS
# check entering parameters + print error messages in the beginning of the functions

# Exploratory data preps + plots -----------------------------------------------
# To be used for data exploration and recoding
# Single distribution of a var (preferably categorical var or numeric var with few values)
ex1 = function(dat, var){
  nsize_listwise = nrow(dat |> drop_na({{var}}))
  vartitle = names(dat |> select({{var}}))
  exx1=dat |> 
    count({{var}},.drop=FALSE) |> 
    mutate(yvar_n=n,
           yvar=n/sum(n)*100,
           xvar = as_factor({{var}}),
           #xvar=fct_reorder(xvar,n), #no reordering because one of the uses is to check order of factor values
           labvar = paste0(round(yvar,0),'% (',yvar_n,')'),
           nsize=sum(n))
  
  p=ggplot(exx1,aes(y = yvar,x = fct_rev(xvar))) +
    geom_bar(stat="identity", width = 0.85,colour=uwb_vals$barcol,fill="#eff34d") +
    geom_text(aes(x = xvar, y = ifelse(yvar < 12,6,(yvar - 6)), label = labvar),
              colour = "black", size = 0.75 * uwb_vals$labsize) +
    labs(y="",x="",
         title=paste0(vartitle,', N=', nsize_listwise,'/',nrow(dat)),
         subtitle='',
         caption='') +
    coord_flip() +
    theme_uwb_horiz() + theme(axis.ticks.x = element_blank(),
                              axis.line.x = element_blank(),
                              axis.text.x = element_blank())
  return(p)
}

# Visual crosstabulation of two vars (var1 goes into rows) #SOMETHING IS WRONG
ex2=function(dat,var1,var2){
  exx2=dat |> 
    count({{var1}},{{var2}}) 
  p=ggplot(exx2,aes(y={{var1}},x={{var2}})) +
    geom_tile(aes(alpha=n),fill="#eff34d") +
    geom_text(aes(label = n), color='black') +
    theme_uwb() + theme(legend.position = 'none')
  return(p)
  }

# Internal data prep functions (used in multiple data prep procedures) ---------
# Impute labs from codebook/fill empty names
impute_labs = function(dat) {
  if (exists("codebook")) {
    dat = dat |> left_join(codebook)
  } 
  else {
    dat = dat |>
      mutate(title = "",
             subtitle = "",
             caption = "")
  }
}
# Generate vars to be used for geom_text
generate_textlabs = function(dat, round_places = 0){
  labdat = dat |> 
    mutate(
      labvar_full = round(yvar, round_places),
      labvar_single = ifelse(yvar < 0.5, "<1", paste0(labvar_full, "")), # for plotting in barplots with single columns
      pos_single = ifelse(yvar < uwb_vals$lim_single_pos, yvar + max(yvar) / 100 * 5, 0.5 * yvar), 
      cvar = uwb_scales$quali[1],
      cvar_text = case_when(pos_single < yvar ~ 'white',
                            pos_single > yvar ~ uwb_scales$quali[1]), 
      labvar = ifelse(abs(yvar) < uwb_vals$lim_stack_no, NA, paste0(labvar_full,"")), #to be compatible with plot_stack/plot_dodge 
      pos = cumsum(yvar) - 0.5 * yvar
    )
}

# Generate xvar from xvar_df and optionally nsize
# xvar_df = default, basic text version as foung in the data set
# xvar_nr = level number, to be used for reordering the modified version
# xvar = final version that goes to the plot (wrapped or not, with or without nsize)
generate_xvar = function(dat, wrap = TRUE, chrnum = uwb_vals$chrnum, nsize = TRUE){
  xdat = dat |> 
    mutate(xvar = case_when(is.factor(xvar_df)== 0 ~ as_factor(xvar_df),
          TRUE ~ as_factor(xvar_df)),
        xvar_nr = as.numeric(xvar))
  if (wrap) {
    xdat = xdat |> 
      mutate(xvar = str_wrap(xvar, width = chrnum) |> 
        str_replace_all("\n", "<br>"))
  }
  if(nsize) {
    xdat = xdat |> 
      mutate(xvar = case_when(nsize < 20 ~ glue("{xvar}<br><span style = 'color:{uwb_vals$c_nsize1}'>n = {nsize}</span>"),
                            nsize < 30 ~ glue("{xvar}<br><span style = 'color:{uwb_vals$c_nsize2}'>n = {nsize}</span>"), 
                            TRUE ~ glue("{xvar}<br><span style = 'color:{uwb_vals$c_nsize3}'>n = {nsize}</span>")
                            ))
  }
  xdat = xdat |> 
    mutate(xvar = fct_reorder(xvar, xvar_nr))
}
  

# Prep functions ---------------------------------------------------------------
# Prep perc data of single var (N instead of percent as an option?)
prep_single = function(dat, var, order = FALSE, drop_na = TRUE){
  single = dat 
  if (drop_na) {
    single = single |> drop_na({{var}})
  }
  single = single |>
    count({{var}},.drop = TRUE) |> 
    mutate(yvar = n/sum(n) * 100,
           xvar = as_factor({{var}}),
           nsize = sum(n),
           nsize_raw = nrow(dat),
           nsize_listwise = nrow(dat |> drop_na({{var}})),
           name = names(dat |> select({{var}}))
           #name = deparse(substitute(var)) 
    ) |> 
    filter(yvar > 0) |> 
    generate_textlabs() |> 
    impute_labs()
  if (order) {
    single = single |> mutate(xvar = fct_reorder(xvar, -n))
  }
  single = single |> 
    mutate(subtitle = paste0("N=", nsize))
  return(single)
}

# Grouped data prep with or without total
# var becomes zvar, grvar becomes xvar 
prep_gr = function(dat, var, grvar, drop_na = TRUE,
                   add_total = FALSE, lab_total = "ZČU",
                  x_wrap = TRUE, x_chrnum = uwb_vals$chrnum){
  groups = dat 
  if (drop_na) {
    groups = groups |> drop_na({{var}})
  }
  groups = groups |>
    group_by({{grvar}}) |> 
    count({{var}}, .drop = FALSE) |> 
    mutate(yvar = n/sum(n)*100,
           xvar_df = {{grvar}}
           #xvar = as.factor(paste0(str_wrap({{grvar}}, 25),"\nn=",sum(n))),
           )
  if (add_total) {
    total = dat |> drop_na({{var}})  |> 
      count({{var}}, .drop = FALSE) |> 
      mutate(yvar = n/sum(n)*100,
             #xvar = as.factor(paste0(lab_total, "\nn=", sum(n))),
             xvar_df = lab_total)
    groups = bind_rows(groups, total)
  }
  groups = groups |> 
    ungroup() |> 
    group_by(xvar) |> 
    filter(n>0) |> 
    mutate(name = names(dat |> select({{var}})),
           #name = deparse(substitute({{var}})),
           nsize = sum(n),
           zvar = {{var}}
           ) |>  
    generate_xvar(wrap = x_wrap, chrnum = x_chrnum) |> 
    generate_textlabs() |> 
    impute_labs() |> 
    ungroup() 
  #return(groups)
}

# Grouped data prep - two levels of grouping 
# grvar becomes xvar? grvar2 is macro grouping var for facets
prep_gr2 = function(dat, var, grvar, grvar2, drop_na = TRUE,
                    add_total = FALSE, lab_total = "ZČU"){
  groups = dat 
  if (drop_na) {
    groups = groups |> drop_na({{var}})
  }
  groups = groups |> 
    group_by({{grvar}},{{grvar2}}) |> 
    count({{var}},.drop=FALSE) |> 
    mutate(yvar=n/sum(n)*100,
           xvar = as.factor(paste0(str_wrap({{grvar}}, 25),"\nn=",sum(n))),
           xvar_df = {{grvar}},
           zvar = {{var}}, 
           zzvar = as.factor(paste0(str_wrap({{grvar2}}, 25),"\nn=",sum(n))),
           zzvarno = {{grvar2}},
           nsize = sum(n))
  if (add_total) {
    total = prep_gr(dat = dat, var = {{var}}, grvar = {{grvar}}) |> 
      select(yvar, xvar, xvar_df, zvar, nsize) |> 
      mutate(#zzvar = xvar,
             #zzvarno = xvar_df,
             zzvar = as.factor(paste0(lab_total, "\nn=", nsize)),
             zzvarno = lab_total)
    groups = bind_rows(groups, total)
  }
  groups = groups |> 
    mutate(name = names(dat |> select({{var}}))) |> 
      #name = deparse(substitute(var)))
    generate_textlabs() |> 
    impute_labs() |> 
    ungroup()
  #filter(yvar>0)
  #return(groups)
}

# Multiple choice data prep (= batttery with 2 options: chosen vs. not chosen) 
# dodelat drop_na = vyhodit lidi, kteri nic nevybrali
prep_mc = function(dat, vars=names(dat), chosen, drop_na = TRUE, order = FALSE) {
  mc = dat
  # if (drop_na) {
  #   mc = mc |> drop_na({{vars}})
  # }
  mc = mc |> 
    pivot_longer(cols = all_of(vars)) |> 
    group_by(name) |> 
    count(value,.drop = FALSE) |>
    mutate(yvar = n/sum(n)*100,
           xvar = name,
           #nsize_raw=nrow(dat),
           # nsize_listwise=nrow(dat |> drop_na({{var}})),
           nsize=sum(n)) |> 
    filter(yvar > 0) |> 
    ungroup() |> 
    generate_textlabs() |> 
    impute_labs() |> 
    filter(value == chosen) 
    if (order) {
      mc = mc |> mutate(xvar = fct_reorder(xvar, -n))
    }
  mc = try(mc |> mutate(xvar = lab), silent = T) #try to rename items with lab from codebook
  return(mc)
}

# Grouped multiple choice data prep 
# grvar becomes xvar
# vars become zvar
# dodelat drop_na
prep_mc_gr = function(dat, vars=names(dat), grvar, chosen,  drop_na = TRUE, 
                      add_total = FALSE, lab_total = "ZČU") {
  mc = dat |> 
    select(c({{grvar}}, all_of(vars))) |> 
    pivot_longer(cols = all_of(vars)) |> 
    group_by(name, {{grvar}}) |> 
    count(value,.drop = FALSE) |>
    mutate(yvar = n/sum(n)*100,
           zvar = name,
           nsize = sum(n),
           xvar = paste0({{grvar}}, "\nn=", nsize),
           xvar_df = {{grvar}},) |> 
    filter(yvar > 0) |> 
    filter(value == chosen) |> 
    ungroup() |> 
    generate_textlabs() |> 
    impute_labs()
  if (add_total) {
    total = prep_mc(dat = dat, vars = vars, chosen = chosen, drop_na = drop_na) |> 
      mutate(zvarno = lab_total, 
             zvar = paste0(lab_total, "\nn=", nsize))
    mc = mc |> bind_rows(total)
  }
  mc = try(mc |> mutate(zvar = lab), silent = T) #try to rename items with lab from codebook
  return(mc)
}

# Prep MC data in long format, total + grouped by faculty
prep_mc_long_faks = function(dat) {
  groups_nsize = dat |> 
    select(ID) |> 
    unique() |>
    left_join(id_faks) |> 
    count(fak) |> 
    rename(nsize = n)
    
  groups = dat |>
    set_names(c("ID", "xvar")) |>
    left_join(id_faks) |> 
    group_by(fak) |> 
    count(xvar) |> 
    mutate(zvar = fak) |> 
    left_join(groups_nsize)
  
  mc = dat |> 
    set_names(c("ID", "xvar")) |>
    count(xvar) |> 
    mutate(nsize = length(unique(dat$ID)),
           zvar = "ZČU") |>
    bind_rows(groups) |> 
    mutate(yvar = n/nsize * 100,
           name = names(dat)[2]
    ) |> 
    filter(yvar > 0) |> 
    generate_textlabs() |> 
    impute_labs() 
}

# Batteries data prep
# Dat is wide data, it is pivoted longer inside the function

prep_bat = function(dat, vars = names(dat), drop_na = TRUE, 
                    x_wrap = TRUE, x_chrnum = uwb_vals$chrnum, x_nsize = FALSE) {
  #levels = dat |> pull(vars[1]) |> levels()
  
  bat = dat |> 
    pivot_longer(cols = all_of(vars)) 
  
  if (drop_na) {
    bat = bat |> drop_na(value)
  }
  
  bat = bat |> 
    group_by(name) |> 
    count(value,.drop = FALSE) |>
    mutate(yvar = n/sum(n)*100,
           xvar_df = name,
           zvar = value,
           nsize = sum(n)) |> 
    filter(yvar > 0) |> 
    generate_textlabs() |> 
    impute_labs() |> 
    ungroup()  
  
  bat = try(bat |> mutate(xvar_df = lab), silent = T) #try to rename items with lab from codebook
  bat = bat |> 
    generate_xvar(wrap = x_wrap, chrnum = x_chrnum, nsize = x_nsize) 
  return(bat)
}

# Grouped battery data prep 
# zvar is value for battery items
# zzvar is grouping var
# xvar is names of battery item
prep_bat_gr = function(dat, vars=names(dat), grvar, 
                       add_total = FALSE, lab_total = "ZČU") {
  bat = dat |> 
    select(c({{grvar}}, all_of(vars))) |> 
    pivot_longer(cols = all_of(vars)) |> 
    group_by(name, {{grvar}}) |> 
    count(value,.drop = FALSE) |>
    mutate(yvar = n/sum(n)*100,
           xvar = name,
           zvar = value,
           #nsize_raw=nrow(dat),
           # nsize_listwise=nrow(dat |> drop_na({{var}})),
           nsize = sum(n),
           zzvar = paste0({{grvar}}, "\nn=", nsize),
           zzvarno = {{grvar}},) |> 
    filter(yvar > 0) |> 
    #ungroup() |> 
    generate_textlabs() |> 
    impute_labs() |> 
    ungroup()
  
  if (add_total) {
    total = prep_bat(dat = dat, vars = vars) |> 
      mutate(zzvarno = lab_total, 
             zzvar = paste0(lab_total, "\nn=", nsize))
    bat = bat |> bind_rows(total)
  }
  
  bat = try(bat |> mutate(xvar = lab), silent = T) #try to rename items with lab from codebook
  return(bat)
}

# Means of a battery
mean_bat = function(dat, vars, round_places = 0,
                       na_lab = c("NA", "Bez odpovědi")) {
  means = dat |> 
    select(all_of(vars)) |> 
    pivot_longer(cols = all_of(vars)) |> 
    mutate(value_num = parse_number(value, na = na_lab)) |> 
    drop_na(value_num) |>
    group_by(name) |> 
    summarise(yvar = mean(value_num, na.rm = T),
              nsize = n()) |> 
    mutate(xvar = name) |> 
    generate_textlabs() |> 
    impute_labs() |> 
    mutate(labvar_full = round(yvar, round_places),
           labvar_single = as.character(labvar_full)
    ) |> 
    ungroup()
}

# Means of a battery, grouped
mean_bat_gr = function(dat, vars, grvar, round_places = 0,
                       add_total = FALSE, lab_total = "ZČU",
                       na_lab = c("NA", "Bez odpovědi")) {
  means = dat |> 
    select(c({{grvar}}, all_of(vars))) |> 
    pivot_longer(cols = all_of(vars)) |> 
    mutate(value_num = parse_number(as.character(value), na = na_lab)) |> 
    drop_na(value_num) |> 
    group_by(name, {{grvar}}) |> 
    summarise(yvar = mean(value_num, na.rm = T),
              nsize = n()) |> 
    mutate(labvar_full = round(yvar, round_places),
           xvar = paste0({{grvar}}, "\nn = ", nsize),
           xvar_df = {{grvar}},
           zvar = name) |> 
    ungroup() |> 
    impute_labs()
  
  if (add_total) {
    total = mean_bat(dat = dat, vars = vars, round_places = round_places, 
                     na_lab = na_lab) |> 
      #select(xvar, yvar, nsize, labvar_full) |> 
      mutate(zvar = xvar,
             xvar = as.factor(paste0(lab_total, "\nN=", nsize)),
             xvar_df = lab_total)
    means = bind_rows(means, total)
    }  

    means = try(means |> mutate(zvar = lab), silent = T) #try to rename items with lab from codebook
}

#DODELAT ADD_TOTAL
mean_bat_gr2 = function(dat, vars, grvar, grvar2, round_places = 0,
                        na_lab = c("NA", "Bez odpovědi")) {
  means = dat |> 
    select(c({{grvar}}, {{grvar2}}, all_of(vars))) |> 
    pivot_longer(cols = all_of(vars)) |> 
    mutate(value_num = parse_number(value, na = na_lab)) |> 
    drop_na(value_num) |> 
    group_by(name, {{grvar}}, {{grvar2}}) |> 
    summarise(yvar = mean(value_num, na.rm = T),
              nsize = n()) |> 
    ungroup() |> 
    impute_labs() |> 
    mutate(labvar_full = round(yvar, round_places),
           xvar = paste0({{grvar}}, "\nn=", nsize),
           zvar = {{grvar2}}
    ) 
  #means = try(means |> mutate(zvar = lab), silent = T) #try to rename items with lab from codebook
}


# Plot functions ---------------------------------------------------------------
#' lollipop plot of single categorical variable. 
#'
#' @param dat A data frame to plot. 
#' It has to include columns named: xvar, yvar, labvar (text labels for yvar), cvar(fill color), title, subtitle, caption.
#' @param horiz TRUE for horinzontal plot, FALSE for vertical plot.
#'
#' @return Plot
#' @export
#'
#' @examples
plot_lolli = function(dat, horiz = TRUE){
  if(horiz){
    d = dat |> mutate(xvar = fct_rev(xvar))
    } else {
      d = dat
    }
  p = ggplot(d, aes(y = yvar, x = xvar)) +
    geom_segment(aes(x = xvar, xend = xvar, y = 0, yend = yvar), colour = uwb_vals$lollistick) +
    geom_point(size = uwb_vals$pointsize, shape = 21, color = "white", fill = d$cvar) +
    geom_text(aes(x = xvar, y = yvar, label = labvar_single),
              colour = "white", size = uwb_vals$labsize) +
    labs(y = "%", x = "",
         title = d$title[1],
         subtitle = d$subtitle[1],
         caption = d$caption[1]) +
    scale_x_discrete(labels = label_wrap(uwb_vals$chrnum)) +
    theme_uwb() + theme(axis.ticks.y = element_blank()
                      )
  if(horiz){
    p=p + coord_flip() +
      theme_uwb_horiz() + theme(axis.ticks.x = element_blank())
  }
  print(p)
}

#' Barplot of a single categorical variable. 
#'
#' @param dat A data frame to plot. 
#' It has to include columns named: xvar, yvar, labvar (text labels for yvar), cvar(fill color), title, subtitle, caption.
#' @param horiz TRUE for horinzontal plot, FALSE for vertical plot.
#'
#' @return Plot
#' @export
#'
#' @examples
plot_bar = function(dat, horiz = TRUE){
  if (horiz){
    d=dat |> mutate(xvar = fct_rev(xvar))
    } else {
      d=dat
  }
  p = ggplot(d,aes(y = yvar,x = xvar)) +
    geom_bar(stat = "identity", width = 0.85,colour = uwb_vals$barcol,fill = d$cvar) +
    geom_text(aes(x = xvar, y = pos_single, label = labvar_single),
              colour = d$cvar_text, size=uwb_vals$labsize) +
    labs(y="%",x="",
         title=d$title[1],
         subtitle=d$subtitle[1],
         caption=d$caption[1]) +
    scale_x_discrete(labels = label_wrap(uwb_vals$chrnum)) +
    theme_uwb() + theme(axis.ticks.y = element_blank()
    )
  if (horiz) {
    p=p + coord_flip() +
      theme_uwb_horiz() 
  } 
  print(p)
}

# Plot stacked percentages by groupvar 
plot_stack = function(dat, horiz = TRUE){
  if (horiz){
    dat = dat |> mutate(xvar = fct_rev(xvar))
  }
  p = ggplot(dat,aes(y = yvar,x = xvar, fill = zvar )) +
    geom_bar(stat = "identity", width = 0.85,colour=uwb_vals$barcol,
             position=position_stack(reverse = TRUE)) +
    scale_fill_uwb("quali") + #default palette is qualitative, change after plotting if necessary
    geom_text(aes(x = fct_rev(xvar), y = pos, label = labvar),
              #family = uwb_vals$font,
              colour = "white", size = uwb_vals$labsize) +
    ylim(0, 100.1) + # Slightly more than 100 to make sure all values are shown
    labs(y = "%", x = "", fill = "",
         title = dat$title[1],
         subtitle = dat$subtitle[1],
         caption = dat$caption[1]) +
        theme_uwb() 
  
  if (horiz) {
    p = p + coord_flip() +
      theme_uwb_horiz() 
  }
  print(p)
}

# Plot dodge percentages by groupvar 
# HORIZONTAL, add option horiz 
plot_hdodge = function(dat){
  p=ggplot(dat,aes(y = yvar,x = fct_rev(xvar), fill = fct_rev(zvar))) +
    geom_bar(stat="identity", width = 0.85,colour=uwb_vals$barcol,
             position=position_dodge(width=0.85)) +
    scale_fill_uwb("quali") +
    geom_text(aes(x = xvar, y = pos_single, label = labvar),
              position = position_dodge(width=0.85),
              #colour = dat$cvar_text, 
              size=uwb_vals$labsize) +
    labs(y="",x="",fill='',
         title=dat$title[1],
         subtitle=dat$subtitle[1],
         caption=dat$caption[1]) +
    coord_flip() +
    theme_uwb_horiz() 
  print(p)
}

# Plot trend - single var
# xvar is the time var
plot_trend = function(dat) {
  p = ggplot(dat, aes(y = yvar, x = xvar, group = 1)) +
    geom_line(size = 1, color = uwb_scales$quali[1]) + 
    geom_point(size = uwb_vals$pointsize, color = uwb_scales$quali[1]) + 
    geom_text(aes(label = labvar_full), color = "white", size = uwb_vals$labsize) +
    labs(y = "", x = "", fill = "",
         title = dat$title[1],
         subtitle = dat$subtitle[1],
         caption = dat$caption[1]) +
    theme_uwb()
  print(p)
}

# Plot trend by a grouping var
# xvar is the time var, zvar is the groups var
# overlap_fix does not work when the plot is facetted, it has to be FALSE in the function and done manually before plotting

plot_trend_gr = function(dat, total, add_points = TRUE) {
  if (missing(total)) { # Larger size of total points
    ddat = dat |> 
      mutate(pointsize = uwb_vals$pointsize)
  } else {
    ddat = dat |> 
      mutate(pointsize = case_when(zvar == total ~ uwb_vals$pointsize + 5,
                                   TRUE ~ uwb_vals$pointsize))
  }
  
  p = ggplot(ddat, aes(y = yvar, x = xvar, group = zvar, color = zvar )) +
    geom_line(size = 1, alpha = 1) + 
    scale_color_uwb("quali") +
    labs(y = "", x = "", color = "",
         title = dat$title[1],
         subtitle = dat$subtitle[1],
         caption = dat$caption[1]) +
    theme_uwb() 
  
  if (add_points) {
    p = p + geom_point(size = ddat$pointsize, alpha = 0.95) + 
      geom_text(aes(label = labvar_full), 
                color = "white", size = 0.75 * uwb_vals$labsize, check_overlap = TRUE) +
      guides(color = guide_legend(override.aes = list(size = 0.5 * uwb_vals$pointsize))) 
  }
  
  return(p)
}


# Bubbleplot = lolli plot without a stick and with jittered light grey values for faculties in the background
plot_bubbles = function(dat, horiz = TRUE, total = "ZČU", diff = 10, 
                        seed = 123, jitter_wid = 0.1) {
  if(horiz){
    d = dat |> mutate(xvar = fct_rev(xvar))
  } else {
    d = dat
  }
  # Add total value for reference to be able to highlight extreme values
  d_ref = d |> 
    filter(zvar == "ZČU") |> 
    select(xvar, yvar) |> 
    rename(yvar_ref = yvar)
  
  d = d |> 
    left_join(d_ref) |> 
    mutate(alpha_color = ifelse(abs(yvar - yvar_ref) > diff & zvar != "ZČU" , 1, 0),
           alpha_grey = ifelse(abs(yvar - yvar_ref) <= diff & zvar != "ZČU" , 1, 0)
           )
  
  # Delete overplotted text labels
  #all_labs = 
  
  p = ggplot(d |> filter(zvar == total), 
             aes(y = yvar, x = xvar)) +
    # Grey midzone 
    geom_jitter(data = d, alpha = d$alpha_grey,
                size = uwb_vals$pointsize - 5, color = "grey75", 
                position = position_jitter(height = 0, width = jitter_wid, seed = seed)) +
    # Extreme colored faks with text labels
    geom_jitter(data = d, aes(color = zvar), alpha = d$alpha_color, 
                size = uwb_vals$pointsize - 5, 
                position = position_jitter(height = 0, width = jitter_wid, seed = seed) ) +
    scale_color_uwb("faks") +
    geom_text(data = d, aes(label = labvar_full), alpha = d$alpha_color,
             position = position_jitter(height = 0, width = jitter_wid, seed = seed),
             color = "white", size = 0.65 * uwb_vals$labsize) +
    geom_point(size = uwb_vals$pointsize, color = uwb_scales$faks[10]) +
    geom_text(aes(label = labvar_single),
              color = "white", size = uwb_vals$labsize) +
    labs(y = "%", x = "", color = "",
         title = d$title[1],
         subtitle = paste0("Zvýrazněné jsou fakulty, které se od celkové hodnoty ZČU liší o více než ", diff, " proc. bodů"),
         caption = d$caption[1],
    ) +
    scale_x_discrete(labels = label_wrap(uwb_vals$chrnum)) +
    theme_uwb() + theme(axis.ticks.y = element_blank())
  
  if(horiz){
    p = p + coord_flip() +
      theme_uwb_horiz() + theme(axis.ticks.x = element_blank())
  }
  
  print(p)
}


# Ribbon plot
# hide_lab = do not show labels of small bumps
# One ribbon is a polygon with lots of border points to be smooth
# plot_ribbon = function(dat, space = 5, fill = uwb_scales$quali, margin = 15){
#   d = dat |> 
#     # Special vars for ggsankey
#     group_by(zvar) |> 
#     mutate(next_x = lead(xvar),
#            next_node = lead(zvar)) |>
#     ungroup() 
#     #relocate?

#   # Space between ribbons
#   space = space
  
#   preplot = ggplot() +
#     geom_sankey_bump(data = d, aes(x = xvar, 
#                                      node = zvar, 
#                                      fill = factor(zvar), 
#                                      group = zvar,
#                                      value = yvar, 
#                                      label = zvar, 
#                                      next_x = next_x, next_z = next_node),
#     space = space, color = "transparent", smooth = 15, 
#     #type = "alluvial"
#     type = "sankey"
#     #show.legend = F
#     ) +
#     geom_sankey_label(size = 3, color = "black") +
#     #scale_fill_uwb("quali") +
#     scale_fill_manual(values = fill) +
#     theme_uwb() +
#     labs(x = "", y = "", fill = "",
#          title = d$title[1],
#          subtitle = d$subtitle[1],
#          caption = d$caption[1]) +
#     theme(legend.position = "bottom") +
#     theme(axis.line.y = element_blank(), 
#           axis.ticks.y = element_blank(),
#           axis.text.y = element_blank())
  
#   # Position of labels (inside ribbon boxes)
#   x_len = length(levels(d$xvar))
  
#   pos_label = ggplot_build(preplot) |>
#     .$data |> 
#     .[[1]] |> 
#     filter(x %in% 1:x_len) |> 
#     group_by(x, group) |> 
#     mutate(ypos = mean(y),
#            ymin = min(y),
#            ymax = max(y)) |> 
#     select(-y) |> 
#     ungroup() |> 
#     unique() |> 
#     left_join(tibble(x = 1:x_len, 
#                      xvar = levels(d$xvar))) |> 
#     left_join(d |> 
#                 select(xvar, yvar, zvar, labvar) |> 
#                 rename(label = zvar))
  
#   # Bar ends
#   bar_ends = pos_label |> 
#     filter(x == 1 | x == x_len) |> 
#     mutate(xmin = case_when(x == 1 ~ 0.75,
#                             x == x_len ~ x_len - 0.1),
#            xmax = case_when(x == 1 ~ 1.1,
#                             x == x_len ~ x_len + 0.25),
#            #ymin = yvar - 0.5 * n + 0.5 * space,
#            #ymax = yvar + 0.5 * n - 0.5 * space
#            )
  
#   # Position of ribbon labels (annotation at the end of the ribbon)
#   last = bar_ends |> 
#     filter(x == x_len) |> 
#     mutate(label = str_wrap(label, margin))
  
#   # Final plot
#   preplot + 
#     geom_rect(data = bar_ends, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#               fill = bar_ends$fill) +
#     geom_text(data = pos_label, aes(x = xvar, y = ypos, label = labvar), 
#               color = "white", size = 0.85 * uwb_vals$labsize) +
#     annotate("text", x = x_len + 0.35, y = last$ypos, label = last$label,
#              color = last$fill, size = uwb_vals$labsize, hjust = 0, lineheight = 0.75) + 
#     guides(fill = "none") +
#     coord_cartesian(clip = "off") +   # This allows text outside the panel
#     theme(plot.margin = unit(c(1, 0.6 * margin, 1, 0), "lines"),
#           plot.title = element_textbox_simple(margin = unit(c(0, -0.6 * margin, 1, 0), "lines") )) # negative right margin pushes text outward
# }


# #  dat = prep_gr(cube, fakulta_studenta, grvar = ar) 
# #  space = 5
# #  hide_lab = 0.5
# # # plot_ribbon(xx, fill = uwb_scales$faks) 
# #   
# # dat = meandata |> filter(str_detect(name, mean_vars[[7]])) |> 
# #   mutate(xvar = as.factor(xvar),
# #          labvar = round(yvar, 1))
# # 
# # plot_ribbon(dat, fill = uwb_scales$quali, margin = 20) + 
# #   labs(title =" -- trends")






