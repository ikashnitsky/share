#===============================================================================
# 2019-11-08 PRC dataviz workshop
# Simple rules and tricks to improve dataviz communication
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

library(tidyverse)
library(magrittr)

# pipes
# https://twitter.com/andrewheiss/status/1173743447171354624
# https://twitter.com/dmi3k/status/1191824875842879489





# RULE 0 -- DO VISUALIZE YOUR DATA  ---------------------------------------

# https://twitter.com/JustinMatejka/status/859075295059562498





# RULE 1 -- text should be horizontal -------------------------------------


# Initial post that I read on the topic
# http://www.b-eye-network.com/view/2468

# Post by Griffith Feeney
# http://demographer.com/dsitl/08-cleveland-dot-plots

# example of a nice usage here
# https://twitter.com/jburnmurdoch/status/1163429037613703175

# and here
# https://barcanumbers.wordpress.com/2018/12/06/who-are-the-best-finishers-in-contemporary-football

# My post
# https://ikashnitsky.github.io/2019/dotplot/



# We are going to improve Figure 2 from 
# Janssen, F., & de Beer, J. (2019). The timing of the transition from mortality compression to mortality delay in Europe, Japan and the United States. Genus, 75(1), 10. 
# https://doi.org/10/ggbtpx


jb <- tibble::tribble(
    ~period, ~region, ~sex,  ~delay, ~compression,
    "1950-1979",    "JP",  "m",  "6.35",       "9.28",
    "1950-1979",    "JP",  "f",  "6.61",      "10.91",
    "1950-1979",    "US",  "m",  "2.76",       "1.84",
    "1950-1979",    "US",  "f",  "4.93",       "1.76",
    "1950-1979",  "NEur",  "m",  "0.15",       "3.90",
    "1950-1979",  "NEur",  "f",  "3.78",       "3.18",
    "1950-1979",  "WEur",  "m",  "1.33",       "3.77",
    "1950-1979",  "WEur",  "f",  "4.15",       "3.27",
    "1950-1979",  "SEur",  "m",  "1.28",       "8.85",
    "1950-1979",  "SEur",  "f",  "3.68",       "8.36",
    "1950-1979",  "EEur",  "m", "-1.21",       "4.20",
    "1950-1979",  "EEur",  "f",  "1.35",       "4.38",
    "1980-2014",    "JP",  "m",  "6.70",       "0.47",
    "1980-2014",    "JP",  "f",  "7.70",       "0.28",
    "1980-2014",    "US",  "m",  "8.17",      "-1.23",
    "1980-2014",    "US",  "f",  "3.56",       "0.47",
    "1980-2014",  "NEur",  "m",  "8.65",      "-0.32",
    "1980-2014",  "NEur",  "f",  "5.25",       "0.69",
    "1980-2014",  "WEur",  "m",  "8.61",       "0.36",
    "1980-2014",  "WEur",  "f",  "5.71",       "0.80",
    "1980-2014",  "SEur",  "m",  "7.36",       "1.78",
    "1980-2014",  "SEur",  "f",  "6.13",       "1.54",
    "1980-2014",  "EEur",  "m",  "0.35",       "4.93",
    "1980-2014",  "EEur",  "f",  "4.38",       "0.86"
) %>% 
    pivot_longer(names_to = "component", cols = delay:compression) %>% 
    mutate(value = value %>% as.numeric()) %>% 
    mutate_if(is_character, as_factor)


# replicate the figure
jb %>% 
    mutate(region_sex = paste(region, sex, sep = "_") %>% 
               as_factor()) %>% 
    ggplot(aes(region_sex, value, fill = component))+
    geom_col()+
    facet_grid(~period)

# almost -- delay should be mapped first
# fix the ordering of stacked bar
jb %>% 
    mutate(region_sex = paste(region, sex, sep = "_") %>% 
               as_factor()) %>% 
    ggplot(aes(region_sex, value, fill = component))+
    geom_col(position = position_stack(reverse = TRUE))+ 
    facet_grid(~period)


# flip the coordinates
jb %>% 
    mutate(region_sex = paste(region, sex, sep = "_") %>% 
               as_factor() %>% 
               fct_rev()) %>% # ggplot goes bottom-up on y axis
    ggplot(aes(region_sex, value, fill = component))+
    geom_col(position = position_stack(reverse = TRUE))+
    coord_flip()+
    facet_grid(~period)


# the legend eats up space
jb %>% 
    mutate(region_sex = paste(region, sex, sep = "_") %>% 
               as_factor() %>% 
               fct_rev()) %>% 
    ggplot(aes(region_sex, value, fill = component))+
    geom_col(position = position_stack(reverse = TRUE))+
    coord_flip()+
    facet_grid(~period)+
    theme(legend.position = "bottom")  # here!


# more faceting options to explore data dimensions

# focus on region comparison
jb %>% 
    mutate(region = region %>% fct_rev()) %>% 
    ggplot(aes(region, value, fill = component))+
    geom_col(position = position_stack(reverse = TRUE))+
    coord_flip()+
    facet_grid(sex~period)+ # <- 
    theme(legend.position = "bottom")


# focus on period comparison
jb %>% 
    mutate(region = region %>% fct_rev()) %>% 
    ggplot(aes(period, value, fill = component))+
    geom_col(position = position_stack(reverse = TRUE))+
    coord_flip()+
    facet_grid(region~sex)+ # <- 
    theme(legend.position = "bottom")

# focus on sex comparison -- as do the authors
jb %>% 
    mutate(region = region %>% fct_rev()) %>% 
    ggplot(aes(sex, value, fill = component))+
    geom_col(position = position_stack(reverse = TRUE))+
    coord_flip()+
    facet_grid(region~period)+ # <- 
    theme(legend.position = "bottom")


# unlike mapping over y axis, faceting goes top-bottom
jb %>% 
    # mutate(region = region %>% fct_rev()) %>% # we don't need this one
    ggplot(aes(sex, value, fill = component))+
    geom_col(position = position_stack(reverse = TRUE))+
    coord_flip()+
    facet_grid(region~period)+ # <- 
    theme(legend.position = "bottom")



# change facet labels -- they are called strips in ggplot
jb %>% 
    ggplot(aes(sex, value, fill = component))+
    geom_col(position = position_stack(reverse = TRUE))+
    coord_flip()+
    facet_grid(region~period)+ 
    theme(
        legend.position = "bottom",
        strip.text.y = element_text(angle = 0) # <- 
    )


# we now have enough space to write out full names !!
jb_names <- jb %>% 
    mutate(
        region = region %>% as_factor() %>% 
            lvls_revalue(
                c(
                    "Japan", "Unites States", "Northern Europe",
                    "Western Europe", "Southern Europe", "Eastern Europe"
                )
            ),
        sex = sex %>% as_factor() %>% lvls_revalue(c("Males", "Females"))
    )

jb_names %>% 
    ggplot(aes(sex, value, fill = component))+
    geom_col(position = position_stack(reverse = TRUE))+
    coord_flip()+
    facet_grid(region~period)+
    theme(
        legend.position = "bottom",
        strip.text.y = element_text(angle = 0)
    )


# final touch -- some themeing
# get a nice font, I love Roboto family developed by Google
# Roboto Condensed is nice because it's narrow but still very readable
library(hrbrthemes); import_roboto_condensed()

jb_names %>% 
    ggplot(aes(sex, value, fill = component))+
    geom_col(position = position_stack(reverse = TRUE))+
    geom_hline(yintercept = 0)+ # add vetical line at 0
    coord_flip()+
    facet_grid(region~period)+
    theme_minimal(base_family = font_rc)+ # change theme & nice font
    theme(
        legend.position = "bottom",
        strip.text.y = element_text(angle = 0)
    )+
    scale_fill_manual(values = c("black", "gray"))+
    labs(
        title = "Increase in life expectancy at birth",
        subtitle = "Decomposition into the effects of compression and delay",
        fill = NULL,
        x = NULL, # attention! we used coor_flip
        y = "Increase in life expectancy at birth over the period"
    )





# RULE 2 -- as large as possible text for presentations -------------------

gg_jb <- last_plot()

ggsave(
    filename = "out.png"
)

# control width and height, in ggplot they are given in  inches
ggsave(
    filename = "out.png", 
    width = 7, height = 5
)


# for presentation we need to increase all the text elements

ggsave("out.png", width = 4, height = 3)

# not nice

# let's increase the text size in the plot
gg_jb +
    theme_minimal(base_size = 16)

# oups, now we've lost additional theme options
gg_jb +
    theme_minimal(base_family = font_rc, base_size = 16)+
    theme(
        legend.position = "bottom",
        strip.text.y = element_text(angle = 0)
    )

ggsave("out.png", width = 7, height = 5)


# we can change specific elements separately in theme()
gg_jb +
    theme_minimal(base_family = font_rc, base_size = 16)+
    theme(
        legend.position = "bottom",
        strip.text.y = element_text(angle = 0, size = 20, face = 2, hjust = 0)
    )

ggsave("out.png", width = 7, height = 5)


# let's change the title
gg_jb +
    theme_minimal(base_family = font_rc, base_size = 16)+
    theme(
        legend.position = "bottom",
        strip.text.y = element_text(angle = 0, size = 20, face = 2, hjust = 0),
        plot.title = element_text(size = 30, family = "Roboto Slab")
    )

ggsave("out.png", width = 7, height = 5)


# too big,  decrease a bit
gg_jb +
    theme_minimal(base_family = font_rc, base_size = 16)+
    theme(
        legend.position = "bottom",
        strip.text.y = element_text(angle = 0, size = 20, face = 2, hjust = 0),
        plot.title = element_text(size = 24, family = "Roboto Slab")
    )

ggsave("out.png", width = 7, height = 5)

#  if we need a smaller size file, we can reduce dpi parameter
# digits per inch, the default is 300
ggsave("out.png", width = 7, height = 5, dpi = 100)

# for better PNG quality and optimization use Cairo graphic device
ggsave("out.png", width = 7, height = 5, dpi = 100, type = "cairo")

# for the best quality use PDF export
ggsave("out.pdf", width = 7, height = 5)

# oups, problems with exporting fonts, again use Cairo graphics
ggsave("out.pdf", width = 7, height = 5, device = cairo_pdf)


# BONUS: own ggsave  functions -----------------------------------------

# one can define own convenience functions for better ggsave options

ggsave_png <- function(
    gg = ggplot2::last_plot(), 
    path = "out.png",
    ar = 0.5625,
    w = 7,
    h = w*ar,
    type = "cairo",
    dpi = 300
){
    ggsave(filename = path, plot = gg, width = w, height = h, type = type, dpi = dpi)
}

ggsave_pdf <- function(
    gg = ggplot2::last_plot(), 
    path = "out.pdf",
    ar = 0.5625, # this default is to match 16/9 screens
    w = 7,
    h = w*ar,
    device = cairo_pdf
){
    ggsave(filename = path, plot = gg, width = w, height = h, device = device)
}   


# test the functions

gg_jb +
    theme_minimal(base_family = font_rc, base_size = 16)+
    theme(
        legend.position = "bottom",
        strip.text.y = element_text(angle = 0, size = 20, face = 2, hjust = 0),
        plot.title = element_text(size = 24, family = "Roboto Slab")
    )
    
out <- last_plot()    

out %>% ggsave_png()    

out %>% ggsave_png(ar = 1, dpi = 100)  

out %>% ggsave_pdf(w = 7, h = 5)
    
    



# RULE 3 -- mind colors, especially regarding colorblind friendliness --------

# colors in ggplot

# Parameter "color" changes the color of lines and points
# Parameter "fill" changes the color of shapes (see the violin example)
# The way you override ggplot"s defaults is to use functions
# scale_color_[...] or scale_fill_[...] (use TAB to see options)
# I really recommend viridis colors as your daily basis
# NOTE: with viridis you need to know if you variable is continuous or categorical
# The video on viridis https://youtu.be/xAoljeRJ3lU

# colorblind friendliness
# https://www.toptal.com/designers/colorfilter?orig_uri=https://infographic.statista.com/normal/chartoftheday_13680_the_legal_status_of_abortion_worldwide_n.jpg&process_type=protan


# paletteer !!!
library(paletteer)
# https://github.com/EmilHvitfeldt/r-color-palettes

# low level manipulations with color palettes
library(prismatic)
# https://github.com/EmilHvitfeldt/prismatic




# random minimal tileplot
crossing(
    x = LETTERS %>% extract(1:4),
    y = 5:8
) %>% 
    mutate(z = runif(16)) %>% 
    ggplot(aes(x, y, fill = z))+
    geom_tile()

gg <- ggplot2::last_plot()

# Genger colors
# https://blog.datawrapper.de/gendercolor/
# https://twitter.com/d_alburez/status/1184120385899581440



# themes
# check out:
# https://evamaerey.github.io/little_flipbooks_library/taming_themes_in_ggplot/taming_ggplot_themes.html
library(ggthemes)

gg + theme_minimal()
gg + theme_bw()
gg + theme_light()
gg + theme_excel()    # ugly, isn"t it?
gg + theme_few()      # one of my favorites
gg + theme_economist()
gg + theme_wsj()
gg + theme_fivethirtyeight() # I love this one
gg + theme_solarized()
gg + theme_dark()
# ... feel free to test them all))
esquisse::esquisser()


# turn all themes dark
# https://github.com/nsgrantham/ggdark

library(ggdark)

gg_jb +
    dark_theme_minimal(base_family = font_rc, base_size = 16)+
    scale_fill_viridis_d(begin = .4)+
    theme(
        legend.position = "bottom",
        strip.text.y = element_text(angle = 0, size = 20, face = 2, hjust = 0),
        plot.title = element_text(size = 24, family = "Roboto Slab")
    )


# scale _ identity
# https://twitter.com/ikashnitsky/status/937786580231696384

n <- 100

tibble(x = runif(n),
       y = runif(n),
       size = runif(n, min = 4, max = 20)) %>%
    ggplot(aes(x, y, size = size)) +
    geom_point(color = "white", pch = 42) +
    scale_size_identity() +
    coord_cartesian(c(0, 1), c(0, 1)) +
    theme_void() +
    theme(
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black")
    )


# generate bubbles of random color and size
n <- sample(20:50, 3)

tibble(
    x = runif(n),
    y = runif(n),
    size = runif(n, min = 3, max = 20),
    color = rgb(runif(n), runif(n), runif(n))
) %>%
    ggplot(aes(x, y, size = size, color = color)) +
    geom_point() +
    scale_color_identity() +
    scale_size_identity() +
    coord_cartesian(c(0, 1), c(0, 1)) +
    theme_void()





# RULE 4 -- highlight what's important for the story -------------------------

# a great example here
# https://barcanumbers.wordpress.com/2018/12/06/who-are-the-best-finishers-in-contemporary-football


library(gapminder)

gapminder %>%
    ungroup() %>%
    ggplot(aes(x = year, y = lifeExp, color = continent, group = country)) +
    geom_path() +
    theme_minimal(base_family = "mono")


# assume we are talking about South Africa
gapminder %>%
    ungroup() %>%
    ggplot(aes(x = year, y = lifeExp, color = continent, group = country)) +
    geom_path() +
    scale_color_grey()+
    geom_path(data = . %>% filter(country == "South Africa"), 
              color = "red", size = 2)+
    annotate("text", x = 1990, y = 65, label = "South Africa", 
             size = 7, color = "red", fontface = 2, family = "mono")+
    theme_minimal(base_family = "mono")





# BUNUS -- animation ------------------------------------------------------

library(gganimate)

# one animation worth thousands (not words) arguments
# https://twitter.com/mikeleeco/status/876792944396730368

# the power of moving charts
# https://twitter.com/jburnmurdoch/status/1107552367795412992?lang=en
# https://www.ft.com/video/83703ffe-cd5c-4591-9b4f-a3c087aa6d19


gapminder %>%
    select(1:4) %>%
    group_by(continent, year) %>%
    summarise(avg_e0 = lifeExp %>% mean) %>%
    ungroup() %>%
    ggplot(aes(x = year, y = avg_e0,
               color = continent)) +
    geom_path() +
    theme_minimal(base_family = "mono")


p <- ggplot2::last_plot() # note gganimate re-writes last_plot()

ani <- p + 
    geom_point()+
    transition_reveal(year)+
    ease_aes("cubic-in-out")

# define the number of data points
nfr <- gapminder %>% pull(year) %>% unique() %>% length()

animate(
    ani, 
    nframes = nfr * 3, 
    width = 500, height = 400, 
    res = 100, 
    start_pause = 3, end_pause = 10
)

# save animation
anim_save('test-anim.gif')
