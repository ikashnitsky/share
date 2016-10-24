################################################################################
#
# Sex ratios 12-10-2016
# Replicate the analysis for the post https://habrahabr.ru/post/311970/ (RUS)
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#
################################################################################

# Erase all objects in memory
rm(list = ls(all = TRUE))

# load required packages
# The code is written and tested on a PC-win7-x64
# R version 3.3.1

# load required packages
library(dplyr) # version 0.5.0
library(readr) # version 1.0.0
library(readxl) # version 0.1.1
library(tidyr) # version 0.6.0
library(broom) # version 0.4.1
library(lubridate) # version 1.6.0
library(stringr) # version 1.0.0
library(texreg) # version 1.36.7

library(ggplot2) # version 2.1.0
library(ggthemes) # version 3.2.0
library(extrafont) # version 0.17
library(RColorBrewer) # version 1.1-2
library(cowplot) # version 0.6.2

library(tmap) # version 1.4-1
library(rgdal) # version 1.1-10
library(rgeos) # version 0.3-19
library(maptools) # version 0.8-39


# set working directory to the one where you have this file
setwd()


################################################################################
# -1- GET STAT DATA

### EUROSTAT ###
# download Eurostat data. We are interested in 
# homicide, which is in data set 'crim_gen' and
# poverty rate, which is in in data set 'ilc_peps01'
# to browse data sets you may use 
# http://ec.europa.eu/eurostat/data/database
# for manual download
# http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing
# we will use the R method to grab the data and read them directely into R
library(eurostat) # version 1.2.21
 
crime <- get_eurostat('crim_gen',time_format = 'num')
homicide <- crime %>% filter(iccs=='ICCS0101', time%in%2000:2007) %>% select(-unit,-iccs)

poverty <- get_eurostat('ilc_peps01',time_format = 'num')
poverty_avg <- poverty %>% 
        filter(age%in%c('Y25-49','Y15-24'),sex=='T',unit=="PC_POP",time%in%2003:2007) %>%
        #select(-unit,-age,-sex,-time) %>% 
        group_by(geo) %>%
        summarise(pov=mean(values,na.rm = T))

# set values for England, Scotland, and Northern Ireland equal to the UK
poverty_uk <- data_frame(geo = c("UKC-L","UKM","UKN"),pov=22.6833)
poverty_avg <- bind_rows(poverty_avg,poverty_uk)

# clean up Global Environment
rm(crime, poverty, poverty_uk)
        

### HUMAN MORTALITY DATABASE ###
# read HMD data directly into R
# please note! the arguments "ik_user_hmd" and "ik_pass_hmd" are my login credidantials
# at the website of Human Mortality Database, which are stored locally at my computer. 
# In order to access the data, you need to create an account at
# http://www.mortality.org/
# and provide your own credidantials to the `readHMDweb()` function
# Be patient. The process takes decent amount of time

library(HMDHFDplus) # version 1.1.8
country <- getHMDcountries()

# We are interested in the data sets called "Exposure-to-risk"
exposures <- list()
for (i in 1: length(country)) {
        cnt <- country[i]
        exposures[[paste0(cnt,'_exp')]] <- readHMDweb(cnt,"Exposures_1x1",ik_user_hmd,ik_pass_hmd)
        
        paste(i,'out of',length(country))
}

# merge all countrys' data into one data set
# and calculate Adult Sex Ratio (males per 100 females) 
asr <- list()
for (i in 1:length(exposures)) {
        di <- exposures[[i]] %>% 
                select(Year,Age,Female,Male) %>% 
                filter(Year %in% 2000:2007, Age %in% 15:49) %>%
                select(-Age) %>%
                group_by(Year) %>%
                summarise_each(funs(sum)) %>%
                ungroup() %>%
                transmute(id_hmd=gsub('_exp','',names(exposures)[i]),
                          year=Year, asr=Male/Female*100)
        asr[[i]] <- di
}
asr <- bind_rows(asr)

# total population HMD
pop_tot <- list()
for (i in 1:length(exposures)) {
        di <- exposures[[i]] %>% 
                select(Year,Age,Total) %>% 
                filter(Year %in% 2000:2007) %>%
                select(-Age) %>%
                group_by(Year) %>%
                summarise_each(funs(sum)) %>%
                ungroup() %>%
                transmute(id_hmd=gsub('_exp','',names(exposures)[i]),
                          year=Year,pop=Total)
        pop_tot[[i]] <- di
}
pop_tot <- bind_rows(pop_tot)




# Get HMD life tables 1x1
lt_f <- list()
lt_m <- list()

for (i in 1: length(country)) {
        cnt <- country[i]
        lt_f[[paste0(cnt,'_lt_f')]] <- readHMDweb(cnt,"fltper_1x1",ik_user_hmd,ik_pass_hmd)
        lt_m[[paste0(cnt,'_lt_m')]] <- readHMDweb(cnt,"mltper_1x1",ik_user_hmd,ik_pass_hmd)
        
        print(paste(i,'out of',length(country)))
}

asr_lt_f <- list()
for (i in 1:length(lt_f)) {
        di <- lt_f[[i]] %>%
                select(Year,Age,lx) %>% 
                filter(Year %in% 2000:2007, Age %in% 15:49) %>%
                select(-Age) %>%
                group_by(Year) %>%
                summarise_each(funs(sum)) %>%
                ungroup() %>%
                transmute(id_hmd=gsub('_lt_f','',names(lt_f)[i]),
                          year=Year,lx_f=lx)
        asr_lt_f[[i]] <- di
}
asr_lt_f <- bind_rows(asr_lt_f)


asr_lt_m <- list()
for (i in 1:length(lt_m)) {
        di <- lt_m[[i]] %>% 
                select(Year,Age,lx) %>% 
                filter(Year %in% 2000:2007, Age %in% 15:49) %>%
                select(-Age) %>%
                group_by(Year) %>%
                summarise_each(funs(sum)) %>%
                ungroup() %>%
                transmute(id_hmd=gsub('_lt_m','',names(lt_m)[i]),
                          year=Year,lx_m=lx)
                
        asr_lt_m[[i]] <- di
}
asr_lt_m <- bind_rows(asr_lt_m)

# calculate Adult Sex Ratio based on LT 
asr_lt <- left_join(asr_lt_f,asr_lt_m, by = c('id_hmd','year')) %>%
        transmute(id_hmd, year, asr_lt = lx_m / lx_f * 100)



# Get HMD births to calculate Sex Ratio at Birth
# Further I use them to weidght LT-based ASR
births <- list()
for (i in 1: length(country)) {
        cnt <- country[i]
        births[[paste0(cnt,'_birth')]] <- readHMDweb(cnt,"Births",ik_user_hmd,ik_pass_hmd)
        
        print(paste(i,'out of',length(country)))
}

bsr <- list()
for (i in 1:length(births)) {
        di <- births[[i]] %>% 
                select(Year,Female,Male) %>% 
                # we take broader interval here to get a more reliable estimate
                filter(Year %in% 1990:2010) %>% 
                transmute(id_hmd=gsub('_birth','',names(births)[i]),
                          year=Year, bsr = Male/Female*100)
        bsr[[i]] <- di
}
bsr <- bind_rows(bsr)

bsr_avg <- bsr %>% select(-year) %>% 
        group_by(id_hmd) %>%
        summarise_each(funs(bsr_avg = mean, bsr_sd = sd)) %>%
        ungroup() 

# use BSR as weights to calculate ASR_LT
asr_lt <- asr_lt %>% left_join(bsr_avg,'id_hmd') %>%
        transmute(id_hmd, year, asr_lt = asr_lt*bsr_avg/100)


# just im case, let's seve the raw HMD data, so that we don't have to download
# them again if needed
ifelse(!dir.exists('data'),dir.create('data'),paste("Directory already exists"))
save(exposures,lt_f,lt_m,births, file = 'data/HMD_all_countries.RData')



### MERGE ALL THE DATA SETS ###

# supplementary list of id codes to interconnect Eurostat and HMD data
ids <- structure(list(id = c("AT", "BE", "BG", "CH", "CZ", "DE", 
                                   "DK", "EE", "EL", "ES", "FI", "FR", "HU", "IE", "IS", "IT", "LT", 
                                   "LV", "NL", "NO", "PL", "PT", "SE", "SI", "SK", "UKC-L", "UKM", 
                                   "UKN"), 
                      id_hmd = c("AUT", "BEL", "BGR", "CHE", "CZE", "DEUTNP", 
                              "DNK", "EST", "GRC", "ESP", "FIN", "FRATNP", "HUN", "IRL", "ISL",
                              "ITA", "LTU", "LVA", "NLD", "NOR", "POL", "PRT", "SWE", "SVK", 
                              "SVK", "GBRTENW", "GBR_SCO", "GBR_NIR")), 
                 class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -28L), 
                 .Names = c("id","id_hmd"))


# join all Eurostat data together
df_es <- inner_join(ids,homicide,by= c('id' = 'geo')) %>%
        inner_join(poverty_avg, by= c('id' = 'geo')) %>%
        transmute(id,id_hmd,year=time,homicide=values,pov)


# join all HMD data together
df_hmd <- pop_tot %>% 
        inner_join(asr, by = c('id_hmd','year')) %>%
        inner_join(asr_lt, by = c('id_hmd','year'))


# join both data sest together
df <- left_join(df_es, df_hmd, by = c('id_hmd','year'))


# calculate homicide rates per 100K
df <- df %>% transmute(id, year, asr, asr_lt, hr = homicide/pop*10e5, pov)


### a trick with the UK for mapping - calculate UK averages
# calculate population weights for the UK parts
uk_popw <- pop_tot %>% filter(id_hmd%in%c('GBRTENW','GBR_SCO','GBR_NIR')) %>% 
        group_by(id_hmd) %>% summarise_all(funs(mean)) %>% ungroup() %>%
        transmute(id=c('UKN','UKM','UKC-L'),w=pop/sum(pop)) %>% arrange(id)

uk <- left_join(uk_popw, df, 'id')  %>%
        transmute(id, year, asr = asr*w, asr_lt = asr_lt*w, hr = hr*w, pov = pov*w) %>%
        group_by(year) %>%
        summarise_at(c('asr','asr_lt','hr','pov'),funs(sum)) %>% 
        transmute(id='UK',asr,asr_lt,hr,pov)

df <- bind_rows(df,uk)


# calvulate averages across years for all countries
df_avg <- df %>% group_by(id) %>% select(-year) %>% summarise_all(funs(mean)) %>% arrange(desc(hr)) 


# clean up Global Environment
rm(di,exposures,lt_f,lt_m,asr_lt_f,asr_lt_m,births,
   country,cnt,i,df_es,df_hmd,homicide,uk,uk_popw)


################################################################################
# -2- GET GEO DATA

# download and unzip the shapefile 
# Here I use a shapefile of all countries of the world, provided by Eurostat
# reference year is 2010

# geodata will be stored in a directory "geodata"
ifelse(!dir.exists('geodata'),dir.create('geodata'),paste("Directory already exists"))
dir.create('geodata')

f <- tempfile()
download.file("http://ec.europa.eu/eurostat/cache/GISCO/geodatafiles/CNTR_2010_20M_SH.zip", destfile = f)
unzip(f, exdir = "geodata/.")
WORLD <- read_shape("geodata/CNTR_2010_20M_SH/CNTR_2010_20M_SH/Data/CNTR_RG_20M_2010.shp")

# colnames to lower case
names(WORLD@data) <- tolower(names(WORLD@data))

# filter only Europe and the neighbouring countries
eu_subset <- c("AT", "BE", "BG", "CH", "CZ", "DE", "DK", 
               "EE", "EL", "ES", "FI", "FR", "HU", "IE", "IS", "IT", "LT", "LV", 
               "NL", "NO", "PL", "PT", "SE", "SI", "SK", "UK", "IM", "FO", "GI", 
               "LU", "LI", "AD", "MC", "MT", "VA", "SM", "HR", "BA", "ME", "MK", 
               "AL", "RS", "RO", "MD", "UA", "BY", "RU", "TR", "CY", "EG", "LY", 
               "TN", "DZ", "MA", "GG", "JE")

EU <- WORLD[WORLD$cntr_id %in% eu_subset,]

plot(EU)

# reproject the shapefile to a pretty projection for mapping Europe
EU_prj <- spTransform(EU, CRS('+init=EPSG:3035 +ellps=GRS80'))

plot(EU_prj) # nice!

# fortify shapefiles - to further plot them usin ggplot2
gd_eu <- fortify(EU_prj, region = 'cntr_id')


# borders between countries
# select only those, who match in two databases
# as long as we have no geodata for England-Wales, Scotland, and Northern Ireeland,
# let's remove them and use the averages to map UK
match <- c(ids$id[1:25],'UK')

EU_26 <- EU_prj[EU_prj$cntr_id %in% match,]

borders <- gDifference(
        as(EU_26,"SpatialLines"),
        as(gUnaryUnion(EU_26),"SpatialLines"),
        byid=TRUE)
temp <- data.frame(len = sapply(1:length(borders), function(i) gLength(borders[i, ])))
rownames(temp) <- sapply(1:length(borders), function(i) borders@lines[[i]]@ID)

BORD <- SpatialLinesDataFrame(borders, data = temp)

plot(BORD) # not perfect, but sort of ok

# again, forify
gd_borders_26 <- fortify(BORD)

# let's save geodata, just in case
save(gd_eu, gd_borders_26, file = 'geodata/eu_geodata.RData')

# clean up Global Environment
rm(BORD,borders,EU, EU_26,EU_prj,eu_subset,f,match,temp,WORLD)


################################################################################
# -3- EXPLORATORY DATA ANALYSIS AND MAPS

# generate pretty colors to use in maps
brbg <- RColorBrewer::brewer.pal(11,'BrBG')

# author's info to sign the maps
auth <- data.frame(x=2600000,y=1400000,lab='Ilya Kashnitsky (ilya.kashnitsky@gmail.com), 2016')

# now, let's create a blank map to be used later
basemap_eu <- ggplot()+
        geom_map(map = gd_eu, data = gd_eu, aes(map_id = id),fill='grey90',color='grey90')+
        coord_equal(ylim=c(1350000,5450000), xlim=c(2500000, 6600000))+
        guides(fill = guide_colorbar(barwidth = 1.5, barheight = 20))+
        
        theme_map()+
        theme(panel.border=element_rect(color = 'black',size=.5,fill = NA),
              legend.position = c(1, 1),
              legend.justification = c(1, 1),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.title = element_text(size=15),
              legend.text = element_text(size=15))+
        scale_x_continuous(expand=c(0,0)) +
        scale_y_continuous(expand=c(0,0)) +
        labs(x = NULL, y = NULL)+
        
        # if you don't have "DejaVu Sans Condensed" font, download here 
        # http://dejavu-fonts.org/wiki/Main_Page
        # then, install it, and register with R using `extrafont` package
        geom_text(data=auth, aes(x,y,label=lab), size = 5, color='grey25', hjust=0,vjust=0,
                  family = "DejaVu Sans Condensed")+
        theme(text=element_text(family = "DejaVu Sans Condensed"))


# create a directory for figures
ifelse(!dir.exists('figures'),dir.create('figures'),paste("Directory already exists"))
dir.create('figures')



### MAPS ###

# map Homicide Rate
map_hr <- basemap_eu + 
        geom_map(map=gd_eu, data=df_avg, aes(map_id=id, fill=hr))+
        geom_path(data = gd_borders_26, aes(x=long, y=lat, group=group), 
                  color = 'grey25', size = .5, linetype = 3)+
        scale_fill_gradientn('Homicide\nper 100K\npopulation\n',colors = brbg[6:11])+
        theme(legend.title = element_text(size=20),
              legend.text = element_text(size=20))

ggsave('figures/map_homicide_all.png', map_hr, width = 8, height = 8, type="cairo-png")



# map Homicide Rate without Baltic countries - as they are evident outliers
map_hr_minus_baltic <- basemap_eu + 
        geom_map(map=gd_eu, data=df_avg %>% filter(!id%in%c('EE','LT','LV')), aes(map_id=id, fill=hr))+
        geom_path(data = gd_borders_26 %>% filter(!id%in%c('EE','LT','LV')), aes(x=long, y=lat, group=group), 
                  color = 'grey25', size = .5, linetype = 3)+
        scale_fill_gradientn('Homicide\nper 100K\npopulation\n',colors = brbg[6:1])+
        theme(legend.title = element_text(size=20),
              legend.text = element_text(size=20))

ggsave('figures/map_homicide_minus_Baltic.png', map_hr_minus_baltic, width = 8, height = 8, type="cairo-png")

# save this two maps together
maps <- plot_grid(map_hr,map_hr_minus_baltic, labels = LETTERS[1:2], label_size = 20)
ggsave('figures/maps_homicide.png', maps, width = 15, height = 8.5, type="cairo-png")


# map Adult Sex Ratio
map_asr <- basemap_eu + 
        geom_map(map=gd_eu, data=df_avg, aes(map_id=id, fill=asr))+
        geom_path(data = gd_borders_26, aes(x=long, y=lat, group=group), 
                  color = 'grey25', size = .5, linetype = 3)+
        scale_fill_gradient2('Sex Ratio\nat 15-49,\nmales\nper 100\nfemales\n',
                             low = brbg[1:5], mid = brbg[6], high = brbg[6:11], midpoint = 100)+
        theme(legend.title = element_text(size=20),
              legend.text = element_text(size=20))

ggsave('figures/map_adult_sex_ratio.png', map_asr, width = 8, height = 8, type="cairo-png")



map_pov <- basemap_eu + 
        geom_map(map=gd_eu, data=df_avg, aes(map_id=id, fill=pov))+
        geom_path(data = gd_borders_26, aes(x=long, y=lat, group=group), 
                  color = 'grey25', size = .5, linetype = 3)+
        scale_fill_gradientn('Poverty\nshare,\nper cents\n',colors = brbg[6:11])+
        theme(legend.title = element_text(size=20),
              legend.text = element_text(size=20))

ggsave('figures/map_poverty_percentage.png', map_pov, width = 8, height = 8, type="cairo-png")



map_asr_lt <- basemap_eu + 
        geom_map(map=gd_eu, data=df_avg, aes(map_id=id, fill=asr_lt))+
        geom_path(data = gd_borders_26, aes(x=long, y=lat, group=group), 
                  color = 'grey25', size = .5, linetype = 3)+
        scale_fill_gradientn('Sex Ratio\nat 15-49,\nmales\nper 100\nfemales\nLT based\n',colors = brbg[6:11])+
        theme(legend.title = element_text(size=20),
              legend.text = element_text(size=20))

ggsave('figures/map_adult_sex_ratio_LT.png', map_asr_lt, width = 8, height = 8, type="cairo-png")







### FIGURES ###

# viz the correlation - ASR
gg_cor <- ggplot(df %>% filter(!id%in%c('EE','LT','LV','UK')), aes(asr,hr))+
        geom_point()+
        stat_smooth(method = 'lm', se=F)+
        xlab('Sex ratio at ages 15-49')+
        ylab('Homicide rate per 100K population')+
        facet_wrap(~year,ncol=4)+
        theme_few(base_family = "DejaVu Sans Condensed", base_size = 15)

ggsave('figures/gg_cor_pop_models.png', gg_cor, width = 12, height = 6.75, type="cairo-png")


# viz the correlation - ASR
gg_cor_lt <- ggplot(df %>% filter(!id%in%c('EE','LT','LV','UK')), aes(asr_lt,hr))+
        geom_point()+
        stat_smooth(method = 'lm', se=F)+
        xlab('Sex ratio at ages 15-49, Life Tables based')+
        ylab('Homicide rate per 100K population')+
        facet_wrap(~year,ncol=4)+
        theme_few(base_family = "DejaVu Sans Condensed", base_size = 15)

ggsave('figures/gg_cor_LT_models.png', gg_cor_lt, width = 12, height = 6.75, type="cairo-png")




################################################################################
# -4- REGRESSION MODELS

# a directory for models
ifelse(!dir.exists('models'),dir.create('models'),paste("Directory already exists"))
dir.create('models')


# run models for population based ASR (omit Baltic countries)

df_23 <- df %>% filter(!id%in%c('EE','LT','LV'))

mod1 <- lm(data=df_23 %>% mutate(year=factor(year)), hr~asr+year)
summary(mod1)

# plus poverty rate
mod2 <- lm(data=df_23 %>% mutate(year=factor(year)), hr~asr+year+pov)
summary(mod2)

# save the models' summaries
htmlreg(list(mod1, mod2), file = 'models/models_pop.html',single.row = T)




# run models for Life Tables based ASR (omit Baltic countries)

mod1_lt <- lm(data=df_23 %>% mutate(year=factor(year)), hr~asr_lt+year)
summary(mod1_lt)

# plus poverty rate
mod2_lt <- lm(data=df_23 %>% mutate(year=factor(year)), hr~asr_lt+pov+year)
summary(mod2_lt)

htmlreg(list(mod1_lt, mod2_lt), file = 'models/models_lt.html',single.row = T)




################################################################################
# -4- ADDITIONAL FIGURE
# https://gist.github.com/ikashnitsky/a578eaef6b122aa2aa2e3469fd2dcbe7

# plot Sex Ratio VS age as lines for all countries

load('data/HMD_all_countries.RData')

sr_age <- list()

for (i in 1:length(exposures)) {
        di <- exposures[[i]]
        sr_agei <- di %>% select(Year,Age,Female,Male) %>% 
                filter(Year %in% 2012) %>%
                select(-Year) %>%
                transmute(country = gsub('_exp','',names(exposures)[i]),
                          age = Age, sr_age = Male / Female * 100)
        sr_age[[i]] <- sr_agei
}
sr_age <- bind_rows(sr_age)

# remove optional populations
sr_age <- sr_age %>% filter(!country %in% c("FRACNP","DEUTE","DEUTW","GBRCENW","GBR_NP"))

# summarize all ages older than 90 (to jerky)
sr_age_90 <- sr_age %>% filter(age %in% 90:110) %>% 
        group_by(country) %>% summarise(sr_age = mean(sr_age, na.rm = T)) %>%
        ungroup() %>% transmute(country, age=90, sr_age)

sr_age_0090 <- bind_rows(sr_age %>% filter(!age %in% 90:110), sr_age_90)


# finaly - plot
gg_sr_age <- ggplot(sr_age_0090, aes(age, sr_age, color = country, group = country))+
        geom_hline(yintercept = 100, color = 'grey50', size = 1)+
        geom_line(size = 1)+
        scale_y_continuous(limits = c(0,120), expand = c(0,0), breaks = seq(0,120,20))+
        scale_x_continuous(limits = c(0,90), expand = c(0,0), breaks = seq(0,80,20))+
        xlab('Age')+
        ylab('Sex ratio, males per 100 females')+
        facet_wrap(~country,ncol=6)+
        theme_few(base_family = "DejaVu Sans Condensed", base_size = 15)+
        theme(legend.position='none')

ggsave('figures/sr_lines.png', gg_sr_age, width = 8, height = 12, type="cairo-png")
                