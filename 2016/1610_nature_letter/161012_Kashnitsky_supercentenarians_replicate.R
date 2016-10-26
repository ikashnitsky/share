################################################################################
#
# Supercentenarians 12-10-2016
# Check the validity of results and comment on 
# Dong, X., Milholland, B. & Vijg, J. Evidence for a limit to human lifespan. Nature (2016). 
# doi.org/10.1038/nature19793
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#
################################################################################

# In order to get the script working, set the working directory to the one
# in which you have just unziped the reproducibility archive
setwd('')

# The code is written and tested on a PC-win7-x64
# R version 3.3.1

# load required packages
# if needed, first install the listed below packages
require(dplyr) # version 0.5.0
require(readr) # version 1.0.0
require(tidyr) # version 0.6.0
require(broom) # version 0.4.1
require(lubridate) # version 1.6.0

require(ggplot2) # version 2.1.0
require(ggthemes) # version 3.2.0
library(extrafont) # version 0.17
library(RColorBrewer) # version 1.1-2

require(HMDHFDplus) # version 1.1.8


# The dtabase on supercentenarians is downloaded manually from 
# http://www.supercentenarians.org/DataBase
# each country is named as "[3-digit ISO code].csv" and stored in a directory called "idl_database"
# NOTE: the uathors of IDL do not allow to distribute the data freely

# TO CITE IDL
# Maier, H. et al. Supercentenarians (Springer, 2010). doi:10.1007/978-3-642-11520-2_2
# Retrieved from http://link.springer.com/chapter/10.1007/978-3-642-11520-2_2


# countries except for the USA, where only the years of birth and death are available
countries <- c('aus','bel','can','che','deu','dnk','esp','fin','fra','gbr','ita','jpn','nor','swe')

# read all the data into one dataframe
df_raw <- list()
for (i in 1:length(countries)){
        df_raw[[i]] <- read_csv2(file = paste0('idl_database/',countries[i],'.csv'),
                             col_types = cols(sex='c', date_b='c', date_d='c'),
                             col_names = c('id','age_y','age_dlb','age_d','sex','country_b','country_d','date_b','date_d','valid'),
                             skip = 1)
}
df_raw <- bind_rows(df_raw)

df <- df_raw %>% mutate(age_d_y = age_d / 365.25,
                        date_b = dmy(date_b),
                        date_d = dmy(date_d),
                        year_d = year(date_d))

# read USA data separately
usa <- read_csv2(file = paste0('idl_database/usa.csv'),
                 col_types = cols(sex='c', date_b='c', date_d='c'),
                 col_names = c('id','age_y','age_dlb','age_d','sex','country_b','country_d','date_b','date_d','valid'),
                 skip = 1) %>%
        # I set the unknown dates of birth/seath to the 1st of July
        mutate(age_d_y = age_d / 365.25,
               date_b = dmy(paste0('01-07-',date_b)),
               date_d = dmy(paste0('01-07-',date_d)),
               year_d = year(date_d))

# now merge back USA to the rest of the data set
df <- bind_rows(df,usa)


# Select the oldest died persons for each calendar year
top1 <- df %>% group_by(year_d) %>% filter(age_d==max(age_d)) %>% ungroup() %>% arrange(year_d)


# models to replicate Dong, Milholland, and Vijg
trend1 <- lm(data = top1 %>% filter(year_d < 1995), age_d_y ~ year_d)
trend2 <- lm(data = top1 %>% filter(year_d >= 1995), age_d_y ~ year_d)

summary(trend1)
summary(trend2)

t1_broom <- augment(trend1)
t2_broom <- augment(trend2) # to check Cook's distance
# second step to check Cook's distance for years 1995, 2006, and 2007
t2_2_broom <- augment(lm(data = top1 %>% filter(year_d %in% 1995:2007, !year_d%in%c(1997,1999)), age_d_y ~ year_d))


trend3 <- lm(data = top1 %>% filter(year_d %in% 1995:2005, !year_d%in%c(1995,1997,1999)), age_d_y ~ year_d)
trend4 <- lm(data = top1 %>% filter(!year_d%in%c(1995,1997,1999,2006,2007)), age_d_y ~ year_d)
trend5 <- lm(data = top1 %>% filter(year_d %in% 1981:1991), age_d_y ~ year_d)

summary(trend3)
summary(trend4)
summary(trend5)


# visualize
brbg <- brewer.pal(11,'BrBG')

gg1 <- ggplot()+
        geom_point(data = top1 %>% filter(year_d < 1995), 
                   aes(year_d,age_d_y), color=brbg[8], size = 2.5)+
        stat_smooth(method = 'lm', se=F, 
                    data = top1 %>% filter(year_d < 1995), 
                    aes(year_d,age_d_y), color=brbg[8])+
        
        geom_point(data = top1 %>% filter(year_d >= 1995), 
                   aes(year_d,age_d_y), color=brbg[3], size = 2.5)+
        stat_smooth(method = 'lm', se=F, 
                    data = top1 %>% filter(year_d >= 1995), 
                    aes(year_d,age_d_y), color=brbg[3])+
        
        geom_point(data = top1 %>% filter(year_d %in% c(1995,1997,1999,2006,2007)), 
                   aes(year_d,age_d_y), color='red', size = 5, pch=13)+
        
        stat_smooth(method = 'lm', se=F, 
                    data = top1 %>% filter(year_d >= 1995, !year_d %in% c(1995,1997,1999,2006,2007)), 
                    aes(year_d,age_d_y), color='red')+
        stat_smooth(method = 'lm', se=F, 
                    data = top1 %>% filter(!year_d %in% c(1995,1997,1999,2006,2007)), 
                    aes(year_d,age_d_y), color='blue')+
        
        geom_point(data = top1 %>% filter(year_d %in% c(1981:1991)), 
                   aes(year_d,age_d_y), color='green', size = 5, pch=1)+
        stat_smooth(method = 'lm', se=F, 
                    data = top1 %>% filter(year_d %in% c(1981:1991)), 
                    aes(year_d,age_d_y), color='green')+
        
        scale_y_continuous(limits = c(108,124),expand = c(0,0), 
                           breaks = seq(108,124,2), labels = seq(108,124,2))+
        scale_x_continuous(limits = c(1960,2010),#expand = c(0,0), 
                           breaks = seq(1960,2010,10), labels = seq(1960,2010,10))+
        
        xlab('Year of death')+
        ylab('Yearly maximum reported age at death (years)')+
        
        theme_few(base_family = 'Arial', base_size = 20)+ 
        
        # create legend
        annotate('segment',x=1961,xend=1964.5,y=seq(123,119,-1),yend=seq(123,119,-1),
                 color = c(brbg[c(8,3)],'red','blue','green'))+
        annotate('text',x=1965,y=seq(123,119,-1), size = 6, hjust=0, vjust=.5,
                 color = c(brbg[c(8,3)],'red','blue','green'),
                 label = c('Trend 1962-1994','Trend 1995-2007','Trend 1995-2007 w/o outliers',
                           'Trend 1962-2007 w/o outliers','Trend 1981-1991'))+
        
        # label outliers
        annotate('text',x=1990,y=110, size = 5, hjust=0, vjust=1, color='red', label='Outliers')+
        annotate('segment',x=1994,y=110.15,xend=c(1994.9,2005,2006),yend=c(112.3,112,111.55),
                 color='red', arrow=arrow(length=unit(0.15,"cm"),type='closed'),size=.3)+
        
        annotate('text',x=2003,y=121, size = 5, hjust=0, vjust=.5, color='red', label='Outliers')+
        annotate('segment',x=2002.75,y=121,xend=c(1998,2000),yend=c(122.3,119.5),
                 color='red', arrow=arrow(length=unit(0.15,"cm"),type='closed'),size=.3)+
        
        # numbers for trend lines
        annotate('text',x=1960,y=seq(123,119,-1),size = 6, hjust=.5, vjust=.5,
                 color = c(brbg[c(8,3)],'red','blue','green'), label=paste(1:5))+
        annotate('text',x=c(1962,1997,2006,1962,1978),y=c(109.7,117.5,114.5,111.2,114),
                 size = 6, hjust=0, vjust=1,
                 color = c(brbg[c(8,3)],'red','blue','green'), label=paste(1:5))


# create a sub-directory for the outputs
ifelse(!dir.exists('out'),dir.create('out'),paste("Directory already exists"))

# save the plot
ggsave('out/fig1_trends.png', gg1, width = 7, height = 7, type="cairo-png")




################################################################################
# CHeck the cohort effect 1870-s VS 1880-s USA

# read HMD data directly into R
# please note! the arguments "ik_user_hmd" and "ik_pass_hmd" are my login credidantials
# at the website of Human Mortality Database, which are stored locally at my computer. 
# In order to access the data, you need to create an account at
# http://www.mortality.org/
# and provide your own credidantials to the `readHMDweb()` function
usa <- readHMDweb(CNTRY = 'USA', item = 'cMx_1x1', username = ik_user_hmd, password = ik_pass_hmd)

usa_check <- usa %>% transmute(year=Year, age=Age, mx=Female) %>%
        filter(year %in% 1870:1889) %>%
        mutate(period = cut_number(year, 2)) %>%
        group_by(age,period) %>%
        summarise(mx = mean(mx)) %>%
        ungroup()
levels(usa_check$period) <- c('1870-s', '1880-s')

gg_usa <- ggplot(usa_check, aes(age,mx))+
        geom_point(aes(color=period),size=2)+
        geom_path(aes(group=period,color=period))+
        scale_x_continuous(limits = c(89.5,110.5),expand = c(0,0))+
        scale_y_continuous(limits = c(0,1),expand = c(0,0))+
        scale_color_manual('Birth cohorts',values = brbg[c(8,2)])+
        xlab('Age')+
        ylab('Cohort mortality rate')+
        theme_few(base_family = 'Arial', base_size = 20)+
        theme(legend.position = 'none')+
        annotate('text', x=c(100,105), y=c(.5,.35), hjust=0,vjust=1, size=8,
                 label = c('1880-s','1870-s'), color = brbg[c(2,8)])+
        annotate('text',x=90, y=.97, hjust=0,vjust=1, size=10, label='Unites States')

ggsave('out/fig2_usa_cohorts.png', gg_usa, width = 7, height = 7, type="cairo-png")


################################################################################
# The  additional check of the statistical significance
# of the difference between cohort born in 1870-s and 1880-s
usa_sign <- usa %>% transmute(year=Year, AGE=Age, MX=Female) %>%
        filter(year %in% 1870:1889, AGE %in% 100:110) %>%
        mutate(COH = cut_number(year, 2))
levels(usa_sign$COH) <- c('1870-s', '1880-s')

# glance at the rates
ggplot(usa_sign, aes(AGE, MX, group=COH,color=COH))+
        geom_jitter()+
        stat_smooth(method = 'lm', se = F)

# linear model to chech the significance
summary(lm(data = usa_sign, formula = MX ~ AGE + COH))

# RESULT: the dummy for birth decade (COH) is highly significant






################################################################################
# CHeck the cohort effect 1870-s VS 1880-s JPN

jpn <- readHMDweb(CNTRY = 'JPN', item = 'cMx_1x1', username = ik_user_hmd, password = ik_pass_hmd)

jpn_check <- jpn %>% transmute(year=Year, age=Age, mx=Female) %>%
        filter(year %in% 1870:1889) %>%
        mutate(period = cut_number(year, 2)) %>%
        group_by(age,period) %>%
        summarise(mx = mean(mx)) %>%
        ungroup()
levels(jpn_check$period) <- c('1870-s', '1880-s')

gg_jpn <- ggplot(jpn_check, aes(age,mx))+
        geom_point(aes(color=period),size=2)+
        geom_path(aes(group=period,color=period))+
        scale_x_continuous(limits = c(89.5,110.5),expand = c(0,0))+
        scale_y_continuous(limits = c(0,1),expand = c(0,0))+
        scale_color_manual('Birth cohorts',values = brbg[c(8,2)])+
        xlab('Age')+
        ylab('Cohort mortality rate')+
        theme_few(base_family = 'Arial', base_size = 20)+
        theme(legend.position = 'none')+
        annotate('text', x=c(105,98), y=c(.45,.65), hjust=0,vjust=1, size=8,
                 label = c('1880-s','1870-s'), color = brbg[c(2,8)])+
        annotate('text',x=90, y=.97, hjust=0,vjust=1, size=10, label='Japan')

ggsave('out/fig_sup.jpn.cohorts.png', gg_jpn, width = 7, height = 7, type="cairo-png")





################################################################################
# CHeck the cohort effect 1870-s VS 1880-s FRA

fra <- readHMDweb(CNTRY = 'FRATNP', item = 'cMx_1x1', username = ik_user_hmd, password = ik_pass_hmd)

fra_check <- fra %>% transmute(year=Year, age=Age, mx=Female) %>%
        filter(year %in% 1870:1889) %>%
        mutate(period = cut_number(year, 2)) %>%
        group_by(age,period) %>%
        summarise(mx = mean(mx)) %>%
        ungroup()
levels(fra_check$period) <- c('1870-s', '1880-s')

gg_fra <- ggplot(fra_check, aes(age,mx))+
        geom_point(aes(color=period),size=2)+
        geom_path(aes(group=period,color=period))+
        scale_x_continuous(limits = c(89.5,110.5),expand = c(0,0))+
        scale_y_continuous(limits = c(0,1),expand = c(0,0))+
        scale_color_manual('Birth cohorts',values = brbg[c(8,2)])+
        xlab('Age')+
        ylab('Cohort mortality rate')+
        theme_few(base_family = 'Arial', base_size = 20)+
        theme(legend.position = 'none')+
        annotate('text', x=c(105,98), y=c(.45,.65), hjust=0,vjust=1, size=8,
                 label = c('1880-s','1870-s'), color = brbg[c(2,8)])+
        annotate('text',x=90, y=.97, hjust=0,vjust=1, size=10, label='France')

ggsave('out/fig_sup.fra.cohorts.png', gg_fra, width = 7, height = 7, type="cairo-png")





################################################################################
# CHeck the cohort effect 1870-s VS 1880-s SWE

swe <- readHMDweb(CNTRY = 'SWE', item = 'cMx_1x1', username = ik_user_hmd, password = ik_pass_hmd)

swe_check <- swe %>% transmute(year=Year, age=Age, mx=Female) %>%
        filter(year %in% 1870:1889) %>%
        mutate(period = cut_number(year, 2)) %>%
        group_by(age,period) %>%
        summarise(mx = mean(mx)) %>%
        ungroup()
levels(swe_check$period) <- c('1870-s', '1880-s')

gg_swe <- ggplot(swe_check, aes(age,mx))+
        geom_point(aes(color=period),size=2)+
        geom_path(aes(group=period,color=period))+
        scale_x_continuous(limits = c(89.5,110.5),expand = c(0,0))+
        scale_y_continuous(limits = c(0,1),expand = c(0,0))+
        scale_color_manual('Birth cohorts',values = brbg[c(8,2)])+
        xlab('Age')+
        ylab('Cohort mortality rate')+
        theme_few(base_family = 'Arial', base_size = 20)+
        theme(legend.position = 'none')+
        annotate('text', x=c(105,98), y=c(.45,.65), hjust=0,vjust=1, size=8,
                 label = c('1880-s','1870-s'), color = brbg[c(2,8)])+
        annotate('text',x=90, y=.97, hjust=0,vjust=1, size=10, label='Sweden')

ggsave('out/fig_sup.swe.cohorts.png', gg_swe, width = 7, height = 7, type="cairo-png")






################################################################################
# CHeck the cohort effect 1870-s VS 1880-s GBR

gbr <- readHMDweb(CNTRY = 'GBR_NP', item = 'cMx_1x1', username = ik_user_hmd, password = ik_pass_hmd)

gbr_check <- gbr %>% transmute(year=Year, age=Age, mx=Female) %>%
        filter(year %in% 1870:1889) %>%
        mutate(period = cut_number(year, 2)) %>%
        group_by(age,period) %>%
        summarise(mx = mean(mx)) %>%
        ungroup()
levels(gbr_check$period) <- c('1870-s', '1880-s')

gg_gbr <- ggplot(gbr_check, aes(age,mx))+
        geom_point(aes(color=period),size=2)+
        geom_path(aes(group=period,color=period))+
        scale_x_continuous(limits = c(89.5,110.5),expand = c(0,0))+
        scale_y_continuous(limits = c(0,1),expand = c(0,0))+
        scale_color_manual('Birth cohorts',values = brbg[c(8,2)])+
        xlab('Age')+
        ylab('Cohort mortality rate')+
        theme_few(base_family = 'Arial', base_size = 20)+
        theme(legend.position = 'none')+
        annotate('text', x=c(105,98), y=c(.45,.65), hjust=0,vjust=1, size=8,
                 label = c('1880-s','1870-s'), color = brbg[c(2,8)])+
        annotate('text',x=90, y=.97, hjust=0,vjust=1, size=10, label='Great Britain')

ggsave('out/fig_sup.gbr.cohorts.png', gg_gbr, width = 7, height = 7, type="cairo-png")

# That's it .)