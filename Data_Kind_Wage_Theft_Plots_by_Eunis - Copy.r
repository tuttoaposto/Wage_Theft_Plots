#Load wage theft national data
library(data.table)
whd <- as.data.frame(fread("whd_whisard.naicHumanReadableLevels.csv", stringsAsFactors = FALSE,  na.strings = ""))
str(whd)

#Load data dictionary
dd <- fread("whd_dd.csv", na.strings = "")
head(dd)
col <- dd$Column_Name[!is.na(dd$Proposed_User_Friendly_Column_Name)| 
                        dd$Column_Name %in% c("findings_start_date", "findings_end_date")]
col <- c(col, grep("naic", names(whd), value = TRUE)) #also need the naic description variables
col

#Subset whd data to just columns of primary interest
whd <- whd[c(col)]
names(whd) #34 columns

summary(whd)

#Get date columns in date values
library(lubridate)
whd$inv_start_date <- ymd(whd$findings_start_date)
whd$inv_end_date <- ymd(whd$findings_end_date)

#Check range of dates
nrow(whd[is.na(whd$inv_start_date), ]) #0 cases without a start date
nrow(whd[is.na(whd$inv_end_date), ]) #0 missing end date

nrow(whd[is.na(whd$findings_start_date), ]) #0 missing start date
nrow(whd[is.na(whd$findings_end_date), ]) #0 missing end date

x <- whd[order(whd$inv_start_date), c("findings_start_date", "inv_start_date")]
head(x); tail(x)  #0200, 1900, 1909-09-15", then 1950s??

x <- whd[order(whd$inv_end_date), c("findings_end_date", "inv_end_date")]
head(x); tail(x)  #"1988 - 2015"

whd$yr_strt <- year(whd$inv_start_date)
whd$yr_end <- year(whd$inv_end_date)
x <- whd[order(whd$inv_start_date), c("findings_start_date", "findings_end_date","inv_start_date", "yr_strt", "yr_end")]
head(x); tail(x)  #"200 - 2016"

x <- whd[order(whd$inv_end_date), c("findings_start_date", "findings_end_date", "inv_end_date", "yr_strt", "yr_end")]
head(x); tail(x)  #"1985 -2024"

#Check distribution of case years
table(whd$yr_strt)
y <- data.frame(yr_strt_cat = ifelse(whd$yr_strt <= 1985, "before 1985", as.character(whd$yr_strt)), yr_strt = whd$yr_strt)

#Just to check year categories
#library(sqldf)
#sqldf("select distinct yr_strt_cat, yr_strt from y order by yr_strt_cat desc, yr_strt")

#Thought I would just plot full range of years including <1985 aggregated, but decided not to

#Not too many data < year 1990. So truncate data at 1990
#Let's plot something
library(ggplot2)
ggplot(data = whd[whd$yr_strt >=1990, ], aes(x = yr_strt)) +
  geom_bar() +
  ggtitle("# Wage Theft Cases over Investigation Years since 1990")+
  xlab('Yaer of Investigation')

#What is the avg # cases during 2003 - 2013?
whd[whd$yr_strt >=2003 & whd$yr_strt <=2013, ] %>% 
        group_by(yr_strt) %>%
        summarise(N = n()) %>%
        summarise(mean = mean(N))

    

#Statistics by sector of industries
library(dplyr)

Cnt <- whd %>% 
    group_by(naic_cd_lvl2, naic_description_lvl2) %>% 
    summarise(N = n()) %>% 
    #arrange(desc(N))  #58137 missing industry description ??
    arrange(naic_description_lvl2)

#Why is there a 'T' at the end of the description? Get rid of it
whd$naic_description_lvl2 <- with(whd, substr(naic_description_lvl2, 1, nchar(naic_description_lvl2) - 1))


#Fill in description for some naic lvl2 codes
whd[whd$naic_cd_lvl2 %in% c("44", "45"), "naic_description_lvl2"] <- 'Retail Trade'
whd[whd$naic_cd_lvl2 %in% c("48", "49"), "naic_description_lvl2"] <- 'Transportation and Warehousing'
whd[whd$naic_cd_lvl2 %in% c("31", "32", "33"), "naic_description_lvl2"] <- 'Manufacturing'


### 1. Cases per sector
cases <- whd %>% 
        group_by(naic_description_lvl2) %>% 
        summarise(N = n()) %>% 
        filter(naic_description_lvl2 != '') %>%
        arrange(desc(N))
cases

#Sort bars by descending stat
Sector_sort1 <- factor(cases$naic_description_lvl2, 
                      levels=cases$naic_description_lvl2)

levels(Sector_sort1) <- gsub(" ", "\n", levels(Sector_sort1))

#Top 10 Sectors with the highest incidence
ggplot(data = cases[1:10, ], aes(x = Sector_sort1[1:10], y = N)) +
  geom_bar(stat = "identity", width = 0.3) + 
  ggtitle("Top 10 Sectors with The Highest Incidence") +
  theme(axis.text.x = element_text(hjust = 1, size = 12)) + #angle = 60
  xlab("Sector")

### 2. Median total back wages by industry sectors
tbw <- whd %>% 
    group_by(naic_description_lvl2) %>%  
    summarise(med_ttl_bw = median(bw_atp_amt)) %>%
    arrange(desc(med_ttl_bw))
tbw

#Create a factor to sort bars by descending stat
Sector_sort2 <- factor(tbw$naic_description_lvl2, 
                      levels=tbw$naic_description_lvl2)

levels(Sector_sort2) <- gsub(" ", "\n", levels(Sector_sort2))
  
#Top 10 Sectors with The Highest Median back wages
ggplot(data = tbw[1:10, ], aes(x = Sector_sort2[1:10], y = med_ttl_bw)) +
  geom_bar(stat = "identity", width = 0.3) + 
  ggtitle("Top 10 Sectors with The Highest Median Total Back Wages") +
  theme(axis.text.x = element_text(hjust = 1)) + #angle = 60
  ylab("Median Total Back Wages ($)") +
  xlab("Sector")

### 3. Total minimum wage back wages in millions
mbw <- whd %>% 
  group_by(naic_description_lvl2) %>%  
  summarise(ttl_min_bw = sum(flsa_mw_bw_atp_amt)/1000000) %>%
  arrange(desc(ttl_min_bw))
mbw

#Create a factor to sort sectors by descending stat
Sector_sort3 <- factor(mbw$naic_description_lvl2, 
                       levels=mbw$naic_description_lvl2)

levels(Sector_sort3) <- gsub(" ", "\n", levels(Sector_sort3))

#Top 10 Sectors with The Highest Mininum back wages
options(scipen = 99)
ggplot(data = mbw[1:10, ], aes(x = Sector_sort3[1:10], y = ttl_min_bw)) +
  geom_bar(stat = "identity", width = 0.3) + 
  ggtitle("Top 10 Sectors with the Highest Amount of Minimum Wage Back Wages") +
  theme(axis.text.x = element_text(hjust = 1)) + #angle = 60
  ylab("Toal Minimum Wage Back Wages ($ Millions)") +
  xlab("Sector")

#*********************************************************************************************************************
#Conclusion:
#Most wages theft activities were recorded during years 2003 to 2013, with an average of 19000 cases per year.
#In general, back wages are very common in Accommodation and Food Services, Administrative and Support, Construction, 
#Health Care and Social Assistance, and Retail sectors.


