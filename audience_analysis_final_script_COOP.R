#FINAL SCRIPT IN SEQUENTIAL ORDER

#Author: DEYVIS MEJIA ZAMBRANA

#statistics ref https://openstax.org/books/statistics/pages/preface 

#see audience_analysis_coop.R document for original data exploration.


# step 1: import data -----------------------------------------------------
file.choose()
ad_exposure<-read.csv( "C:\\Users\\deyvi\\OneDrive\\Desktop\\coursera\\ad_exposure_2021-2023.csv")
site_traffic<-read.csv("C:\\Users\\deyvi\\OneDrive\\Desktop\\coursera\\site_traffic_2021-2023.csv")
user_engagement<-read.csv("C:\\Users\\deyvi\\OneDrive\\Desktop\\coursera\\user_engagement_2021-2023.csv")


# Step 2: required packages -------------------------------------------------
library(tidyverse) #used the package in comments
#library(lubridate)
#librar(readr)
#library(stringr)
#library(dplyr)
#library(tidyr)
library(plotly)
library(ggtext)
library(moments)
library(GGally) #install
library(gganimate)

# step 3: reformatting--------------------------------------------------
#stringr from tidyverse and regex use in quotes
#create columns called Day and Month
ad_exposure_mod<-ad_exposure %>% 
  mutate(Day = str_extract(AdDate,"\\d+"),
         Month = str_extract(AdDate, "\\b\\w+$"))

ad_exposure_mod$Month=as.factor(ad_exposure_mod$Month)
#modify columns to assign number to month name
levels(ad_exposure_mod$Month) #12 levels, no further cleaning
#after this, "Apr" "Aug" "Dec" "Feb" "Jan" "Jul" "Jun" 
# "Mar" "May" "Nov" "Oct" "Sep" 
ad_exposure_mod$Month=as.character(ad_exposure_mod$Month)

ad_exposure_mod$Month[ad_exposure_mod$Month == "Apr"]<-"04"
ad_exposure_mod$Month[ad_exposure_mod$Month == "Aug"]<-"08"
ad_exposure_mod$Month[ad_exposure_mod$Month == "Dec"]<-"12"
ad_exposure_mod$Month[ad_exposure_mod$Month == "Feb"]<-"02"
ad_exposure_mod$Month[ad_exposure_mod$Month == "Jan"]<-"01"
ad_exposure_mod$Month[ad_exposure_mod$Month == "Jul"]<-"07"
ad_exposure_mod$Month[ad_exposure_mod$Month == "Jun"]<-"06"
ad_exposure_mod$Month[ad_exposure_mod$Month == "Mar"]<-"03"
ad_exposure_mod$Month[ad_exposure_mod$Month == "May"]<-"05"
ad_exposure_mod$Month[ad_exposure_mod$Month == "Nov"]<-"11"
ad_exposure_mod$Month[ad_exposure_mod$Month == "Oct"]<-"10"
ad_exposure_mod$Month[ad_exposure_mod$Month == "Sep"]<-"09"

class(ad_exposure_mod$Month)
ad_exposure_mod$Month=as.factor(ad_exposure_mod$Month)
levels(ad_exposure_mod$Month) #check levels, 12
ad_exposure_mod$Month=as.character(ad_exposure_mod$Month)

#making a dates column in YMD format
#first change Year to a character
ad_exposure_mod$Year<-as.character(ad_exposure_mod$Year)
ad_exposure_mod<-ad_exposure_mod %>%
  mutate(YMD = paste(ad_exposure_mod$Year,ad_exposure_mod$Month,
                     ad_exposure_mod$Day, sep = "/"))

class(ad_exposure_mod$YMD)
ad_exposure_mod$YMD<-as.Date(ad_exposure_mod$YMD)
class(ad_exposure_mod$YMD)

# step 4: clean data sets -------------------------------------------------
#Are there missing values? NOPE
sum(is.na(ad_exposure_mod))
sum(is.na(site_traffic))
sum(is.na(user_engagement))

#are there NaNs? only works for numerical formatted columns

#are there any complete duplicate rows? #no
ad_exposure_mod %>% 
  group_by_all() %>% 
  filter(n()>1) %>% 
  ungroup()
site_traffic %>% 
  group_by_all() %>% 
  filter(n()>1) %>% 
  ungroup()
user_engagement %>% 
  group_by_all() %>% 
  filter(n()>1) %>% 
  ungroup()
#all of the initial analysis seems to be valid so far!

#will check if each primary key has unique values.
#test join, note ad_exposure_mod2 exists in the script!
ad_exposure_mod %>% 
  group_by(ExposureID) %>% 
  filter(n()>1) %>% 
  ungroup() #2 duplicate IDs, P3841398 & O1354346
user_engagement %>% 
  group_by(ExposureID) %>% 
  filter(n()>1) %>% 
  ungroup() #2 duplicate IDs, P3841398 & O1354346
site_traffic %>% 
  group_by(ExposureID) %>% 
  filter(n()>1) %>% 
  ungroup() #2 duplicate IDs, P3841398 & O1354346
#these duplicates must be resolved before joining...

#remove specific rows/replace specific values:
#this is as you see fit/your judgement
View(user_engagement)
u_e<-user_engagement[-9585,]
View(site_traffic)
s_t<-site_traffic[-9585,]
View(ad_exposure_mod)
a_e_m<-ad_exposure_mod[-9585,]
View(a_e_m)
a_e_m[1347,4]<-"P3841397"
View(u_e)
u_e[1347,3]<-"P3841397"
View(s_t)
s_t[1347,3]<-"P3841397"



#https://www.statology.org/inner-join-in-r/
#https://www.statology.org/join-multiple-data-frames-dplyr/
#https://www.statology.org/export-data-frame-to-csv-in-r/

# step 5: JOIN datasets ---------------------------------------------------
#working joins, one table at a time
tablex<-inner_join(a_e_m, s_t, by="ExposureID")
tableF<-inner_join(tablex, u_e, by="ExposureID")

#write csv if needed
getwd()
write_csv(tableF, "final_table_R_COOP.csv")

# step 6: exploratory + GROUPS ---------------------------------------------
#first create a table of the revenues for each day to use later
revenue_ad_date<-tableF %>% 
  group_by(YMD) %>% 
  summarize(total_revenue = sum(RevenueGenerated))
  
tableF$Year=as.factor(tableF$Year)

tableF$Gender=as.factor(tableF$Gender)
tableF$SegmentType=as.factor(tableF$SegmentType)
tableF$Browser=as.factor(tableF$Browser)
tableF$Location.DMA.=as.factor(tableF$Location.DMA.)

                          #AGE

tableF$Gender=as.factor(tableF$Gender)
tableF$SegmentType=as.factor(tableF$SegmentType)
tableF$Browser=as.factor(tableF$Browser)
tableF$Location.DMA.=as.factor(tableF$Location.DMA.)

ggplot(data=tableF,aes(x=Age))+
  geom_histogram()+
  theme_classic()+
  labs(title="age distributions")
ggplot(data=tableF, aes(x=Age))+
  geom_histogram(bins = 15, aes(fill=Year),alpha=0.5)+
  theme_classic()+
  labs(title="age distributions by year")+
  facet_wrap(~Year)

ggplot(data=user_engagement, aes(x=Age))+
  geom_density()+
  theme_classic()+
  labs(title="age density distribution")
ggplot(data=user_engagement, aes(x=Age))+
  geom_density(aes(color=Year))+
  theme_classic()+
  labs(title="age density distribution by year")
# next steps:
#analyze age distributions for clicks, no clicks, conversion

tableF %>% 
  group_by(Year) %>% 
  summarize(avg_age = mean(Age), med_age = median(Age), 
            sd_age = sd(Age), variance_age = var(Age))
# no big differences between mean, median, sd, var by year.


                          #GENDER
ggplot(data=tableF, aes(x=Gender))+
  geom_bar(stat="count", aes(fill=Gender))+
  theme_classic()+
  labs(title="total gender counts")
# practically no difference
ggplot(data=tableF, aes(x=Gender))+
  geom_bar(stat="count", aes(fill=Gender))+
  theme_classic()+
  labs(title="gender counts by year")+
  facet_wrap(~Year)
#no big differences
ggplot(data=tableF, aes(x=Gender))+
  geom_bar(stat="count", aes(fill=Gender))+
  theme_classic()+
  labs(title="gender counts by segment type")+
  facet_wrap(~SegmentType)
#no difference
ggplot(data=tableF, aes(x=Gender))+
  geom_bar(stat="count", aes(fill=Gender))+
  theme_classic()+
  labs(title="gender counts by device")+
  facet_wrap(~Device, nrow=1)
#no difference

#no difference between gender but differences between
#browser type!
ggplot(data=user_engagement, aes(x=Gender))+
  geom_bar(stat="count", aes(fill=Gender))+
  theme_classic()+
  labs(title="gender counts by browser")+
  facet_wrap(~Browser, nrow=1)
#chi squared? compare conversions vs not buyers/revenue

ggplot(data=tableF, aes(x=Device))+
  geom_bar()
ggplot(data=tableF, aes(x=Device))+
  geom_bar()+
  facet_wrap(~Year)
ggplot(data=tableF, aes(x=Income))+
  geom_histogram(aes(fill=Device))+
  facet_grid(Device~Year)
#no big differences
tableF %>% 
  group_by(Year, Device) %>% 
  summarize(avg_Inc = mean(Income), med_Inc = median(Income))

#location
ggplot(data = tableF, aes(x=Location.DMA.))+
  geom_bar(aes(fill=Location.DMA.))+
  theme_classic()+
  labs(title="location counts totals")+
  theme(axis.text.x = element_text(angle=70, vjust=0.5))+
  theme(plot.title = element_text(hjust=0.5),
        legend.position = "none")
#separate city and state:
tableF<-tableF %>% 
  mutate(State = str_extract(Location.DMA.,"\\b\\w+$"))
tableF<-tableF %>% 
  mutate(City = gsub("^(.*?),.*", "\\1", Location.DMA.))

class(tableF$City)
tableF$City<-as.factor(tableF$City)
levels(tableF$City)

class(tableF$State)
tableF$State<-as.factor(tableF$State)
levels(tableF$State)
tableF$State=as.character(tableF$State)
#state name NEW YORK is "York" fix it:
#can't modify a factor to a different non-same-class
#convert back to character to modify
tableF$State[tableF$State == "York"]<- "New York"

ggplot(data = tableF, aes(x=State))+
  geom_bar(aes(fill=State))+
  theme_classic()+
  labs(title="State total counts")+
  theme(axis.text.x = element_text(angle=70, vjust=0.5))+
  theme(plot.title = element_text(hjust=0.5),
        legend.position = "none")
ggplot(data = tableF, aes(x=City))+
  geom_bar(aes(fill=City))+
  theme_classic()+
  labs(title="City total counts")+
  theme(axis.text.x = element_text(angle=70, vjust=0.5))+
  theme(plot.title = element_text(hjust=0.5),
        legend.position = "none")

ggplot(data = tableF, aes(x=Location.DMA.))+
  geom_bar(aes(fill=Location.DMA.))+
  theme_classic()+
  labs(title="location counts totals by year")+
  theme(axis.text.x = element_text(angle=70, vjust=0.5))+
  theme(plot.title = element_text(hjust=0.5),
        legend.position = "none")+
  facet_wrap(~Year)

ggplot(data = tableF, aes(x=State))+
  geom_bar(aes(fill=State))+
  theme_classic()+
  labs(title="state counts totals by year")+
  theme(axis.text.x = element_text(angle=70, vjust=0.5))+
  theme(plot.title = element_text(hjust=0.5),
        legend.position = "none")+
  facet_wrap(~Year)
#no changes between years

ggplot(data = tableF, aes(x=City))+
  geom_bar(aes(fill=City))+
  theme_classic()+
  labs(title="city counts totals by year")+
  theme(axis.text.x = element_text(angle=70, vjust=0.5))+
  theme(plot.title = element_text(hjust=0.5),
        legend.position = "none")+
  facet_wrap(~Year)
#no changes between years!

# psychographic----------------------

                          #LIFESTYLE
ggplot(data=tableF, aes(x=Income))+
  geom_histogram()+
  theme_classic()+
  labs(title = "income distributions")
#skewed right to higher income of 200,000!
ggplot(data=tableF, aes(x=Income))+
  geom_density()+
  theme_classic()+
  labs(title = "income density distributions")

ggplot(data=tableF, aes(x=Income))+
  geom_histogram(aes(fill=Gender), alpha=0.5)+
  theme_classic()+
  labs(title = "income distributions by gender")+
  facet_wrap(~Gender)

ggplot(data=tableF, aes(x=Income))+
  geom_density(aes(color=Gender))+
  theme_classic()+
  labs(title = "income density distributions by gender")
#outliers for income?
ggplot(data=tableF, aes(x=Gender,y=Income))+
  geom_boxplot(aes(color=Gender), lwd=1)+
  theme_classic()+
  geom_jitter(alpha=0.05, width=0.3)+
  labs(title = "Income distributed by gender boxplot")
tableF %>% 
  group_by(Gender) %>% 
  summarize(avg_inc = mean(Income), med_inc = median(Income), 
            ad_inc = sd(Income), variance_inc = var(Income))

ggplot(data=user_engagement, aes(x=Gender,y=Income))+
  geom_boxplot(aes(color=Gender), lwd=1)+
  theme_classic()+
  geom_jitter(alpha=0.05, width=0.3)+
  facet_wrap(~Year)
labs(title = "Income distributed by gender for each year boxplot")
#not much difference

# behavioral --------------------

#line graph! See next section under purchase history
tableF %>%
  ggplot(aes(x=YMD, y=RevenueGenerated))+
  geom_line()+
  labs(title="Revenue generated for each ad date")#looks weird

tableF %>%
  ggplot(aes(x=YMD, y=RevenueGenerated))+
  geom_col()+
  labs(title="Revenue generated for each ad date")

#bubble plot
tableF %>%
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=YMD, y=RevenueGenerated))+
  geom_point(aes(size=RevenueGenerated),alpha=0.2)+
  labs(title="Revenue > 0 generated for each ad date,")

tableF %>%
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=YMD, y=RevenueGenerated))+
  geom_point(aes(size=RevenueGenerated,
                 color=RevenueGenerated),alpha=0.2)+
  labs(title="Revenue > 0 generated for each ad date")+
  theme_bw()+
  scale_color_gradient2(low="yellow4",mid="yellowgreen",
                        high = "lawngreen", midpoint=500)

#to plot the sums you need to add each point, done already

ggplot(revenue_ad_date, aes(YMD,total_revenue))+
  geom_line(aes(color=total_revenue), lwd=2)+
  theme_classic()+
  labs(title="Revenue Over the Past Three Years",
       x="Time",y="Revenue (USD)",
       subtitle = "High risk of making zero revenue this year.",
       color=" ")+
  geom_smooth(color="lightsteelblue", se=F)+
  geom_smooth(method = "lm",color="red",se=F)+
  ylim(c(0,50000))+
  annotate("text", x = as.Date("2023-07-23"), 
           y = as.integer(42000), label = "Revenue",
           color="green3", fontface="bold")+
  annotate("text", x = as.Date("2023-07-23"), 
           y = as.integer(38000), label = "Linear Model",
           color="brown2")+
  annotate("text", x = as.Date("2023-07-23"), 
           y = as.integer(34000), label = "LOESS Model",
           color="lightsteelblue")+
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  scale_color_gradient2(low="yellow4",high="green2")
#next steps: add dollar signs to y axis

#simple base r plot below
plot(revenue_ad_date$YMD,revenue_ad_date$total_revenue,
     type="l", xlab="date", ylab="revenue (USD)",
     main="revenue over the last three years",
     col="seagreen2", lwd=3)

                      #PURCHASE HISTORY
#distribution of revenue, filter data where RevGen > 0
tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=RevenueGenerated))+ 
  geom_histogram()+ 
  theme_classic()
#by year
#YIKESSSS PROBLEM!!! REVENUE GOING DOWN!!! Insight
tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=RevenueGenerated))+ 
  geom_histogram()+ 
  theme_classic()+
  facet_wrap(~Year, nrow=1)

tableF_mod<-ad_exposure_mod %>% #use the joined complete df
  filter(RevenueGenerated > 0)

ggplot(data=tableF_mod, aes(x=RevenueGenerated))+ 
  geom_histogram(aes(fill=Month))+ 
  theme_classic()+
  facet_wrap(~Year)
ggplot(data=tableF_mod,
       aes(x=RevenueGenerated))+ 
  geom_histogram(aes(fill=Month))+ 
  theme_classic()+
  facet_grid(Year~Month)+
  theme(legend.position = "none")

ggplot(data = tableF_mod,
       aes(x=Month, y=RevenueGenerated))+
  geom_boxplot(aes(color=Month))+
  facet_wrap(~Year)+
  theme_bw()+
  theme(legend.position = "none")+
  geom_jitter(aes(color=Month, alpha=0.2))

                      #ONLINE ACTIVITY
ggplot(data = tableF, aes(x=SessionDuration))+
  geom_histogram()+
  theme_classic()
ggplot(data = tableF, aes(x=SessionDuration))+
  geom_histogram()+
  theme_classic()+
  facet_wrap(~Year) 
#insight: duration shifts

#two clusters
ggplot(data = tableF, aes(x=SessionDuration))+
  geom_histogram(aes(fill=TrafficChannel), alpha=0.5)+
  theme_classic()
ggplot(data = tableF, aes(x=SessionDuration))+
  geom_histogram(alpha=0.5)+
  theme_classic()+
  facet_wrap(~TrafficChannel, nrow = 1)
#is there a gender difference?
ggplot(data = site_traffic, aes(x=SessionDuration))+
  geom_histogram(alpha=0.5)+
  theme_classic()+
  facet_grid(Year~TrafficChannel)

#bounced? definition? #insight, audience differences
ggplot(data = tableF, aes(x=Bounced))+
  geom_bar(alpha=0.5, aes(fill=Bounced))+
  theme_classic()+
  scale_fill_manual(values = c("green3","red3"))
ggplot(data = tableF, aes(x=Bounced))+
  geom_bar(alpha=0.5, aes(fill=Bounced))+
  theme_classic()+
  scale_fill_manual(values = c("green3","red3"))+
  facet_wrap(~Year, nrow = 1)
tableF %>% 
  group_by(Year, Bounced) %>% 
  summarize(count=n())

ggplot(data = tableF, aes(x=Bounced))+
  geom_bar(alpha=0.5, aes(fill=Bounced))+
  theme_classic()+
  scale_fill_manual(values = c("green3","red3"))+
  facet_wrap(~PagesperSession, nrow = 1)+
  labs(title="Bounced for each page per session")
#insights!? Number of pages predicts bounced?
ggplot(data = tableF, aes(x=PagesperSession))+
  geom_histogram(alpha=0.5)+
  theme_classic()


#ref https://sjspielman.github.io/introverse/articles/color_fill_scales.html
#ref https://r-graphics.org/
#ref https://r-charts.com/ggplot2/axis/
#ref https://r-charts.com/base-r/axes/
#ref https://wilkelab.org/ggtext/

# step 7: Chi-Squared analysis --------------------------------------------

#ref youtube equitable equations
#ref https://statisticsbyjim.com/basics/contingency-table/
#contingency table probabilities https://statisticsbyjim.com/probability/contingency-tables-probabilities/
#ref https://www.statology.org/effect-size-chi-square/ 

# step 8: MODELING --------------------------------------------------------
                    
                    #Modeling revenue over time

fit.revenue<-lm(total_revenue~YMD,revenue_ad_date)
fit.revenue #revenue = -33.92*YMD + 676844.95

summary(fit.revenue)
names(fit.revenue)

#residuals plot
res<-resid(fit.revenue)
plot(fitted(fit.revenue),res, col="red")
abline(0,0, col="blue4")

#visualize in ggplot to add smooth geoms
resVfitrev<-data.frame(fit_revenue<-(fitted(fit.revenue)),
                       residuals<-res)

ggplot(resVfitrev, aes(x=fit_revenue, y=residuals))+
  geom_point(col="red")+
  geom_smooth()+
  geom_smooth(method = "lm", col="green4")+
  theme_classic()

#statology interpretation when: p>.05
moments::kurtosis(res)
moments::skewness(res)
moments::jarque.test(res)
#We do not have sufficient evidence to say that this 
#dataset has a skewness and kurtosis that is different
#from the normal distribution.
#other tests for linear model residuals include
#shapiro-wilk and Cramer-Von-Mises.

#visualize distribution of residuals
hist(res)
var(res) # 28654365 the spread, var=sd^2
sd(res)#how spread out the data is, typical distance a value is from mean
mean(res) #-2.036396e-13 very close to 0?
summary(fit.revenue) #summary of the linear model
#multiple R^2 = 0.8056, adjusted R^2 = 0.7999
#https://www.statology.org/interpret-regression-output-in-r/
#next steps: evaluate the model, add qqplot

#statology: R-squared value is the proportion of the variance 
#in the RESPONSE (Y) variable that can be explained by the 
#predictor (X)variables in the model.

#Medium source: R² lets you quantify just how much better
#the Linear model fits the data as compared to the Mean Model.
#The mean model is known as the null model or intercept only*.

                      #FINDING MODELS
#unfiltered data, only numerical columns
pairsdata1<-data.frame(tableF$SpendonExposure,
                      tableF$RevenueGenerated,
                      tableF$SessionDuration,
                      tableF$PagesperSession,tableF$Age,
                      tableF$Income)
ggpairs(pairsdata1)
#result: 4 statistically significant associations

#numerical data filtered by revenue generated >0
pairsdata %>% 
  filter(tableF.RevenueGenerated >0) %>% 
  ggpairs()
#result: 

#filtered data by GENDER, we want to predict revenue
#GENDER MALE
pairsdata2<-tableF %>% 
  filter(Gender == "Male", RevenueGenerated > 0)
pairsdata2<-data.frame(pairsdata2$SpendonExposure,
                       pairsdata2$RevenueGenerated,
                       pairsdata2$SessionDuration,
                       pairsdata2$PagesperSession,
                       pairsdata2$Age,
                       pairsdata2$Income)
ggpairs(pairsdata2)
#result: income and age sig cor.

#GENDER FEMALE
pairsdata3<-tableF %>% 
  filter(Gender == "Female", RevenueGenerated > 0)
pairsdata3<-data.frame(pairsdata3$SpendonExposure,
                       pairsdata3$RevenueGenerated,
                       pairsdata3$SessionDuration,
                       pairsdata3$PagesperSession,
                       pairsdata3$Age,
                       pairsdata3$Income)
ggpairs(pairsdata3)
#result: age and income sig cor.
#this is going too slow..

#SPEND ON EXPOSURE
tableF %>% 
  filter(RevenueGenerated > 0) %>% 
ggplot(aes(x=SpendonExposure, y=RevenueGenerated))+
  geom_point(aes(color=Gender))+
  geom_smooth(aes(color=Gender),method = "lm")
#slight increase for females?

tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=SpendonExposure, y=RevenueGenerated))+
  geom_point(aes(color=AdFormat))+
  geom_smooth(aes(color=AdFormat),method = "lm") #nothing

tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=SpendonExposure, y=RevenueGenerated))+
  geom_point(aes(color=TrafficChannel))+
  geom_smooth(aes(color=TrafficChannel),method = "lm")

tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=SpendonExposure, y=RevenueGenerated))+
  geom_point(aes(color=Bounced))+
  geom_smooth(aes(color=Bounced),method = "lm")#not interesting

tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=SpendonExposure, y=RevenueGenerated))+
  geom_point(aes(color=SegmentType))+
  geom_smooth(aes(color=SegmentType),method = "lm")#nope

tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=SpendonExposure, y=RevenueGenerated))+
  geom_point(aes(color=OperatingSystem))+
  geom_smooth(aes(color=OperatingSystem),method = "lm")

tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=SpendonExposure, y=RevenueGenerated))+
  geom_point(aes(color=Device))+
  geom_smooth(aes(color=Device),method = "lm")#nope

tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=SpendonExposure, y=RevenueGenerated))+
  geom_point(aes(color=Browser))+
  geom_smooth(aes(color=Browser),method = "lm")

tableF$Year<-as.factor(tableF$Year)
tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=SpendonExposure, y=RevenueGenerated))+
  geom_point(aes(color=Year))+
  geom_smooth(aes(color=Year),method = "lm")


#SESSION DURATION
tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=SessionDuration, y=RevenueGenerated))+
  geom_point(aes(color=Gender))+
  geom_smooth(aes(color=Gender),method = "lm") #slight neg

tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=SessionDuration, y=RevenueGenerated))+
  geom_point(aes(color=AdFormat))+
  geom_smooth(aes(color=AdFormat),method = "lm")#no

tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=SessionDuration, y=RevenueGenerated))+
  geom_point(aes(color=TrafficChannel))+
  geom_smooth(aes(color=TrafficChannel),method = "lm")

tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=SessionDuration, y=RevenueGenerated))+
  geom_point(aes(color=Bounced))+
  geom_smooth(aes(color=Bounced),method = "lm")

tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=SessionDuration, y=RevenueGenerated))+
  geom_point(aes(color=SegmentType))+
  geom_smooth(aes(color=SegmentType),method = "lm")

tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=SessionDuration, y=RevenueGenerated))+
  geom_point(aes(color=OperatingSystem))+
  geom_smooth(aes(color=OperatingSystem),method = "lm")

tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=SessionDuration, y=RevenueGenerated))+
  geom_point(aes(color=Device))+
  geom_smooth(aes(color=Device),method = "lm")

tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=SessionDuration, y=RevenueGenerated))+
  geom_point(aes(color=Browser))+
  geom_smooth(aes(color=Browser),method = "lm")

tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=SessionDuration, y=RevenueGenerated))+
  geom_point(aes(color=Year))+
  geom_smooth(aes(color=Year),method = "lm")

#INCOME
tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=Income, y=RevenueGenerated))+
  geom_point(aes(color=Gender))+
  geom_smooth(aes(color=Gender),method = "lm")

tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=Income, y=RevenueGenerated))+
  geom_point(aes(color=AdFormat))+
  geom_smooth(aes(color=AdFormat),method = "lm")

tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=Income, y=RevenueGenerated))+
  geom_point(aes(color=TrafficChannel))+
  geom_smooth(aes(color=TrafficChannel),method = "lm")

tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=Income, y=RevenueGenerated))+
  geom_point(aes(color=SegmentType))+
  geom_smooth(aes(color=SegmentType),method = "lm")

tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=Income, y=RevenueGenerated))+
  geom_point(aes(color=OperatingSystem))+
  geom_smooth(aes(color=OperatingSystem),method = "lm")

tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=Income, y=RevenueGenerated))+
  geom_point(aes(color=Device))+
  geom_smooth(aes(color=Device),method = "lm")

tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=Income, y=RevenueGenerated))+
  geom_point(aes(color=Browser))+
  geom_smooth(aes(color=Browser),method = "lm")

tableF %>% 
  filter(RevenueGenerated > 0) %>% 
  ggplot(aes(x=Income, y=RevenueGenerated))+
  geom_point(aes(color=Year))+
  geom_smooth(aes(color=Year),method = "lm")



#can we find something to predict total revenue besides time???
#try group by category then summarize total_revenue
tableF %>% 
  filter(RevenueGenerated >0) %>% 
  group_by(Gender) %>% 
  summarize(total_revenue = sum(RevenueGenerated),
            total_spenton_exposure=sum(SpendonExposure)) %>% 
  ggplot(aes(x=total_spenton_exposure,y=total_revenue))+
  geom_point(aes(color=Gender))
tableF %>% 
  filter(RevenueGenerated >0) %>% 
  group_by(CreativeID) %>% 
  summarize(total_revenue = sum(RevenueGenerated),
            total_spenton_exposure=sum(SpendonExposure)) %>% 
  ggplot(aes(x=total_spenton_exposure,y=total_revenue))+
  geom_point(aes(color=CreativeID))+
  theme(legend.position = "none")
tableF %>% 
  filter(RevenueGenerated >0) %>% 
  group_by(CreativeID) %>% 
  summarize(total_revenue = sum(RevenueGenerated),
            total_spenton_exposure=sum(SpendonExposure)) %>% 
  ggplot(aes(x=total_spenton_exposure,y=total_revenue))+
  geom_point(aes(color=CreativeID))+
  geom_smooth(method = "lm", aes(color=CreativeID), se=FALSE)+
  theme(legend.position = "none")
tableF %>% 
  filter(RevenueGenerated >0) %>% 
  group_by(SegmentType) %>% 
  summarize(total_revenue = sum(RevenueGenerated),
            total_spenton_exposure=sum(SpendonExposure)) %>% 
  ggplot(aes(x=total_spenton_exposure,y=total_revenue))+
  geom_point(aes(color=SegmentType))+
  theme(legend.position = "none")

#the above was kinda waste of time...
rev_spent_cID<-tableF %>% 
  filter(RevenueGenerated >0) %>% 
  group_by(CreativeID) %>% 
  summarize(total_revenue = sum(RevenueGenerated),
            total_spenton_exposure=sum(SpendonExposure))

ggplot(rev_spent_cID,aes(x=total_spenton_exposure,y=total_revenue))+
  geom_point()+
  geom_smooth(method = "lm", color="black")+
  theme(legend.position = "none")
#ALL buyers and nonbuyers
tableF %>% 
  group_by(CreativeID) %>% 
  summarize(total_revenue = sum(RevenueGenerated),
            total_spenton_exposure=sum(SpendonExposure)) %>% 
  ggplot(aes(x=total_spenton_exposure,y=total_revenue))+
  geom_point()+
  geom_smooth(method="lm", color="brown")#YES!model below
#how has spend on exposure changed over time? buyers&nonbuyers
tableF %>% 
  group_by(YMD) %>% 
  summarize(total_spend_exposure = sum(SpendonExposure)) %>% 
  ggplot(aes(x=YMD,y=total_spend_exposure))+
  geom_line(color="purple3") #interesting
#now look at only conversions
tableF %>% 
  filter(RevenueGenerated >0) %>% 
  group_by(YMD) %>% 
  summarize(total_spend_exposure = sum(SpendonExposure)) %>% 
  ggplot(aes(x=YMD,y=total_spend_exposure))+
  geom_line(color="orange2") #interesting

#MODEL buyers and nonbuyers grouped by CreativeID
CIDALL<-tableF %>% 
  group_by(CreativeID) %>% 
  summarize(total_revenue = sum(RevenueGenerated),
            total_spenton_exposure=sum(SpendonExposure))
CIDALL %>% arrange(desc(total_revenue))
CIDALL %>% arrange(desc(total_spenton_exposure))

fit.cidall<-lm(total_revenue~total_spenton_exposure,CIDALL)
fit.cidall
#We know the CreativeID might affect the total revenue
#Let's make another model with the creativeID as a factor
#but we don't know the relationship way
fit.test<-lm(RevenueGenerated~SpendonExposure*CreativeID, tableF)
#lots of intercepts and rates..

#now let's look at buyers only rev_spent_cID
rscid<-lm(total_revenue ~ total_spenton_exposure, rev_spent_cID)
rscid
summary(rscid) #R-squared:  0.3191

#residual plot
res2<-resid(rscid)
plot(fitted(rscid),res2)#looks good
abline(0,0,col="blue")

#revenue predict form session duration and pages per session
tableF$PagesperSession<-as.factor(tableF$PagesperSession)
rsdps<-lm(RevenueGenerated ~ SessionDuration*PagesperSession,
          tableF)
summary(rsdps) #R-squared:  0.7929!! for pps as factor
rsdps_prediction<-predict(rsdps)
ggplot(tableF, aes(SessionDuration, RevenueGenerated))+
  geom_point(aes(color=PagesperSession))+
  geom_line(data=tableF, aes(x=RevenueGenerated, y=rsdps_prediction))+
  geom_smooth()
tableF %>% filter(Conversion == "Yes") %>% 
  ggplot(aes(SessionDuration, RevenueGenerated))+
  geom_density_2d_filled(aes(color=after_stat(level)))
tableF %>% filter(Conversion == "Yes") %>% 
  ggplot(aes(SessionDuration, RevenueGenerated))+
  stat_density_2d(aes(fill = ..density..), geom = "raster",
                  contour = FALSE)+
  scale_fill_viridis_c(option = "magma")

#now see pagespersession as a number:
tableF$PagesperSession<-as.numeric(tableF$PagesperSession)
rsdpsn<-lm(RevenueGenerated ~ SessionDuration + PagesperSession,
          tableF)
summary(rsdpsn) #R^2 = 0.69
#formula:

ggplot(tableF,aes(x=SessionDuration,y=RevenueGenerated))+
  geom_point()+
  facet_wrap(~PagesperSession)

#MIXED LM
tableF$PagesperSession<-as.numeric(tableF$PagesperSession)
mixed<-lm(RevenueGenerated ~ SessionDuration + PagesperSession
            + SpendonExposure, tableF)
summary(mixed)
#https://www.statology.org/r-linear-regression-with-categorical-variables/
#ref youtube videos (3)
#ref https://michaelgastner.com/R_for_QR/fitting-a-linear-model.html
#ref https://bio723-class.github.io/Bio723-book/linear-regression-models.html
#ref https://www.originlab.com/doc/Origin-Help/Residual-Plot-Analysis
#ref https://www.qualtrics.com/support/stats-iq/analyses/regression-guides/interpreting-residual-plots-improve-regression/
#ref https://getrecast.com/heteroskedasticity/


# step 9: MODEL evaluation ------------------------------------------------
#Evaluating model of revenue ~ YMD
#evaluating model of revenue ~spentonexposure
#evaluating model of revenue ~ sessionduration * pagespersession

#Model comparson to actual revenue generated

#ref https://people.duke.edu/~rnau/rsquared.htm
#ref see youtube

# animation & sankey -----------------------------------------------------------
#df<-read.csv("C:\\Users\\deyvi\\OneDrive\\Documents\\R_portoflio_projects\\final_table_R_COOP.csv")
data<-data.frame(df$Viewable, df$WasClicked, df$Conversion)
names(data)<-c("Impressions","Clicks","Conversions")

data$Clicks[data$Clicks == "Yes"]<-"Yes!"
data$Clicks[data$Clicks == "No"]<-"No!"

data$Conversions[data$Conversions == "Yes"]<-"YES"
data$Conversions[data$Conversions == "No"]<-"NO"

library(ggsankey)
transformed<-data %>% 
  make_long(Impressions,Clicks,Conversions)

counts<-transformed %>% 
  dplyr::group_by(node) %>% 
  dplyr::tally()

DF<-merge(transformed,counts, by.x="node", by.y="node", all.x = T)

ggplot(DF, aes(x = x, next_x = next_x,
                        node = node,next_node = next_node,
                        fill = factor(node),
               label = paste0(node, " n=",n))) +
  geom_sankey(flow.alpha = .6,
              node.color="grey40") +
  geom_sankey_label(size = 3, color = "black", fill = "white") +
  scale_fill_viridis_d(drop = FALSE) +
  theme_sankey(base_size = 18) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5)) +
  ggtitle("Flow from ad impression to conversion")

ggplot(DF, aes(x = x, next_x = next_x,
               node = node,next_node = next_node,
               fill = factor(node),
               label = paste0(node, " =",n))) +
  geom_sankey(flow.alpha = .6,
              node.color="grey40") +
  geom_sankey_label(size = 3, color = "black", fill = "white") +
  scale_fill_viridis_d(drop = FALSE) +
  theme_sankey(base_size = 18) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5)) +
  ggtitle("Flow from ad impression to conversion")+
  scale_fill_manual(values = c("Yes" ="yellow2","No"="grey",
                               "Yes!"="yellowgreen","No!"="grey",
                               "YES"="lawngreen","NO"="grey"))

ggplot(DF, aes(x = x, next_x = next_x,
               node = node,next_node = next_node,
               fill = factor(node),
               label = paste0(node, " =",n))) +
  geom_sankey(node.color="grey40") +
  geom_sankey_label(size = 3, color = "black", fill = "white") +
  scale_fill_viridis_d(drop = FALSE) +
  theme_sankey(base_size = 18) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5)) +
  ggtitle("Flow from ad impression to conversion")+
  scale_fill_manual(values = c("Yes" ="yellow2","No"="grey30",
                               "Yes!"="yellowgreen","No!"="grey30",
                               "YES"="lawngreen","NO"="grey30"))+
  theme(panel.background = element_rect(fill = "black"))


#ref https://gganimate.com/
#ref https://github.com/davidsjoberg/ggsankey
#ref https://rpubs.com/DragonflyStats/Sankey-networkD3
#ref https://plotly.com/python/sankey-diagram/
#ref https://plotly.com/r/sankey-diagram/ 

abc<-tableF %>% 
  filter(Conversion == "Yes")  
def<-ggplot(data=abc,aes(x=Location.DMA.))+
  geom_bar()
conv<-tableF %>% 
  filter(Conversion == "Yes") %>% 
  summarize(n=n())
bar<-data.frame(abc$Location.DMA.)
bar %>%
  group_by(abc.Location.DMA.) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))
#maybe add year as well or audience attributes?

#ANIMATION REVENUE OVER TIME (DOT)
library(gganimate)
animation<-ggplot(data=revenue_ad_date,
                  aes(x=YMD,y=total_revenue, group = 1))+
  geom_point(color="green3", size=8)+
  theme_bw()+
  labs(x= "Time", y = "Revenue (USD)")+
  ggtitle("Revenue has depricated from 2021 to 2024")+
  ylim(c(0,50000))+
  transition_time(YMD)+
  ease_aes()
animation
anim_save("TechtronicsrevenueV2.gif", animation)



# anova visuals -----------------------------------------------------------

dmod<-david %>% 
  filter(Conversion == "Yes")

ggplot(dmod,aes(y=SessionDuration, x=Income2))+
  geom_jitter(aes(color=Income2),alpha=0.2)+
  theme_classic()+
  stat_summary(fun.y = "mean",geom="crossbar", color = "black")+
  labs(y="Session duration")
  
ggplot(dmod,aes(y=PagesperSession, x=Income2))+
  geom_jitter(aes(color=Income2),alpha=0.2)+
  theme_classic()+
  stat_summary(fun.y = "mean",geom="crossbar", color = "black")+
  labs(y="pages per session")

ggplot(dmod,aes(y=PagesperSession, x=Age2))+
  geom_jitter(aes(color=Age2),alpha=0.2)+
  theme_classic()+
  stat_summary(fun.y = "mean",geom="crossbar", color = "black")+
  labs(y="pages per session")+
  scale_color_manual(values=c("orange2","green4","blue"))

ggplot(dmod,aes(y=SessionDuration, x=Age2))+
  geom_jitter(aes(color=Age2),alpha=0.2)+
  theme_classic()+
  stat_summary(fun.y = "mean",geom="crossbar", color = "black")+
  labs(y="session duration")+
  scale_color_manual(values=c("orange2","green4","blue"))+
  theme(axis.text.x = element_text(colour = c("orange2","green4","blue")))

#OTHER PLOT: 2-d density plot
ggplot(dmod, aes(x=Age,y=SessionDuration))+
  + geom_point(alpha=0.5)+
  + geom_density2d(aes(color=after_stat(level)))+
  + scale_color_viridis_c()
ggplot(dmod, aes(x=Age,y=SessionDuration))+
  ggpointdensity::geom_pointdensity(alpha=0.5)
ggplot(dmod, aes(x=Age,y=SessionDuration))+
  ggpointdensity::geom_pointdensity(alpha=0.5)+
  scale_color_gradientn(colors=rainbow(4))
ggplot(dmod, aes(x=Age,y=SessionDuration))+
  ggpointdensity::geom_pointdensity(alpha=0.5)+
  scale_color_viridis_c(option = "turbo")

#geom_density2d and geom_pointdensity 
ggplot(dmod, aes(x=Age,y=SessionDuration))+
  ggpointdensity::geom_pointdensity(alpha=0.5)+
  scale_color_viridis_c(option = "turbo")+
  geom_density2d(aes(color=after_stat(level)))
#direction = -1 to reverse color scale in viridis scale

#ggnewscale to allow multiple color scales
ggplot(dmod, aes(x=Age,y=SessionDuration))+
  ggpointdensity::geom_pointdensity(alpha=0.5)+
  scale_color_viridis_c(option = "turbo")+
  ggnewscale::new_scale_color()+
  geom_density2d(aes(color=after_stat(level)),lwd=1)+
  scale_color_viridis_c()
#WOOO!!!
#ref: https://cran.r-project.org/web/packages/ggpointdensity/readme/README.html
#ref: https://eliocamp.github.io/ggnewscale/