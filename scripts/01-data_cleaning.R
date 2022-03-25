---
  title: "paper 3"
output: pdf_document
date: '2022-03-11'
---
  
#r setup, get all packages that are needed
library(haven)
library(tidyverse)
library(RStata)
library(ggplot2)
library(grid)
library(gridExtra)
library(here)
library(readr)
library(knitr)
library(dplyr)
library(hrbrthemes)
library(car)

#read the raw data from the cvs file downloaded from European Social Survey website
raw_data <- read_csv(here("ESS1-9e01_1.csv"))

#remove the variables that are not useful
data_c1 <-
  raw_data %>% select(-dweight, -pspwght, -pweight, -edition, -anweight, -ctzcntr, -tporgwk, -regiongb, -jbprtfp, -stfjb,-cntry,-cname,-cproddat, -cseqno,-name,-cedition,-idno)

#combine the two variable that contain income information into one
data_c2 <-
  data_c1 %>% mutate(income = case_when(essround < 4 ~ hinctnt, essround > 3 ~ hinctnta)) %>% filter(income != 77 &

#transform the round of survey to the year of survey                                                                                                                                                                                                               income != 88 & income != 99)
data_c3 <- data_c2 %>% mutate(year = 2000 + essround * 2)

#remove the old variable for income and time of survey
data_c4 <- data_c3 %>% filter(gndr != 9) %>% select(-essround, -hinctnt, -hinctnta)

#devide the raw data set by year
data_02<- data_c4 %>% filter(year == 2002)
data_04<- data_c4 %>% filter(year == 2004)
data_06<- data_c4 %>% filter(year == 2006)
data_08<- data_c4 %>% filter(year == 2008)
data_10<- data_c4 %>% filter(year == 2010)
data_12<- data_c4 %>% filter(year == 2012)
data_14<- data_c4 %>% filter(year == 2014)
data_16<- data_c4 %>% filter(year == 2016)
data_18<- data_c4 %>% filter(year == 2018)

#calculate the average income in each year and visualize the trend by line graph
avg_income <- data_c4 %>% group_by(year) %>% summarise_at(vars(income), list(name = mean))
avg_income %>% ggplot(aes(year, name)) + geom_line(color = "deepskyblue3", size = 1.2) +  geom_vline(xintercept = 2007,
                                                                                                     linetype = 2,
                                                                                                     color = 3) +
  geom_text(
    mapping = aes(x = 2007, y = 7, label = "2007"),
    size = 5,
    color = "gray"
  ) +
  labs(
    x = "Year",
    y = "Avegrage of Income in UK",
    title = "Trend in UK Income",
    caption = "Income data collected from European Social Survay"
  ) +scale_x_continuous(breaks = seq(2000, 2020, 1))+ scale_y_continuous(breaks = seq(4, 8, 0.5))+
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.major.y = element_line(
          color = "gray",
          size = 0.5,
          linetype = 1
        ),
        axis.line =  element_line(
          color = "black",
          size = 0.5,
          linetype = 1
        )
  )

#check the correlation coefficent for all the vearables in data of 2006 and 2010
cor_coefficient_1 <- cor(data_06[ , colnames(data_06) != "income"],  
                         data_06$income)
cor_coefficient_1
cor_coefficient_2 <- cor(data_10[ , colnames(data_10) != "income"],  
                         data_10$income)
cor_coefficient_2


#viusalize the distribution in income by the political stand in 2006 and 2010 using 2d-density graph and bar graph
data_06 %>% filter(lrscale != 77 &
                     lrscale != 88 &
                     lrscale != 99) %>% ggplot(aes(x = lrscale, y = income)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour = "white") + labs(
    x = "Left and Right scale",
    y = "Income",
    title = "Distribution of income by the political stand in UK",
    caption = "Income data collected from European Social Survay in 2010"
  ) +scale_x_continuous(breaks = seq(0, 10, 1))+ scale_y_continuous(breaks = seq(0, 13, 1))+
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.major.y = element_line(
          color = "gray",
          size = 0.5,
          linetype = 1
        ),
        axis.line =  element_line(
          color = "black",
          size = 0.5,
          linetype = 1
        )
  )
data_10 %>% filter(lrscale != 77 &
                     lrscale != 88 &
                     lrscale != 99) %>% group_by(income, lrscale) %>% count() %>% ggplot(aes(x = lrscale, y = n, fill = income)) +
  geom_bar(position = "dodge", stat = "identity")+
  labs(
    x = "Left and Right scale",
    y = "Number of People",
    title = "Distribution of income by the political stand in UK",
    caption = "Income data collected from European Social Survay in 2010"
  ) +scale_x_continuous(breaks = seq(0, 10, 1))+ scale_y_continuous(breaks = seq(0, 120, 20))+
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.major.y = element_line(
          color = "gray",
          size = 0.5,
          linetype = 1
        ),
        axis.line =  element_line(
          color = "black",
          size = 0.5,
          linetype = 1
        )
  )

#calculate the mean of income by gender from 2006 to 2010
data_income_06 <- data_06 %>% group_by(income, gndr) %>% count()
data_income_06
data_income_10 <- data_10 %>% group_by(income, gndr) %>% count()
data_income_10
data_06 %>% group_by(gndr) %>% summarise_at(vars(income), list(name = mean))
data_08 %>% group_by(gndr) %>% summarise_at(vars(income), list(name = mean))
data_10 %>% group_by(gndr) %>% summarise_at(vars(income), list(name = mean))

#viusalize the distribution in income by the gender in 2006 and 2010 using bar graph
data_income_06 %>% mutate(gender = case_when(gndr == 1 ~ "male", gndr == 2 ~ "female")) %>% ggplot(aes(x = income, y = n, fill = gender)) + geom_bar(stat="identity", position="stack") + 
  labs(
    x = "Income",
    y = "Number of People",
    title = "Distribution of income by Gender in UK",
    caption = "Income data collected from European Social Survay in 2006"
  ) +scale_x_continuous(breaks = seq(0, 10, 1))+ scale_y_continuous(breaks = seq(0, 300, 50))+guides(fill=guide_legend(title="Gender"))+
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.major.y = element_line(
          color = "gray",
          size = 0.5,
          linetype = 1
        ),
        axis.line =  element_line(
          color = "black",
          size = 0.5,
          linetype = 1
        )
  )
data_income_10 %>% mutate(gender = case_when(gndr == 1 ~ "male", gndr == 2 ~ "female")) %>% ggplot(aes(x = income, y = n, fill = gender)) + geom_bar(stat="identity", position="stack") +
  labs(
    x = "Income",
    y = "Number of People",
    title = "Distribution of income by Gender in UK",
    caption = "Income data collected from European Social Survay in 2010"
  ) +scale_x_continuous(breaks = seq(0, 10, 1))+ scale_y_continuous(breaks = seq(0, 300, 50))+guides(fill=guide_legend(title="Gender"))+
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.major.y = element_line(
          color = "gray",
          size = 0.5,
          linetype = 1
        ),
        axis.line =  element_line(
          color = "black",
          size = 0.5,
          linetype = 1
        )
  )


#viusalize the trend of income by age in 2006 and 2010 using scatter plot with best fit line and confident interval
data_06_age <- data_06 %>% filter(agea != 999)
g_1<-data_06_age %>% ggplot(aes(x=agea, y=income)) +
  geom_point() +
  geom_smooth(method=lm , color="purple", fill="#69b3a2", se=TRUE) + labs(
    x = "Age",
    y = "Income",
    title = "By age in 2006",
    caption = "Income data collected from European Social Survay in 2006"
  ) +scale_x_continuous(breaks = seq(0, 100, 20))+ scale_y_continuous(breaks = seq(0, 12, 2))+
  theme_classic()

cor(data_06_age$agea, data_06_age$income)

data_10_age <- data_10 %>% filter(agea != 999) 
g_2<-data_10_age %>% ggplot(aes(x=agea, y=income)) +
  geom_point() +
  geom_smooth(method=lm , color="purple", fill="#69b3a2", se=TRUE) +labs(
    x = "Age",
    y = "Income",
    title = "By age in 2010",
    caption = "Income data collected from European Social Survay in 2010"
  ) +scale_x_continuous(breaks = seq(0, 100, 20))+ scale_y_continuous(breaks = seq(0, 12, 2))+
  theme_classic()
cor(data_10_age$agea, data_10_age$income)

#viusalize the trend of income by education level in 2006 and 2010 using scatter plot with best fit line and confident interval
data_06_edu <- data_06 %>% filter(edulvla != 55 &
                                    edulvla != 77 &
                                    edulvla != 88 &
                                    edulvla != 99) 
g_3<-data_06_edu %>% ggplot(aes(x =
                                  edulvla, y = income)) +
  geom_point() +
  geom_smooth(
    method = lm ,
    color = "red",
    fill = "#69b3a2",
    se = TRUE
  )+ labs(
    x = "Education level",
    y = "Income",
    title = "By education level in 2006",
    caption = "Income data collected from European Social Survay in 2006"
  ) +scale_x_continuous(breaks = seq(0, 5, 1))+ scale_y_continuous(breaks = seq(0, 12, 2))+
  theme_classic()
cor(data_06_edu$edulvla, data_06_edu$income)

data_10_edu <- data_10 %>% filter(edulvla != 55 &
                                    edulvla != 77 &
                                    edulvla != 88 &
                                    edulvla != 99) 
g_4<-data_10_edu %>% ggplot(aes(x =
                                  edulvla, y = income)) +
  geom_point() +
  geom_smooth(
    method = lm ,
    color = "red",
    fill = "#69b3a2",
    se = TRUE
  )+labs(
    x = "Education level",
    y = "Income",
    title = "By education level in 2010",
    caption = "Income data collected from European Social Survay in 2010"
  ) +scale_x_continuous(breaks = seq(0, 5, 1))+ scale_y_continuous(breaks = seq(0, 12, 2))+
  theme_classic()
cor(data_10_edu$edulvla, data_10_edu$income)


#viusalize the distribution in income by by worry about work and 2010 using heat map
data_10 %>% filter(wrywprb != 6 & wrywprb != 7 & wrywprb != 8 &  wrywprb != 9) %>% group_by(income, wrywprb) %>% count() %>% ggplot(aes(x = wrywprb, y = income, fill= n)) + 
  geom_tile()+labs(
    x = "Level of worry about work problem",
    y = "income",
    title = "Distribution of income by worry about work in UK",
    caption = "Income data collected from European Social Survay in 2010"
  ) +scale_x_continuous(breaks = seq(0, 5, 1))+ scale_y_continuous(breaks = seq(0, 10, 1))+guides(fill=guide_legend(title="count"))+
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.major.y = element_line(
          color = "gray",
          size = 0.5,
          linetype = 1
        ),
        axis.line =  element_line(
          color = "black",
          size = 0.5,
          linetype = 1
        )
  )

#viusalize the trend of income by planning habit in 2006 and 2010 using scatter plot with best fit line and confident interval
data_06_future <- data_06 %>% filter(plnftr != 77 & plnftr != 88 & plnftr != 99)
g_5 <-data_06_future %>% ggplot(aes(x =plnftr, y = income)) +
  geom_point() +
  geom_smooth(
    method = lm ,
    color = "blue",
    fill = "#69b3a2",
    se = TRUE
  )+labs(
    x = "Planning level",
    y = "Income",
    title = "By planning level in 2006",
    caption = "Income data collected from European Social Survay in 2006"
  ) +scale_x_continuous(breaks = seq(0, 10, 1))+ scale_y_continuous(breaks = seq(0, 12, 2))+
  theme_classic()
cor(data_06_future$plnftr, data_06_future$income)
data_06_future <- data_06 %>% filter(plnftr != 77 & plnftr != 88 & plnftr != 99)
g_6<-data_06_future %>% ggplot(aes(x =
                                     plnftr, y = income)) +
  geom_point() +
  geom_smooth(
    method = lm ,
    color = "blue",
    fill = "#69b3a2",
    se = TRUE
  )+labs(
    x = "Planning level",
    y = "Income",
    title = "By planning level in 2010",
    caption = "Income data collected from European Social Survay in 2010"
  ) +scale_x_continuous(breaks = seq(0, 10, 1))+ scale_y_continuous(breaks = seq(0, 12, 2))+
  theme_classic()
cor(data_06_future$plnftr, data_06_future$income)

#combine all the scatter plot to one
grid.arrange(g_1, g_2,g_3,g_4,g_5, g_6,ncol = 2, top=textGrob("Trend of income by 3 Variables in 2006 and 2010"))


#generate the linear model for the survey data in 2006 and 2010 using all variable 
m_all_06 = lm(income ~ agea + edulvla+ plnftr + lrscale + gndr, data = data_06)
summary(m_all_06)
m_all_10 = lm(income ~ agea + edulvla + wrywprb + lrscale + gndr, data = data_10)
summary(m_all_10)

#check the vif value for both model
Vif_06<-vif(m_all_06)
vif_10<-vif(m_all_10)

#make the table of vif value for both model
Variable_06 <- c("age", "education level", "plan level", "political stand", "gender")
Variable_10 <- c("age", "education level", "worry level", "political stand", "gender")
v_06 <- c(1.023343, 1.007733, 1.032080, 1.025262, 1.019851 )
v_10 <- c(1.053425, 1.001744, 1.052735, 1.023509, 1.017284 )
vif_table<-data.frame(Variable_06, v_06,Variable_10, v_10)
knitr::kable(vif_table, col.names = c("variables in 2006 linear model", "vif value for 2006's variables", "variables in 2010 linear model", "vif value for 2010's variables"), caption = "VIF value from the full linear model of 2006 and 2010", digits = 7, align=c(rep('c',times=4)))

#use BIC to find the best linear model for 2006 and 2010 data
n = length(data_06$income)
bic_06 = step(m_all_06, direction = "backward", k = log(n))

n = length(data_10$income)
bic_10 = step(m_all_10, direciton = "backward", k = log(n))

#overview the final nodel for both year
m_06 = lm(income ~ agea + edulvla+ plnftr + gndr, data = data_06)
summary(m_06)
m_10 = lm(income ~ agea + wrywprb + lrscale + gndr, data = data_10)
summary(m_10)

#generate the table to summarize the 2 model
Variable_06_f <- c("intercept","age", "education level", "plan level",  "gender")
Variable_10_f <- c("intercept","age", "worry level", "political stand", "gender")
bata_06 <- c(9.545974, -0.015840, 0.033963,  -0.161205, -0.433650)
bata_06 <- c( 8.649245, -0.008536, -0.550107, -0.010612, -0.476784)
p_06 <- c("< 2e-16", "1.32e-15", "0.000869", "< 2e-16", "0.000105" )
p_10 <- c("< 2e-16", "2.12e-06", "< 2e-16","2.82e-06", "0.000162" )
vif_table<-data.frame(Variable_06_f, bata_06, p_06,Variable_10_f, bata_06, p_10)
knitr::kable(vif_table, col.names = c("variables in final 2006 linear model", "coefficients for 2006's variables","p-value for 2006's variables", "variables in final 2010 linear model", "coefficients for 2010's variables","p-value for 2010's variables"), caption = "Final linear model of 2006 and 2010", digits = 7, align=c(rep('c',times=6)))


