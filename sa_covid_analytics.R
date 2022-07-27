#library
library(ggplot2)
library(tidyverse)
library(gganimate)
library(ggthemes)
library(rio)

# Data
data <- as_tibble(import("sa_ng_ke_covid_data_june_22.csv"))
data
data$country <- as.factor(data$country)
data


covid_plot<-data %>% ggplot(aes(x=date_confirmation,y=cumulative_cases))+
  geom_line(alpha=0.8)+ geom_point(size=1)
  
covid_plot


covid_plot<- data %>%
  ggplot(aes(x=date_confirmation,y=cumulative_cases, color=country))+
  geom_line(alpha=0.8)+
  geom_point(size=1)+
  scale_y_continuous(breaks = c(500000,1000000,1500000,2000000,2500000,3000000,3500000, 4000000),
                labels=c("500K","1M","1.5M","2M","2.5M","3M","3.5M", "4M" ))+
  theme_solarized_2(light=F)+
  labs(title="Daily Covid 19 Cases by Country From January 2020 to June 2022", 
       caption="Source: Dataset from https://ourworldindata.org")+
  theme(text=element_text(colour="#EEEEEE"),
        title=element_text(colour="#EEEEEE",size=12,face = "bold"),
        plot.title=element_text(hjust=0.5),
        axis.title.x = element_blank(),
        panel.grid.minor.x=element_blank(),
        legend.background = element_blank(),
        legend.key= element_blank(),
        legend.position=c(0.095, 0.81),# legend at top-left, inside the plot
        plot.margin = unit(c(0.5,1.3,0.5,0.5), "cm"))+ 
  scale_x_date(date_labels = "%Y %m",date_breaks ="3 months")

covid_plot
png("covid_plot.png")
jpeg("covid_plot.jpeg")


plot2<- data %>%
  ggplot(aes(x=date_confirmation,y=cumulative_cases, color=country))+
  geom_line(alpha=0.8)+
  geom_point(size=1)+
    scale_y_log10(breaks = c(1e+1,1e+2,1e+3,1e+4,1e+5,1e+6,1e+7),
                labels=c("10","100","1,000","10,000","100,000","1,000,000","10,000,000" ))+
  theme_solarized_2(light=F)+
  labs(title="Daily Covid 19 Cases by Country From January 2020 to June 2022", 
       caption="Source: Dataset from https://ourworldindata.org")+
  theme(text=element_text(colour="#EEEEEE"),
        title=element_text(colour="#EEEEEE",size=8,face = "bold"),
        plot.title=element_text(hjust=0.5),
        axis.title.x = element_blank(),
        panel.grid.minor.x=element_blank(),
        legend.background = element_blank(),
        legend.key= element_blank(),
        legend.position=c(0.095, 0.81),# legend at top-left, inside the plot
        plot.margin = unit(c(0.5,1.3,0.5,0.5), "cm"))+ 
  scale_x_date(date_labels = "%Y %m",date_breaks ="3 months")


plot2

#plot.animation=plot+
 #transition_reveal()+
  #view_follow(fixed_y=T)

animate(plot.animation, height=420,width=700,fps=30,duration=10,end_pause = 120,res=100,rewind=F)
anim_save(covid_cases.gif)