library(readxl)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(ggeffects)
library(ggpubr)
library(stargazer)

dat = read.csv("datasets/covid.csv")
dat$date = as.Date(dat$date)


variants = read.csv("datasets/variants.csv")
variants = variants[variants$variant %in% c("Alpha", "Beta", "Delta", "Gamma","kappa", "Lambda", "Omicron", "others"),]
variants$date = as.Date(variants$date)

countries = c("United States", "China", "United Kingdom", 
              "Brazil", "United Arab Emirates", "Canada", "Italy",
              "Japan", "Russia", "South Africa", "France", "Germany",
              "Turkey", "Portugal", "Singapore", "Vietnam", "Chile",
              "Thailand", "Philippines", "Iran", "Mexico", "India")
countries1 = c("United Kingdom", "India", "South Africa", "United States", "China", "Germany", "Italy")
dat_sub = dat[dat$location %in% countries1, ]

# figure 1
p1 = ggplot(data = variants)+
 geom_point(aes(x = date, y = log10(num_sequences), color = variant), alpha = 0.1)+
 scale_x_date(date_minor_breaks = "1 month", date_labels = "%Y-%m-%d")+
 guides(colour = guide_legend(override.aes = list(alpha = 0.7)))+
 ylab("log10(number of new cases)")+
 labs(caption = "number of cases for each country in the scale of log10")
p1



# figure 3
US_variants = variants[variants[ ,"location"] == "United States",]
p3 = ggplot(data = US_variants)+
 geom_path(aes(x = date, y = perc_sequences, color = variant))+
 scale_x_date(date_minor_breaks = "1 month", date_labels = "%Y-%m-%d")+ 
 ylab("percentage out of all cases(US)")
UK_variants = variants[variants[ ,"location"] == "United Kingdom",]
p4 = ggplot(data = UK_variants)+
 geom_path(aes(x = date, y = perc_sequences, color = variant))+
 scale_x_date(date_minor_breaks = "1 month", date_labels = "%Y-%m-%d")+ 
 ylab("percentage out of all cases(UK)")
India_variants = variants[variants[ ,"location"] == "India",]
p5 = ggplot(data = India_variants)+
 geom_path(aes(x = date, y = perc_sequences, color = variant))+
 scale_x_date(date_minor_breaks = "1 month", date_labels = "%Y-%m-%d")+ 
 ylab("percentage out of all cases(India)")
ggarrange(p3, p4, p5, labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)


# figure 4
p6 = ggplot(data = dat_sub[dat_sub[,"date"] > "2020-10-01",])+
 geom_path(aes(x = date, 
               y = people_fully_vaccinated_per_hundred, color = location), alpha = 1)+
 scale_x_date(date_minor_breaks = "1 month", date_labels = "%Y-%m-%d")
p7 = ggplot(data = dat_sub[dat_sub[,"date"] > "2020-01-22",])+
 geom_point(aes(x = date, y = new_cases, color = location), 
            alpha = 0.6, size = 0.3)+
 geom_vline(xintercept = as.Date("2020-12-18"), 
            color = "grey25", linetype="dotted")+
 geom_text(mapping = aes(x = as.Date("2020-12-18"), y = 400000,
                         label = "Alpha", hjust = 1,
                         vjust = 1), color = "grey25", size = 3)+
 geom_vline(xintercept = as.Date("2021-05-06"), 
            color = "grey25", linetype="dotted")+
 geom_text(mapping = aes(x = as.Date("2021-05-06"), y = 400000,
                         label = "Delta", hjust = "inward",
                         vjust = "inward"), color = "grey25", size = 3)+
 geom_vline(xintercept = as.Date("2021-11-26"), 
            color = "grey25", linetype = "dotted")+
 geom_text(mapping = aes(x = as.Date("2021-11-26"), y = 400000,
                         label = "Omicron", hjust = "inward",
                         vjust = "inward"), color = "grey25", size = 3)+
 scale_x_date(date_minor_breaks = "3 months", date_labels = "%Y-%m-%d")+
 ylab("log10(number of new cases)")+
 labs(caption = "Vertical lines represents the date that a variant is designated as 'variant of concern' (VOC) by WHO")
ggarrange(p6, p7, labels = c("A", "B"), ncol = 1, nrow = 2)



# table 1
regression = lm(new_cases_per_million ~ people_fully_vaccinated_per_hundred,
                data = dat[dat[,"date"] == "2021-12-16",])
summary(regression)
stargazer(regression ,header=FALSE,title="new_cases ~ vaccination_rate", 
          type='latex',digits=3)


# figure 5
US = dat[dat[ ,"location"] == "United States",]

ggplot(data = US)+ 
 geom_point(aes(x = people_fully_vaccinated_per_hundred,
                y = new_cases_per_million), color = "grey50")+
 geom_smooth(aes(x = people_fully_vaccinated_per_hundred,
                 y = new_cases_per_million))

