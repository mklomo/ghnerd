#Packages used
library(cowplot)
library(tidyverse)
library(dplyr)
library(ggplot2)


#LOAD THE DATA
bihar_data <- read_csv("Bihar_sample_data.csv")

#KEEP ONLY FEMALES
bihar_data_females <- filter(bihar_data, adult == 1, female == 1)

#Have a look
head(bihar_data_females)

#Default Histogram
ggplot(bihar_data_females, aes(height_cm))+
  geom_histogram()

#Removing Outliers
h_1 <- ggplot(bihar_data_females, aes(height_cm))+
  geom_histogram()

h_1 + geom_vline(xintercept =  c(100, 200), color = "red", linetype = "dashed" ,size = 2)
ggsave("Bihar_Data_Outliers.pdf")

#Truncating Outliers
bihar_data_females_trunc <- filter(bihar_data_females, height_cm > 100, height_cm < 200)

#Plotting histogram
ggplot(bihar_data_females_trunc, aes(height_cm))+
  geom_histogram(color = "darkblue", fill = "blue")+
  xlab("Height in cm, Bihar Females")
ggsave("Bihar Females Outliers Removed.pdf")


#Analyzing binwidth
bihar_1 <- ggplot(bihar_data_females_trunc, aes(height_cm))+
  geom_histogram(color = "darkblue", fill = "blue", binwidth = 5)+
  xlab("Binwidth = 5")


bihar_2 <- ggplot(bihar_data_females_trunc, aes(height_cm))+
  geom_histogram(color = "darkblue", fill = "blue", binwidth = 10)+
  xlab("Binwidth = 10")


bihar_3 <- ggplot(bihar_data_females_trunc, aes(height_cm))+
  geom_histogram(color = "darkblue", fill = "blue", binwidth = 20)+
  xlab("Binwidth = 20")

bihar_4 <- ggplot(bihar_data_females_trunc, aes(height_cm))+
  geom_histogram(color = "darkblue", fill = "blue", binwidth = 50)+
  xlab("Binwidth = 50")

plot_grid(bihar_1, bihar_2, bihar_3, bihar_4, vjust = 0.2, hjust = -1)
ggsave("Playing with the bins.pdf")


#Height Examination of US Women
us_data <- read_csv("US_sample_data.csv")

head(us_data)

#Filtering for Adult Women
us_data_women_trunc <- filter(us_data, adult == 1, female == 1, height_cm < 200, height_cm > 120)

ggplot(us_data_women_trunc, aes(height_cm))+
  geom_histogram(color = "Black", fill = "red")+
  xlab("Height in cm of US Women")

ggsave("Height of US Women.pdf")

#Kernel Density Estimation
ggplot(us_data_women_trunc, aes(height_cm))+
  geom_histogram(aes(height_cm, ..density..), fill = "white", color = "darkred")+
  geom_density(kernel = "gaussian")

ggsave("US Kernel.pdf")


#Playing with Bandwidth
US_1 <- ggplot(us_data_women_trunc, aes(height_cm))+
  geom_histogram(aes(height_cm, ..density..), fill = "white", color = "darkred")+
  geom_density(kernel = "gaussian", bw=1)+
  xlab("bw=1")

#Playing with Bandwidth
US_1 <- ggplot(us_data_women_trunc, aes(height_cm))+
  geom_histogram(aes(height_cm, ..density..), fill = "white", color = "darkred")+
  geom_density(kernel = "gaussian", bw=1)+
  xlab("bw=1")

US_2 <- ggplot(us_data_women_trunc, aes(height_cm))+
  geom_histogram(aes(height_cm, ..density..), fill = "white", color = "darkred")+
  geom_density(kernel = "gaussian", bw=5)+
  xlab("bw=5")

US_3 <- ggplot(us_data_women_trunc, aes(height_cm))+
  geom_histogram(aes(height_cm, ..density..), fill = "white", color = "darkred")+
  geom_density(kernel = "gaussian", bw=10)+
  xlab("bw=10")

US_4 <- ggplot(us_data_women_trunc, aes(height_cm))+
  geom_histogram(aes(height_cm, ..density..), fill = "white", color = "darkred")+
  geom_density(kernel = "gaussian", bw=20)+
  xlab("bw=20")

plot_grid(US_1, US_2, US_3, US_4, labels = "Kernel Desity with Different Binwidths", hjust = -1, vjust = 0.2)
ggsave("Bnadwidth effect on Kernel using US data.pdf")



#Combining the 2 histograms
ggplot(bihar_data_females_trunc, aes(height_cm))+
  geom_histogram(fill = "blue", color ="darkblue")+
  geom_histogram(data = us_data_women_trunc, aes(height_cm), fill = "red", color = "darkred")
#makes no sense to do as count. Because sample size distorts the essence of the data shown


#more visible as points
ggplot(bihar_data_females_trunc, aes(height_cm))+
  geom_freqpoly(fill = "blue", color ="darkblue")+
  geom_freqpoly(data = us_data_women_trunc, aes(height_cm), fill = "red", color = "darkred")+
  xlab("Height in cm")

ggsave("compare_histograms.pdf")

#Comparing with CDF
ggplot(bihar_data_females_trunc, aes(height_cm))+
  stat_ecdf(color = "darkblue")+
  stat_ecdf(data = us_data_women_trunc, aes(height_cm), color = "darkred")+
  xlab("Height in centimeters")

ggsave("CDF graphs on data.pdf")


















