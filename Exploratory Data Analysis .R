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






