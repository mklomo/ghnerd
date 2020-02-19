
#Webscraping in r using rvest package and selector gadget
install.packages("rvest")
library(rvest)

#Reading the webpage in R
CPadmission <- read_html("http://admissions.calpoly.edu/prospective/profile.html")

#Saving All tables on the webpage

All_Tables <- CPadmission %>%
  html_nodes("table") %>%
  html_table()

#Selecting the First Table
Coll_admisions <- All_Tables[[1]]

#Selecting the Seventh Table
Coll_admisions_7 <- All_Tables[[7]] 



#TRIAL 2 ABEBOOKS WEBSCRAPING
larecherche <- read_html("https://www.abebooks.com/servlet/SearchResults?sts=t&cm_sp=SearchF-_-home-_-Results&kn=&an=Marcel+Proust&tn=A+la+recherche+du+temps+perdu&isbn=")
title_html <- html_nodes(larecherche, "#book-30 a span")
title_text <- html_text(title_html)
title_text <- readr::parse_character(title_html)
price_html <- html_nodes(larecherche, ".srp-item-price")
price_text <- html_text(price_html)
price_text <- readr::parse_number(price_text)
Book_Pricing_Data <- data.frame(title_text,price_text)



#TRIAL 3 edx.org webscraping
edx_subjects <- read_html("https://www.edx.org/subjects")
Subjects_html <- html_nodes(edx_subjects,".mb-4+ .mb-4 .align-items-center")
Subjects_text <- html_text(Subjects_html)










