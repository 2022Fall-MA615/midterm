library(tidyverse)
library(magrittr)
library(readxl)
strawb <- read_xlsx('/Users/yaron/study/615/data/strawberries-2022oct30-a.xlsx',col_names = T)
# Q1
#285CWT = 28500LB

# Q2
result <- filter(strawb, State == 'CALIFORNIA' & 
               Year == 2016 & 
               Domain == 'ORGANIC STATUS')
c1 <- as.numeric(result$Value)
# CV = sd/mean
# mean = strawb_organic$Value[1]
# sd = strawb_organic$`CV (%)`[1] * mean
# 95%CI = (mean - 1.96 * sd, mean + 1.96 *sd)
#231304956 - 1.96 * 0.137 * 231304956
#231304956 + 1.96 * 0.137 * 231304956

# Q3
result_1 <- filter(strawb, State == 'CALIFORNIA' & 
               Year == 2016 & 
               Domain != 'ORGANIC STATUS')
result_2 <- filter(result_1, Value != "(NA)" & 
               Value != "(D)" & 
               Domain != "TOTAL")

#NA
# Q4
type_chemicals <- length(unique(strawb$`Domain Category`)) - 2
type_chemicals

# Q5
che_Flo <- filter(strawb, State == 'FLORIDA' & 
                             Domain != 'ORGANIC STATUS' & 
                             Domain != 'TOTAL')
che_Cali <- filter(strawb, State == 'CALIFORNIA' & 
                                Domain != 'ORGANIC STATUS' & 
                                Domain != 'TOTAL')
dif<-length(unique(che_Cali$`Domain Category`)) - length(unique(che_Flo$`Domain Category`))
dif