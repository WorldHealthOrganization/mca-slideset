library(readxl)
library(tidyverse)
library(gtools)

adol_cod <-  read_excel("C:/Users/kpeve/OneDrive - London School of Hygiene and Tropical Medicine/My OneDrive Documents/WHO/data/raw/adol_cod_2000-2019.xls", 
                        sheet = "estimates")

#columns not to pivot
no_pivot <- c("level", "region", "iso3", "whoname", "year", "model")

#pivot to very long
adol_cod_long <- adol_cod %>% pivot_longer(cols=-no_pivot)

#create age variable
adol_cod_long$age[str_detect(adol_cod_long$name, pattern = c("age5"))] <- "age 5-9" 
adol_cod_long$age[str_detect(adol_cod_long$name, pattern = c("age10"))] <- "age 10-14" 
adol_cod_long$age[str_detect(adol_cod_long$name, pattern = c("age15F"))] <- "females 15-19" 
adol_cod_long$age[str_detect(adol_cod_long$name, pattern = c("age15M"))] <- "males 15-19" 

#statistic
adol_cod_long$statistic <- case_when(str_detect(adol_cod_long$name, pattern = c("qx"))~"rate", str_detect(adol_cod_long$name, pattern = c("fage"))~"fraction", str_detect(adol_cod_long$name, pattern = c("rage"))~"rate", str_detect(adol_cod_long$name, pattern = c("age"))~"deaths")

#cause
adol_cod_long$cause <- case_when(
  str_detect(adol_cod_long$name, pattern = c('mea'))~'Measles',
  str_detect(adol_cod_long$name, pattern = c('hiv'))~'HIV/AIDS',
  str_detect(adol_cod_long$name, pattern = c('lri'))~'Lower respiratory infections',
  str_detect(adol_cod_long$name, pattern = c('tb'))~'Tuberculosis',
  str_detect(adol_cod_long$name, pattern = c('dia'))~'Diarrhoeal diseases',
  str_detect(adol_cod_long$name, pattern = c('mal'))~'Malaria',
  str_detect(adol_cod_long$name, pattern = c('mat'))~'Maternal',
  str_detect(adol_cod_long$name, pattern = c('cmpn'))~'Other communicable, maternal, perinatal, and nutritional causes',
  str_detect(adol_cod_long$name, pattern = c('con'))~'Congenital anomalies',
  str_detect(adol_cod_long$name, pattern = c('car'))~'Cardiovascular',
  str_detect(adol_cod_long$name, pattern = c('dig'))~'Digestive system',
  str_detect(adol_cod_long$name, pattern = c('neo'))~'Neoplasms',
  str_detect(adol_cod_long$name, pattern = c('ncd'))~'Other noncommunicable diseases',
  str_detect(adol_cod_long$name, pattern = c('int'))~'Interpersonal Violence',
  str_detect(adol_cod_long$name, pattern = c('sel'))~'Self-harm',
  str_detect(adol_cod_long$name, pattern = c('dro'))~'Drowning',
  str_detect(adol_cod_long$name, pattern = c('rti'))~'Road traffic injuries',
  str_detect(adol_cod_long$name, pattern = c('inj'))~'Other injuries',
  str_detect(adol_cod_long$name, pattern = c('nat'))~'Natural disasters',
  str_detect(adol_cod_long$name, pattern = c('col'))~'Collective Violence',
  TRUE~"All cause"
)

adol_cod_reshape <- adol_cod_long %>% dplyr::select(-c(name)) %>% pivot_wider(names_from = statistic, values_from = value)

#save
#write.csv(adol_cod_reshape, "C:/Users/kpeve/OneDrive - London School of Hygiene and Tropical Medicine/My OneDrive Documents/WHO/data/raw/adol_cod_2000_2019_reshape.csv", row.names = F)

#combine with under five

child_cod <- read.csv("C:/Users/kpeve/OneDrive - London School of Hygiene and Tropical Medicine/My OneDrive Documents/WHO/data/raw/child_cod_2000_2019_reshape_update220331.csv") 

names(adol_cod_reshape)
names(child_cod)

#add blank columns to match under five file
adol_cod_reshape <- adol_cod_reshape %>% mutate(whoreg6=NA, lb=NA, pnmeth=NA, neometh=NA)

#remove extra cols from under five
child_cod <- child_cod %>% dplyr::select(-c(cause)) %>% mutate(cause=cause_name, model=NA, region=NA) %>% dplyr::select(-c(cause_name))

#reorder columns
adol_cod_reshape <- adol_cod_reshape[,c("level", "iso3", "year", "whoreg6", "region", "whoname", "age", "deaths", "rate", "fraction", "cause", "lb", "pnmeth", "neometh", "model")]
child_cod <- child_cod[,c("level", "iso3", "year", "whoreg6", "region", "whoname", "age", "deaths", "rate", "fraction", "cause", "lb", "pnmeth", "neometh", "model")]


#combine 
combined_child_adol_cod_2000_2019_long_220419 <- rbind(child_cod, adol_cod_reshape)

#write.csv(combined_child_adol_cod_2000_2019_long_220419, "C:/Users/kpeve/OneDrive - London School of Hygiene and Tropical Medicine/My OneDrive Documents/WHO/data/raw/combined_child_adol_cod_2000_2019_long_220419.csv", row.names = F)
