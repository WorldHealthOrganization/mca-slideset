renv::restore()
source("requirements.R")

# child_cod_2000_2019 <- read_excel("C:/Users/kpeve/OneDrive - London School of Hygiene and Tropical Medicine/My OneDrive Documents/WHO/data/raw/child_cod_2000-2019.xls", 
#                                   sheet = "estimates")

child_cod_2000_2019 <- read_excel("C:/Users/kpeve/OneDrive - London School of Hygiene and Tropical Medicine/My OneDrive Documents/WHO/data/raw/child_cod_2000-2019_updated220331.xls", 
                                  sheet = "estimates")

#columns not to pivot
no_pivot <- c("iso3", "year", "level", "whoreg6", "whoname", "lb", "pnmeth", "neometh")

#pivot to very long
child_cod_2000_2019_long <- child_cod_2000_2019 %>% pivot_longer(cols=-no_pivot)

#create age variable
child_cod_2000_2019_long$age[str_detect(child_cod_2000_2019_long$name, pattern = c("neo"))] <- "neonatal" 
child_cod_2000_2019_long$age[child_cod_2000_2019_long$name=="nmr" | child_cod_2000_2019_long$name=="nnd"] <- "neonatal" 
child_cod_2000_2019_long$age[str_detect(child_cod_2000_2019_long$name, pattern = c("p"))] <- "postneonatal" 
child_cod_2000_2019_long$age[str_detect(child_cod_2000_2019_long$name, pattern = c("u"))] <- "under-5" 

#create cause variable
child_cod_2000_2019_long$cause <- child_cod_2000_2019_long$name
child_cod_2000_2019_long$cause <- str_replace_all(child_cod_2000_2019_long$cause, "fneo", "neo")
child_cod_2000_2019_long$cause <- str_replace_all(child_cod_2000_2019_long$cause, "rneo", "neo")
child_cod_2000_2019_long$cause <- str_replace_all(child_cod_2000_2019_long$cause, "fpost", "post")
child_cod_2000_2019_long$cause <- str_replace_all(child_cod_2000_2019_long$cause, "rpost", "post")
child_cod_2000_2019_long$cause <- str_replace_all(child_cod_2000_2019_long$cause, "fufive", "ufive")
child_cod_2000_2019_long$cause <- str_replace_all(child_cod_2000_2019_long$cause, "rufive", "ufive")
child_cod_2000_2019_long$cause[child_cod_2000_2019_long$name=="nmr" | child_cod_2000_2019_long$name=="pnmr" | child_cod_2000_2019_long$name=="u5mr" |
                                 child_cod_2000_2019_long$name=="nnd" | child_cod_2000_2019_long$name=="pnd" | child_cod_2000_2019_long$name=="u5d"
                               ] <- "all_cause"

#type of variable (deaths, rates, fractions)
deaths <- c('neo2', 'neo3', 'neo5', 'neo6', 'neo7', 'neo8', 'neo9', 'neo10', 'neo11', 'neo12', 'neo15', 'neo17', 'neo18', 'neo19', 'post2', 'post3', 'post5', 'post6', 'post7', 'post8', 'post9', 'post10', 'post11', 'post12', 'post13', 'post15', 'post16', 'post17', 'post18', 'ufive2', 'ufive3', 'ufive5', 'ufive6', 'ufive7', 'ufive8', 'ufive9', 'ufive10', 'ufive11', 'ufive12', 'ufive15', 'ufive17', 'ufive18', 'ufive19', "nnd", "pnd", "u5d")

rates <- c('rneo2', 'rneo3', 'rneo5', 'rneo6', 'rneo7', 'rneo8', 'rneo9', 'rneo10', 'rneo11', 'rneo12', 'rneo15', 'rneo17', 'rneo18', 'rneo19', 'rpost2', 'rpost3', 'rpost5', 'rpost6', 'rpost7', 'rpost8', 'rpost9', 'rpost10', 'rpost11', 'rpost12', 'rpost13', 'rpost15', 'rpost16', 'rpost17', 'rpost18', 'rufive2', 'rufive3', 'rufive5', 'rufive6', 'rufive7', 'rufive8', 'rufive9', 'rufive10', 'rufive11', 'rufive12', 'rufive15', 'rufive17', 'rufive18', 'rufive19', "nmr", "pnmr", "u5mr")

fraction <- c('fneo2', 'fneo3', 'fneo5', 'fneo6', 'fneo7', 'fneo8', 'fneo9', 'fneo10', 'fneo11', 'fneo12', 'fneo15', 'fneo17', 'fneo18', 'fneo19', 'fpost2', 'fpost3', 'fpost5', 'fpost6', 'fpost7', 'fpost8', 'fpost9', 'fpost10', 'fpost11', 'fpost12', 'fpost13', 'fpost15', 'fpost16', 'fpost17', 'fpost18', 'fufive2', 'fufive3', 'fufive5', 'fufive6', 'fufive7', 'fufive8', 'fufive9', 'fufive10', 'fufive11', 'fufive12', 'fufive15', 'fufive17', 'fufive18', 'fufive19')

#child_cod_2000_2019_long$type[child_cod_2000_2019_long$name %in% c("nmr", "nnd", "pnmr", "pnd", "u5mr", "u5d")] <- "other"
child_cod_2000_2019_long$type[child_cod_2000_2019_long$name %in% deaths] <- "deaths"
child_cod_2000_2019_long$type[child_cod_2000_2019_long$name %in% rates] <- "rate"
child_cod_2000_2019_long$type[child_cod_2000_2019_long$name %in% fraction] <- "fraction"

#remove name column so reshape possible
child_cod_2000_2019_long$name <- NULL

#pivot wider
child_cod_2000_2019_reshape <- child_cod_2000_2019_long %>% pivot_wider(names_from = type, values_from = value)

#cause name
child_cod_2000_2019_reshape$cause_name <- case_when(
  str_detect(child_cod_2000_2019_reshape$cause, "19") ~ "Other group 1 and other noncommunicable",
  str_detect(child_cod_2000_2019_reshape$cause, "18") ~ "Tuberculosis",
  str_detect(child_cod_2000_2019_reshape$cause, "17") ~ "Injuries",
  str_detect(child_cod_2000_2019_reshape$cause, "16") ~ "Other noncommunicable diseases",
  str_detect(child_cod_2000_2019_reshape$cause, "15") ~ "Congenital anomalies",
  str_detect(child_cod_2000_2019_reshape$cause, "13") ~ "Other group 1",
  str_detect(child_cod_2000_2019_reshape$cause, "12") ~ "Sepsis and other infectious conditions of the newborn",
  str_detect(child_cod_2000_2019_reshape$cause, "11") ~ "Birth asphyxia and birth trauma",
  str_detect(child_cod_2000_2019_reshape$cause, "10") ~ "Prematurity",
  str_detect(child_cod_2000_2019_reshape$cause, "9") ~ "Acute respiratory infections",
  str_detect(child_cod_2000_2019_reshape$cause, "8") ~ "Malaria",
  str_detect(child_cod_2000_2019_reshape$cause, "7") ~ "Meningitis/encephalitis",
  str_detect(child_cod_2000_2019_reshape$cause, "6") ~ "Measles",
  str_detect(child_cod_2000_2019_reshape$cause, "5") ~ "Tetanus",
  str_detect(child_cod_2000_2019_reshape$cause, "3") ~ "Diarrhoeal diseases",
  str_detect(child_cod_2000_2019_reshape$cause, "2") ~ "HIV/AIDS", 
  str_detect(child_cod_2000_2019_reshape$cause, "all_cause") ~ "All cause"
)

#save
#write.csv(child_cod_2000_2019_reshape, "C:/Users/kpeve/OneDrive - London School of Hygiene and Tropical Medicine/My OneDrive Documents/WHO/data/raw/child_cod_2000_2019_reshape.csv", row.names = F)

#write.csv(child_cod_2000_2019_reshape, "C:/Users/kpeve/OneDrive - London School of Hygiene and Tropical Medicine/My OneDrive Documents/WHO/data/raw/child_cod_2000_2019_reshape_update220331.csv", row.names = F)

#5-19s for unicef



## get population data from ghe ## 

#load parquet data
pq_dataset <- open_dataset("C:/Users/kpeve/OneDrive - London School of Hygiene and Tropical Medicine/My OneDrive Documents/WHO/data/raw/dths_yld_daly.parquet", format = "parquet")

#filter for 
pop_ghe <- pq_dataset %>% dplyr::select(iso3, year, age, sex, causename, pop) %>% filter(age < 5 & age!=0) %>% filter(causename=="All Causes") %>% collect()

#add male + female
pop_ghe <- pop_ghe %>% group_by(iso3, age, year) %>% summarise(pop=sum(pop)) 

## create matching age groups
#neonatal 
pop_ghe_neonatal <- pop_ghe %>% filter(age==.1) %>% mutate(age="neonatal")

#postneonatal (.11 + 1)
pop_ghe_postneonatal <- pop_ghe %>% filter(age==.11|age==1) %>% group_by(iso3, year) %>% summarise(pop=sum(pop)) %>% mutate(age="postneonatal")

#under-5 (.1 + .11 + 1)
pop_ghe_under5 <- pop_ghe %>% filter(age==.1|age==.11|age==1) %>% group_by(iso3, year) %>% summarise(pop=sum(pop)) %>% mutate(age="under-5")

#bind all age groups
pop_ghe <- rbind(pop_ghe_neonatal, pop_ghe_postneonatal, pop_ghe_under5)

# join countries

child_cod_2000_2019_reshape_pop <- left_join(child_cod_2000_2019_reshape, pop_ghe, by=c("iso3", "age", "year"))


## for regional data 

#country/area list from xmart: https://extranet.who.int/xmart4/REFMART/data/REF_COUNTRY
xmart_refCountry <- fromJSON("https://frontdoor-l4uikgap6gz3m.azurefd.net/REFMART/REF_COUNTRY")
#INDICATOR_FK,YEAR_FK,ValueNumeric,COUNTRY_FK ValueLow ValueHigh ValueStdDev ValueStdErr Comments COUNTRY_FK__CODE DatasourceLong RESIDENCEAREA_FK__CODE SEX_FK WEALTHQUINTILE_FK

ref_country <- as.data.frame(xmart_refCountry$value)

ref_country$region <- case_when(
  ref_country$GRP_WHO_REGION=="AFR" ~ "1_Afr",
  ref_country$GRP_WHO_REGION=="AMR" ~ "2_Amr",
  ref_country$GRP_WHO_REGION=="EMR" ~ "5_Emr",
  ref_country$GRP_WHO_REGION=="EUR" ~ "4_Eur",
  ref_country$GRP_WHO_REGION=="SEAR" ~ "3_Sear",
  ref_country$GRP_WHO_REGION=="WPR" ~ "6_Wpr"
)

#saveRDS(ref_country, "data/ref_country.rds")
#ref_country <- read_rds("data/ref_country.rds")

pop_ghe <- left_join(pop_ghe, ref_country[,c("CODE_ISO_3", "region")], by=c("iso3"="CODE_ISO_3"))

pop_ghe_region <- pop_ghe %>% group_by(region, age, year) %>% summarise(pop=sum(pop)) %>% rename(whoreg6=region)
pop_ghe_region$level <- "region"

#join regions

child_cod_2000_2019_reshape_pop <- left_join(child_cod_2000_2019_reshape_pop, pop_ghe_region, by=c("whoreg6", "age", "year", "level"))


## global ##

pop_ghe_global <- pop_ghe %>% group_by(age, year) %>% summarise(pop=sum(pop))
pop_ghe_global$level <- "global"

# join 

child_cod_2000_2019_reshape_pop <- left_join(child_cod_2000_2019_reshape_pop, pop_ghe_global, by=c("level", "age", "year"))

#check there aren't any with pop in two columns 
table(!is.na(child_cod_2000_2019_reshape_pop$pop.x) & !is.na(child_cod_2000_2019_reshape_pop$pop.y))
table(!is.na(child_cod_2000_2019_reshape_pop$pop.x) & !is.na(child_cod_2000_2019_reshape_pop$pop))
table(!is.na(child_cod_2000_2019_reshape_pop$pop) & !is.na(child_cod_2000_2019_reshape_pop$pop.y))

#merge to one col
child_cod_2000_2019_reshape_pop$x <- rowSums(child_cod_2000_2019_reshape_pop[,c("pop", "pop.x", "pop.y")], na.rm = T)

#if all NA then make NA
child_cod_2000_2019_reshape_pop$x[is.na(child_cod_2000_2019_reshape_pop$pop) & is.na(child_cod_2000_2019_reshape_pop$pop.x) & is.na(child_cod_2000_2019_reshape_pop$pop.y)] <- NA

#remove extraneous cols
child_cod_2000_2019_reshape_pop <- child_cod_2000_2019_reshape_pop %>% dplyr::select(-c(pop, pop.x, pop.y)) %>% rename(pop=x)

#save
#write.csv(child_cod_2000_2019_reshape_pop, "C:/Users/kpeve/OneDrive - London School of Hygiene and Tropical Medicine/My OneDrive Documents/WHO/data/raw/child_cod_2000_2019_reshape_pop.csv", row.names = F)