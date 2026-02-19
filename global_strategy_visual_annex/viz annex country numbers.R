
#renv::restore()
source("requirements.R")
library(rio)

# country/area list from xmart: https://extranet.who.int/xmart4/REFMART/data/REF_COUNTRY
xmart_refCountry <- fromJSON("https://frontdoor-l4uikgap6gz3m.azurefd.net/REFMART/REF_COUNTRY")
# INDICATOR_FK,YEAR_FK,ValueNumeric,COUNTRY_FK ValueLow ValueHigh ValueStdDev ValueStdErr Comments COUNTRY_FK__CODE DatasourceLong RESIDENCEAREA_FK__CODE SEX_FK WEALTHQUINTILE_FK

ref_country <- as.data.frame(xmart_refCountry$value)
ref_country_members <- ref_country %>% filter(WHO_LEGAL_STATUS == "M")

# saveRDS(ref_country, "data/ref_country.rds")
# ref_country <- read_rds("data/ref_country.rds")

ref_country$region <- case_when(
  ref_country$GRP_WHO_REGION == "AFR" ~ "African Region",
  ref_country$GRP_WHO_REGION == "AMR" ~ "Region of the Americas",
  ref_country$GRP_WHO_REGION == "SEAR" ~ "South-East Asian Region",
  ref_country$GRP_WHO_REGION == "EUR" ~ "European Region",
  ref_country$GRP_WHO_REGION == "EMR" ~ "Eastern Mediterranean Region",
  ref_country$GRP_WHO_REGION == "WPR" ~ "Western Pacific Region"
)

ref_country$income[ref_country$GRP_WB_INCOME=="HIC"] <- "High"
ref_country$income[ref_country$GRP_WB_INCOME=="UMC"] <- "Upper-middle"
ref_country$income[ref_country$GRP_WB_INCOME=="LMC"] <- "Lower-middle"
ref_country$income[ref_country$GRP_WB_INCOME=="LIC"] <- "Low"

ref_country <- ref_country %>% mutate(income=factor(income, levels=c("High", "Upper-middle", "Lower-middle", "Low")))


## mmr 

xmart <- fromJSON("https://frontdoor-l4uikgap6gz3m.azurefd.net/MNCAH/MCA_FACT_DATA?$filter=INDICATOR_FK%20in%20(%22BD_MMR%22,%22BD_NMD%22)")
mmr <- as.data.frame(xmart$value) 
mmr <- mmr %>% filter(YEAR_FK==2017) %>% filter(INDICATOR_FK__CODE=="BD_MMR") %>% left_join(ref_country, by=c("COUNTRY_FK__CODE"="CODE_ISO_3")) %>% filter(WHO_LEGAL_STATUS=="M")

## stillbirth

stillbirth_sb <- read_excel("data/external/Stillbirth-rates-and-deaths_2020_downloadUNICEF220407.xlsx", sheet="Country stillbirth",
                            skip = 14) %>% filter(`Uncertainty.Bounds*`=="Median")%>% left_join(ref_country, by=c("ISO.Code"="CODE_ISO_3")) %>% filter(WHO_LEGAL_STATUS=="M")

## nmr, u5mr

mort_rates_2021_inc <- read_excel("C:/Users/kpeve/OneDrive - London School of Hygiene and Tropical Medicine/My OneDrive Documents/WHO/data/external/UNIGME_data_for_WHO_2021.xlsx", sheet = "UNIGME_Countrydata", skip=6) %>% filter(Sex=="Total") %>% 
  filter(Year==2020.5)%>% left_join(ref_country, by=c("ISO.Code"="CODE_ISO_3")) %>% filter(WHO_LEGAL_STATUS=="M")

table(mort_rates_2021_inc$Indicator)

## adol mr

All_cause_All_years <- read_dta("data/raw/All cause_All years.dta")

adolmr <- All_cause_All_years %>% filter(age ==1) %>% filter(sex == 3) %>% filter(whoname!="") %>% filter(year==2019)


## coverage

xmart <- fromJSON("https://frontdoor-l4uikgap6gz3m.azurefd.net/MNCAH/MCA_FACT_DATA?$filter=INDICATOR_FK%20in%20(%22CANUT_U5S-2SD%22,%22BD_AFR%22,%22UHC_INDEX_REPORTED_UHC_SCI_RMNCH%22,%22GS_SH_LGR_ACSRHE%22)")
# INDICATOR_FK,YEAR_FK,ValueNumeric,COUNTRY_FK ValueLow ValueHigh ValueStdDev ValueStdErr Comments COUNTRY_FK__CODE DatasourceLong RESIDENCEAREA_FK__CODE SEX_FK WEALTHQUINTILE_FK

coverage.outcomes <- as.data.frame(xmart$value) %>% left_join(ref_country, by=c("COUNTRY_FK__CODE"="CODE_ISO_3")) %>% filter(WHO_LEGAL_STATUS=="M") %>% filter(YEAR_FK>=2019& YEAR_FK<2021) %>% filter(MEASDEF_FK=="INDEX"| is.na(MEASDEF_FK)) %>% group_by(COUNTRY_FK__CODE, INDICATOR_FK) %>% slice_max(order_by = (YEAR_FK), n = 1)

table(coverage.outcomes$INDICATOR_FK)

## fuels

xmart <- fromJSON("https://frontdoor-l4uikgap6gz3m.azurefd.net/MNCAH/MCA_FACT_DATA?$filter=INDICATOR_FK%20in%20(%22SDGPOLLUTINGFUELS%22)")
# INDICATOR_FK,YEAR_FK,ValueNumeric,COUNTRY_FK ValueLow ValueHigh ValueStdDev ValueStdErr Comments COUNTRY_FK__CODE DatasourceLong RESIDENCEAREA_FK__CODE SEX_FK WEALTHQUINTILE_FK

fuels <- as.data.frame(xmart$value) %>% filter(RESIDENCEAREA_FK__CODE=="ALL")%>% filter(YEAR_FK>=2019) %>%  left_join(ref_country, by=c("COUNTRY_FK__CODE"="CODE_ISO_3")) %>% filter(WHO_LEGAL_STATUS=="M")%>% group_by(COUNTRY_FK__CODE, INDICATOR_FK) %>% slice_max(order_by = (YEAR_FK), n = 1)

## domestic expenditure

xmart <- fromJSON("https://frontdoor-l4uikgap6gz3m.azurefd.net/MNCAH/MCA_FACT_DATA?$filter=INDICATOR_FK%20in%20(%22GS_OOPSCHE%22,%22GS_GOVHEALTHSPEND%22)")
# INDICATOR_FK,YEAR_FK,ValueNumeric,COUNTRY_FK ValueLow ValueHigh ValueStdDev ValueStdErr Comments COUNTRY_FK__CODE DatasourceLong RESIDENCEAREA_FK__CODE SEX_FK WEALTHQUINTILE_FK

expenditures <- as.data.frame(xmart$value)

expenditures <- left_join(expenditures, ref_country, by=c("COUNTRY_FK__CODE"="CODE_ISO_3")) %>% filter(WHO_LEGAL_STATUS=="M")%>% filter(YEAR_FK>=2019) %>%  group_by(COUNTRY_FK__CODE, INDICATOR_FK) %>% slice_max(order_by = (YEAR_FK), n = 1)
table(expenditures$INDICATOR_FK)

## breg, school

xmart <- fromJSON("https://frontdoor-l4uikgap6gz3m.azurefd.net/MNCAH/MCA_FACT_DATA?$filter=INDICATOR_FK%20in%20(%22WAS_BAS_SAF_WAS_HNDF_SOWT%22,%22RC_BRU5%22,%22WCAH_POCAYP%22)")
# INDICATOR_FK,YEAR_FK,ValueNumeric,COUNTRY_FK ValueLow ValueHigh ValueStdDev ValueStdErr Comments COUNTRY_FK__CODE DatasourceLong RESIDENCEAREA_FK__CODE SEX_FK WEALTHQUINTILE_FK

transform <- as.data.frame(xmart$value)

breg <- transform %>% filter(INDICATOR_FK=="RC_BRU5" & SEX_FK=="BTSX") %>% group_by(COUNTRY_FK) %>% slice_max(order_by = (YEAR_FK), n = 1) %>% filter(YEAR_FK>=2019) %>%  left_join(ref_country, by=c("COUNTRY_FK__CODE"="CODE_ISO_3")) %>% filter(WHO_LEGAL_STATUS=="M")

school <- transform %>% filter(INDICATOR_FK=="WCAH_POCAYP" & SEX_FK=="BTSX" & YEAR_FK>=2019) %>%left_join(ref_country, by=c("COUNTRY_FK__CODE"="CODE_ISO_3")) %>% filter(WHO_LEGAL_STATUS=="M")%>%  group_by(COUNTRY_FK__CODE, INDICATOR_FK) %>% slice_max(order_by = (YEAR_FK), n = 1)
length(unique(school$COUNTRY_FK))

## sanitation

sanitation <- transform %>% filter(INDICATOR_FK=="WAS_BAS_SAF_WAS_HNDF_SOWT" & RESIDENCEAREA_FK=="ALL" & YEAR_FK>=2019)  %>%left_join(ref_country, by=c("COUNTRY_FK__CODE"="CODE_ISO_3")) %>% filter(WHO_LEGAL_STATUS=="M") %>% group_by( COUNTRY_FK, MEASDEF_FK) %>% slice_max(order_by = (YEAR_FK), n = 1) 
length(unique(sanitation$COUNTRY_FK))
