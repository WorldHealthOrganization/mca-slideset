library(jsonlite)
library(httr)

httr::GET("https://extranet.who.int/xmart-api/odata/MNCAH/REF_CauseGHEChild?format=streaming")

x <- httr::GET("https://frontdoor-l4uikgap6gz3m.azurefd.net/MNCAH/MCA_FACT_DATA_AGGREGATED?$top=20")
x$content
y <- fromJSON("https://frontdoor-l4uikgap6gz3m.azurefd.net/MNCAH/REF_CauseGHEChild")

names(y$value)

a <- as.data.frame(y$value)

a

b <- content(x, as = "parsed")
as.data.frame(b$value)


y <- fromJSON("https://frontdoor-l4uikgap6gz3m.azurefd.net/MNCAH/MCA_FACT_DATA?$filter=INDICATOR_FK%20in%20(%22CANUT_EIBF%22)")

y <- fromJSON("https://frontdoor-l4uikgap6gz3m.azurefd.net/MNCAH/MCA_FACT_DATA?$filter=INDICATOR_FK%20in%20(%22CANUT_EIBF%22)&$select=INDICATOR_FK,YEAR_FK,ValueNumeric&$filter=COUNTRY_FK%20in%20(%Afghanistan%22)")

y <- fromJSON("https://frontdoor-l4uikgap6gz3m.azurefd.net/MNCAH/MCA_FACT_DATA?$filter=INDICATOR_FK%20in%20(%22CANUT_EIBF%22)&$select=INDICATOR_FK,YEAR_FK,ValueNumeric,COUNTRY_FK")

y <- fromJSON("https://frontdoor-l4uikgap6gz3m.azurefd.net/MNCAH/MCA_FACT_DATA?$filter=INDICATOR_FK%20in%20(%22CANUT_EIBF%22)%20and%20COUNTRY_FK%20in%20(%22Afghanistan%22)")

y <- fromJSON("https://frontdoor-l4uikgap6gz3m.azurefd.net/MNCAH/MCA_FACT_DATA?$filter=INDICATOR_FK%20in%20(%22CANUT_EIBF%22)%20and%20COUNTRY_FK%20in%20(%22Afghanistan%22)&$select=INDICATOR_FK,YEAR_FK,ValueNumeric,COUNTRY_FK")

a <- as.data.frame(y$value)
a

y <- fromJSON("https://frontdoor-l4uikgap6gz3m.azurefd.net/MNCAH/MCA_FACT_DATA?$filter=INDICATOR_FK%20in%20(%22CANUT_EIBF%22,%22WCAH_ANCSYP%22,%22CANUT_EBF%22)%20and%20COUNTRY_FK%20in%20(%22Afghanistan%22)&$select=INDICATOR_FK,YEAR_FK,ValueNumeric,COUNTRY_FK")

a <- as.data.frame(y$value)
a

## mnh coverage data



