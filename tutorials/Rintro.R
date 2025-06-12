#### Intro to R ####

## getting help

?mean()

## Variable assignment, arithmetic and logical operators 

x = 1
y <- 2
x
y

##

x + y       # x plus y
z <- y - x  # assign to z the value of y minus x
z           # show the value of z
is.na(z)    # is the value of z missing?
z < 3       # is the value of z less than 3?
z == 3      # is the value of z equal to 3?
z != 3      # is the value of z not equal to 3?

##

a <- c(3,5,7)   # assign 3, 5, and 7 to a
a               # show the value of a
a + 1           # add one to a (repeats for each value of a), this value is shown but not saved (no assignment operator)
max(a)          # what is the maximum 
a <- 7          # over write the value of a with 7
a               # show the value of a
rm(a)           # remove the value of a from the environment (can no longer be used)


## Data classes and structures

str(x)                        # structure of x
str("hello")                  # structure of "hello"
b <- list(1, TRUE, "hello")   # lists can hold multiple structures at the same time
str(b)                        # structure of b

##

myvector <- c(2,4,8,3,9,4)  # assign values to "myvector"
myvector                    # show the values of myvector
myvector[2]                 # show the second value of myvector
myvector[1:3]               # show the first three values of myvector
myvector[c(2,4)]            # show the second and fourth values
myvector<=4                 # which values of myvector are <= 4?
myvector[myvector<=4]       # show the values of myvector that are <= 4 

### Importing and exploring data
## Installing packages

install.packages("readxl")  # install package "readxl" to be able to read Excel files into R
library(readxl)             # load package readxl

## Importing Excel data

policySurvey <- read_excel("C:/Users/kpeve/OneDrive - London School of Hygiene and Tropical Medicine/My OneDrive Documents/WHO/Policy-survey/data/raw/PolicySurvey_All.labels.xlsx")

##

head(policySurvey)       # show the first six rows of policySurvey (it has too many columns to show all of them, but you'll see the first six rows of the first few columns and then a list of other columns). Note it also shows the data class of each column. The first several columns are "<chr>" for character strings. 
head(policySurvey$iso3) # show the first six values of the iso3 column
str(policySurvey$MN_42) # show the data class of the MN_42 variable

## Summarising data

table(policySurvey$who_region)          # see the frequencies for who_region
table(policySurvey$AD_26_e_1)           # table of AD_26_e_1 (age minors can receive mental health services without parent/legal consent)
mean(policySurvey$AD_26_e_1, na.rm=T)   # mean of AD_26_e_1, note because there are missing values, you must specify "na.rm=T" to remove (rm) NA values from the equation
summary(policySurvey$AD_26_e_1)         # summary shows the min, max, 1st/3rd quartiles, mean, median, and number of missing values (na.rm=T isn't needed)
min(policySurvey$AD_26_e_1, na.rm = T)  # show minimum value (note na.rm=T is needed), max() shows the maximum value

## Visualising data

install.packages("ggplot2", "stringr")
library(ggplot2)
library(stringr)

# a very simple plot
ggplot(data=policySurvey, aes(x=who_region))+   # aes is for aesthetics, bar plots only need an x or a y but not both, most other types of plots require x and y to be specified 
  geom_bar()                                    # define the type of plot

# let's customise it
ggplot(data=policySurvey, aes(x=str_wrap(who_region, 10), fill=who_region))+   # wrap the region labels after 10 characters to fit better 
  geom_bar() +
  labs(x="WHO Region", y="Number of countries responding to the survey", title="Regional representation in policy survey")+
  theme_classic() + # get rid of the grey tile background
  guides(fill = F)  # don't show the legend for colour

## Xmart data

install.packages("jsonlite", "httr")
library(jsonlite)
library(httr)

xmart <- fromJSON("https://frontdoor-l4uikgap6gz3m.azurefd.net/MNCAH/MCA_FACT_DATA?$filter=INDICATOR_FK%20in%20(%22AC_A4V%22,%22TECI_ORSZINC%22)&$select=INDICATOR_FK,YEAR_FK,COUNTRY_FK,COUNTRY_FK__CODE,ValueNumeric,ValueLow,ValueHigh,ValueStdDev,ValueStdErr,Comments,AGEGROUP_FK,Answer_FK,CauseGHE_FK,CauseGHEChild_FK,CauseENVCAUSE_FK,DatasourceLong,DatasourceShort,ImpSan_FK,ImpWater_FK,LivingArrangements_FK,MEASDEF_FK,MOTHEREDUCATION_FK,ProficiencyLevel_FK,ResideLTA_FK,RESIDENCEAREA_FK,SABDefinition_FK,SEVERITY_FK,SEVERITYHEARING_FK,SEVERITYVISION_FK,SEX_FK,WEALTHQUINTILE_FK")

ANC4_ORSZ <- as.data.frame(xmart$value)

# what are the column names of the data frame
names(ANC4_ORSZ)

## Data manipulation

install.packages("tidyverse")
library(tidyverse)

# Let's filter for just ANC4 rows
# the pipe operator ( %>% ) passes data from one function to the next to continue manipulating the same object. The keyboard shortcut is ctrl+shift+M

ANC4 <- ANC4_ORSZ %>% filter(INDICATOR_FK=="AC_A4V")

# Notice in the environment tab, you now have ANC$_ORSZ (2433 observations) and a second object ANC4 which is 839 observations, let's see what years have data for ANC4

table(ANC4$YEAR_FK)

#let's take just data from 2010 or later

ANC4 <- ANC4 %>% filter(YEAR_FK>=2010) # this replaces the previous ANC4 data frame with a data frame for ANC4 only including rows from 2010 or later
table(ANC4$YEAR_FK)

# how many countries have data

length(unique(ANC4$COUNTRY_FK__CODE))








