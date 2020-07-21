library(foreign)
library(tidyr)
library(dplyr)


#####################################################################################

dt <- file.choose()
mydata <- read.spss(dt, to.data.frame = TRUE)

# convert factored columns to character
mydata %>% mutate_if(is.factor, as.character) -> data


# convert misinterpreted date column into correct date format
spss2date <- function(x) as.Date(x/86400, origin = "1582-10-14")

mydata$LASTFU <- spss2date(mydata$LASTFU)
mydata$opedate <- spss2date(mydata$opedate)

# drop columns with NA and empty rows

newdat <- mydata[!(names(mydata) %in% c("hoten", "IDPAT", "tgmolai2", "TGmolai1", "complication", "autres", "aneauce", "imtype", "causedeath", "causedeath2", "plastie1", "plastie2", "og1", "notes2", "timedeath"))]

# rename columns
names(newdat)[c(1:5, 17, 20, 27, 30, 31, 33, 39:42)] <- c("ID", "Age", "Sex", "Weight", "Heart_rhythm", "Oper_date", "Ring_Group", "Heart_failure","Kidney_failure", "Death", "Last_followup", "Death2", "Redo", "INFO", "Cause_of_redo")

# replace NAs with correct values for GROUP and Ring_Group
newdat <- newdat %>% replace_na(list(GROUP = "NONE", Ring_Group = "NONE"))


# relevel columns 
newdat <- newdat %>% mutate_if(is.character, as.factor)

levels(newdat$INFO)[1] <- "Not specified"

levels(newdat$Heart_failure) <- c("NO", "YES", "MEDIUM")       

levels(newdat$Sex) <- c("M", "F")                    

levels(newdat$bandelet) <- c("YES", "NO")              

levels(newdat$Redo) <- c("NO", "YES")                  

levels(newdat$Kidney_failure) <- c("YES", "NO")

levels(newdat$Death) <- "alive"

levels(newdat$Death2) <- c("dead", "alive")

levels(newdat$pericaeffusion) <- c("YES", "NO")

# Categorize Age variable into groups
newdat <- newdat %>% mutate(Age_group = case_when(Age >= 0  & Age <= 4 ~ '0-4 yrs',
                                              Age >= 5  & Age <= 9 ~ '5-9 yrs',
                                              Age >= 10  & Age <= 14 ~ '10-14 yrs',
                                              Age >= 15  & Age <= 18 ~ '15-18 yrs'))
# save data to csv file
write.csv(newdat, "hospdat.csv")


