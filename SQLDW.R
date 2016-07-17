#setwd("G:/code/lm-example")
setwd("~/Desktop/Code/lm-example")

require(microbenchmark)
require(stringr)

#aaldata$zip[1]
#aaldata$zip[i] <- 7.996 # You can use $, [], or a hybrid notation to locate data
#d#ata$VAR3[1] # Value corrected to 7.996

SPRINTF <- function(x) sprintf("%02d", x)
FORMATC <- function(x) formatC(x, width = 2,flag = 0)
STR_PAD <- function(x) str_pad(x, width=5, side="left", pad="0")

#####
## REFRESH DATA FROM THE DATABASE
####

library(RODBC)
dbhandle <- odbcDriverConnect('Driver={SQL Server Native Client 11.0};Server=tcp:libertydweast2.database.windows.net,1433;Database=LibertyDWPoC;Uid=libertydwpoc@libertydweast2;Pwd=Lib@zur3POC;Encrypt=yes;TrustServerCertificate=no;Connection Timeout=30')

#zipdata = read.csv("data/zip_codes_states.csv")  # read csv file
#zipdata

### TEST 1
##select top 100 * from [dbo].[exp_MasterTable_AIR] ;
test1 <- sqlQuery(dbhandle, 'select top 200 * from [dbo].[FINAL_pol_MasterTable_AIR]')
write.csv(test1, file = "test1.csv")
head(test1)

### TEST 2
##select top 100 * from [dbo].[Master_TABLE_AIR_LM_ALL] ;
test2 <- sqlQuery(dbhandle, 'select top 100 * from [dbo].[Master_TABLE_AIR_LM_ALL]')
write.csv(test2, file = "test2.csv")
head(test2)

## EXPOSURE DATA
##---Exposure count and TIV by ZIP all groups
expdata <- sqlQuery(dbhandle, 'select zip, count(*), sum(Limit1+Limit2+Limit3+Limit4) from [dbo].[exp_MasterTable_AIR] group by zip order by zip')
#write.csv(expdata, file = "expdata.csv")
# Write CSV in R
for (i in 1:10000){
  expdata$zip[i] <- str_pad(toString(expdata$zip[i]), width=5, side="left", pad="0")
}
write.table(expdata, file = "expdata1.csv",row.names=FALSE, na="",col.names=TRUE, sep=",")
head(expdata)

## AAL BY ALL ZIPS
##---AAL By Zip all groups
aaldata <- sqlQuery(dbhandle, 'select cast(zip as char(5)) as zip, sum(GrossLoss)/10000 as Loss from [dbo].[Master_TABLE_AIR_LM_ALL] group by zip order by zip')
for (i in 1:10000){
  aaldata$zip[i] <- str_pad(toString(aaldata$zip[i]), width=5, side="left", pad="0")
}
#write.csv(aaldata, file = "aaldata.csv")
write.table(aaldata, file = "aaldata1.csv",row.names=FALSE, na="",col.names=TRUE, sep=",")
head(aaldata)

## CTE 1%
##---CTE 1% for all groups by zip
ctedata <- sqlQuery(dbhandle, 'select cast(zip as char(5)) as zip, sum(grossloss)/100 as Loss from [dbo].[Master_TABLE_AIR_LM_ALL] where eventid + 270000000 in (select event from [dbo].[CTE_AIR_v12_2013_1p]) group by zip order by zip')
#write.csv(ctedata, file = "ctedata.csv", row.names=FALSE, quote = TRUE, na="",col.names=TRUE, sep=",")
for (i in 1:10000){
  ctedata$zip[i] <- str_pad(toString(ctedata$zip[i]), width=5, side="left", pad="0")
}
write.table(ctedata, file = "ctedata1.csv",row.names=FALSE, quote = TRUE, na="",col.names=TRUE, sep=",")
head(ctedata)
##-- 02:56:25

## PML
##-- PML in 1 Query at Zip level and selecting a row
pmldata <- sqlQuery(dbhandle, 'select * from (select ROW_NUMBER() OVER (ORDER BY max(loss) desc) AS rownumber, cast(zip as char(5)) as zip, yearid, max(Loss) as Loss from (select zip, eventid, yearID, sum(GrossLoss) as Loss from [dbo].[Master_TABLE_AIR_LM_ALL] where zip = 00000 group by zip, eventid, yearID) PML1 group by zip, yearid) foo where rownumber = 100')
#write.csv(pmldata, file = "pmldata.csv")
for (i in 1:10000){
pmldata$zip[1] <- str_pad(toString(pmldata$zip[1]), width=5, side="left", pad="0")
}
write.table(pmldata, file = "pmldata1.csv",row.names=FALSE, na="",col.names=TRUE, sep=",")
head(pmldata)

##EXTRACT AND MAKE DATA

require(microbenchmark)
require(stringr)

zipdata <- readRDS("data/superzip.rds")

library(sqldf)
#zipdata = read.csv("data/zip_codes_states.csv", stringsAsFactors = TRUE)  # read csv file
rm(aaldata)
aaldata = read.csv("aaldata1.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)
for (i in 1:10000){
  aaldata$zip[i] <- str_pad(toString(aaldata$zip[i]), width=5, side="left", pad="0")
}
head(aaldata)
ctedata = read.csv("ctedata1.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)
for (i in 1:10000){
  ctedata$zip[i] <- str_pad(toString(ctedata$zip[i]), width=5, side="left", pad="0")
}
head(ctedata)
expdata = read.csv("expdata1.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)
for (i in 1:10000){
  expdata$zip[i] <- str_pad(toString(expdata$zip[i]), width=5, side="left", pad="0")
}
head(expdata)
pmldata = read.csv("pmldata1.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)
for (i in 1:1){
  pmldata$zip[i] <- str_pad(toString(pmldata$zip[i]), width=5, side="left", pad="0")
}
head(pmldata)

## RENAME KEY COLUMNS
names(expdata)[2] <- "exp_count"
names(expdata)[3] <- "TIV"
names(aaldata)[2] <- "AAL"
names(ctedata)[2] <- "CTE_1"
#names(pmldata)[2] <- "ZIPPER"
names(pmldata)[4] <- "PML_100"
names(zipdata)[1] <- "zip"
names(zipdata)[5] <- "city"
names(zipdata)[6] <- "state"

rm(test)
test <- merge(zipdata, expdata, by="zip", all.x = TRUE)
test <- merge(test, aaldata, by="zip", all.x = TRUE)
test <- merge(test, ctedata, by="zip", all.x = TRUE)
test <- merge(test, pmldata, by="zip", all.x = TRUE)

test[21] <- NULL
test[20] <- NULL
test[14] <- NULL
test[13] <- NULL
test[10] <- NULL
test[9] <- NULL
test[7] <- NULL
test[4] <- NULL
test[3] <- NULL
for (i in 1:10000){
  test$zip[i] <- str_pad(toString(test$zip[i]), width=5, side="left", pad="0")
}
