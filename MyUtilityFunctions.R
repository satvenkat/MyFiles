# returns string w/o leading whitespace
trim.leading <- function (x)  sub("^\\s+", "", x)

# returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#returns the column Number given the data frame and column Name
getColumnNumberForColumnName<-function(df,cN) which( colnames(df)==cN )

#returns TRUE if the column Name is in a data frame
isColumnNameInDataFrame<-function(df,cN){
if(cN %in% colnames(df))
{
  TRUE;
}
  else FALSE;
}

#Re ordering columns in a data frame to start of data frame
reorderDataFrame<-function(df, refCols ) {
#refcols <- c("BusinessLine", "Tier1", "Tier2", "Tier3")
df <- df[, c(refCols, setdiff(names(df), refCols))]
df
}

deleteColumnsFromDataFrame<-function(df, dropCols){
#  dropCols <- c("rowNumber","Tier","HasSubTiers","IncludeData")
  df <-df[ , !(names(df) %in% dropCols)]
  df
}

addColumnsToDataFrame<-function(df,addCols){
  df[addCols] <- NA
  df
}

connectToSQLServer<-function(serverName, dbName){
#  library(odbc)
 # con <- dbConnect(odbc::odbc(), 
 #                  .connection_string = "Driver={Simba SQL Server ODBC Driver}; 
#                                           Server={serverName}; 
#                                           Database=dbName; 
#                                           UID=ENT\\username; 
#                                           PWD=password; 
#                                           Integrated Security=NTLM", encoding="UTF-32")
  library(DBI)
  con <- dbConnect(odbc::odbc(), serverName)
}

writeToDB<-function(serverName, tableName, dataFrameName, appendValue){
 # con <- dbConnect(odbc::odbc(), "E04TCM-GSASQL03")
#  data <- dbWriteTable(con, "FASOperationsFinancials", final, append=TRUE)
#  con <- dbConnect(odbc::odbc(), serverName)
  con <- dbConnect(odbc::odbc(), dsn = serverName)
  data <- dbWriteTable(con, tableName, dataFrameName, append=appendValue)
  dbDisconnect(con)
}
writeToDBWithRODBC<-function(serverName, tableName, dataFrameName, appendValue){
library(RODBC)
ch <- odbcConnect(serverName)
sqlSave(channel = ch, dat = final, tablename = tableName, append=TRUE, rownames = FALSE)
odbcClose(ch)
}

findInstalledPackages<-function() {
  ip = as.data.frame(installed.packages()[,c(1,3:4)])
  ip = ip[is.na(ip$Priority),1:2,drop=FALSE]
  ip
}

installMyPackages<-function() {
  load.lib<-c("ggmap","ggplot2","gridExtra","lubridate","maps","MASS","plyr","xlsx","odbc",
              "RODBC","sp","sqldf","stockPortfolio","stringi","stringr","XLConnect", "xlsxjars",
              "xlsReadWrite","zipcode","data.table","dplyr","installr","knitr","tidyr","tinytex","swirl","rJava")
  
  
  install.lib<-load.lib[!load.lib %in% installed.packages()]
  for(lib in install.lib) install.packages(lib,dependencies=TRUE)
  sapply(load.lib,require,character=TRUE)
}
