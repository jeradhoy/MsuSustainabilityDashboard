#Download R data
library(googlesheets)
library(magrittr)

##Read in data from google sheets gs_auth(token = "shiny_app_token.rds")
allDataSheet <- gs_title("allData")
dataDir <- "./data/Gdrive/"

file.remove(paste0(dataDir, list.files(dataDir)))

for(sheet in gs_ws_ls(allDataSheet)){
  write.csv(as.data.frame(allDataSheet %>% gs_read_csv(ws=sheet)), file=paste0(dataDir, sheet, ".csv"), row.names=F)
  Sys.sleep(3)
}
