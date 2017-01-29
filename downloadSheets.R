#Download R data
library(googlesheets)
library(magrittr)

##Read in data from google sheets gs_auth(token = "shiny_app_token.rds")
allDataSheet <- gs_title("allData")
dataDir <- "./data/Gdrive/"

for(sheet in gs_ws_ls(allDataSheet)){
  #write.csv(as.data.frame(allDataSheet %>% gs_read_csv(ws=sheet)), file=paste0(dataDir, sheet, ".csv"), row.names=F)
  gs_download(from=allDataSheet, ws=sheet, to=paste0(dataDir, sheet, ".csv"), overwrite=T)
}
