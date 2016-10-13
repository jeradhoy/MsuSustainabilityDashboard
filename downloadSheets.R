#Download R data
#MEOWWWW 

##Read in data from google sheets gs_auth(token = "shiny_app_token.rds") 
allDataSheet <- gs_title("allData")
energyData <- as.data.frame(allDataSheet %>% gs_read(ws = "Energy")) write.csv(energyData, file = "./data/energyData.csv", row.names = F)

pcwaste <- as.data.frame(allDataSheet %>% gs_read(ws = "PerCapita"))
write.csv(pcwaste, file = "./data/pcwaste.csv", row.names = F)

waste <- as.data.frame(allDataSheet %>% gs_read(ws = "Waste"))
write.csv(waste, file = "./data/waste.csv", row.names = F)

leedBuildings <- as.data.frame(allDataSheet %>% gs_read(ws = "Leed"))
write.csv(leedBuildings, file = "./data/leedBuildings.csv", row.names = F)

