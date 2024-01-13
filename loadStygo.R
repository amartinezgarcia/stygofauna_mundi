
#-------------#
# Upload data
# written by Tommaso Cancellario
# 07.04.2021 
#-------------#

loadStygo <- function(data.path = NA, working.dir = getwd(), name = NA, mail = NA){

if(!is.na(name) & !is.na(mail)){
  # Load libraries
  if (!require("DBI")) install.packages("DBI")
  require(DBI)
  if (!require("odbc")) install.packages("odbc")
  require(odbc)
  if (!require("RPostgres")) install.packages("RPostgres")
  require(RPostgres)
  
  # DB connection
  con <- dbConnect(RPostgres::Postgres(), 
                   dbname = "Stygofaunamundi",
                   host="stygofaunamundi-3.cifo2yqpfpym.eu-west-1.rds.amazonaws.com",
                   port=5432,
                   user="botosaneanu1986",
                   pass="pallanza2021")
  
  # Retrive data
  main <- dbReadTable(con, "Records.MAIN")
  ecology <- dbReadTable(con, "Records.hydrology")
  record <-  merge(main, ecology, by="STN.ID", all.x = T)
  
# Load reviewed .csv
stygo.1 <- read.csv(data.path)
# Stygo.temporary
write.csv(record, paste0(working.dir, "temporary_stygo.csv"), row.names = F)
temporary.stygo <- read.csv(paste0(working.dir, "temporary_stygo.csv"))
file.remove(paste0(working.dir, "temporary_stygo.csv"))

record.add <- dplyr::setdiff(stygo.1, temporary.stygo)
update.main <- record.add[,1:64] 
# Create info about user
update.main$name <- name
update.main$mail <- mail
update.main$dateUpdate <- Sys.time()
  
hydrology.main <- cbind(record.add$STN.ID, record.add[,65:86] )
colnames(hydrology.main)[1] <- "STN.ID" 
# Create info about user
hydrology.main$name <- name 
hydrology.main$mail <- mail
hydrology.main$dateUpdate <- Sys.time()

# Load data
dbWriteTable(con, "upload.records.MAIN", update.main, append = TRUE, row.names = FALSE)
dbWriteTable(con, "upload.records.hydrology", hydrology.main, append = TRUE, row.names = FALSE)

print("Your data were loaded")

} else(
  print("Plese write your name and mail!")
)

  
}

loadStygo(data.path = "/Users/tom/Desktop/provaStygo.csv", 
          working.dir = "/Users/tom/Desktop/",
          name = "Tommaso Cancellario",
          mail = "tommaso.cancellario@gmail.com”)