Stygodistribution <- function(taxon,rank="species",species.only="T",Records.MAIN,Taxa.MAIN,Access.MAIN){
  library(DBI);library(odbc);library(RPostgres)
  library(sf);library(sp)
  library(mapview);library(leafem);library(leafpop);library(leafpop)
  library(ggplot2);library(devtools);library(htmlwidgets)


  
##1## The outcome is **tax.list**, which is a list with the taxa selected by the user. This list is matched later
# with the table containing the records to make the map
# The are to variables **rank** determines at which taxonomic rank the selection is produced
# **taxon** provide the name of the taxon that will be selected.

      taxa <- Taxa.MAIN      

      if (rank=="phylum") {
        tax.list <- taxa[ which (taxa$Tax.phylum==taxon),]
        print("Filter by Phylum")
      } else if (rank=="class") {
        tax.list <- taxa[which (taxa$Tax.class==taxon),]
        print("Filter by Class")
      } else if (rank=="order") {
        tax.list <- taxa[ which (taxa$Tax.order==taxon),]
        print("Filter by Order")
      } else if (rank=="family") {
        tax.list <- taxa[ which (taxa$Tax.family==taxon),]
        print("Filter by Family")
      } else if (rank=="genus") {
        tax.list <- taxa[which (taxa$Tax.genus==taxon),]
        print("Filter by Genus")
      } else if (rank=="species") {
        tax.list <- taxa[which (taxa$Tax.name==taxon),]
        print("Filter by Species")
      } else if (rank=="group") {
        tax.list <- taxa[ which (taxa$Tax.group==taxon),]
        print("Filter by Group")
      } else if (rank=="subgroup") {
        tax.list <- taxa[which (taxa$Tax.subgroup==taxon),]
        print("Filter by Subgroup")
      } else
        print("Please, add one of the valid ranks in StyMun")

##2## This step allows the user to continue working with all the taxa recorded in **tax.ocurrence** or
# keep only the records indentified to species level using the variable **species.only** in the function
# The filterin i done over the **tax.list**, which comes for the **Taxa.MAIN** table.

if(species.only=="F"){
  tax.list <- tax.list
  print("All records have been selected")
}else if(species.only=="T"){
  tax.list <- tax.list[tax.list$Tax.rank == "SPECIES",]
  print("Only species records selected")
} else
  print("Please, add one of the valid valus for the option species only")

##3## **tax.list** is now matched against **Records.MAIN** to produce a data.frame containing all the entries
# stored in StyMun for the selected taxon. The second line counts the number of records for future reference.
# The third line deletes all the entries not related to a localities and the duplicates

      records <- Records.MAIN
      records <- records[ which (records$Acc.ID != 'na'),]
      
tax.ocurrence <- records[records$Tax.name %in% tax.list$Tax.name,]
total.records <- nrow(tax.ocurrence) ## Number of total entries


unique.taxa <- unique(tax.ocurrence$Tax.name)
count.unique.taxa <- length(unique.taxa) # Counts the number of taxa selected

##4## Here, we select all the unique records for species in localities
taxon.species.rec <- tax.ocurrence[!duplicated(tax.ocurrence$Acc.ID),]

taxon.species.rec <- taxon.species.rec[ which (taxon.species.rec$Acc.ID != "na"),]

taxon.species.rec2 <- taxon.species.rec[,c(6,7,8,11,13)]
count.species.rec <- nrow(taxon.species.rec2)

print(paste0("There are ",total.records," entries for the selected taxa; ",
             count.species.rec," of them there are unique"))

##5## Here, we select all the unique collections for species in localities
#tax.ocurrence2 <- tax.ocurrence[,c(2,5:8,11,13:20,23:24,30:37,40:49)]
#taxon.collection.rec <- tax.ocurrence2[tax.ocurrence2$Acc.ID!="NA"&(tax.ocurrence2$Lib.type=="pri" | tax.ocurrence2$Lib.type=="acc"),]
#taxon.collection.rec2 <- unique(taxon.collection.rec[,c(1:34)])
#n.collection <- nrow(taxon.collection.rec)

access <- Access.MAIN

taxon.localities <- access[which (access$Acc.ID %in% taxon.species.rec2$Acc.ID),c(1,2,10:13)]
taxon.localities <- merge(taxon.species.rec2,taxon.localities,by.x="Acc.ID",by.y="Acc.ID")

## Delete entries without coordinates
taxon.gis <- taxon.localities[!is.na(taxon.localities$Acc.latitude)&!is.na(taxon.localities$Acc.longitude),]
localities.coord <- nrow(taxon.gis)
coordinates(taxon.gis) <- c("Acc.longitude", "Acc.latitude")
crs.geo <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"); proj4string(taxon.gis) <- crs.geo
print(paste0(localities.coord," out of ", count.species.rec," unique localites are georreferenced"))

# Defined the graphical parameters of the map
a <- mapView(taxon.gis, legend=T,alpha=0.2,cex=4,zcol="Acc.type",
             map.types=c("Esri.WorldImagery","OpenStreetMap.DE"))
img <- "https://github.com/amartinezgarcia/stygofauna_mundi/blob/main/Logo1_white2.png?raw=true"
leafem::addLogo(map=a, img=img, src="remote",
                position = "bottomleft",
                offset.x = 5,
                offset.y = 40,
                width = 110,
                height = 110)
}
