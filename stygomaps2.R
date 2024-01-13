#' @name Stygomaps
#' @title Function for plotting georeferrenced localities in a given geographical area
#' @author Alejandro Martinez (25.03.2021)
#'
#' @description
#' This function plots the all the georeferenced localities for a given geographical unit.
#'
#' @param territory the name of the geographic unit for which the data is going to be plotted
#' @param geodivision the rank of the geographical unit (country, region, province)
#'
#' @details
#' This function plots the georreference localities included within a pre-defined area
#' (country, region, province). A list of attributed for each locality is showed
#'
#'
Stygomaps <- function(territory,geodivision="country",access.type="all") {
  
  # Load packages
  library(DBI);library(odbc);library(RPostgres)
  library(sf);library(sp)
  library(mapview);library(leafem);library(leafpop);library(leaflet)
  library(ggplot2);library(devtools);library(htmlwidgets);library(htmltools)

  #Load StygofaunaMundi
  con <- dbConnect(RPostgres::Postgres(),
                   dbname="stygofaunamundi_lv1")

  # Read table with localities
  Access.MAIN <- dbReadTable(con, "Access.MAIN.v3")

  ##1## Define the area area

  if (geodivision=="country") {
    geo.values <- Access.MAIN[Access.MAIN$Acc.country==territory,]
    print("Filter by country")
  } else if (geodivision=="region") {
    geo.values <- Access.MAIN[Access.MAIN$Acc.region==territory,]
    print("Filter by region")
  } else if (geodivision=="province") {
    geo.values <- Access.MAIN[Access.MAIN$Acc.province==territory,]
    print("Filter by country")
  } else
    print("Please, add a valid protocol")

  ###2### We sort the data according to the access type
  if(access.type=="all"){
    cave.gis <- geo.values
    print("All type of access points selected")
  }else{
    cave.gis <- geo.values[ which(geo.values$Acc.type==access.type),]
    print(paste0("only type ",access.type," has been selected"))
  }

  # Calculate total number of records
  nlocalities <- nrow(cave.gis)

  # Delete records without coordinates
  cave.gis.geo <- cave.gis[!is.na(cave.gis$Acc.latitude)&!is.na(cave.gis$Acc.longitude),]

  #Calculate the number of georreferenced entries
  nreflocalities <- nrow(cave.gis.geo)

  #Produce the spatial object  with the selection
  coordinates(cave.gis.geo) <- c("Acc.longitude", "Acc.latitude")
  crs.geo <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  proj4string(cave.gis.geo) <- crs.geo

  ##3## Build the map

  # Subset attributed to be showed in the interactive map
  access.attributes <- c("Acc.ID","Acc.name","Acc.country","Acc.region","Acc.province",
                         "Acc.municipality","Acc.type","Acc.coord.ref",
                         "Water.saline","Water.brackish", "Water.freshwater",
                         "comments")
  access.subset <- cave.gis.geo[colnames(cave.gis.geo@data) %in% access.attributes]

  # Print the sort summary of the information
  print(paste0(nlocalities," localities in ",territory,"; ",nreflocalities," are georreferenced"))


  # Defined the graphical parameters of the map
  #a <- mapView(access.subset, legend=T,alpha=0.2,cex=4,zcol = 'Acc.type',
  #              popup = popupTable(access.subset),
  #              map.types=c("Esri.WorldImagery","OpenStreetMap.DE"))
  # img <- "https://github.com/amartinezgarcia/stygofauna_mundi/blob/main/Logo1_white2.png?raw=true"
  # leafem::addLogo(map=a, img=img, src="local",
  #                 position = "bottomleft",
  #                 offset.x = 5,
  #                 offset.y = 40,
  #                 width = 110,
  #                 height = 110)

  access.subset$label <- paste("<p>", access.subset$Acc.ID, ",", access.subset$Acc.type, "</p>",
                               "<p>", access.subset$Acc.name, "</p>",
                               "<p>", access.subset$Acc.region, ",", access.subset$Acc.province, ",", access.subset$Acc.municipality, "</p>")

  m1 <- leaflet() %>%
    addProviderTiles(providers$Esri.WorldImagery) %>%
    addCircleMarkers(lng=access.subset$Acc.longitude,
                     lat=access.subset$Acc.latitude,
                     color="white",
                     weight = 2,
                     radius=4,
                     label = lapply(access.subset$label,HTML)) %>%
    leafem::addLogo(img="https://github.com/amartinezgarcia/stygofauna_mundi/blob/main/Logo1_white2.png?raw=true",
                    src="remote",
                    position = "bottomleft",
                    offset.x = 5,
                    offset.y = 40,
                    width = 110,
                    height = 110)
  m1
}
