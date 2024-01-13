
fill.species.codes <- function(records,taxa) {

  "%ni%" <- Negate("%in%")
  
  ### Select the species without Tax.ID
  species.fill <- unique(records[c("Tax.name","Tax.group")])
  species.fill <- species.fill[complete.cases(species.fill),] 
  species.fill <- taxa[taxa$Tax.name %in% species.fill$Tax.name,]
  
  ### Select the groups to which the species without Tax.ID belong
  groups <- as.data.frame(unique(species.fill$Tax.group))
  colnames(groups) <- "group"
  

  
new.filled <- data.frame()

for (i in 1:nrow(groups)){
  
  # Select the first group and sort it
  this.group <- groups[i,] # first group
  sp.group <- species.fill[ which(species.fill$Tax.group == this.group),] # select species
  sp.group1 <- sp.group[order(sp.group$Tax.ID),] # sort them by Tax.ID
  
  # Extract the last value
  sp.filled <- sp.group1[!is.na(sp.group1$Tax.ID),]
  last.num <- as.integer(sub(".*\\.", "", sp.filled$Tax.ID[nrow(sp.filled)]))
  this.abbr <- substr(sp.filled$Tax.ID[nrow(sp.filled)], 1,4)
  
  # Fill the species without ID
  
  sp.empty <- sp.group1[is.na(sp.group1$Tax.ID),]
  if(nrow(sp.empty) > 0){     
    
    for (k in 1:nrow(sp.empty)) {
      
      number <- last.num+k
      
      this.ID <- if(number >= 100 & number <1000) {paste0(this.abbr,"0",number)
      } else if (number >= 10 & number <= 99) {this.ID <- paste0(this.abbr,"00",number)
      } else if (number <= 9) {this.ID <- paste0(this.abbr,"000",number)
      } else this.ID <- {paste0(this.abbr,number)
      }
      
      sp.empty[k,]$Tax.ID <- this.ID 
    }
    
    new.filled <- rbind(new.filled,sp.empty)
  } else {new.filled <-     new.filled}
  
} 

new.filled <- new.filled[ which(new.filled$Tax.ID %ni% Taxa.MAIN$Tax.ID),]

new.filled <- new.filled[c("id","Tax.name","Tax.ID")]
new.filled <- new.filled[!duplicated(new.filled),]
return(new.filled)

}

fill.species.codes(Records.MAIN,Taxa.MAIN)
