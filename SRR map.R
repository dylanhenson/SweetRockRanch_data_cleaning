setwd("C:/Users/dhenson/My Documents/Other projects/SRR")

library(readxl)
SRR<- as.data.frame(read_excel("~/Other projects/SRR/SRR guest locations.xlsx"))
SRR

library(readr)
coords <- as.data.frame(read_csv("~/Other projects/SRR/uscitiesv1.3.csv"))
city

#Reformatting SRR
#Capitalization

cap <- function(x){
  #split words
  words <- strsplit(x, " ")
  
    #split letters
    firstLetter <- toupper(sapply(words, function(x) substr(x,1,1)))
    lastLetters <-tolower(sapply(words, function(x) substr(x, 2, nchar(x))))
    
    #combine letters
    newLetters = mapply(paste0, firstLetter, lastLetters)
    
    #newLetters
  #combine words
  newCity <- paste(newLetters, collapse = " ")
    
  newCity
}


#capitalize each letter
City2 = character(nrow(SRR))

for (i in 1:nrow(SRR)){
  City2[i] = cap(SRR$City[i])
}
SRR$City = City2

#Captialize two-letter states
stateCap <- function(x){
  if (substr(substr(x,nchar(x)-4+1,nchar(x)),1,1) == ","){
    newState = toupper((substr(x,nchar(x)-4+1,nchar(x))))
    paste(substr(x,1,nchar(x)-4),newState,collapse = "",sep = "")
  }else{
    x
  }
}

#Capitalizing all states
City3 <- character(nrow(SRR))
for (i in 1:nrow(SRR)){
  City3[i] = stateCap(SRR$City[i])
}

SRR$City = City3

#Separating cities and states
#Cities column
SRR$CityOnly = gsub(",.*$","",SRR$City)

#States column
SRR$StateOnly = gsub(".*, ","",SRR$City)

#Manual update of SRR State codes
SRR$StateOnly
SRR$StateOnly[1] = "CA"
SRR$StateOnly[9] = "CA"
SRR$StateOnly[15] = "CA"
SRR$StateOnly[17] = "TX"
SRR$StateOnly[24] = "GA"
SRR$StateOnly[25] = "CA"
SRR$StateOnly[26] = "AR"
SRR$StateOnly[27] = "CT"
SRR$StateOnly[28] = "DC"
SRR$StateOnly[39] = "MA"
SRR$StateOnly[52] = "CA"
SRR$StateOnly[59] = "CA"
SRR$StateOnly[105] = "CA"
SRR$StateOnly[133] = "CA"
SRR$StateOnly = toupper(SRR$StateOnly)

#Updating SRR cities by hand
SRR$CityOnly
SRR$StateOnly[1] = "CA"

SRR[which(SRR$City == fullSRR[which(is.na(fullSRR$state_name)),1]),]



#Adding coordinates via "coords" dataset

fullSRR = merge(x = SRR, y = coords, by.x = c("CityOnly","StateOnly"), by.y = c("city","state_id"), all.x = T)




write.csv(fullSRR,file="fullSRR.csv")


