if (!('corrplot' %in% installed.packages())) install.packages('corrplot') else 0
library(corrplot)

beer2 <- read.csv('recipes-2.csv')

# checks correlation betweehn Orig_Gravity, Final_Gravity, ABV, IBU, SRM
corrplot.mixed(cor(beer[,2:6]))
# correlation between Orig_Gravity and ABV is very high - can ignore ABV

# Creates new varialbes for each unqiue value under fermentable# 
# gets all the unique values for the fermentable fields
fermentables <- unique(c(as.character(beer$Fermentable1), 
         as.character(beer$Fermentable2), 
         as.character(beer$Fermentable3), 
         as.character(beer$Fermentable4)))

# gets rid of the pesky ''
fermentables <- fermentables[!(fermentables == '')]

# assigns the amounts to the appropiate fields 
for (i in as.character(fermentables)) {
  nc <- rep(0, nrow(beer))
  for (j in 8:11 ) {
    nc[beer[ ,j] == i] <- beer[beer[ ,j] == i,j+4]
    beer[ ,eval(i)] <- nc
  }
}

# rinse & repeat for FirstWortHops
firstwort <- unique(c(as.character(beer$FirstWortHops1), 
                         as.character(beer$FirstWortHops2)))

firstwort <- firstwort[!(firstwort == '')]

# assigns the amounts to the appropiate fields 
for (i in as.character(firstwort)) {
  nc <- rep(0, nrow(beer))
  for (j in 16:17 ) {
    nc[beer[ ,j] == i] <- beer[beer[ ,j] == i,j+2]
    beer[ ,eval(i)] <- nc
  }
}

# The boilhops fields are tricky... Think I'm going to replace the gaps with 'None' and the NAs with 0
boilfields <- c('BoilHops1', 'BoilHops2', 'BoilHops3', 'BoilHops4')
for (i in 1:4) {
  levels(beer[,eval(boilfields[i])]) <- 
    c(levels(beer[,eval(boilfields[i])]), 'None')
  beer[beer[,eval(boilfields[i])] == '',eval(boilfields[i])] <- 'None'
}

boilnumfields <- c('BH_Amount1', 'BH_Amount2', 'BH_Amount3', 'BH_Amount4', 
                   'Boil_Time1', 'Boil_Time2', 'Boil_Time3', 'Boil_Time4')
for (i in 1:length(boilnumfields)) {
  beer[, eval(boilnumfields[i])][is.na(beer[, eval(boilnumfields[i])])] <- 0
}


# Going to do the same with DryHops what I did the to the Fermentable fields, first have to convert the Time to numeric values
beer$DryHop_Time1 <- 
  as.numeric(sapply(beer$DryHop_Time1, function(x) gsub(' Days', '', x)))

beer$DryHop_Time2 <- 
  as.numeric(sapply(beer$DryHop_Time2, function(x) gsub(' Days', '', x)))

DryHop <- unique(c(as.character(beer$DryHop1), 
                      as.character(beer$DryHop2)))

DryHop <- DryHop[!(DryHop == '')]

# assigns the amounts to the appropiate fields 
for (i in as.character(DryHop)) {
  nc <- rep(0, nrow(beer))
  for (j in 32:33 ) {
    nc[beer[ ,j] == i] <- beer[beer[ ,j] == i,j+2]
    beer[ ,eval(i)] <- nc
  }
}


# Mash.... fill the Mashtype gaps with 'Unknown'. Use the non-na values for each field for clustering and assign a '-1' to the NAs. Convert then to factor features
beer$MashAmount <- as.numeric(sapply(beer$MashAmount, function(x) gsub('l', '',x)))
cl <- c()
for (k in 1:(length(unique(beer$MashAmount))-1)) {
  cl[k] <- sum(kmeans(beer$MashAmount[!is.na(beer$MashAmount )], k)$withinss)
}
plot(cl, type = 'l')
# 3 clusters looks good
beer$MashAmountClust <- -1 
beer$MashAmountClust[!is.na(beer$MashAmount )] <- 
  kmeans(beer$MashAmount[!is.na(beer$MashAmount )], 3)$clust 
beer$MashAmountClust <- as.factor(beer$MashAmountClust)

# MashTime
cl <- c()
for (k in 1:(length(unique(beer$MashTime))-1)) {
  cl[k] <- sum(kmeans(beer$MashTime[!is.na(beer$MashTime )], k)$withinss)
}
plot(cl, type = 'l')
# 2 clusters

beer$MashTimeClust <- -1 
beer$MashTimeClust[!is.na(beer$MashTime )] <- 
  kmeans(beer$MashTime[!is.na(beer$MashTime )], 2)$clust 
beer$MashTimeClust <- as.factor(beer$MashTimeClust)


# MashTemp
cl <- c()
for (k in 1:(length(unique(beer$MashTemp))-1)) {
  cl[k] <- sum(kmeans(beer$MashTemp[!is.na(beer$MashTemp )], k)$withinss)
}
plot(cl, type = 'l')
# 3 clusters

beer$MashTempClust <- -1 
beer$MashTempClust[!is.na(beer$MashTemp )] <- 
  kmeans(beer$MashTemp[!is.na(beer$MashTemp )], 3)$clust 
beer$MashTempClust <- as.factor(beer$MashTempClust)

# Yeast. Fermentis / Safale seems to be a good classifier for this field. I can't see how the other information in this field can be useful for now
beer$YeastFermentis <- 
  sapply(beer$Yeast, function(x) gregexpr('Fermentis / Safale', x)) != -1

# YeastAttenuation can just be converted to a numerical value
beer$YeastAttenuation <- 
  sapply(beer$YeastAttenuation, function(x) as.numeric(gsub('%', '', x)))

# YeastTemp is a range. Going to split it into the min and max values
beer$YeastTempMin <- 
  unlist(lapply(strsplit(as.character(beer$YeastTemp), '-'), 
                function(x) min(as.numeric(x))))
beer$YeastTempMax <- 
  unlist(lapply(strsplit(as.character(beer$YeastTemp), '-'), 
                function(x) max(as.numeric(x))))

