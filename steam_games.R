library(tidyverse)
library(forecast)
library(car)
library(leaps)
library(caret)
library(openxlsx)

steam <- read_csv(file.choose())

View(steam)
summary(steam)

# View(subset(steam, QueryName != ResponseName))

#check for duplicate rows
count(steam) - count(unique(steam))

# reduce to unique rows
steam <- unique(steam)


table(steam$RequiredAge)

# view(subset(steam, RequiredAge == 0))

#there are too many falsely age required 0 games listed, after a quick look at the data using tableau regardless of the genre the average required age seems to be 17
# so modifying the data so that required age matches the avg

steam$RequiredAge <- ifelse(steam$RequiredAge == 0, 17, steam$RequiredAge)

# age reclassification, <10 E, <13 E10+, <17 T, <18 M, A

steam <- steam %>%
  mutate(AgeRating = case_when(RequiredAge < 10 ~ "Everyone",
                               RequiredAge < 13 ~ "Everyone10+",
                               RequiredAge < 17 ~ "Teen",
                               RequiredAge < 18 ~ "Mature",
                               .default = "Adult")
  )



table(steam$AgeRating, steam$RequiredAge)



# Genre Setup


steam_binary_conv_df <- steam[,grepl("Genre", names(steam))] * 1

steam_test <-  steam

steam_test$NoGenres <- ifelse(rowSums(steam_binary_conv_df) == 0, 1, 0) # 1 means content has no recorded genre

x_nosec <- select(steam_test ,c(ResponseName, names(steam_test[,grepl("Genre", names(steam_test))])))

count(steam) - count(subset(x_nosec, NoGenres == 0))

# view(subset(x_nosec, NoGenres == 0))

# NoGenres and NoGame genres need to be removed
count(steam_test) - count(steam_test[steam_test$GenreIsNonGame == TRUE | steam_test$NoGenres > 0,])


count(steam_test[-which(steam_test$GenreIsNonGame == TRUE | steam_test$NoGenres > 0),])


steam_test <- steam_test[-which(steam_test$GenreIsNonGame == TRUE | steam_test$NoGenres > 0),]

for (colName in names(steam[,grepl("Genre", names(steam))])) {
  
  newColName <- paste(c(as.character(colName),"Binary"), collapse = "")
  
  steam_test
  
  steam_test[,newColName] <- steam_test[,colName]*1
}


view(steam_test)


# view(select(steam[rowSums(steam_binary_conv_df) == 7,],c(ResponseName, names(steam[,grepl("Genre", names(steam))]))))

max(rowSums(steam_binary_conv_df))


# view(steam[steam$GenreIsNonGame,])


max(steam$DLCCount)




table(steam$PriceCurrency)

# view(steam_test)


# Category Setup


steam_binary_conv_df <- steam_test[,grepl("Category", names(steam_test))] * 1

# view(steam_binary_conv_df)

steam_test$NoCategory <- ifelse(rowSums(steam_binary_conv_df) == 0, 1, 0) # 1 means content has no recorded genre

x_nosec <- select(steam_test ,c(ResponseName, names(steam_test[,grepl("Category", names(steam_test))])))

count(steam_test) - count(subset(x_nosec, NoCategory == 0))

# view(subset(steam_test, NoCategory == 1))


for (colName in names(steam[,grepl("Category", names(steam))])) {
  
  newColName <- paste(c(as.character(colName),"Binary"), collapse = "")
  
  steam_test
  
  steam_test[,newColName] <- steam_test[,colName]*1
}

# view(steam_test[,grepl("Category", names(steam_test))])

help("select_if")

names(select_if(steam, is.logical))

for (colName in names(select_if(steam_test, is.logical))) {
  
  if (paste(c(as.character(colName),"Binary"), collapse = "") %in% names(steam_test)) {
  }
  else {
    print(colName)
    newColName <- paste(c(as.character(colName),"Binary"), collapse = "")
    
    steam_test
    
    steam_test[,newColName] <- steam_test[,colName]*1
    
  }
}


# view(steam_test[,names(select_if(steam_test, is.logical))])
view(steam_test[1:50, names(select_if(steam_test, is.numeric))])


# view(steam_test[!complete.cases(steam_test),])

!complete.cases(steam_test)

names(which(colSums(is.na(steam_test)) > 0))

steam_test[is.na(steam_test$QueryName), ]

steam_test[is.na(steam_test$QueryName), "QueryName"] <- steam_test[is.na(steam_test$QueryName), "ResponseName"] 

steam_test[steam_test$QueryID == 8780, ]

steam_test[is.na(steam_test$ReleaseDate), ]

write.xlsx(steam_test[is.na(steam_test$ReleaseDate), 1:4], 'games_wo_release.xlsx')


steam_test[is.na(steam_test$PriceCurrency), ]

table(steam_test$PriceCurrency)

steam_test[is.na(steam_test$PriceCurrency), "PriceCurrency"] <- "USD"

steam_test[is.na(steam_test$SupportedLanguages), ]

steam$app

table(steam_test$SupportedLanguages)

steam_test[is.na(steam_test$SupportedLanguages), "SupportedLanguages"] <- "English"

steam_test[is.na(steam_test$SupportedLanguages), ]

view(steam_test[is.na(steam_test$Reviews), ])

xyz <- steam_test %>% mutate(num_genes=str_count(SupportedLanguages," ")+1)
view(select(xyz, c(num_genes, SupportedLanguages)))

