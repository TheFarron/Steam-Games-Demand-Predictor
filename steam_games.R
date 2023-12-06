library(tidyverse)
library(forecast)
library(car)
library(leaps)
library(caret)
library(openxlsx)
library(readxl)
library(GGally)
library(ggplot2)


steam <- read_csv(file.choose())

View(steam)
summary(steam)

# View(subset(steam, QueryName != ResponseName))

# Null investigation and ammends

names(which(colSums(is.na(steam)) > 0))

for (i in names(which(colSums(is.na(steam)) > 0))) {
  print(i)
  print(count(steam[is.na(steam[,i]), ]))
}

steam[is.na(steam$QueryName), ]
# Take missing Query Name from Response Name, works for the same purpose
steam[is.na(steam$QueryName), "QueryName"] <- steam[is.na(steam$QueryName), "ResponseName"] 


# Currency Type Fix
steam[is.na(steam$PriceCurrency), c("QueryName", "PriceCurrency")]

table(steam$PriceCurrency)

# All prices shown in USD so safe to asasume missing currencies are USD too
steam[is.na(steam$PriceCurrency), "PriceCurrency"] <- "USD"


# SupportedLanguages Fix
steam[is.na(steam$SupportedLanguages), ]

table(steam$SupportedLanguages)

table(grepl('English', steam$SupportedLanguages, fixed = TRUE),  is.na(steam$SupportedLanguages))

# Majority of Games support English so missing values wil be replaced with English
steam[is.na(steam$SupportedLanguages), "SupportedLanguages"] <- "English"

# Now convert it to number of supported languages
steam[sapply(strsplit(as.character(steam$SupportedLanguages), " "), length) < 2 & nchar(steam$SupportedLanguages) > 9, c("QueryID", "SupportedLanguages")]
steam[steam$QueryID == 11250, "SupportedLanguages"] <- "Russian English Spanish French Japanese Czech"
steam[steam$QueryID == 11260, "SupportedLanguages"] <- "English Russian Spanish Japanese Czech"

steam$SupportedLanguages <- sapply(strsplit(as.character(steam$SupportedLanguages), " "), length) 

# Reviews Fix
# Download all the reviews from steamSpy api New Reviews = Recommendations + Disapprovals
# will replace RecommendationCount with newly generated Recommendations to stay consistent
steam_revs <- read_excel(file.choose())[, c(1,3,4,5)] # steam_games_reviews.xlsx
steam_revs <- steam_revs %>%
  rename("QueryID" = "appid")
steam_revs

count(steam[is.na(steam$Reviews),])
count(steam_revs[is.na(steam_revs$Reviews),])
select(steam, -c(Reviews, RecommendationCount))

steam <- merge(x = select(steam, -c(Reviews, RecommendationCount)), y = steam_revs,  by = "QueryID", all.x = TRUE)

# remaining columns with missing values, except ReleaseDate, can be converted to is.na() columns indicating wehter the game has that information on it store page or not as it is more valuable for analysis

names(which(colSums(is.na(steam)) > 0))

names(which(colSums(is.na(steam)) > 0))[-1]

for (colName in names(which(colSums(is.na(steam)) > 0))[-1]) {
  steam[,colName] <- is.na(steam[,colName])
}

# check columns to see if any char description column without nulls is unnecessary for the analysis
names(select_if(steam, is.character))

# Header image exists for every game therefore unnecessary for analysis
steam <- select(steam, -HeaderImage)

# Get Games with missing release dates, manually find relese dates of all 87 (couldn't find a reliable source or api)
# write.xlsx(steam[is.na(steam$ReleaseDate), 1:4], 'games_wo_release.xlsx')
steam_releases <- read_excel(file.choose())[, c(1,5)] # games_missing_releases.xlsx

steam_releases

steam <- merge(x = steam, y = steam_releases, by = "QueryID", all.x = TRUE)

steam[is.na(steam$ReleaseDate), "ReleaseDate"] <- steam[is.na(steam$ReleaseDate), "ReleaseDateNew"]

steam <- select(steam, -ReleaseDateNew)

# when dates get converted to date data types instead of chars there seems to be some dates formatted weirdly like fall 2016, end of the world etc. aas the number of theese is about 500 hard to manually fill them. Wait until other prep work to determine whether to drop these rows or not
# there is enough data left at the end to drop these

count(steam[is.na(steam$ReleaseDate),])

steam$ReleaseDate <- as.Date(toupper(steam$ReleaseDate), format = '%b %d %Y')

count(steam[is.na(steam$ReleaseDate), ] )

count(steam[!is.na(steam$ReleaseDate), ] )

steam <- steam[!is.na(steam$ReleaseDate), ]

steam$Day <- day(steam$ReleaseDate)
table(steam$Day)

steam$Month <- month(steam$ReleaseDate)
table(steam$Month)

steam$Year <- ifelse(nchar(year(steam$ReleaseDate)) < 4, year(steam$ReleaseDate) + 2000, year(steam$ReleaseDate))
table(steam$Year)

steam <- select(steam, -ReleaseDate)

#check for duplicate rows
count(steam) - count(unique(steam))

# reduce to unique rows
steam <- unique(steam)


table(steam$RequiredAge)

#there are too many falsely age required 0 games listed, after a quick look at the data using tableau regardless of the genre the average required age seems to be 17
# so modifying the data so that required age matches the avg

steam$RequiredAge <- ifelse(steam$RequiredAge == 0, 17, steam$RequiredAge)

# age reclassification, <10 Everyone - 1, <13 Everyone10+ - 2, <17 Teen - 3, <18 Mature - 4, Adult - 5

steam <- steam %>%
  mutate(AgeRating = case_when(RequiredAge < 10 ~ 1,
                               RequiredAge < 13 ~ 2,
                               RequiredAge < 17 ~ 3,
                               RequiredAge < 18 ~ 4,
                               .default = 5)
  )



table(steam$AgeRating, steam$RequiredAge)

steam <- select(steam, -RequiredAge)

# Genre Setup

steam_binary_conv_df <- steam[,grepl("Genre", names(steam))] * 1

steam$NoGenres <- ifelse(rowSums(steam_binary_conv_df) == 0, 1, 0) # 1 means content has no recorded genre

count(steam) - count(subset(steam, NoGenres == 0))

# NoGenres and NoGame genres need to be removed as these refer to content that are not video games, these don't fit within the scope of the project
count(steam) - count(steam[steam$GenreIsNonGame == TRUE | steam$NoGenres > 0,])

count(steam[-which(steam$GenreIsNonGame == TRUE | steam$NoGenres > 0),])

steam <- steam[-which(steam$GenreIsNonGame == TRUE | steam$NoGenres > 0),]

# Can drop these columns as they are no longer useful
steam <- select(steam, -c(GenreIsNonGame, NoGenres))

# Category Setup
steam_binary_conv_df <- steam[,grepl("Category", names(steam))] * 1

# view(steam_binary_conv_df)

steam$NoCategory <- ifelse(rowSums(steam_binary_conv_df) == 0, TRUE, FALSE)

count(steam) - count(subset(steam, NoCategory == TRUE))

# All boolean and binary columns
names(select_if(steam, is.logical))
boolean_cols = names(select_if(steam, is.logical))

names(select_if(steam, is.character))

names(select_if(steam, is.numeric))

# drop "QueryID", "ResponseID", "QueryName", "ResponseName", "PriceCurrency" as these don't add any useful info for the analysis
# drop "SteamSpyPlayersVariance", "SteamSpyOwnersVariance" as well as we will neglect the variance for this project for simplicity
steam <- select(steam, -c("QueryID", "ResponseID", "QueryName", "ResponseName", "PriceCurrency", "SteamSpyPlayersVariance", "SteamSpyOwnersVariance"))

# Export csv for analysis using tableau
#write.csv(steam, 'steam_games_cleaned.csv', row.names = FALSE)


# Convert boolean columns to binary for regression
for (colName in boolean_cols) {
  print(colName)
  
  steam[,colName] <- steam[,colName]*1
}

# Export csv for analysis using tableau
#write.csv(steam, 'steam_games_cleaned_bin.csv', row.names = FALSE)

# remove rows with target var = 0
steam33 <- subset(steam, steam$SteamSpyPlayersEstimate > 0)

summary(steam33$SteamSpyPlayersEstimate)
summary(steam33$SteamSpyOwners)

# normalize to handle outliers
steam33$log_owner <- log(steam33$SteamSpyOwners, 10)
steam33$log_players <- log(steam33$SteamSpyPlayersEstimate, 10)



# Analysis and Predictions


set.seed(123)


# Game Demand prediction -------------------------------------------------------

steam_owner_split <- createDataPartition(steam33$log_owner,p=0.6,list=FALSE)
steam_owner.train.df  <- steam33[steam_owner_split,]
steam_owner.test.df   <- steam33[-steam_owner_split,]


steam_owner.lm.all <- lm(log_owner ~.-SteamSpyOwners-SteamSpyPlayersEstimate-log_players, data = steam_owner.train.df)
summary(steam_owner.lm.all)


steam_owner.lm.all.pred<-predict(steam_owner.lm.all, steam_owner.test.df)

accuracy(steam_owner.lm.all.pred,steam_owner.test.df$log_owner) #accuracy measure
all_acc_owner <- accuracy(steam_owner.lm.all.pred,steam_owner.test.df$log_owner) 
all_acc_owner


# Stepwise methods

owner_m1 <- train(log_owner ~.-SteamSpyOwners-SteamSpyPlayersEstimate-log_players, data = steam_owner.train.df,trControl=trainControl(method='none'), method='glmStepAIC',direction='backward')
owner_m2 <- train(log_owner ~.-SteamSpyOwners-SteamSpyPlayersEstimate-log_players, data = steam_owner.train.df,trControl=trainControl(method='none'), method='glmStepAIC',direction='forward')
owner_m3 <- train(log_owner ~.-SteamSpyOwners-SteamSpyPlayersEstimate-log_players, data = steam_owner.train.df,trControl=trainControl(method='none'), method='glmStepAIC',direction='both')


coef(owner_m1$finalModel)
coef(owner_m2$finalModel)
coef(owner_m3$finalModel)


steam_owner.lm.back.pred <- predict(owner_m1, steam_owner.test.df)
steam_owner.lm.forw.pred <- predict(owner_m2, steam_owner.test.df)
steam_owner.lm.both.pred <- predict(owner_m3, steam_owner.test.df)

steam_owner.back_acc <- accuracy(steam_owner.lm.back.pred, steam_owner.test.df$log_owner)
steam_owner.forw_acc <- accuracy(steam_owner.lm.forw.pred, steam_owner.test.df$log_owner)
steam_owner.both_acc <- accuracy(steam_owner.lm.both.pred, steam_owner.test.df$log_owner)

comp_owner <- rbind(steam_owner.back_acc, steam_owner.forw_acc, steam_owner.both_acc,all_acc_owner)
rownames(comp_owner) <- c("Backwards","Forward","Stepwise","All")
comp_owner

summary(steam_owner.lm.all)
all_acc_owner

write.csv(t(summary(steam_owner.lm.all)$coefficients)[1:2,], 'steam_games_demand_model_coeffs.csv', row.names = TRUE)


# Active Player Base Prediction -----------------------------------------------

steam_player_split <- createDataPartition(steam33$log_players,p=0.6,list=FALSE)
steam_player.train.df  <- steam33[steam_player_split,]
steam_player.test.df   <- steam33[-steam_player_split,]


# All variables included
steam_player.lm.all <- lm(log_players ~.-SteamSpyOwners-SteamSpyPlayersEstimate-log_owner, data = steam_player.train.df)
summary(steam_player.lm.all)

steam_player.lm.all.pred <-predict(steam_player.lm.all, steam_player.test.df)

all_acc_plyr <- accuracy(steam_player.lm.all.pred,steam_player.test.df$log_players) 
all_acc_plyr


# Stepwise methods

player_m1 <- train(log_players ~.-SteamSpyOwners-SteamSpyPlayersEstimate-log_owner, data = steam_player.train.df,trControl=trainControl(method='none'), method='glmStepAIC',direction='backward')
player_m2 <- train(log_players ~.-SteamSpyOwners-SteamSpyPlayersEstimate-log_owner, data = steam_player.train.df,trControl=trainControl(method='none'), method='glmStepAIC',direction='forward')
player_m3 <- train(log_players ~.-SteamSpyOwners-SteamSpyPlayersEstimate-log_owner, data = steam_player.train.df,trControl=trainControl(method='none'), method='glmStepAIC',direction='both')


coef(player_m1$finalModel)
coef(player_m2$finalModel)
coef(player_m3$finalModel)


steam_player.lm.back.pred <- predict(owner_m1, steam_player.test.df)
steam_player.lm.forw.pred <- predict(owner_m2, steam_player.test.df)
steam_player.lm.both.pred <- predict(owner_m3, steam_player.test.df)

steam_player.back_acc <- accuracy(steam_player.lm.back.pred, steam_player.test.df$log_players)
steam_player.forw_acc <- accuracy(steam_player.lm.forw.pred, steam_player.test.df$log_players)
steam_player.both_acc <- accuracy(steam_player.lm.both.pred, steam_player.test.df$log_players)

comp_player <- rbind(steam_player.back_acc, steam_player.forw_acc, steam_player.both_acc,all_acc_plyr)
rownames(comp_player) <- c("Backwards","Forward","Stepwise","All")
comp_player


summary(steam_player.lm.all)
all_acc_plyr

write.csv(t(summary(steam_player.lm.all)$coefficients)[1:2,], 'steam_games_playerbase_model_coeffs.csv', row.names = TRUE)

# Important!!!!! In order to get the actual usable predictions, prediction results should be raised to the power of 10 
# Ex
steam_owner.lm.pred^10

