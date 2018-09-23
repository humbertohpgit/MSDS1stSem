
## Load data set from Github repo

### If Local file - 
### setwd("C:/DATA/HHP/Personal/Degrees/Ms. Data Science (CUNY)/R Working Dir")
### chess_raw <- read.delim("chess_raw.txt", header = FALSE, sep = "\t", stringsAsFactors=FALSE)

chess_raw <- read.delim("https://raw.githubusercontent.com/humbertohpgit/MSDS1stSem/master/chess_raw.txt", header = FALSE, sep = "\t", stringsAsFactors=FALSE)
head(chess_raw)
chess_raw
is(chess_raw)
chess_raw[188,]
library(stringr)

## Extract & format Full Names
playernames <- data.frame(str_extract_all(chess_raw, "[[:upper:]]{3,} [[:upper:]]{1,} [[:upper:]]{0,}[ -][[:upper:]]{0,}"), stringsAsFactors=FALSE)
playernames
playernamesfmt <- data.frame(lapply(playernames, function(x) paste(x, ",")), stringsAsFactors=FALSE)
names(playernamesfmt) <- "PlayerName"
playernamesfmt

## Extract & format States
playerstates <- str_extract_all(chess_raw, "[[:upper:]]{2} \\|")
playerstates
playerstatesfmt <- data.frame(str_extract_all(playerstates, "[[:alpha:]]{2}"), stringsAsFactors=FALSE)
playerstatesfmt
playerstatesfmt2 <- data.frame(lapply(playerstatesfmt, function(x) paste(x, ",")), stringsAsFactors=FALSE)
names(playerstatesfmt2) <- "PlayerState"
playerstatesfmt2

## Extract & format Total Points
playerpoints <- data.frame(str_extract_all(chess_raw, "[0-9]\\.[0-9]"), stringsAsFactors=FALSE)
playerpoints
playerpointsfmt <- data.frame(lapply(playerpoints, function(x) paste(x, ",")), stringsAsFactors=FALSE)
names(playerpointsfmt) <- "PlayerPoints"
playerpointsfmt

## Extract & format Pre-Rating
playerprerating <- str_extract_all(chess_raw, "R:[[:space:]]{1,}[[:alnum:]]{3,}[[:space:]]{0,}")
playerprerating
playerpreratingfmt <- data.frame(str_extract_all(playerprerating, " [[:alnum:]]{3,}"), stringsAsFactors=FALSE)
playerpreratingfmt2 <- data.frame(lapply(playerpreratingfmt, function(x) paste(x, ",")), stringsAsFactors=FALSE)
names(playerpreratingfmt2) <- "PlayerPreRating"
playerpreratingfmt2

## Extract and Calculate Avg. Pre Rating of Opponents
### Extract Opponents for each Player
opps <- str_extract_all(chess_raw, "\\|[1-6]\\.[0-5]  \\|[WDLHUBX][[:print:]]{1,4}\\|[WDLHUBX][[:print:]]{1,4}\\|[WDLHUBX][[:print:]]{1,4}\\|[WDLHUBX][[:print:]]{1,4}\\|[WDLHUBX][[:print:]]{1,4}\\|[WDLHUBX][[:print:]]{1,4}\\|[WDLHUBX][[:print:]]{1,4}\\|")
opps
opps2 <- str_extract_all(opps, "\\|[WDLHUBX][[:print:]]{1,4}\\|[WDLHUBX][[:print:]]{1,4}\\|[WDLHUBX][[:print:]]{1,4}\\|[WDLHUBX][[:print:]]{1,4}\\|[WDLHUBX][[:print:]]{1,4}\\|[WDLHUBX][[:print:]]{1,4}\\|[WDLHUBX][[:print:]]{1,4}\\|")
opps2
opps2 <- data.frame(opps2, stringsAsFactors=FALSE)
names(opps2) <- "Opps"
opps2
opps3 <- lapply(opps2, function(x) str_extract_all(x, "[[:digit:]]{1,2}"))
opps3

### Build table of PreRatings for each Player/Opponent
plyrpreratcalc <- data.frame(str_extract_all(playerprerating, " [[:digit:]]{3,}"), stringsAsFactors=FALSE)
plyrpreratcalc
plyrpreratcalcfmt <- data.frame(cbind(c(1:64), plyrpreratcalc))
plyrpreratcalcfmt
names(plyrpreratcalcfmt) <- c("Player", "PlayerPreRating")
plyrpreratcalcfmt$PlayerPreRating <- as.numeric(plyrpreratcalcfmt$PlayerPreRating)
plyrpreratcalcfmt

### Build table of Opponents for each Player
opps5 <- c(0,0)
for(i in 1:64){
opps4 <- data.frame(cbind(i, as.numeric(opps3$Opps[[i]])))
opps5 <- rbind(opps5, opps4)
}
names(opps5) <- c("Player", "Opps")
opps5 <- opps5[-1,]
opps5

### Map Pre Ratings for each Player's Opponents
library(dplyr)
opps6 <- opps5 %>% left_join(plyrpreratcalcfmt, by = c("Opps" = "Player"))
opps6

### Calculate Avg. Opponent Pre Rating (and give format for final merge)
oppsavgprerating <- summarise(group_by(opps6,Player), mean(PlayerPreRating))
names(oppsavgprerating) <- c("Player", "OppsAvgPreRating")
oppsavgprerating
oppsavgpreratingfmt <- oppsavgprerating[,-1]
oppsavgpreratingfmt

## Build the final dataframe including all the data points (columns) calculated: PlayerName, PlayerState, PlayerPoints, PlayerPreRating, OppsAvgPreRating
player_df <- cbind(playernamesfmt, playerstatesfmt2, playerpointsfmt, playerpreratingfmt2, oppsavgpreratingfmt)
str(player_df)
head(player_df,1)
player_df

## Write the final result set to a CSV file
setwd("C:/DATA/HHP/Personal/Degrees/Ms. Data Science (CUNY)/R Working Dir")
write.csv(player_df, "ChessPlayerStats.csv", row.names = TRUE)

