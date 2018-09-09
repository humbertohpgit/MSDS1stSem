## Week 2 Assignment - SQL and R

install.packages("RMySQL")
library(RMySQL)
library(DBI)

movrevdb <- dbConnect(RMySQL::MySQL(), user='root', password='Pass@word1', dbname='moviereviews', host='localhost')
dbListTables(movrevdb)

query <- dbSendQuery(movrevdb, "select M.movieId, M.title, R.revId, R.revname, R.rating
                                from movies M
                                inner join reviewers R
                                on M.movieId = R.movieId")
  
movrev_df <- fetch(query)
movrev_df

library(ggplot2)
ggplot(movrev_df, aes(x=rating)) + geom_bar() + labs(y = "# of Reviews", x = "Ratings", title = "Movie Reviews") + theme_bw()
