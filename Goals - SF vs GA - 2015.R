library(RGoogleAnalytics)
library(lubridate)
library(dplyr)


#oauth_token <- Auth(client.id = "387545337049-qoon4gdvbqr2fe9l4cti4qs19sptat2p.apps.googleusercontent.com",client.secret = "xnPrepkF2vDBEcWenP56jiox")
# Save the token object for future sessions
#save(oauth_token, file="oauth_token")

# Token -------------------------------------------------------------------


# Load the token object
load("../oauth_token")

profiles <- GetProfiles(oauth_token)

# Paris GA -----

query.list <- Init(start.date = "2015-01-01",
                   end.date = "2015-09-10",
                   dimensions = "ga:date",
                   metrics = "ga:goal16Completions,ga:sessions",
                   #metrics = "ga:sessions",
                   max.results = 90000,
                   table.id = "ga:46856038")

# Create the query object
ga.query <- QueryBuilder(query.list)


GA.paris <- GetReportData(ga.query, oauth_token,paginate_query = F)


GA.paris$date <- as.Date(GA.paris$date,format = "%Y%m%d")
GA.paris$month <- month(GA.paris$date,label = TRUE)


GA.paris.goals.agg <- aggregate(list(Goals=GA.paris$goal16Completions),by=list(Month=GA.paris$month), FUN="sum")
# Average goals per month (excluding current month)
pa.mean <- mean(GA.paris.goals.agg$Goals[c(1:nrow(GA.paris.goals.agg)-1)])
# Sum of goals
pa.sum <- sum(GA.paris.goals.agg$Goals[c(1:nrow(GA.paris.goals.agg)-1)])


# Paris SF -------------

source('../LeadsLoader.R')

SF.paris.goals <- subset(all.leads, all.leads$OwnerId=="00GD0000003Gm8JMAS" & all.leads$LeadSource=="Paris Website" )
SF.paris.goals$date <- as.Date(SF.paris.goals$CreatedDate)
SF.paris.goals$month <- month(SF.paris.goals$date,label = TRUE)
SF.paris.goals <- subset(SF.paris.goals, SF.paris.goals$date >"2015-01-01" )

SF.paris.goals.agg <- aggregate(list(Goals=SF.paris.goals$Id), by=list(Month=SF.paris.goals$month), FUN="length")



paris.compare <- merge.data.frame(GA.paris.goals.agg,SF.paris.goals.agg, by="Month", sort="F")
names(paris.compare) <- c("Month","GA","SF")
paris.compare$Var <- round(1-(with(paris.compare,(GA-SF)/GA)),2)

# Average GA to SF monthly variation
pa.var <- mean(paris.compare$Var[-3])






# Madrid GA -----

query.list <- Init(start.date = "2015-01-01",
                   end.date = "2015-09-10",
                   dimensions = "ga:date",
                   metrics = "ga:goal2Completions,ga:sessions",
                   #metrics = "ga:sessions",
                   max.results = 90000,
                   table.id = "ga:51262284")

# Create the query object
ga.query <- QueryBuilder(query.list)

GA.madrid <- GetReportData(ga.query, oauth_token,paginate_query = F)


GA.madrid$date <- as.Date(GA.madrid$date,format = "%Y%m%d")
GA.madrid$month <- month(GA.madrid$date,label = TRUE)


GA.madrid.goals.agg <- aggregate(list(Goals=GA.madrid$goal2Completions),by=list(Month=GA.madrid$month), FUN="sum")
# Average goals per month (excluding current month)
ma.mean <- mean(GA.madrid.goals.agg$Goals[c(1:nrow(GA.madrid.goals.agg)-1)])
# Total goals
ma.sum <- sum(GA.madrid.goals.agg$Goals[c(1:nrow(GA.madrid.goals.agg)-1)])


# Madrid SF -------------

#source('LeadsLoader.R')

SF.madrid.goals <- subset(all.leads, all.leads$OwnerId=="00GD0000003Go7rMAC" & all.leads$LeadSource=="Madrid Website" )
SF.madrid.goals$date <- as.Date(SF.madrid.goals$CreatedDate)
SF.madrid.goals$month <- month(SF.madrid.goals$date,label = TRUE)
SF.madrid.goals <- subset(SF.madrid.goals, SF.madrid.goals$date >"2015-01-01" )

SF.madrid.goals.agg <- aggregate(list(Goals=SF.madrid.goals$Id), by=list(Month=SF.madrid.goals$month), FUN="length")



madrid.compare <- merge.data.frame(GA.madrid.goals.agg,SF.madrid.goals.agg, by="Month", sort="F")
names(madrid.compare) <- c("Month","GA","SF")
madrid.compare$Var <- round(1-(with(madrid.compare,(GA-SF)/GA)),2)

# Average GA to SF monthly variation
ma.var <- mean(madrid.compare$Var[-3])
                  






# Heidelberg GA -----

query.list <- Init(start.date = "2015-01-01",
                   end.date = "2015-09-10",
                   dimensions = "ga:date",
                   metrics = "ga:goal5Completions,ga:sessions",
                   #metrics = "ga:sessions",
                   max.results = 90000,
                   table.id = "ga:52041505")

# Create the query object
ga.query <- QueryBuilder(query.list)

GA.heidelberg <- GetReportData(ga.query, oauth_token,paginate_query = F)


GA.heidelberg$date <- as.Date(GA.heidelberg$date,format = "%Y%m%d")
GA.heidelberg$month <- month(GA.heidelberg$date,label = TRUE)


GA.heidelberg.goals.agg <- aggregate(list(Goals=GA.heidelberg$goal5Completions),by=list(Month=GA.heidelberg$month), FUN="sum")
# Average goals per month (excluding current month)
hd.mean <- mean(GA.heidelberg.goals.agg$Goals[c(1:nrow(GA.heidelberg.goals.agg)-1)])
# Total goals
hd.sum <- sum(GA.heidelberg.goals.agg$Goals[c(1:nrow(GA.heidelberg.goals.agg)-1)])


# Heidelberg SF -------------

#source('LeadsLoader.R')

SF.heidelberg.goals <- subset(all.leads, all.leads$OwnerId=="00GD0000003GnllMAC" & all.leads$LeadSource=="Heidelberg Website" )
SF.heidelberg.goals$date <- as.Date(SF.heidelberg.goals$CreatedDate)
SF.heidelberg.goals$month <- month(SF.heidelberg.goals$date,label = TRUE)
SF.heidelberg.goals <- subset(SF.heidelberg.goals, SF.heidelberg.goals$date >"2015-01-01" )

SF.heidelberg.goals.agg <- aggregate(list(Goals=SF.heidelberg.goals$Id), by=list(Month=SF.heidelberg.goals$month), FUN="length")



heidelberg.compare <- merge.data.frame(GA.heidelberg.goals.agg,SF.heidelberg.goals.agg, by="Month", all = "T", sort="F")
names(heidelberg.compare) <- c("Month","GA","SF")
heidelberg.compare$Var <- round(1-(with(heidelberg.compare,(GA-SF)/GA)),2)

# Average GA to SF monthly variation
hd.var <- mean(heidelberg.compare$Var[-3])




# Compare All Sites (SF)-----------


compare.all.sf <- merge.data.frame(paris.compare[,c(-2,-4)], madrid.compare[,c(-2,-4)], by='Month',sort="F")
compare.all.sf <- merge.data.frame(compare.all.sf, heidelberg.compare[,c(-2,-4)], by='Month',sort="F")
names(compare.all.sf) <- c("Month","Paris","Madrid","Heidelberg")





paris.REG <- subset(SF.paris.goals, SF.paris.goals$Status=="REG")
paris.qualy <- subset(SF.paris.goals, SF.paris.goals$Status %in% c("REG", "Paid Tuition","Accepted","App In"))
paris <- c(sum(GA.paris$sessions),sum(paris.compare$GA), sum(paris.compare$SF),nrow(paris.qualy),nrow(paris.REG))


madrid.REG <- subset(SF.madrid.goals, SF.madrid.goals$Status=="REG")
madrid.qualy <- subset(SF.madrid.goals, SF.madrid.goals$Status %in% c("REG", "Paid Tuition","Accepted","App In"))
madrid <- c(sum(GA.madrid$sessions),sum(madrid.compare$GA), sum(madrid.compare$SF),nrow(madrid.qualy),nrow(madrid.REG))


heidelberg.REG <- subset(SF.heidelberg.goals, SF.heidelberg.goals$Status=="REG")
heidelberg.qualy <- subset(SF.heidelberg.goals, SF.heidelberg.goals$Status %in% c("REG", "Paid Tuition","Accepted","App In"))

heidelberg.compare$extrapolation <- round(heidelberg.compare$SF/.78,0) # workaround to fill in lost GA months
heidelberg <- c(sum(GA.heidelberg$sessions),sum(heidelberg.compare$extrapolation), sum(heidelberg.compare$SF),nrow(heidelberg.qualy),nrow(heidelberg.REG))

all <- data.frame(rbind(paris,madrid,heidelberg))
names(all) <- c("Sessions","GA","SF","Qualy","REG")
rownames(all) <- c("Paris", "Madrid", "Heidelberg")


all.props <- data.frame(cbind(Sessions=all$Sessions,RatetoSF=round(all$SF/all$Sessions,2),SF=all$SF,RatetoQualy=round(all$Qualy/all$SF,2),Qualy=all$Qualy))
rownames(all.props) <- c("Paris", "Madrid", "Heidelberg")
                        
