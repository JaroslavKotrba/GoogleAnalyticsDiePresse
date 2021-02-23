# DiePresse

#---1)-Traffic------------------------------------------------------------------------------------------------------------

setwd("~/R/DiePresse")
dir()

library(readxl)
Traffic <- read_excel("Dataset_Die_Presse.xlsx", sheet = 1)

Traffic <- data.frame(lapply(Traffic,as.character), stringsAsFactors = FALSE)
colnames(Traffic) <- c(c("channel", "date", "pageViews", "sessions", "users"))
Traffic$date <- gsub('^(.{4})(.*)$', '\\1/\\2', Traffic$date)
Traffic$date <- gsub('^(.{7})(.*)$', '\\1/\\2', Traffic$date)

Traffic <- as.data.frame(unclass(Traffic))
str(Traffic)

# 31 Tage = ein Monat
# Fuenf Kanaele

Traffic$date <- as.Date(as.character(Traffic$date), "%Y/%m/%d")
Traffic$pageViews <- as.numeric(as.character(Traffic$pageViews))
Traffic$sessions <- as.numeric(as.character(Traffic$sessions))
Traffic$users <- as.numeric(as.character(Traffic$users))

summary(Traffic)

cor(Traffic[3:5])

# Anzahl von Kanaelen (5) ungleiche Verteilung von Beobachtungen zwischen der Kanaelen.
# Hohe Korrelation

library(tidyverse)
library(lubridate)
Traffic$test <- Traffic$users/Traffic$pageViews
Traffic$day <- wday(Traffic$date, label = T, abbr = F)
Days <- data.frame(Traffic %>% group_by(day) %>% 
                         summarise(
                           totalPageViews = sum(pageViews),
                           freq = n(),
                           totalPageViews.AVG = round(sum(pageViews)/31,2),
                           totalSessions = sum(sessions),
                           totalUsers = sum(users),
                           usersPageViews = round(mean(test),2)
                         ))
Days <- Days
print(Days)

library(tidyverse)
Channels <- data.frame(Traffic %>% group_by(channel) %>% 
                            summarise(
                              totalPageViews = sum(pageViews),
                              freq = n(),
                              totalPageViews.AVG = round(sum(pageViews)/31,2),
                              totalSessions = sum(sessions),
                              totalUsers = sum(users),
                              usersPageViews = round(mean(test),2)
                            ))
Channels <- Channels
print(Channels)

Channels$test/Channels$totalPageViews.AVG
# SEA hat 1731.61 pageViews pro Tag, die zweite groeste Reichweite.

library(tidyverse)
TrafficDate <- data.frame(Traffic %>% group_by(date,channel) %>% 
                            summarise(
                              pageViews = sum(pageViews),
                              sessions = sum(sessions),
                              users = sum(users)
                            ))
TrafficDate <- TrafficDate
TrafficDate

# library(openxlsx)
# list_of_datasets <- list("TrafficDate" = TrafficDate)
# write.xlsx(list_of_datasets, file= "TrafficData.xlsx")

library(ggplot2)
require(scales)
library(plotly)
g <- ggplot(data = TrafficDate,
       mapping = aes(x = date, y = pageViews)) +
  geom_point(color="blue") +
  geom_line() +
  ylab("") +
  xlab("") +
  facet_grid(facets = channel ~ .) +
  scale_x_date(labels = function(x) format(x, "%d-%b")) +
  theme_bw()

ggplotly(g)

g <- ggplot(TrafficDate, aes(x=date)) + 
  geom_line(aes(y = pageViews), color = "darkred") +
  facet_grid(facets = channel ~ .) +
  geom_line(aes(y = sessions), color = "darkblue", linetype="dashed") +
  facet_grid(facets = channel ~ .) +
  geom_line(aes(y = users), color = "darkgreen", linetype="dotted") +
  facet_grid(facets = channel ~ .)

ggplotly(g)

# Die Trends sehen sehr aehnlich aus, SEA und SEO sind die besten, (none) kann man nicht beurteilen. 
# Ich wuerde empfehlen, SEA und SEO zu helfen und dort Geld und Zeit zu investieren.
# Bei SEO gibt es auf jeden Fall "space" nach oben.

#---2)-Articles------------------------------------------------------------------------------------------------------------

setwd("~/R/DiePresse")
dir()

library(readxl)
Articles <- read_excel("Dataset_Die_Presse.xlsx", sheet = 2)

Articles <- data.frame(lapply(Articles,as.character), stringsAsFactors = FALSE)
Articles <- as.data.frame(unclass(Articles))
str(Articles)

Articles$articleID <- as.character(Articles$articleID)
Articles$pageViews <- as.numeric(as.character(Articles$pageViews))
Articles$conversions <- as.numeric(as.character(Articles$conversions))
str(Articles)

# Es gibt fuenf Abteilungen.

summary(Articles)

# Economist und politics haben die meisten Artikeln geschrieben.
# Die gleiche Nummer von "yes" und "no" fuer alle Artikel.

# TOP 10 articles pageViews
TOP_10 <- Articles[order(-Articles$pageViews),]
head(TOP_10,10)

# TOP 10 articles conversions
TOP_10 <- Articles[order(-Articles$conversions),]
head(TOP_10,10)

library(ggplot2)
require(scales)
library(plotly)
g <- ggplot(data=Articles, aes(x=conversions, y=pageViews, text = paste0("ArticleID: ", articleID,"\n","PageView: ", pageViews,"\n","Conversions: ", conversions))) +
  geom_point(stat = "identity", aes(fill = articleDepartment, size=conversions)) +
  scale_y_continuous(labels = comma) +
  theme(legend.position="right") +
  labs(fill = "") +
  theme_bw()
g <- ggplotly(g, tooltip = c("text"))
g

# Die erfolgreichsten Artikel sind lifestyle und opinion rechts oben.
# Rechts unten gibt es jede Menge von guten Artikeln, auf die man mehr Aufmerksam machen sollte.

# Departments pageViews
Departments <- data.frame(Articles %>% group_by(articleDepartment) %>% 
                            summarise(
                              totalConversions = sum(conversions),
                              totalPageViews = sum(pageViews),
                              freq = n(),
                              totalPageViews.AVG = mean(pageViews),
                              totalPageViews.MED = median(pageViews)
                            ))
Departments <- Departments
Departments

# Bei economist und politics sollte man sich mehr auf die Quantitaet konzentrieren. 
# Die aktuellen Themen werden nur kurz gelesen.

# Departments conversions
Departments <- data.frame(Articles %>% group_by(articleDepartment, isArticlePremium) %>% 
                            summarise(
                              totalConversions = sum(conversions),
                              totalPageViews = sum(pageViews),
                              totalArticleWritten = n(),
                              totalConversions.AVG = round(mean(conversions),1),
                              totalConversions.MED = median(conversions),
                              totalConversions.pageViews = round(sum(pageViews)/sum(conversions),1),
                              totalPageViews.AVG = round(mean(pageViews),1),
                              totalPageViews.MED = median(pageViews)
                            ))
Departments <- Departments
Departments

# Die erfolgreichsten Abteilungen was die Conversions pro Seite betrifft gehn an lifestyle (64.7) und opinion (63.4) 
# man sollte mehr auf die Qualitaet statt Quantitaet weiter achten und posten & sharen.
# Opinion (1521) und lifestyle (1612.5) haben auch wenigsten pageViews pro Conversion man sollte diese unterstuetzen und verbreiten.
# Problem bei der Kultur abteilung? Es gibt meistens Views pro Seite (115409.5) bei premium Artikeln, aber am wenigsten Conversions (54.3). 
# Ist etwas falsch eingestellt, dass nicht zu conversions kommt? Sie sind vermutlich stark unterstuezt (posten, sharen...) aber vergeblich, 
# statt das sollte man sich auf andere Abteilungen konzentrieren.
# Economist und politics schwache Ergebnisse bei totalPageViews pro Artikl. Man muss immer die aktuellsten Artikl schreiben.

library(openxlsx)
list_of_datasets <- list("Departments" = Departments)
write.xlsx(list_of_datasets, file= "ArticlesData.xlsx")

library(ggplot2)
require(scales)
library(plotly)
g <- ggplot(data=Departments, aes(x=articleDepartment, y=totalPageViews)) +
  geom_bar(stat = "identity", aes(fill = isArticlePremium)) +
  ylab("pageViews") +
  xlab("articleDeparment") +
  scale_y_continuous(labels = comma) +
  theme(legend.position="top") +
  scale_fill_discrete("premium") +
  theme_bw()

ggplotly(g)

# Fuer politics gibt es immer noch Luft nach oben. Ich empfehle mehr premium Artikl, wie bei economist, weil politics mehr pageViews hat. 
# Die selben Leute werden sich economist und politics anschauen.
