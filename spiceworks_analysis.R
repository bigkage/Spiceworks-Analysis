
library(dplyr)
library(ggplot2)
# library(ggvis)

tickets <- read.csv("report-all_tickets.csv")

tickets$date <- as.Date(tickets$Create.Date, format = "%Y-%m-%d")

tickets <- mutate(tickets, year = format(date, format="%Y"))

tickets$year <- as.factor(tickets$year)

tickets$Email.Message <- NULL

tickets <- mutate(tickets, Days.Open = gsub("< 1 day", "0", Days.Open)) %>% 
  mutate(Days.Open = gsub(" days", "", Days.Open)) %>% 
  mutate(Days.Open = gsub(" day", "", Days.Open)) %>% 
  mutate(Days.Open = gsub("\\d+s", "0", Days.Open)) %>% 
  mutate(Days.Open = as.integer(Days.Open))

# tickets <- mutate(tickets, Create.Date = as.Date(Create.Date))

##### fix missing locations #####
tickets$loc2 <- ""

tickets <- mutate(tickets, loc2 = ifelse(grepl("(?i)FHS", Summary), "FHS", loc2))
tickets <- mutate(tickets, loc2 = ifelse(grepl("(?i)CADD", Summary), "FHS", loc2))
tickets <- mutate(tickets, loc2 = ifelse(grepl("(?i)Fife High School|Butchett|Sarah George|Shula|twardowski|fhshelp|howell|fhs|gieck", Description), "FHS", loc2))

tickets <- mutate(tickets, loc2 = ifelse(grepl("(?i)CJH", Summary), "CJH", loc2))
tickets <- mutate(tickets, loc2 = ifelse(grepl("(?i)karen slavens|aker|hannah", Description), "CJH", loc2))

tickets <- mutate(tickets, loc2 = ifelse(grepl("(?i)SLM", Summary), "SLMS", loc2))
tickets <- mutate(tickets, loc2 = ifelse(grepl("(?i)Surprise Lake Middle School", Description), "SLMS", loc2))
tickets <- mutate(tickets, loc2 = ifelse(grepl("(?i)slmshelp|garrett|slms", Description), "SLMS", loc2))

tickets <- mutate(tickets, loc2 = ifelse(grepl("(?i)^END", Summary), "Endeavour", loc2))
tickets <- mutate(tickets, loc2 = ifelse(grepl("(?i)Endeavour", Summary), "Endeavour", loc2))
tickets <- mutate(tickets, loc2 = ifelse(grepl("(?i)wolfdorn|madams|tillinghast|endeavour|Jason Smerer|lacroix", Description), "Endeavour", loc2))

tickets <- mutate(tickets, loc2 = ifelse(grepl("(?i)^HED", Summary), "Hedden", loc2))
tickets <- mutate(tickets, loc2 = ifelse(grepl("(?i)Hedden", Summary), "Hedden", loc2))
tickets <- mutate(tickets, loc2 = ifelse(grepl("(?i)Hedden", Description), "Hedden", loc2))

tickets <- mutate(tickets, loc2 = ifelse(grepl("(?i)^DIS", Summary), "Discovery", loc2))
tickets <- mutate(tickets, loc2 = ifelse(grepl("(?i)Discovery|Mahoney", Summary), "Discovery", loc2))
tickets <- mutate(tickets, loc2 = ifelse(grepl("(?i)discoveryhelp|discovery", Description), "Discovery", loc2))


tickets <- mutate(tickets, loc2 = ifelse(grepl("(?i)^ESC|FSD|ESD", Summary), "Admin", loc2))
tickets <- mutate(tickets, loc2 = ifelse(grepl("(?i)^LOC", Summary), "LOC", loc2))
tickets <- mutate(tickets, loc2 = ifelse(grepl("(?i)^TRANS|TRNS|TRN", Summary), "Trans", loc2))
tickets <- mutate(tickets, loc2 = ifelse(grepl("(?i)transpo", Summary), "Trans", loc2))
tickets <- mutate(tickets, loc2 = ifelse(grepl("(?i)swiger|burrus|janci|payroll|mcfarland", Description), "Admin", loc2))

tickets <- mutate(tickets, Location = as.character(Location))

missing_loc <- filter(tickets, loc2 == "") %>% 
  select(Summary,Description,Location,loc2)

tickets <- mutate(tickets, loc2 = ifelse(Location == "", loc2, Location))
tickets <- mutate(tickets, loc2 = ifelse(loc2 == "", Location, loc2))

tickets$oldlocation <- tickets$Location
tickets$Location <- tickets$loc2
tickets$loc2 <- NULL

tickets$Location <- as.factor(tickets$Location)

tickets$Location <- factor(tickets$Location, levels = c("Discovery", "Endeavour", "Hedden", "SLMS", "CJH", "FHS", "Admin", "Trans", "LOC", "Not Specific"))

##### fix missing categories #####

tickets <- mutate(tickets, full_text = paste(Summary, Description, sep = " "))



##### visualize #####

# tickets per building
ggplot(tickets, aes(x = Location, fill = year)) +
  geom_bar(position = "dodge") +
  ggtitle("All Tickets")

tickets %>% 
  filter(!is.na(Location)) %>% 
  filter(Location != "Trans") %>% 
  filter(Location != "LOC") %>% 
ggplot(aes(x = date)) +
  geom_histogram(binwidth = 7) +
  facet_grid(Location ~ .) +
  ggtitle("All Tickets")


##### text mining #####

ticket_corpus <- Corpus(VectorSource(as.character(tickets$full_text)))

# ticket_corpus <- tm_map(trucks_corpus, removePunctuation)
ticket_corpus <- tm_map(ticket_corpus, content_transformer(tolower)) 
ticket_corpus <- tm_map(ticket_corpus, removeWords, stopwords("english"))
ticket_corpus <- tm_map(ticket_corpus, stripWhitespace)
# trucks_corpus <- tm_map(trucks_corpus, stemDocument)

dtm <- DocumentTermMatrix(trucks_corpus)
