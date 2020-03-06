# Prepare downloaded datasets and analyze
# Web scraping project
# STA 6233 - Advanced R Programming
# by Ben Graf
# Created 2020-02-17, Last edited 2020-03-04

#---- SETUP ----

#Bring in Libraries
pacman::p_load(pacman,tidyverse,ggpubr,strucchange,grDevices)

#Bring in datasets
setwd("/Users/Ben/Library/Mobile Documents/com~apple~CloudDocs/Documents/UTSA Master's/Semester 2/STA 6233 Advanced R Programming/Web Scraping Project/Datasets")

allmovies <- read.csv("./top movies/boxoffice.csv")

babynames <- data.frame(name=character(), sex=character(), count=numeric(), babyyear=numeric(), sexyearrank=numeric())
for (years in 1880:2018) {
  tempbaby <- read.csv(paste0("./baby names/yob",years,".txt"), header = FALSE) %>% group_by(V2) %>% mutate(babyyear = years, sexyearrank = min_rank(desc(V3)))
  names(tempbaby)[1:3] <- c("name", "sex", "count")
  babynames <- bind_rows(babynames,tempbaby)
}
rm(tempbaby)

# RELOAD CHARACTER DATA INSTEAD OF RE-SCRAPING (AFTER SCRAPING ONCE AND WRITING TO CSV)
#charallyears <- read.csv("./scraped character names/top5movie_all_characters.csv", stringsAsFactors = FALSE)   #This version is missing movies that did not scrape the first pass
charallyears <- read.csv("./scraped character names/top5movie_all_characters_found_missing.csv", stringsAsFactors = FALSE)   #This version has all movies.
missingmovies <- charallyears %>% filter(is.na(moviechar))   #How many had errors?  Need to fix those manually.
charallyears <- charallyears %>% filter(!is.na(moviechar))


#---- SCRAPE ALL OF THE MOVIE CHARACTER DATA I NEED ----
# WARNING: THE CODE BELOW HITS IMDB MANY TIMES TO SCRAPE THE CHARACTER DATA FOR ALL TOP 5 MOVIES.
# AFTER SCRAPING ONE TIME AND WRITING TO A CSV, CAN SKIP THIS CODE AND USE THE RELOAD LINE ABOVE.

#Make a vector with only the top 5 movies in each year, based on lifetime box office gross
#Baby names data only goes to 2018, so I can only get 5 years (or more) after the movie year for movies from 2013 or earlier
best5movies <- allmovies %>% filter(year <= 2013) %>% group_by(year) %>% mutate(movieyearrank = min_rank(desc(lifetime_gross))) %>% filter(movieyearrank <= 5)

charallyears <- data.frame(moviechar=character(), moviename=character(), movieyear=integer())
for (i in 1:nrow(best5movies)) {
  tempchar <- as.data.frame(charscrape(as.character(best5movies[i,]$title),best5movies[i,]$year), stringsAsFactors=FALSE) %>%
    mutate(moviename = as.character(best5movies[i,]$title), movieyear = best5movies[i,]$year)
  names(tempchar)[1] <- "moviechar"
  charallyears <- bind_rows(charallyears,tempchar)
  Sys.sleep(5)   #IMDB requested a crawl delay of 5 seconds between server hits
}
rm(tempchar)
missingmovies <- charallyears %>% filter(is.na(moviechar))   #How many had errors?  Need to fix those manually.
write.csv(charallyears,"./scraped character names/top5movie_all_characters.csv", row.names = FALSE)


#---- RE-SCRAPE MOVIES THAT WERE MISSED THE FIRST TIME ----

#After researching possible issues with the movies that were missed, I created the missingrename.csv file
missingrename <- read.csv("./top movies/missingrename.csv", stringsAsFactors = FALSE)

#Set up a data frame with just the names/years to search for
missingscrape <- as.data.frame(matrix(nrow = nrow(missingrename), ncol = 2))
for (a in 1:nrow(missingrename)) {
  if (is.na(missingrename[a,"revisedname"])) {
    missingscrape[a,1] <- missingrename[a,"moviename"]
  } else {
    missingscrape[a,1] <- missingrename[a,"revisedname"]
  }
  if (is.na(missingrename[a,"revisedyear"])) {
    missingscrape[a,2] <- missingrename[a,"movieyear"]
  } else {
    missingscrape[a,2] <- missingrename[a,"revisedyear"]
  }
}

charmissing <- data.frame(moviechar=character(), moviename=character(), movieyear=integer())
for (i in 1:nrow(missingscrape)) {
  tempchar <- as.data.frame(charscrape(as.character(missingscrape[i,1]),missingscrape[i,2]), stringsAsFactors=FALSE) %>%
    mutate(moviename = as.character(missingrename[i,"moviename"]), movieyear = missingrename[i,"movieyear"])
  names(tempchar)[1] <- "moviechar"
  charmissing <- bind_rows(charmissing,tempchar)
  Sys.sleep(5)   #IMDB requested a crawl delay of 5 seconds between server hits
}
rm(tempchar)
missingmoviesstill <- charmissing %>% filter(is.na(moviechar))   #How many had errors?  Need to fix those manually.
charallyears <- bind_rows(charallyears,charmissing)
write.csv(charallyears,"./scraped character names/top5movie_all_characters_found_missing.csv", row.names = FALSE)


#---- BREAK OUT THE SCRAPED NAMES INTO INDIVIDUAL NAMES USING CHARNAMES FUNCTION ----

howmanychars <- 5
movies <- distinct(charallyears,moviename)
allcharnames <- data.frame(indivname=character(), fullname=character(), moviename=character(), movieyear=integer())
for (j in 1:nrow(movies)) {
  tempall <- charallyears %>% filter(moviename==movies[j,])
  tempall2 <- charnames(tempall,howmanychars) %>% 
    mutate(moviename = tempall$moviename[1], movieyear = tempall$movieyear[1])
  allcharnames <- bind_rows(allcharnames,tempall2)
}
rm(tempall,tempall2)


#---- SET UP THE ANALYSIS DATAFRAME ----

yearplusorminus <- 10
yearframe <- as.data.frame(matrix(nrow = nrow(allcharnames), ncol = 2*yearplusorminus+1))
countframe <- as.data.frame(matrix(nrow = nrow(allcharnames), ncol = 2*yearplusorminus+1))
numNA <- as.data.frame(matrix(nrow = nrow(allcharnames), ncol = 1))
num4 <- as.data.frame(matrix(nrow = nrow(allcharnames), ncol = 1))
chowpvalue <- as.data.frame(matrix(nrow = nrow(allcharnames), ncol = 1))
for (k in 1:nrow(allcharnames)) {
  yearframe[k,] <- c((allcharnames$movieyear[k]-yearplusorminus):(allcharnames$movieyear[k]+yearplusorminus))
  countframe[k,] <- namesubset(allcharnames$indivname[k], allcharnames$movieyear[k], yearplusorminus)
  numNA[k,] <- sum(is.na(countframe[k,]))
  num4[k,] <- sum(countframe[k,]==4, na.rm = TRUE)
  if (numNA[k,] + num4[k,] < 17) {
    x <- as.vector(yearframe[k,], mode = "numeric")
    y <- as.vector(countframe[k,], mode = "numeric")
    chowpvalue[k,] <- sctest(y ~ x, type = "Chow", point = 11)[[2]]
  } else {
    chowpvalue[k,] <- NA
  }
}

#Name columns
for (m in 1:yearplusorminus) {
  names(yearframe)[m] <- paste0("Year",yearplusorminus+1-m,"Before")
  names(yearframe)[2*yearplusorminus+2-m] <- paste0("Year",yearplusorminus-m+1,"After")
  names(countframe)[m] <- paste0("Count",yearplusorminus+1-m,"Before")
  names(countframe)[2*yearplusorminus+2-m] <- paste0("Count",yearplusorminus-m+1,"After")
}
names(yearframe)[yearplusorminus+1] <- "YearMovie"
names(countframe)[yearplusorminus+1] <- "CountMovie"
names(numNA) <- "numNA"
names(num4) <- "num4"
names(chowpvalue) <- "chowPvalue"

#Combine things into one data frame
analyzenames <- bind_cols(allcharnames, yearframe, countframe, numNA, num4, chowpvalue)
rm(yearframe, countframe, numNA, num4, chowpvalue)

alpha <- 0.01   #The sensitivity for our chow test. A movie character is considered to have had an effect on a baby name if the p-value is less than this.
effectthreshold <- 2   #A movie (not just one character) is considered to have had an effect on baby naming if its characters had at least this many name effects.
analyzenames <- analyzenames %>% mutate(nameeffect = (chowPvalue < alpha))
analyzenames <- analyzenames %>% group_by(moviename, movieyear) %>% mutate(movieeffect = (sum(nameeffect, na.rm = TRUE) >= effectthreshold))

#Slim down name analysis for easier viewing
slimanalyze <- analyzenames %>% select(indivname, fullname, moviename, movieyear, numNA, num4, chowPvalue, nameeffect, movieeffect)

#Slim down to summary of effects by movie (instead of individual names)
movieanalyze <- analyzenames %>% select(moviename, movieyear, movieeffect) %>% unique()
nummovieeffects <- sum(movieanalyze$movieeffect)
nummovies <- nrow(movieanalyze)
nummovieeffects/nummovies

numNAnames <- sum(is.na(slimanalyze$nameeffect))
numnames <- nrow(analyzenames)
numNAnames/numnames

#Check how many movies have fewer than 5 individual names (not characters) to analyze
movienamenumber <- slimanalyze %>% count(moviename)
table(movienamenumber$n)
sum(movienamenumber$n < 5)
sum(movienamenumber$n < 4)

table(movieanalyze$movieyear)

#Results with the following settings:  howmanychars = 5, yearplusorminus = 10, (numNA[k,] + num4[k,] < 17), alpha = 0.01
#For effectthreshold = (1, 2, 3, 4, 5)
movieeffectpercent <- data.frame(EffectThreshold = c(1, 2, 3, 4, 5), Percentage = c(90.87948, 74.2671, 50.81433, 29.31596, 13.02932))
ggplot(movieeffectpercent, aes(EffectThreshold, Percentage)) + 
  geom_col(fill = "#FF7F0EFF") +
  labs(title = "Percent of movies having at least the number of name effects shown") +
  xlab("Movie Effect Threshold") +
  geom_text(aes(label = format(round(Percentage, digits = 1), nsmall = 1)), nudge_y = 4, size = 20) +
  scale_y_continuous(limits=c(0,100)) +
  theme_classic() +
  list(theme(legend.position = "none",
             plot.title = element_text(size = 55),
             axis.title = element_text(size = 50),
             axis.text = element_text(size = 40)))

#Examine how many movies had effects over time
effectsovertime <- movieanalyze %>% group_by(movieyear) %>% summarize(numwitheffect = sum(movieeffect))
#effectsovertime <- movieanalyze %>% group_by(movieyear) %>% summarize(percentwitheffect = (sum(movieeffect) / n()))
effectsovertime <- effectsovertime %>% filter(effectsovertime$movieyear>=1967)
ggscatter(effectsovertime, x = "movieyear", y = "numwitheffect",
                add = "reg.line",               # Add regression line
                conf.int = TRUE,                # Add confidence interval
                color = "#1F77B4FF", palette = "d3", # Color by groups "timeframe"
                title = "How many of the top 5 movies each year affected baby names?",
                size = 10
) +
  stat_cor(aes(color = "navyblue"), size = 15, label.y = 1) +   # Add correlation coefficient
  xlab("Movie Release") +
  ylab("Number with Effect") +
  theme_classic() +
  list(theme(legend.position = "none",
             plot.title = element_text(size = 55),
             axis.title = element_text(size = 50),
             axis.text = element_text(size = 40)))



#---- PLOT A NAME ----

thename <- "Oscar"

#This stretch of code plots a name in the years surrounding a movie's release (missing SSA data is shown as '4' in this chart)
themovie <- "The Odd Couple"
nameindex <- which((analyzenames$indivname==thename) & (analyzenames$moviename==themovie))
x <- as.data.frame(t(analyzenames[nameindex,5:(4+2*yearplusorminus+1)]))
y <- as.data.frame(t(analyzenames[nameindex,(4+2*yearplusorminus+1+1):(4+4*yearplusorminus+2)]))
timeframe <- as.data.frame(c(rep("Before",times = yearplusorminus), "Movie", rep("After",times = yearplusorminus)))
names(x) <- "years"
names(y) <- "totals"
names(timeframe) <- "timeframe"
toplot <- bind_cols(x,y,timeframe)
ggscatter(toplot, x = "years", y = "totals",
                add = "reg.line",               # Add regression line
                conf.int = TRUE,                # Add confidence interval
                color = "timeframe", palette = "d3", # Color by groups "timeframe"
                shape = "timeframe",                  # Change point shape by groups "timeframe"
                size = 7,
                title = eval(parse(text=paste0("expression('10 years before & after'~bolditalic('",themovie,"')~'for the name'~bold('",thename,"'))")))
)+
  stat_cor(aes(color = timeframe), size = 10) +   # Add correlation coefficient
  xlab("Year") +
  ylab("New babies with this name") +
  geom_text(data=toplot[yearplusorminus+1,], aes(label=years), nudge_y = 0.08*toplot[yearplusorminus+1,2], size = 10) +
  theme_classic() +
  list(theme(legend.position = "none",
             plot.title = element_text(size = 35),
             axis.title = element_text(size = 35),
             axis.text = element_text(size = 20)))

#This stretch of code plots a name over all available SSA data (missing SSA data is not shown in this chart)
babynames %>% 
  filter(name==thename) %>% 
  group_by(babyyear) %>% 
  summarize(total = sum(count)) %>% 
  ggplot() +
  geom_line(aes(x = babyyear, y = total)) +
  labs(title = eval(parse(text=paste0("expression('Popularity of the name'~bold('",thename,"'))"))))
