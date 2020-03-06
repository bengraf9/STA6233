# Experimenting and testing done prior to building final code
# Web scraping project
# STA 6233 - Advanced R Programming
# by Ben Graf
# Created 2020-02-15, Last edited 2020-03-04


#---- OLD VERSION OF FUNCTION SEPARATING INTO INDIVIDUAL NAMES ----

charnamesOLD <- function(char_vector, number = length(char_vector)) {
  #Take a vector of character names and split them into individual names (do not care about first/last)
  #The process is: (a) split each string, (b) convert to one long vector, (c) convert to "title" case, (d) eliminate duplicate names, (e) eliminate common non-names
  
  if (number > length(char_vector)) {
    number <- length(char_vector)
  }
  charnames <- char_vector[(1:number)] %>% str_split(" ") %>% unlist() %>% str_to_title() %>% unique()
  
  #Get rid of common non-names
  charnames <- charnames[!charnames %in% c("","\n","/","-","the","and","&")]
  #Get rid of common honorifics
  charnames <- charnames[!charnames %in% c("Master", "Maestro", "Mr", "Mr.", "Miss", "Mrs", "Mrs.", "Ms", "Ms.", "Mx", "Mx.", "M", "M.", "Sir", "Sire", "Mistress", "Madam", "Madame", "Ma'am", 
                                           "Esquire", "Esq", "Esq.", "Senior", "Sr", "Sr.", "Junior", "Jr", "Jr.", "I", "Ii", "Iii", "Iv", "V", "Vi", "Vii", "Viii", "Ix", "X", "Honourable", 
                                           "Honorable", "Hon", "Hon.", "Right", "Most", "Doctor", "Dr", "Dr.", "Professor", "Prof", "Prof.", "Chancellor", "Vice-chancellor", "Principal", 
                                           "President", "Pres", "Pres.", "Vice", "Vp", "Warden", "Regent", "Rector", "Provost", "Director", "Dir", "Dir.", "Coach", "Agent", "Uncle", "Aunt",
                                           "Cousin", "Grandfather", "Grandmother", "Grandpa", "Grandma")]
  #Get rid of religious titles
  charnames <- charnames[!charnames %in% c("Pope", "Cardinal", "Bishop", "Reverend", "Rev", "Rev.", "Revd", "Revd.", "Father", "Fr", "Fr.", "Pastor", "Pr", "Pr.", "Brother", "Br", "Br.", 
                                           "Sister", "Mother", "Elder", "Rabbi", "Cantor", "Grand", "Venerable")]
  #Get rid of royal titles
  charnames <- charnames[!charnames %in% c("Emperor", "Empress", "Kaiser", "Tsar", "King", "Queen", "High", "Great", "Archduke", "Archduchess", "Prince", "Princess", "Duke", "Duchess", "Crown",
                                           "Dauphin", "Jarl", "Sovereign", "Marquis", "Marquess", "Count", "Countess", "Viscount", "Baron", "Baroness", "Dame", "Lord", "Lady", "Knight",
                                           "Maid")]
  #Get rid of military ranks
  charnames <- charnames[!charnames %in% c("Admiral", "Adm", "Adm.", "Rear", "Commodore", "Comm", "Comm.", "Captain", "Capt", "Capt.", "Cpt", "Cpt.", "Commander", "Cdr", "Cdr.", "Lieutenant", 
                                           "Lt", "Lt.", "Ensign", "Ens", "Ens.", "Midshipman", "Officer", "Cadet", "Warrant", "Petty", "Seaman", "Field", "Marshal", "General", "Gen", "Gen.", 
                                           "Major", "Maj", "Maj.", "Brigadier", "Brig", "Brig.", "Colonel", "Col", "Col.", "Ltc", "Commandant", "First", "1st", "Second", "2d", "2nd", 
                                           "Sergeant", "Sgt", "Sgt.", "MSgt", "MSgt.", "Corporal", "Cpl", "Cpl.", "Bombardier", "Private", "Pvt", "Pvt.", "Gunner", "Trooper", "Air", "Chief", 
                                           "Group", "Wing", "Leader", "Flight", "Pilot", "Airman", "Amn", "Amn.")]
  
  return(charnames)
}


#---- OLD VERSION OF FUNCTION TO GET THE BABY NAME DATA BEFORE AND AFTER A PARTICULAR MOVIE CAME OUT

namesubsetOLD <- function(indivname, movieyear, yearplusorminus = 10) {
  #Take a name and movie year and return a data frame with (yearplusorminus*2+1) rows.
  #The rows represent the yearplusorminus years before the movie year, the movie year itself, and the yearplusorminus years after the movie year
  #Each row contains the total # of babies given that name for that year 
  
  namesubsettemp <- babynames %>% 
    filter(name==indivname,babyyear>=movieyear-yearplusorminus,babyyear<=movieyear+yearplusorminus) %>% 
    group_by(babyyear) %>% 
    mutate(total = sum(count), timeframe = "before") %>% 
    select(-sex, -count, -sexyearrank) %>% 
    unique()
  
  namesubset <- as.data.frame(matrix(nrow = yearplusorminus*2+1, ncol = 4))
  names(namesubset) <- c("name", "babyyear", "total", "timeframe")
  for (i in 1:(yearplusorminus*2+1)) {
    curyear <- movieyear-yearplusorminus+i-1
    if (curyear %in% namesubsettemp$babyyear) {
      ind <- which(namesubsettemp$babyyear==curyear)
      namesubset[i,] <- namesubsettemp[ind,]
    } else {
      namesubset[i,] <- c(indivname, curyear, NA, "before")   #To generating a random number if <5 to avoid breaking Chow test if no entries exist (floor(runif(1,min = 0, max = 5)))
    }
  }
  rm(namesubsettemp)
  namesubset$babyyear <- as.numeric(namesubset$babyyear)
  namesubset$total <- as.numeric(namesubset$total)
  namesubset[which(namesubset$babyyear == movieyear), "timeframe"] <- "movie"
  namesubset[which(namesubset$babyyear > movieyear), "timeframe"] <- "after"
  
  return(namesubset)
}


#---- SCRAPE ONE MOVIE ----

#Website to scrape; data to target
searchurl <- "https://www.imdb.com/search/title/?title=Star+Wars+&title_type=feature&release_date=2019-01-01,2019-12-31&view=simple"   #Star Wars: Episode IX - The Rise of Skywalker
theurl <- "https://www.imdb.com/title/tt0076759/?ref_=nv_sr_srsg_9"   #Star Wars: Episode IV - A New Hope
theurl2 <- "https://www.imdb.com/title/tt0120338/?ref_=fn_al_tt_1"    #Titanic
#I use CSS selector to figure out what table to read
linktarget <- ".mode-simple:nth-child(1) a"   #This is where the first link of a search result is located
datatarget <- ".character"   #This is just the characters. Use "#titleCast" for the full table, with actors and characters.
titleconfirmtarget <- "h1"   #This is the movie title on the movie page


#Download the search page html, and hone in on the link to the target movie page
searchhtml <- read_html(searchurl)
get_link <- searchhtml %>% html_nodes(linktarget) %>% html_attr("href")
thelink <- paste0("https://www.imdb.com/",get_link[1])


#Download the movie page html, and hone in on target data
moviehtml <- read_html(thelink)
get_characters <- moviehtml %>% html_nodes(datatarget) %>% html_text()
get_title <- moviehtml %>% html_nodes(titleconfirmtarget) %>% html_text() %>% trim()   #This is the movie title from the webpage, to confirm we got to the right page

#Clean this data up, eliminating stuff in parentheses, extra white space, and duplicates
parenhits <- str_locate(get_characters,"\\(")   #Where are open parentheses?
for (i in 1:nrow(parenhits)) {
  if (!is.na(parenhits[i,"start"])) {
    get_characters[[i]] <- str_sub(get_characters[[i]],1,(parenhits[i,"start"]-1))   #Drop everything from an open parenthesis onward
  }
}
get_characters <- unique(trim(get_characters))
#PROBABLY WANT TO PASS THIS ITEM SO CHARACTER NAMES CAN BE RECONSTRUCTED


#---- SEPARATE INTO INDIVIDUAL NAMES ----

#Separate full character names into individual names (do not care about first/last)
#Each vector below has the number of main characters indicated (e.g., charnames3 has the first 3 listed main characters for that film)
#In each case, the process is: (a) split each string, (b) convert to one long vector, (c) eliminate duplicate names

charnames1 <- get_char[(1:1)] %>% str_split(" ") %>% unlist() %>% str_to_title() %>% unique()
charnamesALL <- get_char %>% str_split(" ") %>% unlist() %>% str_to_title() %>% unique()

charnames1 <- unique(unlist(str_split(get_characters[(1:1)]," ")))
charnames2 <- unique(unlist(str_split(get_characters[(1:2)]," ")))
charnames3 <- unique(unlist(str_split(get_characters[(1:3)]," ")))
charnames4 <- unique(unlist(str_split(get_characters[(1:4)]," ")))
charnames5 <- unique(unlist(str_split(get_characters[(1:5)]," ")))
charnamesALL <- unique(unlist(str_split(get_characters," ")))

#Get rid of common non-names
charnames <- charnames[!charnames %in% c("","\n","/","-","the","and","&")]
#Get rid of common honorifics
charnames <- charnames[!charnames %in% c("Master", "Maestro", "Mr", "Mr.", "Miss", "Mrs", "Mrs.", "Ms", "Ms.", "Mx", "Mx.", "M", "M.", "Sir", "Sire", "Mistress", "Madam", "Madame", "Ma'am", 
                                         "Esquire", "Esq", "Esq.", "Senior", "Sr", "Sr.", "Junior", "Jr", "Jr.", "I", "Ii", "Iii", "Iv", "V", "Vi", "Vii", "Viii", "Ix", "X", "Honourable", 
                                         "Honorable", "Hon", "Hon.", "Right", "Most", "Doctor", "Dr", "Dr.", "Professor", "Prof", "Prof.", "Chancellor", "Vice-chancellor", "Principal", 
                                         "President", "Pres", "Pres.", "Vice", "Vp", "Warden", "Regent", "Rector", "Provost", "Director", "Dir", "Dir.", "Coach", "Agent", "Uncle", "Aunt",
                                         "Cousin", "Grandfather", "Grandmother")]
#Get rid of religious titles
charnames <- charnames[!charnames %in% c("Pope", "Cardinal", "Bishop", "Reverend", "Rev", "Rev.", "Revd", "Revd.", "Father", "Fr", "Fr.", "Pastor", "Pr", "Pr.", "Brother", "Br", "Br.", 
                                         "Sister", "Mother", "Elder", "Rabbi", "Cantor", "Grand", "Venerable")]
#Get rid of royal titles
charnames <- charnames[!charnames %in% c("Emperor", "Empress", "Kaiser", "Tsar", "King", "Queen", "High", "Great", "Archduke", "Archduchess", "Prince", "Princess", "Duke", "Duchess", "Crown",
                                         "Dauphin", "Jarl", "Sovereign", "Marquis", "Marquess", "Count", "Countess", "Viscount", "Baron", "Baroness", "Dame", "Lord", "Lady", "Knight",
                                         "Maid")]
#Get rid of military ranks
charnames <- charnames[!charnames %in% c("Admiral", "Adm", "Adm.", "Rear", "Commodore", "Comm", "Comm.", "Captain", "Capt", "Capt.", "Cpt", "Cpt.", "Commander", "Cdr", "Cdr.", "Lieutenant", 
                                         "Lt", "Lt.", "Ensign", "Ens", "Ens.", "Midshipman", "Officer", "Cadet", "Warrant", "Petty", "Seaman", "Field", "Marshal", "General", "Gen", "Gen.", 
                                         "Major", "Maj", "Maj.", "Brigadier", "Brig", "Brig.", "Colonel", "Col", "Col.", "Ltc", "Commandant", "First", "1st", "Second", "2d", "2nd", 
                                         "Sergeant", "Sgt", "Sgt.", "MSgt", "MSgt.", "Corporal", "Cpl", "Cpl.", "Bombardier", "Private", "Pvt", "Pvt.", "Gunner", "Trooper", "Air", "Chief", 
                                         "Group", "Wing", "Leader", "Flight", "Pilot", "Airman", "Amn", "Amn.")]



#---- TESTING ----

head(babynames)
babynames %>% filter(babyyear == 2018,count == 5) %>% head(50)
babynames %>% filter(name == "Deshon")


#Make a vector with only the top 5 movies in each year, based on lifetime box office gross
best5movies <- allmovies %>% group_by(year) %>% mutate(movieyearrank = min_rank(desc(lifetime_gross))) %>% filter(movieyearrank <= 5)

best1977 <- best5movies %>% filter(year == 1977)
# INSTEAD OF RE-SCRAPING, I CAN JUMP TO THE RELOAD LINE BELOW
char1977 <- data.frame(moviechar=character(), moviename=character(), movieyear=integer())
for (i in 1:5) {
  tempchar <- as.data.frame(charscrape(as.character(best1977[i,]$title),best1977[i,]$year), stringsAsFactors=FALSE) %>%
    mutate(moviename = as.character(best1977[i,]$title), movieyear = best1977[i,]$year)
  names(tempchar)[1] <- "moviechar"
  char1977 <- bind_rows(char1977,tempchar)
  Sys.sleep(5)   #Pause this many seconds between individual server hits
}
rm(tempchar)
write.csv(char1977,"./scraped character names/characters_from_1977.csv", row.names = FALSE)
# RELOAD INSTEAD OF RE-SCRAPING
char1977 <- read.csv("./scraped character names/characters_from_1977.csv", stringsAsFactors = FALSE)

#Break out the scraped names into individual names using the charnames function I wrote
howmanychars <- 5
movies <- distinct(char1977,moviename)
allcharnames1977 <- data.frame(indivname=character(), fullname=character(), moviename=character(), movieyear=integer())
for (j in 1:nrow(movies)) {
  tempall <- char1977 %>% filter(moviename==movies[j,])
  tempall2 <- charnames(tempall,howmanychars) %>% 
    mutate(moviename = tempall$moviename[1], movieyear = tempall$movieyear[1])
  allcharnames1977 <- bind_rows(allcharnames1977,tempall2)
}
rm(tempall,tempall2)



#Look at one name
#Overall count by year
yearplusorminus <- 10
nameindex <- 22
babynames %>% 
  filter(name==allcharnames1977$indivname[nameindex]) %>% 
  group_by(babyyear) %>% 
  summarize(total = sum(count)) %>% 
  ggplot() +
  geom_line(aes(x = babyyear, y = total)) +
  labs(title = paste0("Popularity of the name ",allcharnames1977$indivname[nameindex]))

#Zoom to 10 years on either side of movie release
babynames %>% 
  filter(name==allcharnames1977$indivname[nameindex],babyyear>=allcharnames1977$movieyear[nameindex]-yearplusorminus,babyyear<=allcharnames1977$movieyear[nameindex]+yearplusorminus) %>% 
  group_by(babyyear) %>% 
  summarize(total = sum(count)) %>% 
  ggplot() +
  geom_line(aes(x = babyyear, y = total)) +
  labs(title = paste0("Popularity of the name ",allcharnames1977$indivname[nameindex]))

#Rank instead of count
babynames %>% 
  filter(name==allcharnames1977$indivname[nameindex]) %>% 
  group_by(babyyear) %>% 
  summarize(minyearrank = min(sexyearrank)) %>% 
  ggplot() +
  geom_line(aes(x = babyyear, y = minyearrank)) +
  labs(title = paste0("Rank of the name ",allcharnames1977$indivname[nameindex]," (lower is better)"))

#Zoom rank to 10 years plus/minus
babynames %>% 
  filter(name==allcharnames1977$indivname[nameindex],babyyear>=allcharnames1977$movieyear[nameindex]-yearplusorminus,babyyear<=allcharnames1977$movieyear[nameindex]+yearplusorminus) %>% 
  group_by(babyyear) %>% 
  summarize(minyearrank = min(sexyearrank)) %>% 
  ggplot() +
  geom_line(aes(x = babyyear, y = minyearrank)) +
  labs(title = paste0("Rank of the name ",allcharnames1977$indivname[nameindex]," (lower is better)"))


#Try a plot with a regression line
yearplusorminus <- 10
nameindex <- 5
namesubsettemp <- babynames %>% 
  filter(name==allcharnames1977$indivname[nameindex],babyyear>=allcharnames1977$movieyear[nameindex]-yearplusorminus,babyyear<=allcharnames1977$movieyear[nameindex]+yearplusorminus) %>% 
  group_by(babyyear) %>% 
  mutate(total = sum(count), timeframe = "before") %>% 
  select(-sex, -count, -sexyearrank) %>% 
  unique()
namesubset <- as.data.frame(matrix(nrow = yearplusorminus*2+1, ncol = 4))
names(namesubset) <- c("name", "babyyear", "total", "timeframe")
for (i in 1:(yearplusorminus*2+1)) {
  curyear <- allcharnames1977$movieyear[nameindex]-yearplusorminus+i-1
  if (curyear %in% namesubsettemp$babyyear) {
    ind <- which(namesubsettemp$babyyear==curyear)
    namesubset[i,] <- namesubsettemp[ind,]
  } else {
    namesubset[i,] <- c(allcharnames1977$indivname[nameindex], curyear, NA, "before")   #To generating a random number if <5 to avoid breaking Chow test if no entries exist (floor(runif(1,min = 0, max = 5)))
  }
}
rm(namesubsettemp)
namesubset$babyyear <- as.numeric(namesubset$babyyear)
namesubset$total <- as.numeric(namesubset$total)
namesubset[which(namesubset$babyyear == allcharnames1977$movieyear[nameindex]), "timeframe"] <- "movie"
namesubset[which(namesubset$babyyear > allcharnames1977$movieyear[nameindex]), "timeframe"] <- "after"

## SHOULD I DEAL WITH NAMES IN BOTH GENDERS DIFFERENTLY (BETTER)? Right now I am dropping sex, count, and sexyearrank columns. I would need a different approach if I want those back.
namesubsettest <- namesubset(allcharnames1977$indivname[nameindex], allcharnames1977$movieyear[nameindex], 10)

sp <- ggscatter(namesubset, x = "babyyear", y = "total",
                add = "reg.line",               # Add regression line
                conf.int = TRUE,                # Add confidence interval
                color = "timeframe", palette = "jco", # Color by groups "timeframe"
                shape = "timeframe",                  # Change point shape by groups "timeframe"
                title = paste0("10 Years Before & After ",allcharnames1977$moviename[nameindex]," for the name ",allcharnames1977$indivname[nameindex])
)+
  stat_cor(aes(color = timeframe))#, label.x = 3)       # Add correlation coefficient
sp
sctest(namesubset$total ~ namesubset$babyyear, type = "Chow", point = 11)

plot(namesubset$babyyear,namesubset$total)

#Compare means before and after
babynames %>% 
  filter(name==allcharnames1977$indivname[nameindex], babyyear > 1920) %>% 
  select(count,babyyear) %>% 
  group_by(babyyear) %>%
  summarize(total = sum(count)) %>% 
  select(total) %>% 
  unlist() %>% 
  hist(breaks = 50, main = paste0("Histogram of ",allcharnames1977$indivname[nameindex]))


namesubset <- babynames %>% 
  filter(name==allcharnames1977$indivname[nameindex],babyyear>=allcharnames1977$movieyear[nameindex]-yearplusorminus,babyyear<=allcharnames1977$movieyear[nameindex]+yearplusorminus) %>% 
  mutate(timeframe = "before")
namesubset <- bind_rows(namesubset,namesubset[yearplusorminus+1,]) %>% arrange(babyyear)
namesubset[(yearplusorminus+2):(yearplusorminus*2+2),"timeframe"] <- "after"


effectthreshold <- 2   #A movie (not just one character) is considered to have had an effect on baby naming if its characters had at least this many name effects.
tempnames <- analyzenames %>% filter(moviename=="Avatar") %>% select(indivname, fullname, moviename, movieyear, numNA, chowPvalue, nameeffect)
sum(tempnames$nameeffect, na.rm = TRUE)
tempnames <- tempnames %>% group_by(moviename, movieyear) %>% mutate(movieeffect = (sum(nameeffect, na.rm = TRUE) >= effectthreshold))


harry <- slimanalyze[str_which(slimanalyze$moviename,"Harry Potter"),]
starwars <- slimanalyze %>% filter(moviename == "Star Wars")
rome <- slimanalyze %>% filter(moviename == "Gladiator")
avatar <- slimanalyze %>% filter(moviename == "Avatar")
hunger <- slimanalyze %>% filter(moviename == "The Hunger Games")
write.csv(rome,"./gladiator.csv", row.names = FALSE)

  
nametoplot <- "Caleb"
babynames %>% 
  filter(name==nametoplot) %>% 
  group_by(babyyear) %>% 
  summarize(total = sum(count)) %>% 
  ggplot() +
  geom_line(aes(x = babyyear, y = total)) +
  labs(title = paste0("Popularity of the name ",nametoplot))


#---- PLOT "CARRIE" FROM "SMOKEY AND THE BANDIT" ----
thename <- "Carrie"
#This stretch of code plots a name in the years surrounding a movie's release (missing SSA data is shown as '4' in this chart)
themovie <- "Smokey and the Bandit"
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
  geom_text(data=toplot[yearplusorminus+1,], aes(label=years), nudge_y = 0.03*toplot[yearplusorminus+1,2], size = 10) +
  theme_classic() +
  list(theme(legend.position = "none",
             plot.title = element_text(size = 50),
             axis.title = element_text(size = 40),
             axis.text = element_text(size = 35)))


#---- PLOT "TONY" FROM "WEST SIDE STORY" ----
thename <- "Tony"
#This stretch of code plots a name in the years surrounding a movie's release (missing SSA data is shown as '4' in this chart)
themovie <- "West Side Story"
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
  geom_text(data=toplot[yearplusorminus+1,], aes(label=years), nudge_y = 0.04*toplot[yearplusorminus+1,2], size = 10) +
  theme_classic() +
  list(theme(legend.position = "none",
             plot.title = element_text(size = 50),
             axis.title = element_text(size = 40),
             axis.text = element_text(size = 35)))



#---- PLOT "CASSIDY" FROM "BUTCH CASSIDY AND THE SUNDANCE KID" ----
thename <- "Cassidy"
#This stretch of code plots a name in the years surrounding a movie's release (missing SSA data is shown as '4' in this chart)
themovie <- "Butch Cassidy and the Sundance Kid"
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
  geom_text(data=toplot[yearplusorminus+1,], aes(label=years), nudge_y = 6, size = 10) +
  theme_classic() +
  list(theme(legend.position = "none",
             plot.title = element_text(size = 45),
             axis.title = element_text(size = 40),
             axis.text = element_text(size = 35)))


#---- PLOT "PATTON" FROM "PATTON" ----
thename <- "Patton"
#This stretch of code plots a name in the years surrounding a movie's release (missing SSA data is shown as '4' in this chart)
themovie <- "Patton"
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
  geom_text(data=toplot[yearplusorminus+1,], aes(label=years), nudge_y = 0.5, size = 10) +
  theme_classic() +
  list(theme(legend.position = "none",
             plot.title = element_text(size = 50),
             axis.title = element_text(size = 40),
             axis.text = element_text(size = 35)))

#---- PLOT "WAYNE" FROM "BATMAN" ----
thename <- "Wayne"
#This stretch of code plots a name in the years surrounding a movie's release (missing SSA data is shown as '4' in this chart)
themovie <- "Batman"
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
  stat_cor(aes(color = timeframe), size = 10, label.x = 1985) +   # Add correlation coefficient
  xlab("Year") +
  ylab("New babies with this name") +
  geom_text(data=toplot[yearplusorminus+1,], aes(label=years), nudge_y = 0.08*toplot[yearplusorminus+1,2], size = 10) +
  theme_classic() +
  list(theme(legend.position = "none",
             plot.title = element_text(size = 50),
             axis.title = element_text(size = 40),
             axis.text = element_text(size = 35)))
