# Functions for movie character scraping and manipulation
# Web scraping project
# STA 6233 - Advanced R Programming
# by Ben Graf
# Created 2020-02-15, Last edited 2020-03-02

#---- SETUP ----

#Bring in Libraries
pacman::p_load(pacman,rvest,tidyverse,polite,urltools)

#Create a trim function which uses regular expressions to clean white space
trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

session <- bow("https://www.imdb.com", user_agent = "If you have questions...tethergrane@yahoo.com", delay = 5, force = TRUE)
session


#---- FUNCTION TO SCRAPE ONE MOVIE FROM IMDB.COM ----

charscrape <- function(moviename, movieyear) {      
  #Scrape IMDB for the top-billed character names for the specified movie.
  #Note: moviename is used as a search term, so it does not necessarily need to match the exact IMDB title
  
  print(paste0("Starting search for ",moviename,", ",movieyear,"."))

  #Set up the search url for this movie
  movienameurl <- url_encode(moviename)   #Converts non-standard characters to url format (e.g., %20 for space)
  searchurl <- paste0("https://www.imdb.com/search/title/?title=",movienameurl,"&title_type=feature&release_date=",movieyear,"-01-01,",movieyear,"-12-31&view=simple")

  #I used SelectorGadget to figure out what tables to read
  linktarget <- ".mode-simple:nth-child(1) a"   #This is the first link of a search result on the search page
  datatarget <- ".character"                    #This is the characters on the movie page
  titleconfirmtarget <- "h1"                    #This is the movie title on the movie page
  
  #Download the search page html, and hone in on the link to the target movie page
  searchhtml <- read_html(searchurl)
  get_link <- searchhtml %>% html_nodes(linktarget) %>% html_attr("href")
  thelink <- paste0("https://www.imdb.com/",get_link[1])
  
  #Download the movie page html, and hone in on target character data
  if (thelink == "https://www.imdb.com/NA") {
    print(paste0("ERROR for ",moviename,".  NOT FOUND."))
    get_characters <- NA
  } else {
    Sys.sleep(5)   #IMDB requested a crawl delay of 5 seconds between server hits
    moviehtml <- read_html(thelink)
    get_characters <- moviehtml %>% html_nodes(datatarget) %>% html_text()   #This is the character data we want
    get_title <- moviehtml %>% html_nodes(titleconfirmtarget) %>% html_text() %>% trim()   #This is the movie title from the webpage, to confirm we got to the right page
    
    print(paste0("Found page for ",get_title,"."))
    
    #Clean this data up, eliminating stuff in parentheses, extra white space, and duplicates
    parenhits <- str_locate(get_characters,"\\(")   #Where are open parentheses?
    for (i in 1:nrow(parenhits)) {
      if (!is.na(parenhits[i,"start"])) {
        get_characters[[i]] <- str_sub(get_characters[[i]],1,(parenhits[i,"start"]-1))   #Drop everything from an open parenthesis onward
      }
    }
    get_characters <- unique(trim(get_characters))    
  }

    return(get_characters)
}


#---- FUNCTION TO SEPARATE FULL NAMES INTO INDIVIDUAL NAMES ----

charnames <- function(char_vector, number = nrow(char_vector)) {
  #Take a data frame of character names (along with associated movie names and year) and split them into individual names (do not care about first/last)
  #The process is: (a) split each string, (b) convert to one long vector, (c) convert to "title" case, (d) eliminate duplicate names, (e) eliminate common non-names
  
  if (number > nrow(char_vector)) {
    number <- nrow(char_vector)
  }
  
  charnames <- data.frame(indivname=character(), fullname=character())
  for (i in 1:number) {
    tempindiv <- char_vector$moviechar[i] %>% str_split(" ") %>% unlist() %>% str_to_title() %>% as.data.frame()
    names(tempindiv)[1] <- "indivname"
    tempindiv$fullname <- char_vector$moviechar[i]
    charnames <- bind_rows(charnames, tempindiv)
  }
  charnames <- charnames[!duplicated(charnames["indivname"]),]
  
  #Convert non-English letters (like á or ø)
  charnames$indivname <- iconv(charnames$indivname, to='ASCII//TRANSLIT')
  #Remove non-letters from names (babynames data only contains letters)
  charnames$indivname <- str_replace_all(charnames$indivname,"[^A-z]","") %>% str_to_title()
  #Get rid of common non-names
  charnames <- charnames[!charnames$indivname %in% c("","\n","/","-","the", "The", "and","&"),]
  #Get rid of initials
  charnames <- charnames[!grepl("^[A-Z][.]$",charnames$indivname),]
  #Get rid of common honorifics
  charnames <- charnames[!charnames$indivname %in% c("Master", "Maestro", "Mr", "Mr.", "Miss", "Mrs", "Mrs.", "Ms", "Ms.", "Mx", "Mx.", "M", "M.", "Sir", "Sire", "Mistress", "Madam", "Madame", "Ma'am", 
                                           "Esquire", "Esq", "Esq.", "Senior", "Sr", "Sr.", "Junior", "Jr", "Jr.", "I", "Ii", "Iii", "Iv", "V", "Vi", "Vii", "Viii", "Ix", "X", "Honourable", 
                                           "Honorable", "Hon", "Hon.", "Right", "Most", "Doctor", "Dr", "Dr.", "Professor", "Prof", "Prof.", "Chancellor", "Vice-chancellor", "Principal", 
                                           "President", "Pres", "Pres.", "Vice", "Vp", "Warden", "Regent", "Rector", "Provost", "Director", "Dir", "Dir.", "Coach", "Agent", "Uncle", "Aunt",
                                           "Cousin", "Grandfather", "Grandmother", "Grandpa", "Grandma", "Husband", "Wife", "Son", "Daughter", "Grandson", "Granddaughter", "Man", "Woman", "Boy", "Girl"),]
  #Get rid of religious titles
  charnames <- charnames[!charnames$indivname %in% c("Pope", "Cardinal", "Bishop", "Reverend", "Rev", "Rev.", "Revd", "Revd.", "Father", "Fr", "Fr.", "Pastor", "Pr", "Pr.", "Brother", "Br", "Br.", 
                                           "Sister", "Mother", "Elder", "Rabbi", "Cantor", "Grand", "Venerable"),]
  #Get rid of royal titles
  charnames <- charnames[!charnames$indivname %in% c("Emperor", "Empress", "Kaiser", "Tsar", "King", "Queen", "High", "Great", "Archduke", "Archduchess", "Prince", "Princess", "Duke", "Duchess", "Crown",
                                           "Dauphin", "Jarl", "Sovereign", "Marquis", "Marquess", "Count", "Countess", "Viscount", "Baron", "Baroness", "Dame", "Lord", "Lady", "Knight",
                                           "Maid"),]
  #Get rid of military ranks
  charnames <- charnames[!charnames$indivname %in% c("Admiral", "Adm", "Adm.", "Rear", "Commodore", "Comm", "Comm.", "Captain", "Capt", "Capt.", "Cpt", "Cpt.", "Commander", "Cdr", "Cdr.", "Lieutenant", 
                                           "Lt", "Lt.", "Ensign", "Ens", "Ens.", "Midshipman", "Officer", "Cadet", "Warrant", "Petty", "Seaman", "Field", "Marshal", "General", "Gen", "Gen.", 
                                           "Major", "Maj", "Maj.", "Brigadier", "Brig", "Brig.", "Colonel", "Col", "Col.", "Ltc", "Commandant", "First", "1st", "Second", "2d", "2nd", 
                                           "Sergeant", "Sgt", "Sgt.", "MSgt", "MSgt.", "Corporal", "Cpl", "Cpl.", "Bombardier", "Private", "Pvt", "Pvt.", "Gunner", "Trooper", "Air", "Chief", 
                                           "Group", "Wing", "Leader", "Flight", "Pilot", "Airman", "Amn", "Amn."),]
  
  return(charnames)
}


#---- FUNCTION TO GET THE BABY NAME DATA BEFORE AND AFTER A PARTICULAR MOVIE CAME OUT

namesubset <- function(indivname, movieyear, yearplusorminus = 10) {
  #Take a name and movie year and return a vector with (yearplusorminus*2+1) columns and one row.
  #The vector represents the yearplusorminus years before the movie year, the movie year itself, and the yearplusorminus years after the movie year
  #It contains the total # of babies given that name for that year 
  
  namesubsettemp <- babynames %>% 
    filter(name==indivname,babyyear>=movieyear-yearplusorminus,babyyear<=movieyear+yearplusorminus) %>% 
    group_by(babyyear) %>% 
    mutate(total = sum(count)) %>% 
    select(babyyear,total) %>% 
    unique()
  
  namesubset <- vector(mode = "numeric", length = yearplusorminus*2+1)
  for (i in 1:(yearplusorminus*2+1)) {
    curyear <- movieyear-yearplusorminus+i-1
    if (curyear %in% namesubsettemp$babyyear) {
      ind <- which(namesubsettemp$babyyear==curyear)
      namesubset[i] <- namesubsettemp[ind,2]
    } else {   #If the name is not in the babynames dataset for that year...
      if (curyear > 2018) {
        namesubset[i] <- NA   #This year is beyond the available data
      } else {
        namesubset[i] <- 4   #This year had <5 occurrences.  Using NA instead of an actual number (say, 0 or 4) has pros and cons.
      }
    }
  }
  rm(namesubsettemp)
  
  return(namesubset)
  
}
