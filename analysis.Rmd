## -----------------------------------------------------------------------------------------------------
library(glue)
library(readxl)
library(rvest)
library(tidyverse)
library(dplyr)
library(ggmap)
library(writexl)

#mapdist(from = "Madison Square Garden, NY", to = "The Palace of Auburn Hills, MI")
ggmap::register_google("AIzaSyBMmMSEDcTcGH1M8gXvEcEYACj8FvDKZwc")

ncaa <- rvest::read_html("https://www.sports-reference.com/cbb/seasons/2019-school-stats.html")
ncaa_table <- ncaa %>% rvest::html_table()

ncaa_table <- ncaa_table[[1]]

## -----------------------------------------------------------------------------------------------------

href_place <- ncaa %>% rvest::html_nodes("#basic_school_stats a") %>%
  rvest::html_attr("href")
href_place <- substr(href_place, 1, nchar(href_place)-9)
href_place

citie_links <- paste("https://www.sports-reference.com", href_place, sep="")
citie_links[1]

cities <- c()
for( i in 1:length(citie_links)){
  citie <- rvest::read_html(citie_links[i]) %>% 
    rvest::html_nodes("h1+ p") %>% rvest::html_text()
  citie <- substr(citie, 16, nchar(citie)-1)
  cities[i] <- citie

}

## -----------------------------------------------------------------------------------------------------

years <- 2009:2018
links <- c()

for(i in 1:length(years)){
 links[i] <- paste0("https://www.sports-reference.com/cbb/seasons/", years[i], "-school-stats.html", sep = "") 
}

home_table <- c()

for (i in 1:length(links)){
  data <- rvest::read_html(links[i])
  new_data <- data %>% rvest::html_table()
  home_table[i] <- new_data
}

## -----------------------------------------------------------------------------------------------------

### Create links for every team's game log page from 2019 season
href <- ncaa %>% rvest::html_nodes("#basic_school_stats a") %>%
  rvest::html_attr("href")
href <- substr(href, 1, nchar(href)-5)
logs <- paste("https://www.sports-reference.com", href, "-gamelogs.html", sep="")

# Create links for every team's schedule page from 2019 season
schedule <- paste("https://www.sports-reference.com", href, "-schedule.html", sep="")

schedule_list <- c()

for(i in 1:length(schedule)) {
  
  link <- rvest::read_html(schedule[i])
  
  date <- link %>% rvest::html_nodes(".right+ .left a") %>% rvest::html_text()
  length(date)

  stadium <- link %>% rvest::html_nodes(".left:nth-child(15)") %>% rvest::html_text()
  stadium <- str_trim(stadium, side = "right")
  length(stadium)
  
  place <- data.frame(date, stadium)
  
  schedule_list[[i]] <- place
  
  print(i)
}

#################################################################

# Make a list to store each data frame obtained from game logs of each team in 2019
log_table <- c()
school_names <- ncaa %>% rvest::html_nodes("#basic_school_stats a") %>% rvest::html_text()

arena_link <- "https://en.wikipedia.org/wiki/List_of_NCAA_Division_I_basketball_arenas"

arenas <- rvest::read_html(arena_link) %>% html_table()
arenas <- arenas[[2]]

arenas1 <- arenas[, c(2, 5)]

arenas1$Team <-  as.character(arenas1$Team)
school_names <- as.character(school_names)

arenas1$Team[arenas1$Team == "UAB"] <- "Alabama-Birmingham"
arenas1$Team[arenas1$Team == "Albany"] <- "Albany (NY)"
arenas1$Team[arenas1$Team == "BYU"] <- "Brigham Young"
arenas1$Team[arenas1$Team == "Central Connecticut"] <- "Central Connecticut State"
arenas1$Team[arenas1$Team == "The Citadel"] <- "Citadel"
arenas1$Team[arenas1$Team == "Charleston"] <- "College of Charleston"
arenas1$Team[arenas1$Team == "UConn"] <- "Connecticut"
arenas1$Team[arenas1$Team == "Gardner-Webb"] <- "Gardner-Webb"
arenas1$Team[arenas1$Team == "Illinois-Chicago"] <- "Illinois"
arenas1$Team[arenas1$Team == "Kentucky men[g]"] <- "Kentucky"
arenas1$Team[arenas1$Team == "LIU"] <- "Long Island University"
arenas1$Team[arenas1$Team == "Louisiana–Monroe"] <- "Louisiana-Monroe"
arenas1$Team[arenas1$Team == "LIU"] <- "Long Island University"
arenas1$Team[arenas1$Team == "Loyola (Chicago)"] <- "Loyola (IL)"
arenas1$Team[arenas1$Team == "Loyola (Maryland)"] <- "Loyola (MD)"
arenas1$Team[arenas1$Team == "Maryland Eastern Shore"] <- "Maryland-Eastern Shore"
arenas1$Team[arenas1$Team == "UNLV"] <- "Nevada-Las Vegas"
arenas1$Team[arenas1$Team == "Nicholls"] <- "Nicholls State"
arenas1$Team[arenas1$Team == "North Carolina (men)"] <- "North Carolina"
arenas1$Team[arenas1$Team == "UNC Asheville"] <- "North Carolina-Asheville"
arenas1$Team[arenas1$Team == "UNC Greensboro"] <- "North Carolina-Greensboro"
arenas1$Team[arenas1$Team == "UNC Wilmington"] <- "North Carolina-Wilmington"
arenas1$Team[arenas1$Team == "Penn"] <- "Pennsylvania"
arenas1$Team[arenas1$Team == "Prairie View A&M"] <- "Prairie View"
arenas1$Team[arenas1$Team == "Purdue Fort Wayne"] <- "Purdue-Fort Wayne"
arenas1$Team[arenas1$Team == "Saint Mary's"] <- "Saint Mary's (CA)"
arenas1$Team[arenas1$Team == "Sam Houston"] <- "Sam Houston State"
arenas1$Team[arenas1$Team == "USC"] <- "Southern California"
arenas1$Team[arenas1$Team == "SMU"] <- "Southern Methodist"
arenas1$Team[arenas1$Team == "Southern Miss"] <- "Southern Mississippi"
arenas1$Team[arenas1$Team == "St. Francis Brooklyn"] <- "St. Francis (NY)"
arenas1$Team[arenas1$Team == "St. John's"] <- "St. John's (NY)"
arenas1$Team[arenas1$Team == "UTEP"] <- "Texas-El Paso"
arenas1$Team[arenas1$Team == "UTRGV"] <- "Texas-Rio Grande Valley"
arenas1$Team[arenas1$Team == "UTSA"] <- "Texas-San Antonio"
arenas1$Team[arenas1$Team == "TCU"] <- "Texas Christian"
arenas1$Team[arenas1$Team == "UC Davis"] <- "UC-Davis"
arenas1$Team[arenas1$Team == "UC Irvine"] <- "UC-Irvine"
arenas1$Team[arenas1$Team == "UC Riverside"] <- "UC-Riverside"
arenas1$Team[arenas1$Team == "UC Santa Barbara"] <- "UC-Santa Barbara"
arenas1$Team[arenas1$Team == "UCF"] <- "Central Florida"
arenas1$Team[arenas1$Team == "California"] <- "University of California"
arenas1$Team[arenas1$Team == "Bradley men"] <- "Bradley"
arenas1$Team[arenas1$Team == "Creighton men"] <- "Creighton"
arenas1$Team[arenas1$Team == "Creighton men"] <- "Creighton"
arenas1$Team[arenas1$Team == "Georgetown men"] <- "Georgetown"
arenas1$Team[arenas1$Team == "Green Bay men"] <- "Green Bay"
arenas1$Team[arenas1$Team == "Idaho State men"] <- "Idaho State"
arenas1$Team[arenas1$Team == "IUPUI men"] <- "IUPUI"
arenas1$Team[arenas1$Team == "Kentucky men"] <- "Kentucky"
arenas1$Team[arenas1$Team == "Marquette men"] <- "Marquette"
arenas1$Team[arenas1$Team == "Milwaukee men"] <- "Milwaukee"
arenas1$Team[arenas1$Team == "UNLV men"] <- "Nevada-Las Vegas"
arenas1$Team[arenas1$Team == "North Carolina State men"] <- "North Carolina State"
arenas1$Team[arenas1$Team == "Northeastern men"] <- "Northeastern"
arenas1$Team[arenas1$Team == "Providence men"] <- "Providence"
arenas1$Team[arenas1$Team == "Seton Hall men"] <- "Seton Hall"
arenas1$Team[arenas1$Team == "Sienal men"] <- "Siena"
arenas1$Team[arenas1$Team == "St. John's men"] <- "St. John's (NY)"

indexes <- c()

for(i in 1:length(school_names)){
  vec <- school_names[i]==arenas1$Team
  sum_vec <- sum(vec)
  if(sum_vec == 0) {
    indexes[i] <- i
  }
  else {
    indexes[i] <- NA
  }
}

missing <- indexes[!is.na(indexes)]

#######################################################################

logs_clean <- logs[-missing]

school_names <- school_names[-missing]

log_table <- c()

for(i in 1:length(logs)){
  data <- rvest::read_html(logs_clean[i])
  new_data <- data %>% rvest::html_table()
  new_data <- data.frame(new_data)
  
  names1 <- new_data[1,1:2]
  names2 <- new_data[1, 4:ncol(new_data)]
  
  game_clean1 = new_data[!grepl("W/L", new_data$Var.5),]
  game_clean2 = game_clean1[!grepl("School", game_clean1$School), ]
  colnames(game_clean2) <- c(names1, "Other", names2) 
  
  home <- rep(school_names[i], length(game_clean2$G))
  length(home)
  
  game_clean2 <- cbind(game_clean2, home)
  
  log_table[[i]] <- game_clean2
  
  print(i)
}

for(i in 1:length(log_table)){
  
  temp1 <- arenas1[arenas1$Team==school_names[i], ]
  temp2 <- temp1$Arena
  length(temp2)
  
  home_arena <- rep(temp2, length(log_table[[i]]$G))
  length(home_arena)
  
  log_table[[i]] <- cbind(log_table[[i]], home_arena)
  print(i)
}

bound_list <- c()
schedule_list <- schedule_list[-missing]

for(i in 1:length(log_table)) {
  bound <- cbind(log_table[[i]], schedule_list[[i]])
  bound_list[[i]] <- bound
}

for(i in 1:length(bound_list)){
  for(j in 1:length(bound_list[[i]]$stadium)){
    if(bound_list[[i]]$stadium[j] == "Multipurpose Activity Center") {
      bound_list[[i]]$stadium[j] <- "Long Beach, NJ"
    }
  }
}

distance <- c()

for(i in 1:length(bound_list)){
  for(j in 1:length(bound_list[[i]]$home_arena)){
    if(bound_list[[i]]$stadium[j] == "John Gray Gymnasium") {
      distance[j] <- -999
    }
    else if(bound_list[[i]]$stadium[j] == "Virgin Islands Sport & Fitness Center") {
      distance[j] <- -999
    }
    else if(bound_list[[i]]$stadium[j] == "Kendal Isaacs Gymnasium") {
      distance[j] <- -999
    }
    else if(bound_list[[i]]$stadium[j] == "Baoshan Sports Center") {
      distance[j] <- -999
    }
    else if(bound_list[[i]]$stadium[j] == "Imperial Arena at Atlantis Resort") {
      distance[j] <- -999
    }
    else if(bound_list[[i]]$stadium[j] == "Montego Bay Convention Centre") {
      distance[j] <- -999
    }
    else if(bound_list[[i]]$stadium[j] == "Lahaina Civic Center") {
      distance[j] <- -999
    }
    else if(bound_list[[i]]$stadium[j] == "SSE Arena") {
      distance[j] <- -999
    }
    else if (bound_list[[i]]$stadium[j] == "Stan Sheriff Center") {
      distance[j] <- -999
    }
    else if(bound_list[[i]]$home_arena[j] == bound_list[[i]]$stadium[j]) {
      distance[j] <- 0
    } 
    else if(bound_list[[i]]$stadium[j] == ""){
      distance[j] <- ""
    }
    else {
      dist_temp <- ggmap::mapdist(bound_list[[i]]$home_arena[j], bound_list[[i]]$stadium[j])
      miles <- dist_temp$miles
      distance[j] <- miles
      #print(j)
    }
  }
  bound_list[[i]] <- cbind(bound_list[[i]], distance)
  distance <- c()
  print(i)
}

bound_list_new <- bound_list[-175]

for(i in 1:length(bound_list_new)) {
  var_names <- colnames(bound_list_new[[i]])
  na_index <- which(var_names == "NA")
  bound_list_new[[i]] <- bound_list_new[[i]][, -na_index]
  
}

test_names <- names(bound_list_new[[1]])
names_equal_list <- c()
for(i in 1:length(bound_list_new)) {
  names_equal <- names(bound_list_new[[i]]) == test_names
  names_sum <- sum(names_equal)
  names_equal_list[i] <- names_sum
}

col_count <- c()
for(i in 1:length(bound_list_new)){
  num_col <- ncol(bound_list_new[[i]])
  col_count[i] <- num_col
}

bound_list_new[[174]] <- bound_list_new[[174]][,-45]
bound_list_new[[175]] <- bound_list_new[[175]][,-45]

clean_bound_list <- bound_list_new

strength <- do.call("rbind", clean_bound_list)

writexl::write_xlsx(strength, "C:\\Users\\Jared\\Desktop\\strength.xlsx")
#writexl::write_xlsx(rural_anes_samples, "C:\\Users\\Jared\\Desktop\\\\rural_anes_samples.xlsx" )

## -----------------------------------------------------------------------------------------------------

bound_list <- bound_list[-175]

before_date <- c()

for(i in 1:length(bound_list)){
  for(j in 1:length(bound_list[[i]]$Date)) {
    if(j == 1) {
      before_date[j] <- 5
    }
    else {
      before_date[j] <- bound_list[[i]]$Date[j-1]
    }
  }
  bound_list[[i]] <- cbind(bound_list[[i]], before_date)
  print(i)
}

## -----------------------------------------------------------------------------------------------------
library(lubridate)

strength_df <- readxl::read_excel("strength.xlsx")

new_date <- c()

for(i in 1:length(strength_df$Date)) {
  if(i > 1){
    new_date[i] <- strength_df$Date[i-1]
  }
  if(i == 1){
    new_date[i] <- is.na(new_date[i])
  }
}

strength_df <- cbind(strength_df, new_date)

rest <- abs(time_length(interval(strength_df$Date, strength_df$new_date), "day"))
index <- which(rest > 50)
rest[index] <- 10
rest[1] <- 10
  
strength_df <- cbind(strength_df, rest)

# write_xlsx(strength_df, "C:\\Users\\Jared\\Desktop\\strengthdate.xlsx")

clune <- which(strength_df$stadium == "Clune Arena")
strength_df$stadium[clune] <- "Cadet Field House"

elmore <- which(strength_df$stadium == "Elmore Health Science Building")
strength_df$stadium[elmore] <- "Elmore Gymnasium"

wells <- which(strength_df$stadium == "Wells Fargo Arena")
strength_df$stadium[wells] <- "Desert Financial Arena"

ho <- which(strength_df$stadium == "H.O. Clemmons Arena")
strength_df$stadium[ho] <- "K. L. Johnson Complex"

convo <- which(strength_df$stadium == "Convocation Center")
strength_df$stadium[convo] <- "First National Bank Arena"

dunn <- which(strength_df$stadium == "Winfield Dunn Center")
strength_df$stadium[dunn] <- "Dunn Center"

pavilion <- which(strength_df$stadium == "The Pavilion")
strength_df$stadium[pavilion] <- "The Pavilion at ARC"

thunder <- which(strength_df$stadium == "The Thunderdome")
strength_df$stadium[thunder] <- "The Pavilion at ARC"

csu <- which(strength_df$stadium == "CSU Fieldhouse")
strength_df$stadium[csu] <- "Buccaneer Field House"

centry <- which(strength_df$stadium == "CenturyLink Center")
strength_df$stadium[centry] <- "CHI Health Center Omaha"

chase <- which(strength_df$stadium == "Chase Family Arena")
strength_df$stadium[chase] <- "Chase Arena"

mills <- which(strength_df$stadium == "Millis Center")
strength_df$stadium[mills] <- "Nido & Mariana Qubein Arena and Conference Center"

cowan <- which(strength_df$stadium == "Cowan Spectrum")
strength_df$stadium[cowan] <- "Idaho Central Credit Union Arena"

jmu <- which(strength_df$stadium == "JMU Convocation Center")
strength_df$stadium[jmu] <- "Atlantic Union Bank Center"

memorial <- which(strength_df$stadium == "Memorial Athletic & Convocation Center")
strength_df$stadium[memorial] <- "Memorial Athletic and Convocation Center"

vines <- which(strength_df$stadium == "Vines Center")
strength_df$stadium[vines] <- "Liberty Arena"

henderson <- which(strength_df$stadium == "Henderson Center")
strength_df$stadium[henderson] <- "Cam Henderson Center"

hh <- which(strength_df$stadium == "H&HP Complex")
strength_df$stadium[hh] <- "Health and Human Performance Education Complex"

murphy <- which(strength_df$stadium == "Murphy Athletic Center")
strength_df$stadium[murphy] <- "Murphy Center"

bank <- which(strength_df$stadium == "Bank of Kentucky Center")
strength_df$stadium[bank] <- "BB&T Arena"

palestra <- which(strength_df$stadium == "The Palestra")
strength_df$stadium[palestra] <- "Palestra"

north <- which(strength_df$stadium == "North Athletic Complex")
strength_df$stadium[north] <- "UPMC Events Center"

nest <- which(strength_df$stadium == "The Nest")
strength_df$stadium[nest] <- "Colberg Court"

nest <- which(strength_df$stadium == "The Nest")
strength_df$stadium[nest] <- "Colberg Court"

hornet <- which(strength_df$home_arena == "Hornets Nest")
strength_df$home_arena[hornet] <- "Colberg Court"

war <- which(strength_df$stadium == "War Memorial Gymnasium")
strength_df$stadium[war] <- "The Sobrato Center"

connolly <- which(strength_df$stadium == "Connolly Center")
strength_df$stadium[connolly] <- "Redhawk Center"

centrum <- which(strength_df$stadium == "Centrum")
strength_df$stadium[centrum] <- "America First Event Center"

island <- which(strength_df$stadium == "Island Federal Credit Union")
strength_df$stadium[island] <- "Island Federal Credit Union Arena"

ed <- which(strength_df$stadium == "Ed & Rae Schollmaier Arena")
strength_df$stadium[ed] <- "Schollmaier Arena"

health_ <- which(strength_df$stadium == "Health & Physical Education Arena")
strength_df$stadium[health_] <- "Health and Physical Education Arena"

new_distance <- c()

other_strength_df <- strength_df
index <- which(is.na(other_strength_df$stadium))
other_strength_df <- other_strength_df[-index, ]

for(i in 3001:length(other_strength_df$stadium)){
  if(other_strength_df$stadium[i] == "John Gray Gymnasium") {
    new_distance[i] <- -999
  }
  else if(other_strength_df$stadium[i] == "Virgin Islands Sport & Fitness Center") {
    new_distance[i] <- -999
  }
  else if(other_strength_df$stadium[i] == "Kendal Isaacs Gymnasium") {
    new_distance[i] <- -999
  }
  else if(other_strength_df$stadium[i] == "Baoshan Sports Center") {
    new_distance[i] <- -999
  }
  else if(other_strength_df$stadium[i] == "Imperial Arena at Atlantis Resort") {
    new_distance[i] <- -999
  }
  else if(other_strength_df$stadium[i] == "Montego Bay Convention Centre") {
    new_distance[i] <- -999
  }
  else if(other_strength_df$stadium[i] == "Lahaina Civic Center") {
    new_distance[i] <- -999
  }
  else if(other_strength_df$stadium[i] == "SSE Arena") {
    new_distance[i] <- -999
  }
  else if (other_strength_df$stadium[i] == "Stan Sheriff Center") {
    new_distance[i] <- -999
  }
  else if(other_strength_df$home_arena[i] == other_strength_df$stadium[i]) {
    new_distance[i] <- 0
  } 
  else if(is.na(other_strength_df$stadium[i]) == TRUE){
    new_distance[i] <- "your mom"
  }
  else {
    dist_temp <- ggmap::mapdist(other_strength_df$home_arena[i], other_strength_df$stadium[i])
    miles <- dist_temp$miles
    new_distance[i] <- miles
  }
  print(i)
}

my_strength <- cbind(other_strength_df, new_distance)

# write_xlsx(my_strength, "C:\\Users\\Jared\\Desktop\\mystrength.xlsx")

cols <- c(3, 42, 44, 45)
my_strength2 <- my_strength[, -cols]
my_strength2 <- my_strength2[, -c(40, 41)]
colnames(my_strength2) <- c("Game", "Date", "Opponent", "W/L", "Points", "Opponent Points",
                            "FG", "FGA", "FG%", "3P", "3PA", "3P%", "FT", "FTA", "FT%",
                            "ORB", "TRB", "AST", "STL", "BLK", "TOV", "PF",
                            "FGO", "FGAO", "FG%O", "3PO", "3PAO", "3P%O", "FTO", "FTAO", "FT%O",
                            "ORBO", "TRBO", "ASTO", "STLO", "BLKO", "TOVO", "PFO",
                            "Team", "Rest", "Travel") 

num_cols <- 5:38

my_strength2[, num_cols] = dplyr::apply(my_strength2[, num_cols], 2, as.numeric)

point_margin <- my_strength2$Points-my_strength2$`Opponent Points`

my_strength3 <- cbind(my_strength2, point_margin)

## -----------------------------------------------------------------------------------------------------
library(randomForest)
library(glue)
library(readxl)
library(rvest)
library(tidyverse)
library(dplyr)
library(ggmap)
library(writexl)
library(corrplot)
library(MASS)
library(rpart)
library(gbm)

# Read in data frame
strength_data <- read_excel("mystrength3.xlsx")

## FIlter out missing data or -999 flags
nothing <- which(strength_data$Travel == -999)
strength_data <- strength_data[-nothing, ]

strength_data <- na.omit(strength_data)

### Remove columns with colinearity 
strength_data2 <- strength_data[, -c(7:8, 10:11, 13:14, 23:24, 26:27, 29:30)]

#############################################################################

### Find time rested by home team

d2 <- which(strength_data2$Opponent == "BYU")
strength_data2$Opponent[d2] <- "Brigham Young"

d2 <- which(strength_data2$Opponent == "UCSB")
strength_data2$Opponent[d2] <- "UC-Santa Barbara"

d2 <- which(strength_data2$Opponent == "California")
strength_data2$Opponent[d2] <- "University of California"

d2 <- which(strength_data2$Opponent == "UCF")
strength_data2$Opponent[d2] <- "Central Florida"

d2 <- which(strength_data2$Opponent == "UConn")
strength_data2$Opponent[d2] <- "Connecticut"

d2 <- which(strength_data2$Opponent == "Detroit")
strength_data2$Opponent[d2] <- "Detroit Mercy"

d2 <- which(strength_data2$Opponent == "Detroit")
strength_data2$Opponent[d2] <- "Detroit Mercy"

d2 <- which(strength_data2$Opponent == "ETSU")
strength_data2$Opponent[d2] <- "East Tennessee State"

d2 <- which(strength_data2$Opponent == "LIU")
strength_data2$Opponent[d2] <- "Long Island University"

d2 <- which(strength_data2$Opponent == "UMass")
strength_data2$Opponent[d2] <- "Massachusetts"

d2 <- which(strength_data2$Opponent == "Ole Miss")
strength_data2$Opponent[d2] <- "Mississippi"

d2 <- which(strength_data2$Opponent == "UNLV")
strength_data2$Opponent[d2] <- "Nevada-Las Vegas"

d2 <- which(strength_data2$Opponent == "UNC Asheville")
strength_data2$Opponent[d2] <- "North Carolina-Asheville"

d2 <- which(strength_data2$Opponent == "NC State")
strength_data2$Opponent[d2] <- "North Carolina State"

d2 <- which(strength_data2$Opponent == "UNC")
strength_data2$Opponent[d2] <- "North Carolina"

d2 <- which(strength_data2$Opponent == "UNC Greensboro")
strength_data2$Opponent[d2] <- "North Carolina-Greensboro"

d2 <- which(strength_data2$Opponent == "UNC Wilmington")
strength_data2$Opponent[d2] <- "North Carolina-Wilmington"

d2 <- which(strength_data2$Opponent == "Penn")
strength_data2$Opponent[d2] <- "Pennsylvania"

d2 <- which(strength_data2$Opponent == "Pitt")
strength_data2$Opponent[d2] <- "Pittsburgh"

d2 <- which(strength_data2$Opponent == "Saint Mary's")
strength_data2$Opponent[d2] <- "Saint Mary's (CA)"

d2 <- which(strength_data2$Opponent == "USC")
strength_data2$Opponent[d2] <- "Southern California"

d2 <- which(strength_data2$Opponent == "SIU-Edwardsville")
strength_data2$Opponent[d2] <- "SIU Edwardsville"

d2 <- which(strength_data2$Opponent == "SMU")
strength_data2$Opponent[d2] <- "Southern Methodist"

d2 <- which(strength_data2$Opponent == "Southern Miss")
strength_data2$Opponent[d2] <- "Southern Mississippi"

d2 <- which(strength_data2$Opponent == "TCU")
strength_data2$Opponent[d2] <- "Texas Christian"

d2 <- which(strength_data2$Opponent == "UT-Martin")
strength_data2$Opponent[d2] <- "Tennessee-Martin"

d2 <- which(strength_data2$Opponent == "UTEP")
strength_data2$Opponent[d2] <- "Texas-El Paso"

d2 <- which(strength_data2$Opponent == "VCU")
strength_data2$Opponent[d2] <- "Virginia Commonwealth"

d2 <- which(strength_data2$Opponent == "UTSA")
strength_data2$Opponent[d2] <- "Texas-San Antonio"

opp <- unique(strength_data2$Opponent)
tm <- unique(strength_data2$Team)

d1check <- tm %in% opp
d1index <- which(d1check == FALSE)

#writexl::write_xlsx(strength_data2, "C:\\Users\\Jared\\Desktop\\strengthdata4.1.xlsx")

other_rest <- c()

for(i in 1:nrow(strength_data2)) {
  for(j in 1:nrow(strength_data2)){
    if(strength_data2$Date[i] == strength_data2$Date[j] & strength_data2$Opponent[j] == strength_data2$Team[i]){
      other_rest[i] <- strength_data2$Rest[j]
    }
  }
  print(i)
}

opponent_rest <- other_rest
strength_data2 <- cbind(strength_data2, opponent_rest)

writexl::write_xlsx(strength_data2, "C:\\Users\\Jared\\Documents\\strengthdata4.4.xlsx")

## -----------------------------------------------------------------------------------------------------
###Modeling#################################################################################

strength_data_ready <- read_excel("strengthdata4.4.xlsx")

num <- which(is.na(strength_data_ready$opponent_rest))

strength_sched <- na.omit(strength_data_ready)

###
### Data set now only contains games played on the continental united states between two division 1 basketball teams
###

no_travel <- which(strength_sched$Travel <= 10)
strength_sched_travel <- strength_sched[-no_travel,] 

strength_sched_travel$Team <- factor(strength_sched_travel$Team)

my_teams <- levels(strength_sched_travel$Team)
my_teams1 <- my_teams[1:50]
boxplot1 <- which(strength_sched_travel$Team %in% my_teams1)
boxplot_data1 <- strength_sched_travel[boxplot1, ]

my_teams2 <- my_teams[51:100]
boxplot2 <- which(strength_sched_travel$Team %in% my_teams2)
boxplot_data2 <- strength_sched_travel[boxplot2, ]

my_teams3 <- my_teams[101:150]
boxplot3 <- which(strength_sched_travel$Team %in% my_teams3)
boxplot_data3 <- strength_sched_travel[boxplot3, ]

my_teams4 <- my_teams[151:200]
boxplot4 <- which(strength_sched_travel$Team %in% my_teams4)
boxplot_data4 <- strength_sched_travel[boxplot4, ]

my_teams5 <- my_teams[201:250]
boxplot5 <- which(strength_sched_travel$Team %in% my_teams5)
boxplot_data5 <- strength_sched_travel[boxplot5, ]

my_teams6 <- my_teams[251:300]
boxplot6 <- which(strength_sched_travel$Team %in% my_teams6)
boxplot_data6 <- strength_sched_travel[boxplot6, ]

my_teams7 <- my_teams[301:length(my_teams)]
boxplot7 <- which(strength_sched_travel$Team %in% my_teams7)
boxplot_data7 <- strength_sched_travel[boxplot7, ]


## -----------------------------------------------------------------------------------------------------
###
### Test and Training set Point Margin
###

train_index = sample(nrow(strength_sched_travel), 0.90*nrow(strength_sched_travel))
college_train = strength_sched_travel[train_index, ]
college_test = strength_sched_travel[-train_index, ]

trainX = model.matrix(point_margin~Travel+Rest+opponent_rest, data=college_train)[,-c(1)]
testX = model.matrix(point_margin~Travel+Rest+opponent_rest, data=college_test)[,-c(1)]

random_pm_train <- randomForest::randomForest(college_train$point_margin~Travel+Rest+opponent_rest, data = data.frame(trainX), ntree=125)
varImpPlot(random_pm_train)
plot(random_pm_train)

random_pm_preds = predict(random_pm_train, data.frame(testX), type="response")

###
### Lasso 
###

library(glmnet)

no_mod <- c(1:27)
modelX <- strength_sched_travel[, -no_mod]
ncaaX <- model.matrix(point_margin~., data = modelX)[,-c(1)]

lambda_ = exp(seq(-15, 15, length = 1000))
lasso_lm = glmnet(ncaaX, modelX$point_margin, alpha=1, lambda = lambda)
plot(lasso_lm)

lasso_cv = cv.glmnet(ncaaX, modelX$point_margin, alpha=1, lambda = lambda)
lbestlam = lasso_cv$lambda.min
plot(lasso_cv)

lasso_preds = predict(lasso_lm,ncaaX,s=lbestlam,type="response")

###
### Cross Validation Point Margin Random Forest
###

set.seed(19)
pm_validation = sample(nrow(strength_sched_travel), .05*nrow(strength_sched_travel))
college_validation_pm = strength_sched_travel[pm_validation,]
college_nonvalidation_pm = strength_sched_travel[-pm_validation,]

k = 10

nonval_sample_pm = sample(nrow(college_nonvalidation_pm))
nonval_deciles_pm = quantile(1:nrow(college_nonvalidation_pm), seq(0, 1, by=1/k))

cv_list_pm = list()

for(i in 1:k){
  randomized_dec_pm = nonval_sample_pm[ceiling(nonval_deciles_pm[i]):floor(nonval_deciles_pm[i+1])]
  cv_list_pm[[i]] = college_nonvalidation_pm[randomized_dec_pm, ]
}

pred_list_pm = list()

for(i in 1:k) {
  cv_dat_pm = do.call(rbind, cv_list_pm[-i])
  cvX_pm = model.matrix(point_margin ~Travel+Rest+opponent_rest, data=cv_dat_pm)[,-c(1)]
  rf_cv_pm = randomForest::randomForest(cv_dat_pm$point_margin ~ Travel+Rest+opponent_rest, data=data.frame(cvX_pm),ntree=150)
  test_dat_pm = cv_list_pm[[i]]
  test_datX_pm = model.matrix(point_margin ~Travel+Rest+opponent_rest, data=test_dat_pm)[,-c(1)]
  pred_list_pm[[i]] = predict(rf_cv_pm, data.frame(test_datX_pm), type="response")
  print(i)
}

cv_preds_pm = do.call(c, pred_list_pm)

###
### Cross Validation Point Margin Gradient Boosting
###

set.seed(19)
pm_validation_gb = sample(nrow(strength_sched_travel), .05*nrow(strength_sched_travel))
college_validation_pm_gb = strength_sched_travel[pm_validation_gb,]
college_nonvalidation_pm_gb = strength_sched_travel[-pm_validation_gb,]
#head(college_validation)

k = 10

nonval_sample_pm_gb = sample(nrow(college_nonvalidation_pm_gb))
nonval_deciles_pm_gb = quantile(1:nrow(college_nonvalidation_pm_gb), seq(0, 1, by=1/k))

cv_list_pm_gb = list()

for(i in 1:k){
  randomized_dec_pm_gb = nonval_sample_pm_gb[ceiling(nonval_deciles_pm_gb[i]):floor(nonval_deciles_pm_gb[i+1])]
  cv_list_pm_gb[[i]] = college_nonvalidation_pm_gb[randomized_dec_pm_gb, ]
}

pred_list_pm_gb = list()

for(i in 1:k) {
  cv_dat_pm_gb = do.call(rbind, cv_list_pm_gb[-i])
  cvX_pm_gb = model.matrix(point_margin ~Travel+Rest+opponent_rest, data=cv_dat_pm_gb)[,-c(1)]
  rf_cv_pm_gb = gbm::gbm(cv_dat_pm_gb$point_margin ~ Travel+Rest+opponent_rest, data=data.frame(cvX_pm_gb))
  test_dat_pm_gb = cv_list_pm_gb[[i]]
  test_datX_pm_gb = model.matrix(point_margin ~Travel+Rest+opponent_rest, data=test_dat_pm_gb)[,-c(1)]
  pred_list_pm_gb[[i]] = predict(rf_cv_pm_gb, data.frame(test_datX_pm_gb), type="response")
  print(i)
}

cv_preds_pm_gb = do.call(c, pred_list_pm_gb)

################## Compare

plot(density(cv_preds_pm))
lines(density(college_nonvalidation_pm$point_margin),col="blue")
lines(density(lasso_preds),col="green")
lines(density(random_pm_preds),col="red")
lines(density(cv_preds_pm_gb), col = "cyan")

mean((cv_preds_pm-college_nonvalidation_pm$point_margin[nonval_sample_pm])^2)
mean((cv_preds_pm_gb-college_nonvalidation_pm_gb$point_margin[nonval_sample_pm_gb])^2)
mean((lasso_preds-modelX$point_margin)^2)
mean((random_pm_preds-college_test$point_margin)^2)

sqrt(mean((cv_preds_pm_gb-college_nonvalidation_pm_gb$point_margin[nonval_sample_pm_gb])^2))

cVallX <- model.matrix(point_margin~Travel+Rest+opponent_rest, data = college_nonvalidation_pm)[,c(-1)]
gb_cVall <- gbm::gbm(college_nonvalidation_pm$point_margin~., data= data.frame(cVallX))

valX = model.matrix(point_margin ~Travel+Rest+opponent_rest, data=college_validation_pm)[,-c(1)]
val_preds = predict(gb_cVall,data.frame(valX),type="response")

plot(density(val_preds))
lines(density(college_validation_pm$point_margin),col="blue")

mean((val_preds-college_validation_pm$point_margin)^2)
sqrt(mean((val_preds-college_validation_pm$point_margin)^2))

##################################################################

make_data <- function(rest, opp_rest){
  set.seed(19)

  rest_sim <- rep(rest, 3316)
  rest_opp_sim <- rep(opp_rest, 3316)
  travel_sim <- sample(10:3326, size = 3316)
  
  sim_data <- data.frame(cbind(rest_sim, rest_opp_sim, travel_sim))
  colnames(sim_data) <- c("Rest", "opponent_rest", "Travel")
  
  sim_preds <- predict(gb_cVall, sim_data, type = "response")
  sim_results <- data.frame(sim_data$Travel, sim_preds)
}

datas <- c()
temps <- c()

for(i in 1:10){
  for(j in 1:10){
    rest_group <- rep(i, 3316)
    opponent_rest_group <- rep(j, 3316)
    temps[[j]] <- data.frame(make_data(i,j))
    temps[[j]] <- cbind(temps[[j]], rest_group, opponent_rest_group)
    
  }
  datas[[i]] <- temps
  temps <- c()
}

big <- data.frame()

for(i in 1:10){
  for(j in 1:10){
    temp <- datas[[i]][[j]]
    big <- rbind(big, temp)
  }
}


big <- data.frame(big)

total_preds <- big %>% group_by(sim_data.Travel) %>%
  summarize(sim_preds = mean(sim_preds))

total_preds2 <- big %>% group_by(rest_group) %>% 
  mutate(rest_group = factor(rest_group))

total_preds3 <- big %>% group_by(opponent_rest_group) %>% 
  mutate(opponent_rest_group = factor(opponent_rest_group))

ggplot(total_preds2, aes(x= rest_group, y = sim_preds)) +
  geom_boxplot()+
  theme_bw()+
  labs(x="Days Rested by Away Team", y = "Predicted Point Margin")+
  ggtitle("Away Team Point Margin by Days Rested") +
  theme(plot.title = element_text(hjust = .5))+
  geom_hline(yintercept = 0, col = "red", lwd = 1)

ggplot(total_preds3, aes(x= opponent_rest_group, y = sim_preds)) +
  geom_boxplot()+
  theme_bw() +
  labs(x="Days Rested by Home Team", y = "Predicted Point Margin")+
  ggtitle("Away Team Point Margin by Days Rested") +
  theme(plot.title = element_text(hjust = .5))+
  geom_hline(yintercept = 0, col = "red", lwd = 1)

ggplot(total_preds, aes(x=sim_data.Travel, y=sim_preds)) +
  geom_point()+
  theme_bw()+
  labs(x="Distance Traveled by Away Team", y = "Predicted Point Margin")+
  ggtitle("Away Team Point Margin by Distance") +
  theme(plot.title = element_text(hjust = .5))+
  geom_hline(yintercept = 0, col = "red", lwd = 1)


#######################################################################################

## -----------------------------------------------------------------------------------------------------

strength_sched_travel$turn_over_ratio <- strength_sched_travel$TOVO-strength_sched_travel$TOV

###
### Test and Training set Point Margin
###

train_index = sample(nrow(strength_sched_travel), 0.90*nrow(strength_sched_travel))
college_train = strength_sched_travel[train_index, ]
college_test = strength_sched_travel[-train_index, ]

#head(college_train, n=10)

trainX = model.matrix(turn_over_ratio~Travel+Rest+opponent_rest, data=college_train)[,-c(1)]
testX = model.matrix(turn_over_ratio~Travel+Rest+opponent_rest, data=college_test)[,-c(1)]

random_pm_train <- randomForest::randomForest(college_train$turn_over_ratio~Travel+Rest+opponent_rest, data = data.frame(trainX), ntree=125)
varImpPlot(random_pm_train)
plot(random_pm_train)

random_pm_preds = predict(random_pm_train, data.frame(testX), type="response")

###
### Lasso 
###

library(glmnet)
head(strength_sched_travel)

no_mod <- c(1:27, 30)
modelX <- strength_sched_travel[, -no_mod]
ncaaX <- model.matrix(turn_over_ratio~., data = modelX)[,-c(1)]

lambda_ = exp(seq(-15, 15, length = 1000))
lasso_lm = glmnet(ncaaX, modelX$turn_over_ratio, alpha=1, lambda = lambda)
plot(lasso_lm)

lasso_cv = cv.glmnet(ncaaX, modelX$turn_over_ratio, alpha=1, lambda = lambda)
lbestlam = lasso_cv$lambda.min
plot(lasso_cv)

lasso_preds = predict(lasso_lm,ncaaX,s=lbestlam,type="response")

###
### Cross Validation Point Margin Random Forest
###

set.seed(19)
pm_validation = sample(nrow(strength_sched_travel), .05*nrow(strength_sched_travel))
college_validation_pm = strength_sched_travel[pm_validation,]
college_nonvalidation_pm = strength_sched_travel[-pm_validation,]

k = 10

nonval_sample_pm = sample(nrow(college_nonvalidation_pm))
nonval_deciles_pm = quantile(1:nrow(college_nonvalidation_pm), seq(0, 1, by=1/k))

cv_list_pm = list()

for(i in 1:k){
  randomized_dec_pm = nonval_sample_pm[ceiling(nonval_deciles_pm[i]):floor(nonval_deciles_pm[i+1])]
  cv_list_pm[[i]] = college_nonvalidation_pm[randomized_dec_pm, ]
}

pred_list_pm = list()

for(i in 1:k) {
  cv_dat_pm = do.call(rbind, cv_list_pm[-i])
  cvX_pm = model.matrix(turn_over_ratio ~Travel+Rest+opponent_rest, data=cv_dat_pm)[,-c(1)]
  rf_cv_pm = randomForest::randomForest(cv_dat_pm$turn_over_ratio ~ Travel+Rest+opponent_rest, data=data.frame(cvX_pm),ntree=80)
  test_dat_pm = cv_list_pm[[i]]
  test_datX_pm = model.matrix(turn_over_ratio ~Travel+Rest+opponent_rest, data=test_dat_pm)[,-c(1)]
  pred_list_pm[[i]] = predict(rf_cv_pm, data.frame(test_datX_pm), type="response")
  print(i)
}

cv_preds_pm = do.call(c, pred_list_pm)

###
### Cross Validation Point Margin Gradient Boosting
###

set.seed(19)
pm_validation_gb = sample(nrow(strength_sched_travel), .05*nrow(strength_sched_travel))
college_validation_pm_gb = strength_sched_travel[pm_validation_gb,]
college_nonvalidation_pm_gb = strength_sched_travel[-pm_validation_gb,]
#head(college_validation)

k = 10

nonval_sample_pm_gb = sample(nrow(college_nonvalidation_pm_gb))
nonval_deciles_pm_gb = quantile(1:nrow(college_nonvalidation_pm_gb), seq(0, 1, by=1/k))

cv_list_pm_gb = list()

for(i in 1:k){
  randomized_dec_pm_gb = nonval_sample_pm_gb[ceiling(nonval_deciles_pm_gb[i]):floor(nonval_deciles_pm_gb[i+1])]
  cv_list_pm_gb[[i]] = college_nonvalidation_pm_gb[randomized_dec_pm_gb, ]
}

pred_list_pm_gb = list()

for(i in 1:k) {
  cv_dat_pm_gb = do.call(rbind, cv_list_pm_gb[-i])
  cvX_pm_gb = model.matrix(turn_over_ratio ~Travel+Rest+opponent_rest, data=cv_dat_pm_gb)[,-c(1)]
  rf_cv_pm_gb = gbm::gbm(cv_dat_pm_gb$turn_over_ratio ~ Travel+Rest+opponent_rest, data=data.frame(cvX_pm_gb))
  test_dat_pm_gb = cv_list_pm_gb[[i]]
  test_datX_pm_gb = model.matrix(turn_over_ratio ~Travel+Rest+opponent_rest, data=test_dat_pm_gb)[,-c(1)]
  pred_list_pm_gb[[i]] = predict(rf_cv_pm_gb, data.frame(test_datX_pm_gb), type="response")
  print(i)
}

cv_preds_pm_gb = do.call(c, pred_list_pm_gb)

### Test Performance

plot(density(cv_preds_pm))
lines(density(college_nonvalidation_pm$turn_over_ratio),col="blue")
lines(density(lasso_preds),col="green")
lines(density(random_pm_preds),col="red")
lines(density(cv_preds_pm_gb), col = "cyan")
#plot(density(college_nonvalidation_pm$turn_over_ratio),col="blue")

mean((cv_preds_pm-college_nonvalidation_pm$turn_over_ratio[nonval_sample_pm])^2)
mean((cv_preds_pm_gb-college_nonvalidation_pm_gb$turn_over_ratio[nonval_sample_pm_gb])^2)
mean((lasso_preds-modelX$turn_over_ratio)^2)
mean((random_pm_preds-college_test$turn_over_ratio)^2)

sqrt(mean((lasso_preds-modelX$turn_over_ratio)^2))

########################################################################################

### Validation

cVallX <- model.matrix(turn_over_ratio~Travel+Rest+opponent_rest, data = college_nonvalidation_pm)[,c(-1)]
gb_cVall <- gbm::gbm(college_nonvalidation_pm$turn_over_ratio~., data= data.frame(cVallX))

valX = model.matrix(turn_over_ratio ~Travel+Rest+opponent_rest, data=college_validation_pm)[,-c(1)]
val_preds = predict(gb_cVall,data.frame(valX),type="response")

plot(density(val_preds))
lines(density(college_validation_pm$turn_over_ratio),col="blue")

mean((val_preds-college_validation_pm$turn_over_ratio)^2)
sqrt(mean((val_preds-college_validation_pm$turn_over_ratio)^2))

###############################################################################

make_data <- function(rest, opp_rest){
  set.seed(19)

  rest_sim <- rep(rest, 3316)
  rest_opp_sim <- rep(opp_rest, 3316)
  travel_sim <- sample(10:3326, size = 3316)
  
  sim_data <- data.frame(cbind(rest_sim, rest_opp_sim, travel_sim))
  colnames(sim_data) <- c("Rest", "opponent_rest", "Travel")
  
  sim_preds <- predict(gb_cVall, sim_data, type = "response")
  sim_results <- data.frame(sim_data$Travel, sim_preds)
}

datas <- c()
temps <- c()

for(i in 1:10){
  for(j in 1:10){
    rest_group <- rep(i, 3316)
    opponent_rest_group <- rep(j, 3316)
    temps[[j]] <- data.frame(make_data(i,j))
    temps[[j]] <- cbind(temps[[j]], rest_group, opponent_rest_group)
    
  }
  datas[[i]] <- temps
  temps <- c()
}

big <- data.frame()

for(i in 1:10){
  for(j in 1:10){
    temp <- datas[[i]][[j]]
    big <- rbind(big, temp)
  }
}


big <- data.frame(big)

total_preds <- big %>% group_by(sim_data.Travel) %>%
  summarize(sim_preds = mean(sim_preds))

total_preds2 <- big %>% group_by(rest_group) %>% 
  mutate(rest_group = factor(rest_group))

total_preds3 <- big %>% group_by(opponent_rest_group) %>% 
  mutate(opponent_rest_group = factor(opponent_rest_group))

ggplot(total_preds2, aes(x= rest_group, y = sim_preds)) +
  geom_boxplot()+
  theme_bw()+
  labs(x="Days Rested by Away Team", y = "Predicted Turnover Ratio")+
  ggtitle("Away Team Turnover Ratio by Days Rested") +
  theme(plot.title = element_text(hjust = .5))+
  geom_hline(yintercept = 0, col = "red", lwd = 1)

ggplot(total_preds3, aes(x= opponent_rest_group, y = sim_preds)) +
  geom_boxplot()+
  theme_bw() +
  labs(x="Days Rested by Home Team", y = "Predicted Turnover Ratio")+
  ggtitle("Away Team Turnover Ratio by Days Rested") +
  theme(plot.title = element_text(hjust = .5))+
  geom_hline(yintercept = 0, col = "red", lwd = 1)

ggplot(total_preds, aes(x=sim_data.Travel, y=sim_preds)) +
  geom_point()+
  theme_bw()+
  labs(x="Distance Traveled by Away Team", y = "Predicted Turnover Ratio")+
  ggtitle("Away Team Turnover Ratio By Distance") +
  theme(plot.title = element_text(hjust = .5))+
  geom_hline(yintercept = 0, col = "red", lwd = 1)

## -----------------------------------------------------------------------------------------------------

train_index = sample(nrow(strength_sched_travel), 0.90*nrow(strength_sched_travel))
college_train = strength_sched_travel[train_index, ]
college_test = strength_sched_travel[-train_index, ]

#head(college_train, n=10)

trainX = model.matrix(`FG%`~Travel+Rest+opponent_rest, data=college_train)[,-c(1)]
testX = model.matrix(`FG%`~Travel+Rest+opponent_rest, data=college_test)[,-c(1)]

random_pm_train <- randomFOrest::randomForest(college_train$`FG%`~Travel+Rest+opponent_rest, data = data.frame(trainX), ntree=125)
varImpPlot(random_pm_train)
plot(random_pm_train)

random_pm_preds = predict(random_pm_train, data.frame(testX), type="response")

###
### Lasso 
###

library(glmnet)
head(strength_sched_travel)

no_mod <- c(1:6,8:27,30, 32)
modelX <- strength_sched_travel[, -no_mod]

ncaaX <- model.matrix(`FG%`~., data = modelX)[,-c(1)]

lambda_ = exp(seq(-15, 15, length = 1000))
lasso_lm = glmnet(ncaaX, modelX$`FG%`, alpha=1, lambda = lambda)
plot(lasso_lm)

lasso_cv = cv.glmnet(ncaaX, modelX$`FG%`, alpha=1, lambda = lambda)
lbestlam = lasso_cv$lambda.min
plot(lasso_cv)

lasso_preds = predict(lasso_lm,ncaaX,s=lbestlam,type="response")

###
### Cross Validation Point Margin Random Forest
###

set.seed(19)
pm_validation = sample(nrow(strength_sched_travel), .05*nrow(strength_sched_travel))
college_validation_pm = strength_sched_travel[pm_validation,]
college_nonvalidation_pm = strength_sched_travel[-pm_validation,]
#head(college_validation)

k = 10

nonval_sample_pm = sample(nrow(college_nonvalidation_pm))
nonval_deciles_pm = quantile(1:nrow(college_nonvalidation_pm), seq(0, 1, by=1/k))

cv_list_pm = list()

for(i in 1:k){
  randomized_dec_pm = nonval_sample_pm[ceiling(nonval_deciles_pm[i]):floor(nonval_deciles_pm[i+1])]
  cv_list_pm[[i]] = college_nonvalidation_pm[randomized_dec_pm, ]
}

pred_list_pm = list()

for(i in 1:k) {
  cv_dat_pm = do.call(rbind, cv_list_pm[-i])
  cvX_pm = model.matrix(`FG%` ~Travel+Rest+opponent_rest, data=cv_dat_pm)[,-c(1)]
  rf_cv_pm = randomForest::randomForest(cv_dat_pm$`FG%` ~ Travel+Rest+opponent_rest, data=data.frame(cvX_pm),ntree=60)
  test_dat_pm = cv_list_pm[[i]]
  test_datX_pm = model.matrix(`FG%` ~Travel+Rest+opponent_rest, data=test_dat_pm)[,-c(1)]
  pred_list_pm[[i]] = predict(rf_cv_pm, data.frame(test_datX_pm), type="response")
  print(i)
}

cv_preds_pm = do.call(c, pred_list_pm)

###
### Cross Validation Point Margin Gradient Boosting
###

set.seed(19)
pm_validation_gb = sample(nrow(strength_sched_travel), .05*nrow(strength_sched_travel))
college_validation_pm_gb = strength_sched_travel[pm_validation_gb,]
college_nonvalidation_pm_gb = strength_sched_travel[-pm_validation_gb,]
#head(college_validation)

k = 10

nonval_sample_pm_gb = sample(nrow(college_nonvalidation_pm_gb))
nonval_deciles_pm_gb = quantile(1:nrow(college_nonvalidation_pm_gb), seq(0, 1, by=1/k))

cv_list_pm_gb = list()

for(i in 1:k){
  randomized_dec_pm_gb = nonval_sample_pm_gb[ceiling(nonval_deciles_pm_gb[i]):floor(nonval_deciles_pm_gb[i+1])]
  cv_list_pm_gb[[i]] = college_nonvalidation_pm_gb[randomized_dec_pm_gb, ]
}

pred_list_pm_gb = list()

for(i in 1:k) {
  cv_dat_pm_gb = do.call(rbind, cv_list_pm_gb[-i])
  cvX_pm_gb = model.matrix(`FG%` ~Travel+Rest+opponent_rest, data=cv_dat_pm_gb)[,-c(1)]
  rf_cv_pm_gb = gbm::gbm(cv_dat_pm_gb$`FG%` ~ Travel+Rest+opponent_rest, data=data.frame(cvX_pm_gb))
  test_dat_pm_gb = cv_list_pm_gb[[i]]
  test_datX_pm_gb = model.matrix(`FG%` ~Travel+Rest+opponent_rest, data=test_dat_pm_gb)[,-c(1)]
  pred_list_pm_gb[[i]] = predict(rf_cv_pm_gb, data.frame(test_datX_pm_gb), type="response")
  print(i)
}

cv_preds_pm_gb = do.call(c, pred_list_pm_gb)

plot(density(cv_preds_pm))
lines(density(college_nonvalidation_pm$`FG%`),col="blue")
lines(density(lasso_preds),col="green")
lines(density(random_pm_preds),col="red")
lines(density(cv_preds_pm_gb), col = "cyan")
#plot(density(college_nonvalidation_pm$`FG%`),col="blue")

mean((cv_preds_pm-college_nonvalidation_pm$`FG%`[nonval_sample_pm])^2)
mean((cv_preds_pm_gb-college_nonvalidation_pm_gb$`FG%`[nonval_sample_pm_gb])^2)
mean((lasso_preds-modelX$`FG%`)^2)
mean((random_pm_preds-college_test$`FG%`)^2)

sqrt(mean((cv_preds_pm_gb-college_nonvalidation_pm_gb$`FG%`[nonval_sample_pm_gb])^2))

### Validation

cVallX <- model.matrix(`FG%`~Travel+Rest+opponent_rest, data = college_nonvalidation_pm)[,c(-1)]
random_cVall <- randomForest::randomForest(cv_dat_pm$`FG%`~Travel+Rest+opponent_rest, data=data.frame(cvX_pm),ntree=60)

valX = model.matrix(`FG%`~Travel+Rest+opponent_rest, data=college_validation_pm)[,-c(1)]
val_preds = predict(random_cVall,data.frame(valX),type="response")

plot(density(val_preds))
lines(density(college_validation_pm$`FG%`),col="blue")

mean((val_preds-college_validation_pm$`FG%`)^2)
sqrt(mean((val_preds-college_validation_pm$`FG%`)^2))

#######################################################

make_data <- function(rest, opp_rest){
  set.seed(19)

  rest_sim <- rep(rest, 3316)
  rest_opp_sim <- rep(opp_rest, 3316)
  travel_sim <- sample(10:3326, size = 3316)
  
  sim_data <- data.frame(cbind(rest_sim, rest_opp_sim, travel_sim))
  colnames(sim_data) <- c("Rest", "opponent_rest", "Travel")
  
  sim_preds <- predict(random_cVall, sim_data, type = "response")
  sim_results <- data.frame(sim_data$Travel, sim_preds)
}

datas <- c()
temps <- c()

for(i in 1:10){
  for(j in 1:10){
    rest_group <- rep(i, 3316)
    opponent_rest_group <- rep(j, 3316)
    temps[[j]] <- data.frame(make_data(i,j))
    temps[[j]] <- cbind(temps[[j]], rest_group, opponent_rest_group)
    
  }
  datas[[i]] <- temps
  temps <- c()
}

big <- data.frame()

for(i in 1:10){
  for(j in 1:10){
    temp <- datas[[i]][[j]]
    big <- rbind(big, temp)
  }
}


big <- data.frame(big)

total_preds <- big %>% group_by(sim_data.Travel) %>%
  summarize(sim_preds = mean(sim_preds))

total_preds2 <- big %>% group_by(rest_group) %>% 
  mutate(rest_group = factor(rest_group))

total_preds3 <- big %>% group_by(opponent_rest_group) %>% 
  mutate(opponent_rest_group = factor(opponent_rest_group))

ggplot(total_preds2, aes(x= rest_group, y = sim_preds)) +
  geom_boxplot()+
  theme_bw()+
  labs(x="Days Rested by Away Team", y = "Predicted FG Percentage")+
  ggtitle("Away Team FG Percentage by Days Rested") +
  theme(plot.title = element_text(hjust = .5))

ggplot(total_preds3, aes(x= opponent_rest_group, y = sim_preds)) +
  geom_boxplot()+
  theme_bw() +
  labs(x="Days Rested by Home Team", y = "Predicted FG Percentage")+
  ggtitle("Away Team FG Percentage by Days Rested") +
  theme(plot.title = element_text(hjust = .5))

ggplot(total_preds, aes(x=sim_data.Travel, y=sim_preds)) +
  geom_point()+
  theme_bw()+
  labs(x="Distance Traveled by Away Team", y = "Predicted FG Percentage")+
  ggtitle("Away Team FG Percentage By Distance") +
  theme(plot.title = element_text(hjust = .5))
  
## -----------------------------------------------------------------------------------------------------


train_index = sample(nrow(strength_sched_travel), 0.90*nrow(strength_sched_travel))
college_train = strength_sched_travel[train_index, ]
college_test = strength_sched_travel[-train_index, ]

#head(college_train, n=10)

trainX = model.matrix(PF~Travel+Rest+opponent_rest, data=college_train)[,-c(1)]
testX = model.matrix(PF~Travel+Rest+opponent_rest, data=college_test)[,-c(1)]

random_pm_train <- randomForest::randomForest(college_train$PF~Travel+Rest+opponent_rest, data = data.frame(trainX), ntree=125)
varImpPlot(random_pm_train)
plot(random_pm_train)

random_pm_preds = predict(random_pm_train, data.frame(testX), type="response")

###
### Lasso 
###

library(glmnet)

no_mod <- c(1:15,17:27,30, 32)
modelX <- strength_sched_travel[, -no_mod]

ncaaX <- model.matrix(PF~., data = modelX)[,-c(1)]

lambda_ = exp(seq(-15, 15, length = 1000))
lasso_lm = glmnet(ncaaX, modelX$PF, alpha=1, lambda = lambda)
plot(lasso_lm)

lasso_cv = cv.glmnet(ncaaX, modelX$PF, alpha=1, lambda = lambda)
lbestlam = lasso_cv$lambda.min
plot(lasso_cv)

lasso_preds = predict(lasso_lm,ncaaX,s=lbestlam,type="response")

###
### Cross Validation Point Margin Random Forest
###

set.seed(19)
pm_validation = sample(nrow(strength_sched_travel), .05*nrow(strength_sched_travel))
college_validation_pm = strength_sched_travel[pm_validation,]
college_nonvalidation_pm = strength_sched_travel[-pm_validation,]

k = 10

nonval_sample_pm = sample(nrow(college_nonvalidation_pm))
nonval_deciles_pm = quantile(1:nrow(college_nonvalidation_pm), seq(0, 1, by=1/k))

cv_list_pm = list()

for(i in 1:k){
  randomized_dec_pm = nonval_sample_pm[ceiling(nonval_deciles_pm[i]):floor(nonval_deciles_pm[i+1])]
  cv_list_pm[[i]] = college_nonvalidation_pm[randomized_dec_pm, ]
}

pred_list_pm = list()

for(i in 1:k) {
  cv_dat_pm = do.call(rbind, cv_list_pm[-i])
  cvX_pm = model.matrix(PF~Travel+Rest+opponent_rest, data=cv_dat_pm)[,-c(1)]
  rf_cv_pm = randomForest::randomForest(cv_dat_pm$PF ~ Travel+Rest+opponent_rest, data=data.frame(cvX_pm),ntree=60)
  test_dat_pm = cv_list_pm[[i]]
  test_datX_pm = model.matrix(PF ~Travel+Rest+opponent_rest, data=test_dat_pm)[,-c(1)]
  pred_list_pm[[i]] = predict(rf_cv_pm, data.frame(test_datX_pm), type="response")
  print(i)
}

cv_preds_pm = do.call(c, pred_list_pm)

###
### Cross Validation Point Margin Gradient Boosting
###

set.seed(19)
pm_validation_gb = sample(nrow(strength_sched_travel), .05*nrow(strength_sched_travel))
college_validation_pm_gb = strength_sched_travel[pm_validation_gb,]
college_nonvalidation_pm_gb = strength_sched_travel[-pm_validation_gb,]
#head(college_validation)

k = 10

nonval_sample_pm_gb = sample(nrow(college_nonvalidation_pm_gb))
nonval_deciles_pm_gb = quantile(1:nrow(college_nonvalidation_pm_gb), seq(0, 1, by=1/k))

cv_list_pm_gb = list()

for(i in 1:k){
  randomized_dec_pm_gb = nonval_sample_pm_gb[ceiling(nonval_deciles_pm_gb[i]):floor(nonval_deciles_pm_gb[i+1])]
  cv_list_pm_gb[[i]] = college_nonvalidation_pm_gb[randomized_dec_pm_gb, ]
}

pred_list_pm_gb = list()

for(i in 1:k) {
  cv_dat_pm_gb = do.call(rbind, cv_list_pm_gb[-i])
  cvX_pm_gb = model.matrix(PF~Travel+Rest+opponent_rest, data=cv_dat_pm_gb)[,-c(1)]
  rf_cv_pm_gb = gbm::gbm(cv_dat_pm_gb$PF~Travel+Rest+opponent_rest, data=data.frame(cvX_pm_gb))
  test_dat_pm_gb = cv_list_pm_gb[[i]]
  test_datX_pm_gb = model.matrix(PF~Travel+Rest+opponent_rest, data=test_dat_pm_gb)[,-c(1)]
  pred_list_pm_gb[[i]] = predict(rf_cv_pm_gb, data.frame(test_datX_pm_gb), type="response")
  print(i)
}

cv_preds_pm_gb = do.call(c, pred_list_pm_gb)

plot(density(cv_preds_pm))
lines(density(college_nonvalidation_pm$PF),col="blue")
lines(density(lasso_preds),col="green")
lines(density(random_pm_preds),col="red")
lines(density(cv_preds_pm_gb), col = "cyan")
#plot(density(college_nonvalidation_pm$PF),col="blue")

mean((cv_preds_pm-college_nonvalidation_pm$PF[nonval_sample_pm])^2)
mean((cv_preds_pm_gb-college_nonvalidation_pm_gb$PF[nonval_sample_pm_gb])^2)
mean((lasso_preds-modelX$PF)^2)
mean((random_pm_preds-college_test$PF)^2)

sqrt(mean((cv_preds_pm_gb-college_nonvalidation_pm_gb$PF[nonval_sample_pm_gb])^2))

### Validation

cVallX <- model.matrix(PF~Travel+Rest+opponent_rest, data = college_nonvalidation_pm)[,c(-1)]
random_cVall <- randomForest::randomForest(cv_dat_pm$PF~Travel+Rest+opponent_rest, data=data.frame(cvX_pm),ntree=60)

valX = model.matrix(PF~Travel+Rest+opponent_rest, data=college_validation_pm)[,-c(1)]
val_preds = predict(random_cVall,data.frame(valX),type="response")

plot(density(val_preds))
lines(density(college_validation_pm$PF),col="blue")

mean((val_preds-college_validation_pm$PF)^2)
sqrt(mean((val_preds-college_validation_pm$PF)^2))

#######################################################

make_data <- function(rest, opp_rest){
  set.seed(19)

  rest_sim <- rep(rest, 3316)
  rest_opp_sim <- rep(opp_rest, 3316)
  travel_sim <- sample(10:3326, size = 3316)
  
  sim_data <- data.frame(cbind(rest_sim, rest_opp_sim, travel_sim))
  colnames(sim_data) <- c("Rest", "opponent_rest", "Travel")
  
  sim_preds <- predict(random_cVall, sim_data, type = "response")
  sim_results <- data.frame(sim_data$Travel, sim_preds)
}

datas <- c()
temps <- c()

for(i in 1:10){
  for(j in 1:10){
    rest_group <- rep(i, 3316)
    opponent_rest_group <- rep(j, 3316)
    temps[[j]] <- data.frame(make_data(i,j))
    temps[[j]] <- cbind(temps[[j]], rest_group, opponent_rest_group)
    
  }
  datas[[i]] <- temps
  temps <- c()
}

big <- data.frame()

for(i in 1:10){
  for(j in 1:10){
    temp <- datas[[i]][[j]]
    big <- rbind(big, temp)
  }
}


big <- data.frame(big)

total_preds <- big %>% group_by(sim_data.Travel) %>%
  summarize(sim_preds = mean(sim_preds))

total_preds2 <- big %>% group_by(rest_group) %>% 
  mutate(rest_group = factor(rest_group))

total_preds3 <- big %>% group_by(opponent_rest_group) %>% 
  mutate(opponent_rest_group = factor(opponent_rest_group))

ggplot(total_preds2, aes(x= rest_group, y = sim_preds)) +
  geom_boxplot()+
  theme_bw()+
  labs(x="Days Rested by Away Team", y = "Predicted Number of Personal Fouls")+
  ggtitle("Away Team Personal Fouls by Days Rested") +
  theme(plot.title = element_text(hjust = .5))

ggplot(total_preds3, aes(x= opponent_rest_group, y = sim_preds)) +
  geom_boxplot()+
  theme_bw() +
  labs(x="Days Rested by Home Team", y = "Predicted Number of Personal Fouls")+
  ggtitle("Away Team Personal Fouls by Days Rested") +
  theme(plot.title = element_text(hjust = .5))

ggplot(total_preds, aes(x=sim_data.Travel, y=sim_preds)) +
  geom_point()+
  theme_bw()+
  labs(x="Distance Traveled by Away Team", y = "Predicted Number of Personal Fouls")+
  ggtitle("Away Team Personal Fouls By Distance") +
  theme(plot.title = element_text(hjust = .5))

## -----------------------------------------------------------------------------------------------------

strength_sched_logit <- strength_sched_travel
strength_sched_logit$Win = ifelse(grepl('W',strength_sched_logit$`W/L`),1,0)

### Make Train and Test Sets

nocol <- c(1:27, 30)

mod_df = strength_sched_logit[,-nocol]
mod_df <- data.frame(mod_df)
mod_df1 = mod_df

for(i in 1:ncol(mod_df)){
  mod_df1[,i] = as.numeric(mod_df[,i])
}

train_iters = sample(1:nrow(mod_df),.9*nrow(mod_df))
train_df = mod_df1[train_iters,]
test_df = mod_df1[-train_iters,]

trainX = model.matrix(~ .,data=train_df)[,-c(1)]
testX = model.matrix(~ .,data=test_df)[,-c(1)]

### Lasso 

library(glmnet)

lambda = exp(seq(-5,-2,length=100))
lasso_lm = glmnet::glmnet(trainX[,-c(4)],trainX[,4],alpha=1,lambda=lambda,family="binomial")
plot(lasso_lm)

lasso_cv = glmnet::cv.glmnet(trainX[,-c(4)],trainX[,4],alpha=1,lambda = lambda,family="binomial")
lbestlam = lasso_cv$lambda.min
plot(lasso_cv)

lcoefs = predict(lasso_lm,s=lbestlam,type="coefficient")
lvars = names(lcoefs[lcoefs[,1]!=0,])[-c(1)]

### Logistic

library(bestglm)
data <- data.frame(trainX[, c(lvars, "Win")])

train_log_1 <- bestglm::glm(Win~Travel+opponent_rest, data = data, family=binomial(link = logit))
summary(train_log_1)
r_square <- 1-(6709/6738) ### R squared of .004

### Random Forest

rf_mod = randomForest::randomForest(as.factor(Win) ~ ., data=data.frame(trainX))
varImpPlot(rf_mod)

### Evaluation

log1_preds = predict(train_log_1,test_df)
rf_preds = predict(rf_mod,data.frame(testX))

### Confusion Matrix

table(data.frame(ifelse(log1_preds>.5,1,0),test_df$Win))
table(data.frame(rf_preds,test_df$Win))

#### gains plot
library(WVPlots)

modelX = model.matrix(~ .,data=mod_df1)[,-c(1)]
rf_full = randomForest::randomForest(as.factor(Win) ~ ., data=data.frame(modelX))
mod_df1$rf_preds = predict(rf_full,type="prob")[,2]

WVPlots::ROCPlot(mod_df1,"rf_preds","Win",1,title="NCAA RF Gains")
# library(pROC) is also good for getting raw AUC/ROC

WVPlots::GainCurvePlot(mod_df1,"rf_preds","Win",title="NCAA RF Gains",wizard_color = "blue")

### Not great results for Win / Loss
