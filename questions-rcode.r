# read in grambank data
grambank <- read.csv("grambank-values-qonly.csv")

# define headers for new data frame
headers <- c('Language','GB257','GB260','GB262','GB263','GB264','GB285','GB286','GB291','GB297')

# create new data frame to be populated
grambank_gw <- data.frame(matrix(ncol = 10, nrow = 0))

# add headers to new data frame
colnames(grambank_gw) <- headers

# populate new data frame with values for individual Grambank parameters
for (row in 1:nrow(grambank)) {
  language <- grambank$Language_ID[row]
  parameter <- grambank$Parameter_ID[row]
  value <- grambank$Value[row]
  if (language %in% grambank_gw$Language == FALSE) {
    grambank_gw <- rbind(grambank_gw, data.frame(Language=language,GB257='?',GB260='?',GB262='?',GB263='?',GB264='?',GB285='?',GB286='?',GB291='?',GB297='?'))
    levels(grambank_gw$GB257) <- c("?", "1", "0")
    levels(grambank_gw$GB260) <- c("?", "1", "0")
    levels(grambank_gw$GB262) <- c("?", "1", "0")
    levels(grambank_gw$GB263) <- c("?", "1", "0")
    levels(grambank_gw$GB264) <- c("?", "1", "0")
    levels(grambank_gw$GB285) <- c("?", "1", "0")
    levels(grambank_gw$GB286) <- c("?", "1", "0")
    levels(grambank_gw$GB291) <- c("?", "1", "0")
    levels(grambank_gw$GB297) <- c("?", "1", "0")
  }
  grambank_gw[which(grambank_gw$Language == language), which(colnames(grambank_gw)==parameter)] <- value
}

# add new column for coding based on those parameters
grambank_gw['Marking']='MultipleStrategies'

# define factor levels for new column coding
levels(grambank_gw$Marking) <- c("MultipleStrategies", "NoQuestionMarking", "UnknownQuestionMarking", "IntonationOnly", "WordOrderOnly", "ParticleOnly", "ParticleAndMorphOnly", "MorphOnly", "ToneOnly", "VNotVOnly")

# add coding based on the parameters
for (row in 1:nrow(grambank_gw)) {
  if (('0' %in% grambank_gw[row,2])&('0' %in% grambank_gw[row,3])&('0' %in% grambank_gw[row,4])&('0' %in% grambank_gw[row,5])&('0' %in% grambank_gw[row,6])&('0' %in% grambank_gw[row,7])&('0' %in% grambank_gw[row,8])&('0' %in% grambank_gw[row,9])&('0' %in% grambank_gw[row,10])) {
    grambank_gw[row, 11] <- 'NoQuestionMarking'
  }
  else if (!('1' %in% grambank_gw[row,2])&!('1' %in% grambank_gw[row,3])&!('1' %in% grambank_gw[row,4])&!('1' %in% grambank_gw[row,5])&!('1' %in% grambank_gw[row,6])&!('1' %in% grambank_gw[row,7])&!('1' %in% grambank_gw[row,8])&!('1' %in% grambank_gw[row,9])&!('1' %in% grambank_gw[row,10])) {
    grambank_gw[row, 11] <- 'UnknownQuestionMarking'
  }
  else if (('1' %in% grambank_gw[row,2])&!('1' %in% grambank_gw[row,3])&!('1' %in% grambank_gw[row,4])&!('1' %in% grambank_gw[row,5])&!('1' %in% grambank_gw[row,6])&!('1' %in% grambank_gw[row,7])&!('1' %in% grambank_gw[row,8])&!('1' %in% grambank_gw[row,9])&!('1' %in% grambank_gw[row,10])) {
    grambank_gw[row, 11] <- 'IntonationOnly'
  }
  else if (('1' %in% grambank_gw[row,3])&!('1' %in% grambank_gw[row,2])&!('1' %in% grambank_gw[row,4])&!('1' %in% grambank_gw[row,5])&!('1' %in% grambank_gw[row,6])&!('1' %in% grambank_gw[row,7])&!('1' %in% grambank_gw[row,8])&!('1' %in% grambank_gw[row,9])&!('1' %in% grambank_gw[row,10])) {
    grambank_gw[row, 11] <- 'WordOrderOnly'
  }
  else if ((('1' %in% grambank_gw[row,4])||('1' %in% grambank_gw[row,5])||('1' %in% grambank_gw[row,6]))&!('1' %in% grambank_gw[row,2])&!('1' %in% grambank_gw[row,3])&!('1' %in% grambank_gw[row,7])&!('1' %in% grambank_gw[row,8])&!('1' %in% grambank_gw[row,9])&!('1' %in% grambank_gw[row,10])) {
    grambank_gw[row, 11] <- 'ParticleOnly'
  }
  else if (('1' %in% grambank_gw[row,7])&!('1' %in% grambank_gw[row,2])&!('1' %in% grambank_gw[row,3])&!('1' %in% grambank_gw[row,4])&!('1' %in% grambank_gw[row,5])&!('1' %in% grambank_gw[row,6])&!('1' %in% grambank_gw[row,8])&!('1' %in% grambank_gw[row,9])&!('1' %in% grambank_gw[row,10])) {
    grambank_gw[row, 11] <- 'ParticleAndMorphOnly'
  }
  else if (('1' %in% grambank_gw[row,8])&!('1' %in% grambank_gw[row,2])&!('1' %in% grambank_gw[row,3])&!('1' %in% grambank_gw[row,4])&!('1' %in% grambank_gw[row,5])&!('1' %in% grambank_gw[row,6])&!('1' %in% grambank_gw[row,7])&!('1' %in% grambank_gw[row,9])&!('1' %in% grambank_gw[row,10])) {
    grambank_gw[row, 11] <- 'MorphOnly'
  }
  else if (('1' %in% grambank_gw[row,9])&!('1' %in% grambank_gw[row,2])&!('1' %in% grambank_gw[row,3])&!('1' %in% grambank_gw[row,4])&!('1' %in% grambank_gw[row,5])&!('1' %in% grambank_gw[row,6])&!('1' %in% grambank_gw[row,7])&!('1' %in% grambank_gw[row,8])&!('1' %in% grambank_gw[row,10])) {
    grambank_gw[row, 11] <- 'ToneOnly'
  }
  else if (('1' %in% grambank_gw[row,10])&!('1' %in% grambank_gw[row,2])&!('1' %in% grambank_gw[row,3])&!('1' %in% grambank_gw[row,4])&!('1' %in% grambank_gw[row,5])&!('1' %in% grambank_gw[row,6])&!('1' %in% grambank_gw[row,7])&!('1' %in% grambank_gw[row,8])&!('1' %in% grambank_gw[row,9])) {
    grambank_gw[row, 11] <- 'VNotVOnly'
  }
}

# new column for whether there is one (or more) particle or not
grambank_gw['Particles']='No particle'

# define two factor levels for the new column
levels(grambank_gw$Particles) <- c("Particle", "No particle")

# code the two levels for the new column
for (row in 1:nrow(grambank_gw)) {
  if (('1' %in% grambank_gw[row,4])||('1' %in% grambank_gw[row,5])||('1' %in% grambank_gw[row,6]||'1' %in% grambank_gw[row,7])) {
    grambank_gw[row, 12] <- 'Particle'}}

# new column for the position of particles
grambank_gw['Position']='No particle'

# define factor levels for new column
levels(grambank_gw$Position) <- c("Initial", "Final", "Other", "Multiple", "No particle")

# code the levels for the new position column
for (row in 1:nrow(grambank_gw)) {
  if (('1' %in% grambank_gw[row,4])&!('1' %in% grambank_gw[row,5])&!('1' %in% grambank_gw[row,6])) {
    grambank_gw[row, 13] <- 'Initial'
  }
  else if (!('1' %in% grambank_gw[row,4])&('1' %in% grambank_gw[row,5])&!('1' %in% grambank_gw[row,6])) {
    grambank_gw[row, 13] <- 'Final'
  }
  else if (!('1' %in% grambank_gw[row,4])&!('1' %in% grambank_gw[row,5])&('1' %in% grambank_gw[row,6])) {
    grambank_gw[row, 13] <- 'Other'
  }
  else if (('1' %in% grambank_gw[row,4])||('1' %in% grambank_gw[row,5])||('1' %in% grambank_gw[row,6])) {
    grambank_gw[row, 13] <- 'Multiple'
  }
}

# read in latitude and longitude data from Grambank
languages <- read.csv("languages.csv")

# add latitude and longitude columns to new data frame
grambank_gw['Latitude']=0
grambank_gw['Longitude']=0

# populate the Latitude and Longitude columns
for (row in 1:nrow(grambank_gw)) {
  lgname <- as.character(grambank_gw$Language[row])
  lgno <- which(languages$ID == lgname)
  grambank_gw$Latitude[row] <- languages$Latitude[lgno]
  grambank_gw$Longitude[row] <- languages$Longitude[lgno]
}

# time to make some world maps
library(tidyverse)
cbbPalette <- c('#CC6677', '#332288', '#DDCC77', '#117733', '#88CCEE', '#882255', '#44AA99', '#999933', '#AA4499', '#DDDDDD')
world_coordinates <- map_data("world")
basemap <- ggplot() + theme_classic() + geom_map( 
    data = world_coordinates, map = world_coordinates, 
    aes(long, lat, map_id = region), 
    color = "white", fill= "#999999"
  )

# get rid of unknown and unmarked languages
figure1data <- grambank_gw %>% filter(Marking != 'UnknownQuestionMarking')
figure1data <- figure1data %>% filter(Marking != 'NoQuestionMarking')

# replace labels with nicer ones
library(plyr)
library(dplyr)
recode(figure1data$Marking, IntonationOnly = "Intonation only (398)", MorphOnly = "Morphology only (170)", MultipleStrategies = "Multiple strategies (610)", ParticleOnly = "Particle only (501)", ParticleAndMorphOnly = "Particle and morphology (1)", ToneOnly = "Tone only (32)", VNotVOnly = "V-not-V only (9)", WordOrderOnly = "Word order only (12)") -> figure1data$Marking

# figure 1 plots all question strategies
figure1 <- basemap + theme(axis.line=element_blank(),axis.text.x=element_blank(),
           axis.text.y=element_blank(),axis.ticks=element_blank(),
           axis.title.x=element_blank(),
           axis.title.y=element_blank()) + geom_point( 
     data = figure1data, 
     aes(Longitude, Latitude, color = Marking, 
         shape=Particles), 
     alpha = 1
   ) + scale_colour_manual(values=cbbPalette)

# save figure 1 to file
ggsave(plot=figure1,"figure1.pdf")

# get rid of languages without particles for figure 2
figure2data <- figure1data %>% filter(Position != 'No particle')

# recode position to include numbers
recode(figure2data$Position, Initial = 'Initial (181)', Final = 'Final (583)', Other = 'Other (128)', Multiple = 'Multiple (129)') -> figure2data$Position

# figure 2 plots the position of particles in languages that have them
figure2 <- basemap + theme(axis.line=element_blank(),axis.text.x=element_blank(),
           axis.text.y=element_blank(),axis.ticks=element_blank(),
           axis.title.x=element_blank(),
           axis.title.y=element_blank()) + geom_point( 
     data = figure2data, 
     aes(Longitude, Latitude, color = Position), 
     alpha = 1
   ) + scale_colour_manual(values=cbbPalette)

# save figure 2 to file
ggsave(plot=figure2,"figure2.pdf")

# output the csv
write.csv(grambank_gw,"grambank_gw.csv")

# get dataset for figure 4, based on WLW to appear
wlw <- read.csv("wlw-diachrony.csv")

#recode WLW category to include numbers
recode(wlw$Category, 'Content interrogative' = 'Content interrogative (11)', 'Disjunction' = 'Disjunction (32)', 'Focus particle' = 'Focus particle (2)', 'Modality' = 'Modality (1)', 'Multiple sources' = 'Multiple (7)', 'Subordinator' = 'Subordinator (11)') -> wlw$Category

# figure 4 plots the distribution of diachronic sources
figure4 <- basemap + theme(axis.line=element_blank(),axis.text.x=element_blank(),
           axis.text.y=element_blank(),axis.ticks=element_blank(),
           axis.title.x=element_blank(),
           axis.title.y=element_blank()) + geom_point( 
     data = wlw, 
     aes(Longitude, Latitude, color = Category), 
     alpha = 1
   ) + scale_colour_manual(values=cbbPalette)

# save figure 4 to file
ggsave(plot=figure4,"figure4.pdf")