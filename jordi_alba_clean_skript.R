### Filter Player 
install.packages("patchwork")
install.packages("ggnewscale")
install.packages("rjson")

library(rjson)
library(data.table)

competitions <- fromJSON(file="xxx.json")

competitions.df <- data.frame(do.call(rbind,competitions),stringsAsFactors = FALSE)


match.files <- list.files(path="xxxx",
                          full.names = TRUE,recursive = TRUE)


matches.list <- list()
for(i in 1:(length(match.files))){
  match.temp <- fromJSON(file=match.files[i]) ##Loop through each file which contains all the matches for a given competition and season and obtain the necessary match information
  
  matches <- lapply(match.temp, function(x) data.frame(t(unlist(x)),stringsAsFactors = FALSE))
  matches.df <- rbindlist(matches,fill=TRUE) #we use rbindlist instead of do.call(rbind,) because of column mismatch
  matches.list[[i]] <- matches.df #this assigns matches.df to the matches.list list that we initialized 
  
}


all.matches.df <- data.frame(rbindlist(matches.list,fill=TRUE))


###we are going to remove a lot of columns to just make our dataset clean
columns.to.keep <- names(which(unlist(lapply(all.matches.df,function(x) length(which(is.na(x)))))==0))

all.matches.clean <- all.matches.df[,columns.to.keep] #this selects the columns by column name 
all.matches.clean$match_week <- as.numeric(all.matches.clean$match_week) #convert some variables to numeric
all.matches.clean$home_score <- as.numeric(all.matches.clean$home_score)
all.matches.clean$away_score <- as.numeric(all.matches.clean$away_score)


#unique(unlist(lapply(event.temp,function(x) x$type$name)))

##### Data-Set 1 (Jordi Alba - Left Back)

####Obtain Events JOrdi####
event.files <- list.files(path="xxxx",
                          full.names = TRUE,recursive = TRUE)

event.list <- list()
for(i in 1:length(event.files)){
  event.temp <- fromJSON(file=event.files[i])
  
  
  jordi_raw <- (lapply(event.temp,function(x) x$player$name))
  jordi_raw[sapply(jordi_raw, is.null)] <- NA
  jordi.index <- which(unlist(lapply(jordi_raw,function(x) x))=="Jordi Alba Ramos")
  
  jordi.df <- data.frame(matrix(NA,nrow=1,ncol=9))
  colnames(jordi.df) <- c("Possession","Do","Name","Team","Style","Position","Spieler","Lokation","Lokation 2")
  
  for(p in seq_along(jordi.index)){
    jordi.temp <- event.temp[[jordi.index[p]]]
    possession <- jordi.temp$possession
    Do <- jordi.temp$type$name
    Name <- jordi.temp$team$name
    Team <- jordi.temp$possession_team$name
    Style <- jordi.temp$play_pattern$name
    Position <- jordi.temp$position$name
    Spieler <- jordi.temp$player$name
    Lokation <- jordi.temp$location
    
    row.toadd <- c(possession,Do,Name,Team,Style, Position, Spieler,Lokation)
    jordi.df <- rbind(jordi.df,row.toadd)
  }
  
#  jordi.df$Lokation <- as.numeric(jordi.df$Lokation)
 # jordi.df$`Lokation 2`<- as.numeric(jordi.df$`Lokation 2`)
#  jordi.alba.df2 <- na.omit(jordi.df)
  
  jordi.list <- list(jordi.df)
  
  match.id <- strsplit(basename(event.files[i]),"[.]")[[1]][1]
  
  event.list[[match.id]] <- (jordi.df)
  
}



jordievents.df <- data.frame(do.call(rbind,event.list),stringsAsFactors = TRUE)
jordievents.df <- setDT(jordievents.df, keep.rownames = TRUE)
jordievents.df$match_id <- gsub("\\..*", "",jordievents.df$rn)
jordievents.df$Lokation <- as.numeric(jordievents.df$Lokation)
jordievents.df$Lokation.2<- as.numeric(jordievents.df$Lokation.2)
jordievents.df <- na.omit(jordievents.df)


all = merge(jordievents.df, all.matches.clean)


df.jordievents <- all %>%
  filter(Name == "Barcelona"  ) %>%
  filter(competition.competition_name == "La Liga" ) %>%
  filter (Position == "Left Back")

### Plot

###plot - density

palette <- paletteer::paletteer_d("RColorBrewer::YlOrRd", direction = 1)

pitch <- ggplot() +
          annotate_pitch(dimensions = pitch_statsbomb, fill = "gray95") +
            theme_pitch()

p1.jordi <-   pitch +
    geom_density_2d_filled(data = df.jordievents, aes(x = Lokation, y = Lokation.2 , fill = ..level..,), alpha = .8, 
                         contour_var = "ndensity", breaks = seq(0.1, 1.0, length.out = 10)) +
    scale_x_continuous(limits = c(0, 120)) +
      scale_y_continuous(limits = c(0, 80)) +
        scale_fill_manual(values = c(palette), aesthetics = c("fill")) + 
           scale_y_reverse() +
            theme(legend.position="none") +
              theme(plot.title =element_text(color = "black")) +
                labs(title="Event Heat Map of Jordi Alba",
                      subtitle = "Events include Offensive and Defensive Actions caught on Camera",
                        caption = "Data source: StatsBombs")
  
 
p2.jordi <- pitch +
      geom_point(data = df.jordievents, aes(x = Lokation, y = Lokation.2), alpha = 0.4)+
         scale_x_continuous(limits = c(0, 120)) +
           scale_y_continuous(limits = c(0, 80)) +
             scale_fill_manual(values = c(palette), aesthetics = c("fill")) + 
               scale_y_reverse() +
                 theme(legend.position="none") +
                   theme(plot.title =element_text(color = "black")) +
                     labs(title="Event Heat Map of Jordi Alba clustered by Action",
                        subtitle = "Events include Offensive and Defensive Actions caught on Camera",
                          caption = "Data source: StatsBombs") +
                            facet_wrap(~Do)


####Obtain Events Right Back####
event.list <- list()
for(i in 1:length(event.files)){
  event.temp <- fromJSON(file=event.files[i])
  
  
  rightraw <- (lapply(event.temp,function(x) x$position$name))
  rightraw[sapply(rightraw, is.null)] <- NA
  right.index.2 <- which(unlist(lapply(rightraw,function(x) x))=="Right Back")
  
  right.df <- data.frame(matrix(NA,nrow=1,ncol=9))
  colnames(right.df) <- c("Possession","Do","Name","Team","Style","Position","Spieler","Lokation","Lokation 2")
  
  for(p in seq_along(right.index.2)){
    right.temp <- event.temp[[right.index.2[p]]]
    possession <- right.temp$possession
    Do <- right.temp$type$name
    Name <- right.temp$team$name
    Team <- right.temp$possession_team$name
    Style <- right.temp$play_pattern$name
    Position <- right.temp$position$name
    Spieler <- right.temp$player$name
    Lokation <- right.temp$location
    
    row.toadd <- c(possession,Do,Name,Team,Style, Position, Spieler,Lokation)
    right.df <- rbind(right.df,row.toadd)
  }
 
  right.list <- list(right.df)
  
  match.id <- strsplit(basename(event.files[i]),"[.]")[[1]][1]
  
  event.list[[match.id]] <- right.df
  
}


rightbackevents.df <- data.frame(do.call(rbind,event.list),stringsAsFactors = TRUE)
rightbackevents.df <- setDT(rightbackevents.df, keep.rownames = TRUE)
rightbackevents.df$match_id <- gsub("\\..*", "",rightbackevents.df$rn)
rightbackevents.df$Lokation <- as.numeric(rightbackevents.df$Lokation)
rightbackevents.df$Lokation.2<- as.numeric(rightbackevents.df$Lokation.2)
rightbackevents.df <- na.omit(rightbackevents.df)


unique(df.jordievents$match_id)

df.rightbackevents <- rightbackevents.df %>%
  filter(Name == "Barcelona") %>%
  filter (match_id %in%  c(unique(df.jordievents$match_id)))


palette2 <- paletteer::paletteer_d("Viridis::Virdis", direction = 1)

 
 p3.rightback <- pitch +
  geom_density_2d_filled(data = rightbackevents.df, aes(x = Lokation, y = Lokation.2) , alpha = .8, 
                         contour_var = "ndensity", breaks = seq(0.1, 1.0, length.out = 10)) +
   scale_x_continuous(limits = c(0, 120)) +
     scale_y_continuous(limits = c(0, 80)) +
       scale_y_reverse() +
        theme(legend.position="none") +
          theme(plot.title =element_text(color = "black")) +
            labs(title="Event Heat Map of Barcelona's Right Back",
              subtitle = "Events include Offensive and Defensive Actions caught on Camera",
                caption = "Data source: StatsBombs")

p.comparison <- pitch +
   geom_density_2d_filled(data = df.jordievents, aes(x = Lokation, y = Lokation.2 , fill = ..level..,), alpha = .8, 
                          contour_var = "ndensity", breaks = seq(0.1, 1.0, length.out = 10)) +
   scale_x_continuous(limits = c(0, 120)) +
   scale_y_continuous(limits = c(0, 80)) +
   scale_fill_manual(values = c(palette), aesthetics = c("fill")) + 
   scale_y_reverse () +
   new_scale_fill() +
   geom_density_2d_filled(data = df.rightbackevents, aes(x = Lokation, y = Lokation.2 , fill = ..level..,), alpha = .8, 
                          contour_var = "ndensity", breaks = seq(0.1, 1.0, length.out = 10)) +
   scale_fill_viridis_d(option ="D", aesthetics = c("fill")) +
   theme(legend.position="none") +
   theme(plot.title =element_text(color = "black")) +
   labs(title="Event Heat Map of Jordi Alba",
        subtitle = "Events include Offensive and Defensive Actions caught on Camera",
        caption = "Data source: StatsBombs")
 

########## Passes

event.files <- list.files(path="xxxx",
                          full.names = TRUE,recursive = TRUE)
for(i in 1:length(event.files)){
  event.temp <- fromJSON(file=event.files[i])
  
  pass.index <- which(unlist(lapply(event.temp,function(x) x$type$name))=="Pass")
  
  #obtain the passes just for team1 (the first element in teamids)
  # pass.team1 <- pass.index[which(unlist(lapply(pass.index,function(x) event.temp[[x]]$team$name))==teamids[1])]
  
  pass.team1.df <- data.frame(matrix(NA,nrow=1,ncol=14))
  colnames(pass.team1.df) <- c("Possession","Passer","X.Pass","Y.Pass",
                               "Pass.Type","Receiver","X.Receive","Y.Receive",
                               "Pass.Length","Pass.Angle","Body.Part","Team","Player","Positons")
  
  for(p in 1:length(pass.index)){
    pass.temp <- event.temp[[pass.index[p]]]
    possession <- pass.temp$possession
    passer <- pass.temp$player$name
    pass.location <- pass.temp$location
    pass.type <- pass.temp$pass$height$name
    receiver <- pass.temp$pass$recipient$name
    receive.location <- pass.temp$pass$end_location
    pass.length <- pass.temp$pass$length
    pass.angle <- pass.temp$pass$angle
    body.part <- pass.temp$pass$body_part$name
    Team <- pass.temp$possession_team$name
    Player <- pass.temp$player$name
    Positions <- pass.temp$position$name
    
    
    row.toadd <- c(possession,passer,pass.location,pass.type,receiver,receive.location,pass.length,pass.angle,body.part, Team, Player, Positions)
    pass.team1.df <- rbind(pass.team1.df,row.toadd)
  }
  
  match.id <- strsplit(basename(event.files[i]),"[.]")[[1]][1]
  
  event.list[[match.id]] <- (pass.team1.df)
  
}

passmatches.df <- data.frame(do.call(rbind,event.list),stringsAsFactors = TRUE)
passmatches.df <- setDT(passmatches.df, keep.rownames = TRUE)
passmatches.df$match_id <- gsub("\\..*", "",passmatches.df$rn)

df.pass <- passmatches.df %>%
  filter(Team == "Barcelona") %>%
  filter (Player ==  "Jordi Alba Ramos") %>%
filter (match_id %in%  c(unique(df.jordievents$match_id)))
df.pass$X.Pass <- as.numeric(df.pass$X.Pass)
df.pass$Y.Pass <- as.numeric(df.pass$Y.Pass) 
df.pass$X.Receive <- as.numeric(df.pass$X.Receive)
df.pass$Y.Receive <- as.numeric(df.pass$Y.Receive)


### forwardpasses

df.forward.pass <- df.pass %>%
  filter (X.Pass < X.Receive)

pitch +
geom_density_2d_filled(data = df.forward.pass, aes(x = X.Receive, y = Y.Receive , fill = ..level..,), alpha = .8, 
                       contour_var = "ndensity", breaks = seq(0.1, 1.0, length.out = 10)) +
  geom_point(data= df.forward.pass, aes(x = X.Receive, y = Y.Receive)) +
    scale_x_continuous(limits = c(0, 120)) +
      scale_y_continuous(limits = c(0, 80)) +
        scale_y_reverse() +
          scale_fill_manual(values = c(palette), aesthetics = c("fill")) + 
            theme(legend.position="none") +
              theme(plot.title =element_text(color = "black")) +
                labs(title="Event Heat Map of Jordi Alba",
                  subtitle = "Events include Offensive and Defensive Actions caught on Camera",
                    caption = "Data source: StatsBombs")


#allpasses
pitch  + geom_segment(data=df.pass, aes(x=X.Pass,xend=X.Receive,
                                       y=Y.Pass,yend=Y.Receive),size=1.5,arrow=arrow(length = unit(0.03, "npc"))) + 
  scale_y_reverse() +
  facet_wrap(~ Receiver)


