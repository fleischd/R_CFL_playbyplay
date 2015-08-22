require(rvest)

allplays <- data.frame(X1=character(),game=character(),year=character(),stringsAsFactors=FALSE,row.names=NULL)

year <- "2015"
gamenum <- "1"

while(as.numeric(year)<=2015) {

      #get the page
        
      page <- eval(parse(text=eval(parse( text = "sprintf(\"html(\\\"http://liveplay.cflcentral.com/LPFiles/%s_%s_cfllive_playbyplay.html\\\")\",gamenum,year)"))))
      
      
      #get the plays
      
      playlist <- data.frame(html_table(html_nodes(page,"table")[7]),row.names=NULL)
      
      #check for loop
      
      if(head(playlist,n=1)[1] == "1st Quarter") {
      
      #add game info
      
      playlist$game <- sprintf("%s-G%s",year,gamenum)
      
      playlist$year <- year
      
      #insert into allplays
      
      allplays <- rbind(allplays, playlist)
      
      #roll index
      
      gamenum = as.character(as.numeric(gamenum)+1)
      
      next }
      
      year = as.character(as.numeric(year)+1)
      gamenum = "1"
      }

#parse data

allplays$playnum <- substr(allplays$X1,1,regexpr("/",allplays$X1)-2)

for (i in 1:nrow(allplays)){
  
  if(regexpr("Quarter",allplays$X1[i]) > -1) { qname <- substr(allplays$X1[i],1,1)[1] }
  allplays$Quarter[i] <- qname
  
  if(i==1) Offense <- "NA"
  if(regexpr("HAMILTON  at",allplays$X1[i]) > -1) { Offense <- "HAMILTON" }
  else if(regexpr("MONTREAL  at",allplays$X1[i]) > -1) { Offense <- "MONTREAL" }
  else if(regexpr("OTTAWA  at",allplays$X1[i]) > -1) { Offense <- "OTTAWA" }
  else if(regexpr("TORONTO  at",allplays$X1[i]) > -1) { Offense <- "TORONTO" }
  else if(regexpr("B.C.  at",allplays$X1[i]) > -1) { Offense <- "B.C." }
  else if(regexpr("SASKATCHEWAN  at",allplays$X1[i]) > -1) { Offense <- "SASKATCHEWAN" }
  else if(regexpr("CALGARY  at",allplays$X1[i]) > -1) { Offense <- "CALGARY" }
  else if(regexpr("EDMONTON  at",allplays$X1[i]) > -1) { Offense <- "EDMONTON" }
  else if(regexpr("WINNIPEG  at",allplays$X1[i]) > -1) { Offense <- "WINNIPEG" }
  allplays$OffTeam[i] <- Offense
  
  
  timer <- substr(allplays$X1[i],as.numeric(regexpr("\\(",allplays$X1[i])[1])+1,as.numeric(regexpr("\\)",allplays$X1[i])[1])-1)
  allplays$time[i] <- timer
  
  if(as.numeric(regexpr("Pass",allplays$X1[i]))>1) {
    if(as.numeric(regexpr("Completed",allplays$X1[i]))>1) {
      allplays$playtype[i] <- "Pass"
      allplays$playresult[i] <- "Completed"
    } else {
      allplays$playtype[i] <- "Pass"
      if(as.numeric(regexpr("tercept",allplays$X1[i]))>1) {
        allplays$playresult[i] <- "Interception"
      }
      else {
        allplays$playresult[i] <- "Incomplete"
      }
    }
  }
  else if(as.numeric(regexpr("Run",allplays$X1[i]))>1){
    allplays$playtype[i] <- "Run"
    allplays$playresult[i] <- "Run"
  }
  else if(as.numeric(regexpr("Kickoff",allplays$X1[i]))>1){
    allplays$playtype[i] <- "Kickoff"
    allplays$playresult[i] <- "Kickoff"
  }
  else if(as.numeric(regexpr("Punt",allplays$X1[i]))>1){
    allplays$playtype[i] <- "Punt"
    allplays$playresult[i] <- "Punt"
  }
  else if(as.numeric(regexpr("Sack",allplays$X1[i]))>1){
    allplays$playtype[i] <- "Pass"
    allplays$playresult[i] <- "Sack"
  }
  else if(as.numeric(regexpr("Field Goal",allplays$X1[i]))>1){
    allplays$playtype[i] <- "Field Goal"
    if(as.numeric(regexpr("issed",allplays$X1[i]))>1){
      allplays$playresult[i] <- "Missed"
    }
    else{
      allplays$playresult[i] <- "Good"
    }
  }
  else if(as.numeric(regexpr("onvert",allplays$X1[i]))>1){
    allplays$playtype[i] <- "Convert"
    if(as.numeric(regexpr("ailed",allplays$X1[i]))>1){
      allplays$playresult[i] <- "Failed"
    }
    else {
      allplays$playresult[i] <- "Good" 
    }
  }
  else if(as.numeric(regexpr("knee",allplays$X1[i]))>1){
    allplays$playtype[i] <- "Concede"
    allplays$playresult[i] <- "kneel"
  }
  else if(as.numeric(regexpr("afety",allplays$X1[i]))>1){
    allplays$playtype[i] <- "Safety"
    allplays$playresult[i] <- "Safety"
  }
  else if(as.numeric(regexpr("Team Loss",allplays$X1[i]))>1){
    allplays$playtype[i] <- "Unspecified Loss"
    allplays$playresult[i] <- "Unspecified"
  }
  else if(as.numeric(regexpr("PENALTY",allplays$X1[i]))>1){
    allplays$playtype[i] <- "Pre-snap Penalty"
    allplays$playresult[i] <- "No Play"
  }
  
  next
}