# Remove all of the elements currently loaded in R
rm(list=ls(all=TRUE))
# add work directory
setwd('/Users/xianqing/Desktop/Fwd_ Data of Agency Task')
# print csv per task
print_csv <- 0


####### agent paradox ####
ap <- read.csv('data_2000.csv')
# eliminate SCdO participant (my test)
ap <- ap[ap$Participant.Private.ID != "1841491",] 
ap <- ap[ap$Participant.Private.ID != "1854814",]
ap <- ap[ap$Participant.Private.ID != "1796065",] 
ap = ap[ap$Response != "A loading delay of more than 10s was detected.",]
# relevant columns
rel_cols <- c("Experiment.Version","Task.Version","Spreadsheet.Name","Participant.Private.ID","Trial.Number",
              "Zone.Name","Zone.Type","Response","Correct","bin_dur","out_dur","simResp","A_png","noA_png","condition")
# filter by relevant columns
ap <- ap[ap$Response != "", rel_cols]

# create condition and bin size column
ap$condition2 <- paste0(ap$condition,"_",ap$bin_dur)
rel_cols <- c("Experiment.Version","Task.Version","Spreadsheet.Name","Participant.Private.ID","Trial.Number",
              "Zone.Name","Zone.Type","Response","Correct","bin_dur","out_dur","simResp","A_png","noA_png","condition","condition2")
ap <- ap[ap$condition2 != "NA_NA", rel_cols]

 
# create participant and condition vector
participants <- unique(ap$Participant.Private.ID)[!is.na(unique(ap$Participant.Private.ID))]
conditions <- unique(ap$condition2)[unique(ap$condition2) != "" & unique(ap$condition2) != "_NA"]
binLength <- unique(ap$bin_dur)[!is.na(unique(ap$bin_dur))]


# clean and pool by participant
for (s in 1:length(participants)) {
  # get one subject
  oneSubj <- ap[ap$Participant.Private.ID == participants[s] & !is.na(ap$Participant.Private.ID),]
  # write.csv(oneSubj,paste0(getwd(),"/duoyihang.csv"),row.names = F)
  # condition order
  cond_order <- unique(oneSubj$condition2)[unique(oneSubj$condition2) != "" & unique(ap$condition2) != "_NA"]
  
  # only ratings
  oneSubjRat <- oneSubj[oneSubj$Response != "keypress" & oneSubj$Response != "A loading delay of more than 10s was detected." & 
                          oneSubj$Response != "A loading delay of more than 10s was detected." & 
                          oneSubj$Zone.Type == "response_slider_endValue",]
  oneSubjRat$condition2 <- rep(cond_order,each = 3)
  rep
  # only behaviour
  oneSubjBeh <- oneSubj[oneSubj$Response == "keypress",]
  oneSubjBeh$condition2 <- rep(cond_order,each = 40)
  oneSubjBeh$Trial.Number <- rep(1:40,length(cond_order))
  
  if (s == 1) {
    AP_rat <- oneSubjRat
    AP_beh <- oneSubjBeh
  } else {
    AP_rat <- rbind(AP_rat,oneSubjRat)
    AP_beh <- rbind(AP_beh,oneSubjBeh)
  }
}; rm(oneSubj,oneSubjRat,oneSubjBeh)


# ratings
AP_rat$Zone.Type <- NULL
AP_rat$Zone.Name <- as.factor(as.character(AP_rat$Zone.Name))
levels(AP_rat$Zone.Name) <- c("hum","sim","oth")
# change variable type relevant columns
AP_rat$Response <- as.numeric(as.character(AP_rat$Response))

AP_rat$condition <- substr(AP_rat$condition2,1,2)
AP_rat$bin_dur <- substr(AP_rat$condition2,4,10)


#### visualize agent paradox ####
library(ggplot2)
ggplot(AP_rat,aes(x=condition,y=Response,col=Zone.Name)) + 
  geom_hline(yintercept = 0, col = "gray50", linetype = "dashed") + 
  stat_summary(fun.data = mean_cl_boot,position = position_dodge(0.2)) + 
  facet_grid(bin_dur ~ .) + 
  theme_classic()


# behaviour
# change variable type relevant columns
AP_beh$Participant.Private.ID <- as.factor(AP_beh$Participant.Private.ID)
AP_beh$simResp <- as.integer(substr(AP_beh$simResp,1,1))
AP_beh$bulbOn <- as.character(ifelse(AP_beh$Correct == 1, as.character(AP_beh$A_png), as.character(AP_beh$noA_png)))
AP_beh$bulbOn <- substr(AP_beh$bulbOn,1,1)
#AP_beh$condOrder <- rep(1:40,nrow(AP_beh)/40)
AP_beh$eventType <- paste0("h",AP_beh$Correct,"s",AP_beh$simResp,"-",AP_beh$bulbOn)

#### agent paradox combine ratings and behaviour ####
AP_beh$nRespBin <- AP_beh$ratHum <- AP_beh$ratSim <- AP_beh$ratOth <- NA
for (i in 1:length(participants)) {
  for (j in 1:length(conditions)) {
    whereBehVec <- AP_beh$Participant.Private.ID == participants[i] & AP_beh$condition == conditions[j]
    whereRatVec <- AP_rat$Participant.Private.ID == participants[i] & AP_rat$condition == conditions[j]
    AP_beh[whereBehVec,"ratHum"] <- AP_rat[whereRatVec & AP_rat$Zone.Name == "hum","Response"]
    AP_beh[whereBehVec,"ratSim"] <- AP_rat[whereRatVec & AP_rat$Zone.Name == "sim","Response"]
    AP_beh[whereBehVec,"ratOth"] <- AP_rat[whereRatVec & AP_rat$Zone.Name == "oth","Response"]
    
  } # end j condition cycle
} # end i participant cycle

ap <- AP_beh[,c("Participant.Private.ID","condition","Trial.Number","nRespBin","Correct","simResp","bulbOn","eventType","ratHum","ratSim","ratOth")]
colnames(ap) <- c("subj","cond","nBin","nRespBin","humResp","simResp","bulbOn","eventType","ratHum","ratSim","ratOth")

AP_beh$condition3 <- paste0(substr(AP_beh$condition2,1,2))
AP_beh$humResp <- paste0(substr(ap$humResp,1,1))

ggplot(ap,aes(x=nBin,humResp))+ stat_summary() + facet_grid(.~cond)
ggplot(AP_beh,aes(x=Trial.Number,y=Participant.Private.ID,fill=Correct)) + 
  geom_tile() + 
  facet_grid(bin_dur~condition3) + 
  theme_classic()
ggplot(AP_beh,aes(x=Trial.Number,y=as.integer(bulbOn))) + 
  stat_summary(geom = 'line') + 
  facet_grid(bin_dur~condition3) + 
  labs(x = 'Trial Number', y = 'p(egg open)') +
  theme_classic()
ggplot(AP_beh,aes(x=Trial.Number,y=as.integer(humResp))) + 
  stat_summary(geom = 'line') + 
  facet_grid(bin_dur~condition3) + 
  labs(x = 'Trial Number', y = 'p(human response)') +
  theme_classic()


#### print agent paradox ####
if (print_csv == 1) {
  write.csv(ap,paste0(getwd(),"/lf_ap_2000.csv"),row.names = F)
  write.csv(AP_rat,paste0(getwd(),"/lf_ap_onlyRatings_2000.csv"),row.names = F)
  write.csv(AP_beh,paste0(getwd(),"/lf_ap_onlybehavior_2000.csv"),row.names = F)
}

