# Remove all of the elements currently loaded in R
rm(list=ls(all=TRUE))
# add work directory
setwd('/Users/xianqing/Desktop/Fwd_ Data of Agency Task')

data.mean = read.csv('AP_ratMean.csv')
library(ggplot2)
library(ggthemes)
library(dplyr)
library(data.table)

ggplot(data.mean, aes(x = condition))+
  ggtitle("Human Control")+
  geom_line(aes(y = hum_750), group = 1, size = 1.5, col = 'orange') +
  geom_line(aes(y = hum_1200), group = 1, size = 1.5, col = 'darkblue') +
  geom_point(aes(y = hum_750), size = 3, col = 'orange') +
  geom_point(aes(y = hum_1200), size = 3, col = 'darkblue') +
  ylab('mean rating')+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = 'left')

ggplot(data.mean, aes(x = condition))+
  ggtitle("Computer Control")+
  geom_line(aes(y = sim_750), group = 1, size = 1.5, col = 'orange') +
  geom_line(aes(y = sim_1200), group = 1, size = 1.5, col = 'darkblue') +
  geom_point(aes(y = sim_750), size = 3, col = 'orange') +
  geom_point(aes(y = sim_1200), size = 3, col = 'darkblue') +
  ylab('mean rating')+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = 'left')

ggplot(data.mean, aes(x = condition))+
  ggtitle("Other Control")+
  geom_line(aes(y = oth_750), group = 1, size = 1.5, col = 'orange') +
  geom_line(aes(y = oth_1200), group = 1, size = 1.5, col = 'darkblue') +
  geom_point(aes(y = oth_750), size = 3, col = 'orange') +
  geom_point(aes(y = oth_1200), size = 3, col = 'darkblue') +
  ylab('mean rating')+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = 'left')

rat_condition = read.csv('ap_Ratings_condition.csv')
aprat = tidyr::gather(rat_condition, "condition", "rating", 5:10)
aprat = aprat[, -2]
ap_control = tidyr::spread(aprat, controlType, rating)
write.csv(ap_control,paste0(getwd(),"/ap_control.csv"),row.names = F)

ratings = read.csv('lf_ap_onlyRatings_2000.csv')

# relevant columns
rel_cols <- c("Participant.Private.ID","Zone.Name","bin_dur", "Response","condition")
# filter by relevant columns
ratings <- ratings[ratings$Response != "", rel_cols]

# ANOVA for main effects
ap2000_condition = tidyr::spread(ratings, condition, Response)
ap2000_control = tidyr::spread(ratings, Zone.Name, Response)
ap2000_binlength = tidyr::spread(ratings, bin_dur, Response)

write.csv(ap2000_condition, paste0(getwd(), "/ap2000_condition.csv"), row.names = F)
write.csv(ap2000_control, paste0(getwd(), "/ap2000_control.csv"), row.names = F)
write.csv(ap2000_binlength, paste0(getwd(), "/ap2000_binlength.csv"), row.names = F)

# t test comparing condition_control
ratings$condition3 <- paste0(ratings$condition,"_",ratings$Zone.Name)
ratings = ratings[, c(-2, -5)]
ap2000_cond_contr = tidyr::spread(ratings, condition3, Response)
write.csv(ap2000_cond_contr, paste0(getwd(), "/ap2000_cond_contr.csv"), row.names = F)

# t test comparing bin_condition_control
ratings$condition4 <- paste0(ratings$condition3,"_",ratings$bin_dur)
ratings = ratings[, c(-2, -4)]
ap2000_bin_cond_contr = tidyr::spread(ratings, condition4, Response)
write.csv(ap2000_bin_cond_contr, paste0(getwd(), "/ap2000_bin_cond_contr.csv"), row.names = F)



# rat_condition = read.csv('ap_Ratings_condition.csv')
# aprat = tidyr::gather(rat_condition, "condition", "rating", 5:10)
# aprat = aprat[, -2]
# ap_control = tidyr::spread(aprat, controlType, rating)
# write.csv(ap_control,paste0(getwd(),"/ap_control.csv"),row.names = F)

