library(ggplot2)
library(reshape2)                   # used to transform data from wide to long format
library(RColorBrewer)               # Provides color schemes for maps

matches = read.csv(file.choose(), stringsAsFactors = TRUE)         #choose the matches csv file
deliveries = read.csv(file.choose(), stringsAsFactors = TRUE)      #choose the deliveries csv file

df = merge(matches, deliveries, by.x = "id", by.y = "match_id") 

#merging data frames with common columns
df$season = as.factor(df$season)

vk = subset(df, df$batsman == "V Kohli")
msd = subset(df, df$batsman == "MS Dhoni")

#############################################################################################

##Plotting Kohli Vs Dhoni runs by season

vk_season = aggregate(batsman_runs ~ season, data = vk, FUN = sum)
colnames(vk_season) = c("season", "runs_kohli")

msd_season = aggregate(batsman_runs ~ season, data = msd, FUN = sum)
colnames(msd_season) = c("season", "runs_dhoni")

vk_msd_season = merge(vk_season, msd_season)
vk_msd_season_long = melt(vk_msd_season) #Transforms the data frame to one, with dhoni/kohli as factors

ggplot(vk_msd_season_long, aes(x = season, y = value, fill = variable)) + 
  geom_bar(stat="identity", position = "dodge") + #dodge -- place bars side-to-side
  scale_fill_manual(values = c("green","blue")) + #scale_fill_manual for barplots
  ggtitle("Kohli vs Dhoni -- Runs by Seasons") +
  labs(x = "Season", y = "Runs")

############################################################################################

##Dismissal counts by seasons -- Kohli vs Dhoni 

vk_dismissal = subset(vk, vk$player_dismissed == "V Kohli")[,c("season", "dismissal_kind")]
vk_dismissal_long = melt(vk_dismissal)
vk_dismissal_count = as.data.frame(table(vk_dismissal$season))
colnames(vk_dismissal_count) = c("season", "vk_dismissal")


msd_dismissal = subset(msd, msd$player_dismissed == "MS Dhoni")[,c("season", "dismissal_kind")]
msd_dismissal_long = melt(msd_dismissal)
msd_dismissal_count = as.data.frame(table(msd_dismissal$season))
colnames(msd_dismissal_count) = c("season", "msd_dismissal")

vk_msd_dismissal = merge(vk_dismissal_count, msd_dismissal_count)
vk_msd_dismissal_long = melt(vk_msd_dismissal)

ggplot(vk_msd_dismissal_long, aes(x = season, y = value, fill = variable)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values = c("green","purple")) + 
  ggtitle("Kohli vs Dhoni -- Dismissals by Seasons") +
  labs(x = "Season", y = "Dismissals")

############################################################################################

##Share of runs made -- MS Dhoni

msd_share = as.data.frame(table(msd$total_runs))

ggplot(msd_share, aes(x = factor(1), y = msd_share$Freq, fill = factor(msd_share$Var1))) + 
  geom_bar(stat = "identity")+
  coord_polar(theta = "y")+
  ggtitle("Dhoni's Run Share")+
  labs(x = "",y = "")+
  scale_fill_discrete(guide_legend(title = "Run Color"))

###########################################################################################

#Share of runs made -- Virat Kohli
vk_share = as.data.frame(table(vk$total_runs))

ggplot(vk_share, aes(x = factor(1), y = vk_share$Freq, fill = factor(vk_share$Var1))) + 
  geom_bar(stat = "identity")+
  coord_polar(theta = "y")+
  ggtitle("Kohli's Run Share")+
  labs(x="", y="")+
  scale_fill_discrete(guide_legend(title = "Run Color"))

###########################################################################################
#Pacing the innings -- Kohli Vs Dhoni
vk_over_runs = aggregate(batsman_runs ~ over, data = vk, FUN = sum)

vk_overs_faced = as.data.frame(table(vk$over))
colnames(vk_overs_faced) = c("over", "vk_freq")

vk_over_runs = merge(vk_over_runs, vk_overs_faced)

vk_over_runs$vk_strike_rate = (vk_over_runs$batsman_runs/vk_over_runs$vk_freq)*100

msd_over_runs = aggregate(batsman_runs ~ over, data = msd, FUN = sum)

msd_overs_faced = as.data.frame(table(msd$over))
colnames(msd_overs_faced) = c("over", "msd_freq")

msd_over_runs = merge(msd_over_runs, msd_overs_faced)

msd_over_runs$msd_strike_rate = (msd_over_runs$batsman_runs/msd_over_runs$msd_freq)*100

vk_msd_strike_rate = merge(vk_over_runs[,c("over", "vk_strike_rate")], msd_over_runs[,c("over", "msd_strike_rate")])

vk_msd_strike_rate$over = as.factor(vk_msd_strike_rate$over)
vk_msd_strike_rate_long = melt(vk_msd_strike_rate)

ggplot(vk_msd_strike_rate_long, aes(over, value, group = variable, col = variable)) + 
  geom_point() + geom_smooth(se=F)+
  ggtitle("Kohli vs Dhoni -- Strike Rate by Over") +
  labs(x = "Over", y = "Strike Rate")

