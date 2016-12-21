# data analysis for:
# Acerbi, Tennie & Mesoudi
# “Social learning solves the problem of narrow-peaked search landscapes: 
# experimental evidence in humans”
#
# LOAD LIBRARIES ---------------------------------------------------------------
library(ggplot2)
library(reshape2)
library(plyr)
library(lme4)
library(MASS)
library(Rmisc)
library(pastecs)
library(ggsci)
library(merTools)

# IMPORT FILE -----------------------------------------------------

#import csv file - set working dir to wherever your file "data.csv" is located, or add the path to the command below
peak_data <- read.csv("data.csv")
names(peak_data) <- tolower(names(peak_data))
peak_data$learning <- tolower(peak_data$learning)
peak_data$peaks <- tolower(peak_data$peaks)

# NOTES ON DATA
# each row = one participant. The first few columns have demographics (sex, age) and condition (peaks = wide vs narrrow, learning = individual vs social)
# variable names are in the format season_hunt_measure. For example, s2_h4_score is the score on hunt 4 of season 2.
# season p = practice session, which only social learners did.
# hei = height (1-100); wid = width (1-100), thi = thickness (1-100), sha = shape (1-4), col = colour (1-4)
# kills = score on that hunt (/1000), shown = kills shown to the participant after the random error is added, score = cumulative/running total of all hunts up to that point (out of 30000)
# rank = rank from 1 (1st) to 6 (6th); copied = whether the player clicked to view someone else (0=no, 1=yes); model = the number of the other player that they copied (1-5)
# changeX = whether the participant changed the value of X (e.g. changeH = change height). 0=no, 1=yes, -1=not applicable (because it was the first hunt, where they were forced to enter values for everything)


# CREATE VARIABLES --------------------------------------------------------

# calculate total number of copies ----------------------------------------

count_function <- function(x) sum(x==1, na.rm=TRUE) 

peak_data$total_copies_s1 <- apply(peak_data[c("s1_h1_copied","s1_h2_copied","s1_h3_copied","s1_h4_copied","s1_h5_copied","s1_h6_copied","s1_h7_copied","s1_h8_copied","s1_h9_copied","s1_h10_copied","s1_h11_copied","s1_h12_copied","s1_h13_copied","s1_h14_copied","s1_h15_copied","s1_h16_copied","s1_h17_copied","s1_h18_copied","s1_h19_copied","s1_h20_copied","s1_h21_copied","s1_h22_copied","s1_h23_copied","s1_h24_copied","s1_h25_copied","s1_h26_copied","s1_h27_copied","s1_h28_copied","s1_h29_copied","s1_h30_copied")], 1, count_function) 
peak_data$total_copies_s1_prop <- peak_data$total_copies_s1 / 29

peak_data$total_copies_s2 <- apply(peak_data[c("s2_h1_copied","s2_h2_copied","s2_h3_copied","s2_h4_copied","s2_h5_copied","s2_h6_copied","s2_h7_copied","s2_h8_copied","s2_h9_copied","s2_h10_copied","s2_h11_copied","s2_h12_copied","s2_h13_copied","s2_h14_copied","s2_h15_copied","s2_h16_copied","s2_h17_copied","s2_h18_copied","s2_h19_copied","s2_h20_copied","s2_h21_copied","s2_h22_copied","s2_h23_copied","s2_h24_copied","s2_h25_copied","s2_h26_copied","s2_h27_copied","s2_h28_copied","s2_h29_copied","s2_h30_copied")], 1, count_function)
peak_data$total_copies_s2_prop <- peak_data$total_copies_s2 / 29

peak_data$total_copies_s3 <- apply(peak_data[c("s3_h1_copied","s3_h2_copied","s3_h3_copied","s3_h4_copied","s3_h5_copied","s3_h6_copied","s3_h7_copied","s3_h8_copied","s3_h9_copied","s3_h10_copied","s3_h11_copied","s3_h12_copied","s3_h13_copied","s3_h14_copied","s3_h15_copied","s3_h16_copied","s3_h17_copied","s3_h18_copied","s3_h19_copied","s3_h20_copied","s3_h21_copied","s3_h22_copied","s3_h23_copied","s3_h24_copied","s3_h25_copied","s3_h26_copied","s3_h27_copied","s3_h28_copied","s3_h29_copied","s3_h30_copied")], 1, count_function)
peak_data$total_copies_s3_prop <- peak_data$total_copies_s3 / 29

peak_data$total_copies <- peak_data$total_copies_s1 + peak_data$total_copies_s2 + peak_data$total_copies_s3
peak_data$total_copies_prop <- peak_data$total_copies / (3*29)

# calculate final score/kills ---------------------------------------------

# mean final score across all 3 hunts
peak_data$mean_final_score <- (peak_data$s1_h30_score + peak_data$s2_h30_score + peak_data$s3_h30_score) / 3

# mean final kills across all 3 hunts
peak_data$mean_final_kills <- (peak_data$s1_h30_kills + peak_data$s2_h30_kills + peak_data$s3_h30_kills) / 3


# correction for demonstrator scores --------------------------------------

#also need to correct for slightly different final scores (cumulative / final hunt) of the best demonstrators across the two conditions:
# wide:
# season 1 = 25525 / 993
# season 2 = 26728 / 994
# season 3 = 26101 / 997
# narrow:
# season 1 = 24838 / 945
# season 2 = 25784 / 963
# season 3 = 24019 / 982

#create variables recoding condition into best demonstrator score, then divide each mean score by this value to get normalised score (0=same as best dem, negative=worse, positive=better)
peak_data$best_dem_S1_score <- car::recode(peak_data$peaks, "'narrow'='24838'; 'wide'='25525'")
peak_data$s1_score_weighted <- peak_data$s1_h30_score / peak_data$best_dem_S1_score
peak_data$best_dem_S1_kills <- car::recode(peak_data$peaks, "'narrow'='945'; 'wide'='993'")
peak_data$s1_kills_weighted <- peak_data$s1_h30_kills / peak_data$best_dem_S1_kills

peak_data$best_dem_S2_score <- car::recode(peak_data$peaks, "'narrow'='25784'; 'wide'='26728'")
peak_data$s2_score_weighted <- peak_data$s2_h30_score / peak_data$best_dem_S2_score
peak_data$best_dem_S2_kills <- car::recode(peak_data$peaks, "'narrow'='963'; 'wide'='994'")
peak_data$s2_kills_weighted <- peak_data$s2_h30_kills / peak_data$best_dem_S2_kills

peak_data$best_dem_S3_score <- car::recode(peak_data$peaks, "'narrow'='24019'; 'wide'='26101'")
peak_data$s3_score_weighted <- peak_data$s3_h30_score / peak_data$best_dem_S3_score
peak_data$best_dem_S3_kills <- car::recode(peak_data$peaks, "'narrow'='982'; 'wide'='997'")
peak_data$s3_kills_weighted <- peak_data$s3_h30_kills / peak_data$best_dem_S3_kills

#calculate mean weighted score
peak_data$mean_score_weighted <- (peak_data$s1_score_weighted + peak_data$s2_score_weighted + peak_data$s3_score_weighted) / 3
peak_data$mean_kills_weighted <- (peak_data$s1_kills_weighted + peak_data$s2_kills_weighted + peak_data$s3_kills_weighted) / 3

# restructure into long format --------

#season1
#score per hunt
peak_data_longscoresS1 <- peak_data[, c("participant", "learning", "peaks","age","sex","s1_h1_kills","s1_h2_kills","s1_h3_kills","s1_h4_kills","s1_h5_kills","s1_h6_kills","s1_h7_kills","s1_h8_kills","s1_h9_kills","s1_h10_kills","s1_h11_kills","s1_h12_kills","s1_h13_kills","s1_h14_kills","s1_h15_kills","s1_h16_kills","s1_h17_kills","s1_h18_kills","s1_h19_kills","s1_h20_kills","s1_h21_kills","s1_h22_kills","s1_h23_kills","s1_h24_kills","s1_h25_kills","s1_h26_kills","s1_h27_kills","s1_h28_kills","s1_h29_kills","s1_h30_kills")]
names(peak_data_longscoresS1) <- c("participant", "learning", "peaks","age","sex", 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)
peak_data_longscoresS1 <- melt(peak_data_longscoresS1, id = c("participant", "learning", "peaks","age","sex"), variable.name = "hunt", value.name = "kills")
#add copies
peak_data_longscoresS1_copies <- peak_data[, c("participant", "s1_h1_copied","s1_h2_copied","s1_h3_copied","s1_h4_copied","s1_h5_copied","s1_h6_copied","s1_h7_copied","s1_h8_copied","s1_h9_copied","s1_h10_copied","s1_h11_copied","s1_h12_copied","s1_h13_copied","s1_h14_copied","s1_h15_copied","s1_h16_copied","s1_h17_copied","s1_h18_copied","s1_h19_copied","s1_h20_copied","s1_h21_copied","s1_h22_copied","s1_h23_copied","s1_h24_copied","s1_h25_copied","s1_h26_copied","s1_h27_copied","s1_h28_copied","s1_h29_copied","s1_h30_copied")]
peak_data_longscoresS1_copies <- melt(peak_data_longscoresS1_copies, id = c("participant"), variable.name = "hunt", value.name = "copies")
peak_data_longscoresS1$copies <- peak_data_longscoresS1_copies$copies
rm(peak_data_longscoresS1_copies)
#sort and add season identifier
peak_data_longscoresS1 <- arrange(peak_data_longscoresS1, participant, hunt)
peak_data_longscoresS1$season <- "season 1"

#season2
#score per hunt
peak_data_longscoresS2 <- peak_data[, c("participant", "learning", "peaks","age","sex", "s2_h1_kills","s2_h2_kills","s2_h3_kills","s2_h4_kills","s2_h5_kills","s2_h6_kills","s2_h7_kills","s2_h8_kills","s2_h9_kills","s2_h10_kills","s2_h11_kills","s2_h12_kills","s2_h13_kills","s2_h14_kills","s2_h15_kills","s2_h16_kills","s2_h17_kills","s2_h18_kills","s2_h19_kills","s2_h20_kills","s2_h21_kills","s2_h22_kills","s2_h23_kills","s2_h24_kills","s2_h25_kills","s2_h26_kills","s2_h27_kills","s2_h28_kills","s2_h29_kills","s2_h30_kills")]
names(peak_data_longscoresS2) <- c("participant", "learning", "peaks","age","sex", 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)
peak_data_longscoresS2 <- melt(peak_data_longscoresS2, id = c("participant", "learning", "peaks","age","sex"), variable.name = "hunt", value.name = "kills")
#add copies
peak_data_longscoresS2_copies <- peak_data[, c("participant", "s2_h1_copied","s2_h2_copied","s2_h3_copied","s2_h4_copied","s2_h5_copied","s2_h6_copied","s2_h7_copied","s2_h8_copied","s2_h9_copied","s2_h10_copied","s2_h11_copied","s2_h12_copied","s2_h13_copied","s2_h14_copied","s2_h15_copied","s2_h16_copied","s2_h17_copied","s2_h18_copied","s2_h19_copied","s2_h20_copied","s2_h21_copied","s2_h22_copied","s2_h23_copied","s2_h24_copied","s2_h25_copied","s2_h26_copied","s2_h27_copied","s2_h28_copied","s2_h29_copied","s2_h30_copied")]
peak_data_longscoresS2_copies <- melt(peak_data_longscoresS2_copies, id = c("participant"), variable.name = "hunt", value.name = "copies")
peak_data_longscoresS2$copies <- peak_data_longscoresS2_copies$copies
rm(peak_data_longscoresS2_copies)
#sort and add season identifier
peak_data_longscoresS2 <- arrange(peak_data_longscoresS2, participant, hunt)
peak_data_longscoresS2$season <- "season 2"

#season3
#score per hunt
peak_data_longscoresS3 <- peak_data[, c("participant", "learning", "peaks","age","sex", "s3_h1_kills","s3_h2_kills","s3_h3_kills","s3_h4_kills","s3_h5_kills","s3_h6_kills","s3_h7_kills","s3_h8_kills","s3_h9_kills","s3_h10_kills","s3_h11_kills","s3_h12_kills","s3_h13_kills","s3_h14_kills","s3_h15_kills","s3_h16_kills","s3_h17_kills","s3_h18_kills","s3_h19_kills","s3_h20_kills","s3_h21_kills","s3_h22_kills","s3_h23_kills","s3_h24_kills","s3_h25_kills","s3_h26_kills","s3_h27_kills","s3_h28_kills","s3_h29_kills","s3_h30_kills")]
names(peak_data_longscoresS3) <- c("participant", "learning", "peaks","age","sex", 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)
peak_data_longscoresS3 <- melt(peak_data_longscoresS3, id = c("participant", "learning", "peaks","age","sex"), variable.name = "hunt", value.name = "kills")
#add copies
peak_data_longscoresS3_copies <- peak_data[, c("participant", "s3_h1_copied","s3_h2_copied","s3_h3_copied","s3_h4_copied","s3_h5_copied","s3_h6_copied","s3_h7_copied","s3_h8_copied","s3_h9_copied","s3_h10_copied","s3_h11_copied","s3_h12_copied","s3_h13_copied","s3_h14_copied","s3_h15_copied","s3_h16_copied","s3_h17_copied","s3_h18_copied","s3_h19_copied","s3_h20_copied","s3_h21_copied","s3_h22_copied","s3_h23_copied","s3_h24_copied","s3_h25_copied","s3_h26_copied","s3_h27_copied","s3_h28_copied","s3_h29_copied","s3_h30_copied")]
peak_data_longscoresS3_copies <- melt(peak_data_longscoresS3_copies, id = c("participant"), variable.name = "hunt", value.name = "copies")
peak_data_longscoresS3$copies <- peak_data_longscoresS3_copies$copies
rm(peak_data_longscoresS3_copies)
#sort and add season identifier
peak_data_longscoresS3 <- arrange(peak_data_longscoresS3, participant, hunt)
peak_data_longscoresS3$season <- "season 3"

#combine seasons into one dataframe, make hunt numeric, and remove separate seasons
peak_data_longscores <- rbind(peak_data_longscoresS1, peak_data_longscoresS2, peak_data_longscoresS3)
peak_data_longscores$hunt <- as.numeric(peak_data_longscores$hunt)
rm(peak_data_longscoresS1, peak_data_longscoresS2, peak_data_longscoresS3)

# restructure into shorter format --------

#season1
peak_data_shorterscoresS1 <- peak_data[, c("participant", "learning", "peaks","age","sex","s1_h30_kills","s1_h30_score", "s1_kills_weighted", "s1_score_weighted", "total_copies_s1_prop")]
peak_data_shorterscoresS1$season <- "season 1"
names(peak_data_shorterscoresS1) <- c("participant", "learning", "peaks","age","sex", "kills", "score", "weighted_kills", "weighted_score", "copies", "season")
#season2
peak_data_shorterscoresS2 <- peak_data[, c("participant", "learning", "peaks","age","sex","s2_h30_kills","s2_h30_score", "s2_kills_weighted", "s2_score_weighted", "total_copies_s2_prop")]
peak_data_shorterscoresS2$season <- "season 2"
names(peak_data_shorterscoresS2) <- c("participant", "learning", "peaks","age","sex", "kills", "score", "weighted_kills", "weighted_score", "copies", "season")
#season3
peak_data_shorterscoresS3 <- peak_data[, c("participant", "learning", "peaks","age","sex","s3_h30_kills","s3_h30_score", "s3_kills_weighted", "s3_score_weighted", "total_copies_s3_prop")]
peak_data_shorterscoresS3$season <- "season 3"
names(peak_data_shorterscoresS3) <- c("participant", "learning", "peaks","age","sex", "kills", "score", "weighted_kills", "weighted_score", "copies", "season")

#combine seasons into one dataframe and sort, remove separate seasons
peak_data_shorterscores <- rbind(peak_data_shorterscoresS1, peak_data_shorterscoresS2, peak_data_shorterscoresS3)
peak_data_shorterscores <- arrange(peak_data_shorterscores, participant, season)
rm(peak_data_shorterscoresS1, peak_data_shorterscoresS2, peak_data_shorterscoresS3)


# ANALYSES ----------------------------------------------------------------

# 1. Overall mutlilevel regression ----------------------------------

# graph of score over time -------------------------------------------

# Shows: in all cases there is an advantage for the social learners, but particulalarly in the narrow environment
ggplot(peak_data_longscores, aes(hunt, kills, colour = learning)) + facet_grid(peaks ~ season) + stat_summary(fun.y = mean, geom = "line", size = 1, aes(group = learning, colour = learning)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 0.5) + scale_y_continuous(limits = c(100,1000)) + coord_cartesian(ylim=c(400, 1000)) + scale_x_continuous(breaks = c(1,5,10,15,20,25,30)) + labs(x = "hunt", y = "score") + theme_bw(base_size = 10) + theme(legend.position = c(0.5,0.53), legend.title = element_blank(), legend.direction = "horizontal", legend.background = element_rect(colour = "dark grey"), legend.key = element_blank()) + scale_colour_manual(values=c("#E69F00", "#56B4E9")) 

# save to file
ggsave(file="fig2.pdf", width = 13.7, height = 9, units = "cm")


# multilevel regression models --------------------------------------------

# null random-intercept model, with observations nested within seasons, within participants
summary(random.intercepts.model <- lmer(kills ~ 1 + (1|participant / season), data=peak_data_longscores, REML=FALSE))

summary(random.slopes.model <- lmer(kills ~ 1 + ( hunt | participant / season), data=peak_data_longscores, REML=FALSE))

#add hunt as fixed effect
summary(hunt.model <- lmer(kills ~ hunt + ( hunt |participant / season), data=peak_data_longscores, REML=FALSE))

#compare null/random intercept, null/random slopes and hunt fixed effect. Sig improvement in each case
anova(random.intercepts.model, random.slopes.model, hunt.model)

#add learning
summary(hunt.learning.model <- lmer(kills ~ hunt + learning + (hunt |participant / season), data=peak_data_longscores, REML=FALSE))

#add peaks
summary(hunt.peak.learning.model <- lmer(kills ~ hunt + learning + peaks + (hunt |participant / season), data=peak_data_longscores, REML=FALSE))

#add sex
summary(hunt.peak.learning.sex.model <- lmer(kills ~ hunt + peaks + learning + sex + (hunt|participant / season), data=peak_data_longscores, REML=FALSE))

#add age
summary(hunt.peak.learning.sex.age.model <- lmer(kills ~ hunt + peaks + learning + age + sex + (hunt|participant / season), data=peak_data_longscores, REML=FALSE))

#compare models - peaks and learning both have large effects, sex and age not so much (p=0.049). Given no predicted effect of age or sex, lack of previous effects, and weakness of present effect, going to ignore them in subsequent analyses
anova(random.slopes.model, hunt.model, hunt.learning.model, hunt.peak.learning.model, hunt.peak.learning.sex.model, hunt.peak.learning.sex.age.model)

#look at interactions:

#add hunt*peak interation
summary(huntXpeak.learning.model <- lmer(kills ~ hunt * peaks + learning +  (hunt|participant / season), data=peak_data_longscores, REML=FALSE))

#add hunt*learning interation
summary(huntXlearning.peak.model <- lmer(kills ~ hunt * learning + peaks + (hunt|participant / season), data=peak_data_longscores, REML=FALSE))

#add peaks*learning interation - not significant
summary(learningXpeak.hunt.model <- lmer(kills ~ hunt + learning * peaks +  (hunt|participant / season), data=peak_data_longscores, REML=FALSE))

#add hunt interactions only
summary(huntXlearning.huntXpeak.model <- lmer(kills ~ hunt * learning + hunt * peaks +  (hunt|participant / season), data=peak_data_longscores, REML=FALSE))

#add peaks*learning*hunt interation
summary(huntXpeakXlearning.model <- lmer(kills ~ hunt * learning * peaks + (hunt|participant / season), data=peak_data_longscores, REML=FALSE))

#compare models - 2 two-way interactions: hunt*learning and hunt*peaks. No 3-way interaction. hunt*learning makes sense given that all learners start out similar at hunt 1 and end up different at hunt 30. hunt*peaks likewise: difference only emerges at later hunts. No interaction between peaks and learning, suggesting that social learners are not relatively better off in narrow than wide.
anova(random.slopes.model, hunt.model, hunt.peak.learning.model, huntXpeak.learning.model, huntXlearning.peak.model, huntXlearning.huntXpeak.model, huntXpeakXlearning.model)


# 2. Does performance over time increase? ------------------------------------------

#hunt alone in each condition
#social narrow, slope = 11.39 (se=0.71; 95% CI 9.95, 12.83)
summary(nullModel <- lmer(kills ~ 1 +  (hunt|participant / season), data=peak_data_longscores, subset=(learning == "social" & peaks == "narrow"), REML=FALSE))
summary(huntModel <- lmer(kills ~ hunt +  (hunt|participant / season), data=peak_data_longscores, subset=(learning == "social" & peaks == "narrow"), REML=FALSE))
anova(nullModel, huntModel)
confint(huntModel) 

#social wide, slope = 13.19 (se=0.47; 95% CI 12.24, 14.15)
summary(nullModel <- lmer(kills ~ 1 +  (hunt|participant / season), data=peak_data_longscores, subset=(learning == "social" & peaks == "wide"), REML=FALSE))
summary(huntModel <- lmer(kills ~ hunt + (hunt|participant / season), data=peak_data_longscores, subset=(learning == "social" & peaks == "wide"), REML=FALSE))
anova(nullModel, huntModel)
confint(huntModel) 

#individual narrow, slope = 2.67 (se=0.64; 95% CI 1.28, 4.06)
summary(nullModel <- lmer(kills ~ 1 +  (hunt|participant / season), data=peak_data_longscores, subset=(learning == "individual" & peaks == "narrow"), REML=FALSE))
summary(huntModel <- lmer(kills ~ hunt + (hunt|participant / season), data=peak_data_longscores, subset=(learning == "individual" & peaks == "narrow"), REML=FALSE))
anova(nullModel, huntModel)
confint(huntModel) 

#individual wide, slope = 7.51 (se=1.31; 95% CI 4.82, 10.20)
summary(nullModel <- lmer(kills ~ 1 +  (hunt|participant / season), data=peak_data_longscores, subset=(learning == "individual" & peaks == "wide"), REML=FALSE))
summary(huntModel <- lmer(kills ~ hunt + (hunt|participant / season), data=peak_data_longscores, subset=(learning == "individual" & peaks == "wide"), REML=TRUE))
anova(nullModel, huntModel)
confint(huntModel, parm = c("hunt")) 


# 3. Is individual learning easier in the wide condition? -----------------

#now remove effect of hunt, just look at final hunt score (score on hunt 30) and final cumulative score (sum of all hunt scores), with season again as a repeated (random) factor. Both are significant. ILers have total scores 1667.60 [se=466.9, CI 737.70, 2597.61] calories higher in the wide condition than the narrow condition, and end with 118.81 [se=21.89, CI 75.20, 162.41] more calories

summary(nullModel <- lmer(kills ~ 1 + (1 | season), data=peak_data_shorterscores, subset=(learning == "individual"), REML=FALSE))
summary(peaksModel <- lmer(kills ~ peaks + (1 | season), data=peak_data_shorterscores, subset=(learning == "individual"), REML=FALSE))
anova(nullModel,peaksModel)
confint(peaksModel)

summary(nullModel <- lmer(score ~ 1 + (1 | season), data=peak_data_shorterscores, subset=(learning == "individual"), REML=FALSE))
summary(peaksModel <- lmer(score ~ peaks + (1 | season), data=peak_data_shorterscores, subset=(learning == "individual"), REML=FALSE))
anova(nullModel,peaksModel)
confint(peaksModel)


#graphs to show this:
#score by season:
ggplot(subset(peak_data_shorterscores, learning=="individual"), aes(peaks, score, colour = peaks)) + facet_wrap(~ season) + geom_boxplot(outlier.size = NA) + geom_point() + theme_bw(base_size = 12) + theme(legend.position='none') + scale_color_npg() + ylab("final cumulative score") + xlab("peak width")

# save to file
ggsave(file="figS1.jpg", width = 13.7, height = 9, units = "cm")

#and overall (means of 3 seasons)
ggplot(subset(peak_data, learning=="individual"), aes(peaks, mean_final_score, colour = peaks)) + geom_boxplot(outlier.size = NA) + geom_point() + ggtitle("individual learners") + theme_bw(base_size = 12) + theme(legend.position='none') + scale_color_npg() + ylab("final cumulative score") + xlab("peak width")

#kills by season:
ggplot(subset(peak_data_shorterscores, learning=="individual"), aes(peaks, kills, colour = peaks)) + facet_wrap(~ season) + geom_boxplot(outlier.size = NA) + geom_point() + theme_bw(base_size = 12) + theme(legend.position='none') + scale_color_npg() + ylab("final hunt score") + xlab("peak width")

# save to file
ggsave(file="figS2.jpg", width = 13.7, height = 9, units = "cm")

#and overall (means of 3 seasons)
ggplot(subset(peak_data, learning=="individual"), aes(peaks, mean_final_kills, colour = peaks)) + geom_boxplot(outlier.size = NA) + geom_point() + ggtitle("individual learners") + theme_bw(base_size = 12) + theme(legend.position='none') + scale_color_npg() + ylab("final cumulative score") + xlab("peak width")


# 4. Do social learners out-perform individual learners? ---------------

#same as (3) but comparing learning rather than peaks

#narrow peaks - yes, significant
summary(nullModel <- lmer(kills ~ 1 + (1 | season), data=peak_data_shorterscores, subset=(peaks == "narrow"), REML=FALSE))
summary(learningModel <- lmer(kills ~ learning + (1 | season), data=peak_data_shorterscores, subset=(peaks == "narrow"), REML=FALSE))
anova(nullModel,learningModel)
confint(learningModel)

summary(nullModel <- lmer(score ~ 1 + (1 | season), data=peak_data_shorterscores, subset=(peaks == "narrow"), REML=FALSE))
summary(learningModel <- lmer(score ~ learning + (1 | season), data=peak_data_shorterscores, subset=(peaks == "narrow"), REML=FALSE))
anova(nullModel,learningModel)
confint(learningModel)

#wide peaks - yes, significant
summary(nullModel <- lmer(kills ~ 1 + (1 | season), data=peak_data_shorterscores, subset=(peaks == "wide"), REML=FALSE))
summary(learningModel <- lmer(kills ~ learning + (1 | season), data=peak_data_shorterscores, subset=(peaks == "wide"), REML=FALSE))
anova(nullModel,learningModel)
confint(learningModel)

summary(nullModel <- lmer(score ~ 1 + (1 | season), data=peak_data_shorterscores, subset=(peaks == "wide"), REML=FALSE))
summary(learningModel <- lmer(score ~ learning + (1 | season), data=peak_data_shorterscores, subset=(peaks == "wide"), REML=FALSE))
anova(nullModel,learningModel)
confint(learningModel)

#score by learning and peaks:
ggplot(peak_data_shorterscores, aes(learning, score, colour = learning)) + facet_grid(peaks ~ season) + geom_boxplot(outlier.size = NA) + geom_point(size = 0.5) + theme_bw(base_size = 12) + theme(legend.position='none') + ylab("final cumulative score") + xlab("learning") + scale_colour_manual(values=c("#E69F00", "#56B4E9"))

# save to file
ggsave(file="figS3.jpg", width = 13.7, height = 9, units = "cm")

#kills by learning and peaks:
ggplot(peak_data_shorterscores, aes(learning, kills, colour = learning)) + facet_grid(peaks ~ season) + geom_boxplot(outlier.size = NA) + geom_point() + theme_bw(base_size = 12) + theme(legend.position='none') + ylab("final hunt score") + xlab("learning") + scale_colour_manual(values=c("#E69F00", "#56B4E9"))

# save to file
ggsave(file="figS4.jpg", width = 13.7, height = 9, units = "cm")


# 5. Do social learners do better in the wide condition? -----------------------

#first without correction for different demonstrator performance
#SLers score about 50 calories higher in wide than narrow
summary(nullModel <- lmer(kills ~ 1 + (1 | season), data=peak_data_shorterscores, subset=(learning == "social"), REML=FALSE))
summary(peaksModel <- lmer(kills ~ peaks + (1 | season), data=peak_data_shorterscores, subset=(learning == "social"), REML=FALSE))
anova(nullModel,peaksModel)
confint(peaksModel)

#and 1333 calories higher overall
summary(nullModel <- lmer(score ~ 1 + (1 | season), data=peak_data_shorterscores, subset=(learning == "social"), REML=FALSE))
summary(peaksModel <- lmer(score ~ peaks + (1 | season), data=peak_data_shorterscores, subset=(learning == "social"), REML=FALSE))
anova(nullModel,peaksModel)
confint(peaksModel)


#BUT scores are not normalised. Do this and the effect of peaks gets much smaller
summary(nullModel <- lmer(weighted_kills ~ 1 + (1 | season), data=peak_data_shorterscores, subset=(learning == "social"), REML=FALSE))
summary(peaksModel <- lmer(weighted_kills ~ peaks + (1 | season), data=peak_data_shorterscores, subset=(learning == "social"), REML=FALSE))
anova(nullModel,peaksModel)
confint(peaksModel)

summary(nullModel <- lmer(weighted_score ~ 1 + (1 | season), data=peak_data_shorterscores, subset=(learning == "social"), REML=FALSE))
summary(peaksModel <- lmer(weighted_score ~ peaks + (1 | season), data=peak_data_shorterscores, subset=(learning == "social"), REML=FALSE))
anova(nullModel,peaksModel)
confint(peaksModel)

# create figure 3, comparing scores----------------------------

plot1 <- ggplot(subset(peak_data, learning=="individual"), aes(peaks, mean_final_kills, colour = peaks)) + geom_boxplot(outlier.size = NA) + geom_point() + ggtitle("individual learners") + theme_bw(base_size = 9) + theme(legend.position='none') + scale_color_npg() + ylab("final hunt score") + xlab("") + scale_y_continuous(limits = c(500,1000))
plot2 <- ggplot(subset(peak_data, learning=="social"), aes(peaks, mean_final_kills, colour = peaks)) + geom_boxplot(outlier.size = NA) + geom_point()+  ggtitle("social learners") + theme_bw(base_size = 9) + theme(legend.position='none') + scale_color_npg() + ylab("final hunt score") + xlab("peak width") + scale_y_continuous(limits = c(500,1000))
plot3 <- ggplot(subset(peak_data_shorterscores, learning=="social"), aes(peaks, weighted_kills, colour = peaks)) + geom_boxplot(outlier.size = NA) + geom_point()+  ggtitle("social learners") + theme_bw(base_size = 9) + theme(legend.position='none') + scale_color_npg() + ylab("normalised final hunt score") + xlab("")

multiplot(plot1, plot2, plot3, cols = 3)

pdf(file = "fig3.pdf", width = 5.4, height = 3)
multiplot(plot1, plot2, plot3, cols = 3)
dev.off()


# 6. Do social learners copy more often in the narrow condition? ---------------

# graphs of copy frequency ------------------------------------------------

#social learners only

# by season - jitter plot
ggplot(subset(peak_data_shorterscores, learning=="social"), aes(peaks, copies, colour = peaks)) + facet_wrap(~ season) + geom_boxplot(outlier.size = NA) + geom_jitter(width = 0.8, alpha = 0.7, size = 0.7) + theme_bw(base_size = 9) + theme(legend.position='none') + scale_color_npg() + ylab("proportion of copying") + xlab("peak width")

# by season - count plot
ggplot(subset(peak_data_shorterscores, learning=="social"), aes(peaks, copies, colour = peaks)) + facet_wrap(~ season) + geom_boxplot(outlier.size = NA) + geom_count(alpha = 0.7) + theme_bw(base_size = 9) + theme(legend.position='none') + scale_color_npg() + ylab("proportion of copying") + xlab("peak width")

# save to file
ggsave(file="fig4.pdf", width = 13.7, height = 9, units = "cm")

#mean across all seasons
ggplot(subset(peak_data, learning=="social"), aes(peaks, total_copies_prop, colour = peaks)) + geom_boxplot(outlier.size = NA) + geom_jitter(width = 0.8, alpha = 0.7, size = 0.7) + theme_bw(base_size = 9) + theme(legend.position='none') + scale_color_npg() + ylab("proportion of copying") + xlab("peak width")


# descriptive stats for copy frequency--------------------------------

by(peak_data$total_copies_s1_prop[peak_data$learning == "social"], peak_data$peaks[peak_data$learning == "social"], stat.desc)
by(peak_data$total_copies_s2_prop, peak_data$peaks[peak_data$learning == "social"], stat.desc)
by(peak_data$total_copies_s3_prop, peak_data$peaks[peak_data$learning == "social"], stat.desc)
by(peak_data$total_copies_prop[peak_data$learning == "social"], peak_data$peaks[peak_data$learning == "social"], stat.desc)

# stats for copy frequency ------------------------------------------------

#check for normality - not really
ggplot(subset(peak_data, learning=="social"), aes(total_copies_prop)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x= "copying", y="Density")

#one option: non-parametric Wilcoxon test. Not significant (p=0.35)
wilcox.test(total_copies_prop ~ peaks, data = subset(peak_data, learning=="social"), paired = FALSE)
wilcox.test(total_copies_s1_prop ~ peaks, data = subset(peak_data, learning=="social"), paired = FALSE)
wilcox.test(total_copies_s2_prop ~ peaks, data = subset(peak_data, learning=="social"), paired = FALSE)
wilcox.test(total_copies_s3_prop ~ peaks, data = subset(peak_data, learning=="social"), paired = FALSE)

#alternative is quasibinomial regression. Again, no sig difference 95% CI[-0.93, 0.28]
summary(copyModel <- glm(total_copies_prop ~ peaks, family = quasibinomial, data = subset(peak_data, learning=="social")))
confint(copyModel)
plot(copyModel)
hist(residuals(copyModel))
qqnorm(residuals(copyModel))

# nor for each season separately
summary(copyModel <- glm(total_copies_s1_prop ~ peaks, family = quasibinomial, data = subset(peak_data, learning=="social")))
summary(copyModel <- glm(total_copies_s2_prop ~ peaks, family = quasibinomial, data = subset(peak_data, learning=="social")))
summary(copyModel <- glm(total_copies_s3_prop ~ peaks, family = quasibinomial, data = subset(peak_data, learning=="social")))

#alternatively, use multilevel model, with season as a random factor. Can't find a way to do quasibinomial multilevel model, though. Use binomial instead. Not hugely different to the quasibinomial, so maybe stick to that one.
summary(nullModel <- glmer(copies ~ 1 + (1 | season), family=binomial, data=peak_data_shorterscores, subset=(learning == "social")))
summary(copyModel <- glmer(copies ~ peaks + (1 | season), family=binomial, data=peak_data_shorterscores, subset=(learning == "social")))
anova(nullModel,copyModel)
confint(copyModel)
plot(copyModel)
hist(residuals(copyModel))
qqnorm(residuals(copyModel))


# 7. Does copying more lead to higher scores? -----------------------------

# graphs of score x copy frequency -------------------------------------
# use weighted score, to properly compare wide and narrow conditions

#season 1 - positive slope only for narrow condition
ggplot(subset(peak_data, learning == "social"), aes(total_copies_s1_prop, s1_score_weighted, colour = peaks)) + geom_point(aes(shape = peaks)) + geom_smooth(method=glm, alpha = 0.1, aes(linetype=peaks)) + theme_bw()

#season 2 - positive slope for both narrow and wide
ggplot(subset(peak_data, learning == "social"), aes(total_copies_s2_prop, s2_score_weighted, colour = peaks)) + geom_point(aes(shape = peaks)) + geom_smooth(method=glm, alpha = 0.1, aes(linetype=peaks)) + theme_bw()

#season 3 - postiive slope for both narrow and wide
ggplot(subset(peak_data, learning == "social"), aes(total_copies_s3_prop, s3_score_weighted, colour = peaks)) + geom_point(aes(shape = peaks)) + geom_smooth(method=glm, alpha = 0.1, aes(linetype=peaks)) + theme_bw()

#all seasons - virtually identical positive slope for both narrow and wide. But, not multilevel corrected (for this see below)
ggplot(subset(peak_data, learning == "social"), aes(total_copies_prop, mean_score_weighted, colour = peaks)) + geom_point(aes(shape = peaks)) + geom_smooth(method=glm, alpha = 0.1, aes(linetype=peaks)) + theme_bw()

# for interest, un-transformed scores
ggplot(subset(peak_data, learning == "social"), aes(total_copies_prop, mean_final_score, colour = peaks)) + geom_point(aes(shape = peaks)) + geom_smooth(method=glm, alpha = 0.1, aes(linetype=peaks)) + theme_bw()

# regression of copy and score --------------------------------------------

ggplot(subset(peak_data, learning == "social"), aes(mean_score_weighted)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x= "kills", y="Density")

# predict score from copy frequency - averaged across season, not multilevel
# narrow
summary(scoreXcopyModel <- glm(mean_score_weighted ~ total_copies_prop, data = subset(peak_data, learning == "social" & peaks == "narrow")))
plot(scoreXcopyModel)

# wide
summary(scoreXcopyModel <- glm(mean_score_weighted ~ total_copies_prop, data = subset(peak_data, learning == "social" & peaks == "wide")))
plot(scoreXcopyModel)


#multilevel version with season as random factor. Shows that narrow line is steeper than wide, indicating that social learning is more beneficial in narrow condition
#narrow:
summary(scoreXcopy.narrow <- lmer(weighted_score ~ copies + (1 | season), data=peak_data_shorterscores, subset=(learning == "social" & peaks == "narrow"), REML=FALSE))
confint(scoreXcopy.narrow)

# calculate line and 80% PI using merTools package
copies <- (0:500)/500
season <- rep(0,501)
copy_values <- data.frame(copies, season)
conf_int_narrow <- predictInterval(scoreXcopy.narrow, stat = "mean", newdata = copy_values, type = "linear.prediction", include.resid.var = FALSE, level = 0.8)
peaks <- rep("narrow",501)
conf_int_narrow <- data.frame(copies, peaks, conf_int_narrow)

#wide:
summary(scoreXcopy.wide <- lmer(weighted_score ~ copies + (1 | season), data=peak_data_shorterscores, subset=(learning == "social" & peaks == "wide"), REML=FALSE))
confint(scoreXcopy.wide)

conf_int_wide <- predictInterval(scoreXcopy.wide, stat = "mean", newdata = copy_values, type = "linear.prediction", include.resid.var = FALSE, level = 0.8)
peaks <- rep("wide",501)
conf_int_wide <- data.frame(copies, peaks, conf_int_wide)

conf_int_combined <- rbind(conf_int_narrow, conf_int_wide)

ggplot() + geom_line(data = conf_int_combined, aes(copies, fit, ymin = lwr, ymax = upr, group = peaks, colour = peaks), size = 1) + geom_ribbon(data = conf_int_combined, aes(copies, fit, ymin = lwr, ymax = upr, group = peaks, fill = peaks), alpha = 0.2) + geom_point(data = subset(peak_data, learning == "social"), mapping = aes(total_copies_prop, mean_score_weighted, colour = peaks, shape = peaks), size = 1.8) + theme_bw(base_size = 12) + scale_color_npg() + theme(legend.position = c(0.8,0.2), legend.title = element_blank(), legend.background = element_rect(colour = "dark grey"), legend.key = element_blank()) + labs(x = "copy frequency", y = "normalised cumulative score")

ggsave(file="fig5.pdf", width = 10, height = 9, units = "cm")

# 8. How does individual variation in copying compare across the conditions? -----

#looks like there are more low-copiers in the wide condition (distribution is shifted to the right, compared to narrow copiers)

ggplot(subset(peak_data_shorterscores, learning == "social"), aes(copies)) + facet_grid(peaks ~ season) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white", binwidth = 0.1) + labs(x= "copy frequency", y="Density")

#mean across all seasons
ggplot(subset(peak_data, learning == "social"), aes(total_copies_prop)) + facet_wrap(~ peaks) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white", binwidth = 0.1) + labs(x= "copy frequency", y="Density")

# frequency tables for copying frequency (ignore the first individual learning table)
#season 1
table(peak_data$total_copies_s1, peak_data$peaks, peak_data$learning)
#season 2
table(peak_data$total_copies_s2, peak_data$peaks, peak_data$learning)
#season 3
table(peak_data$total_copies_s3, peak_data$peaks, peak_data$learning)
#mean across all seasons
table(peak_data$total_copies, peak_data$peaks, peak_data$learning)


# 9. Do narrow copiers copy earlier than wide copiers? --------------------

#graph of copy frequency over hunt, averaging across seasons
ggplot(subset(peak_data_longscores, learning == "social"), aes(hunt, copies, colour = peaks)) + stat_summary(fun.y = mean, geom = "line", size = 1.5, aes(group = peaks, colour = peaks)) + stat_summary(fun.y = mean, geom = "point", size = 1.5, aes(shape = peaks)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1, alpha = 0.7) + labs(x = "hunt", y = "copies") + theme_bw(base_size = 12) + scale_color_npg() + scale_x_continuous(breaks = c(1,5,10,15,20,25,30))

# save to file
ggsave(file="figS6.jpg", width = 13.7, height = 9, units = "cm")

#graph of copy frequency over hunt, per season
ggplot(subset(peak_data_longscores, learning == "social"), aes(hunt, copies, colour = peaks))  + facet_wrap(~ season) + stat_summary(fun.y = mean, geom = "line", size = 1.5, aes(group = peaks, colour = peaks)) + stat_summary(fun.y = mean, geom = "point", size = 1.5, aes(shape = peaks)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1, alpha = 0.7) + labs(x = "hunt", y = "copies") + theme_bw(base_size = 12) + scale_color_npg() + scale_x_continuous(breaks = c(1,5,10,15,20,25,30))

# save to file
ggsave(file="figS7.jpg", width = 13.7, height = 9, units = "cm")

