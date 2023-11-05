################################################################################
################## November 2023 Northern Poll ###################################
################################################################################

# load in packages 
library(foreign)
library(rstudioapi)
library(haven)
library(dplyr)
library(tidyverse)
library(survey)
library(pollster)
library(ggplot2)
library(gridExtra)
library(stringi)
library(stringr)
library(sjlabelled)
library(papeR)
library(labelled)
library(ggrepel)
library(gtable)
library(zipWRUext2)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
### onu_color_vec creation
library(RColorBrewer)
library(colorspace)
pal_ext <- (brewer.pal(8,"Set2"))
onu_color_vec <- c("#f26b27",pal_ext )


### set the working directory to local file 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


### read in the survey data 
q_items <- read.csv("northern_poll1024txt.csv",nrows=2)
q_items <- t(q_items)
q_items <- as.data.frame(q_items)
q_items$V2 <- row.names(q_items)
q_items$V2 <- str_replace_all(q_items$V2, "\\."," ")

### read in data 
final_data2allwB <- readRDS("final_data2allwB.rds")

## race simplified, following wru BISG format   
final_data2allwB$race_simp <- ""
final_data2allwB$race_simp[final_data2allwB$race_1==1] <- "white"
final_data2allwB$race_simp[final_data2allwB$race_2==1] <- "black"
final_data2allwB$race_simp[final_data2allwB$race_3==1] <- "other"
final_data2allwB$race_simp[final_data2allwB$race_4==1] <- "asian/pi"
final_data2allwB$race_simp[final_data2allwB$race_5==1] <- "asian/pi"
final_data2allwB$race_simp[final_data2allwB$race_6==1] <- "other"
final_data2allwB$race_simp[final_data2allwB$race_7==1] <- "other"
final_data2allwB$race_simp[final_data2allwB$race_simp==""] <- "other"

### create simpler race field 
final_data2allwB$race_simp2 = "white"
final_data2allwB$race_simp2[final_data2allwB$race_simp!="white"] = "non-white"

### now, create the folders by bloc 
block_vec <- (unique(word(colnames( final_data2allwB),1,sep="_")))
block_vec <- block_vec[20:50]
### let's find and replace if "."
block_vec <- gsub("\\.","_", block_vec)
# gsub('\\.', '-', x)
block_vec <- (unique(word(block_vec,1,sep="_")))


## create the vector of non response 
non_response_vec <- (unique(word(colnames( final_data2allwB),2,sep="_")))
## they are: dr, na, dk, nc, 
final_data2allwB <- subset(final_data2allwB, select=-c(potus.matchup6))
# word(df1$V1,1,sep = "\\|")

## run loop to create the folders by block 
main_dir <- getwd()

for (i in 1:length(block_vec)) {
  sub_dir <- block_vec[i]
  output_dir <- file.path(main_dir, sub_dir)
  if (!dir.exists(output_dir)){
    dir.create(output_dir)
  } else {
    print("Dir already exists!")
  }
}

##### Political Knowledge ######

final_data2allwB$correct_pid1_1 <- 0 # pres
final_data2allwB$correct_pid1_2 <- 0 # senate 
final_data2allwB$correct_pid1_3 <- 0 # house
final_data2allwB$correct_pid1_4 <- 0 # OH senate
final_data2allwB$correct_pid1_5 <- 0 # OH House 
final_data2allwB$correct_pid1_6 <- 0 # OH gov 
final_data2allwB$correct_pid1_7 <- 0 # US Supreme court 
### add in the correct 
final_data2allwB$correct_pid1_1[final_data2allwB$polknow_ns_pid1_1=="Democratic"] <- 1
final_data2allwB$correct_pid1_2[final_data2allwB$polknow_ns_pid1_2=="Democratic"] <- 1
final_data2allwB$correct_pid1_3[final_data2allwB$polknow_ns_pid1_3=="Republican"] <- 1
final_data2allwB$correct_pid1_4[final_data2allwB$polknow_ns_pid1_4=="Republican"] <- 1
final_data2allwB$correct_pid1_5[final_data2allwB$polknow_ns_pid1_5=="Republican"] <- 1
final_data2allwB$correct_pid1_6[final_data2allwB$polknow_ns_pid1_6=="Republican"] <- 1
final_data2allwB$correct_pid1_7[final_data2allwB$polknow_ns_pid1_7=="Republican"] <- 1

### get total correct 
final_data2allwB$correct_total_pid1 <- final_data2allwB$correct_pid1_1 + final_data2allwB$correct_pid1_2+
  final_data2allwB$correct_pid1_3+final_data2allwB$correct_pid1_4+final_data2allwB$correct_pid1_5+
  final_data2allwB$correct_pid1_6+final_data2allwB$correct_pid1_7
## for second part, pid2 
### now the second part 
polknow2_vec <- c("Joseph Biden", "J.D. Vance", "Kamala Harris","Mike Pence", "Mike DeWine","Donald Trump" )
final_data2allwB$correct_pid2_1 <- 0 # biden
final_data2allwB$correct_pid2_2 <- 0 # vance 
final_data2allwB$correct_pid2_3 <- 0 # harris
final_data2allwB$correct_pid2_5 <- 0 # pence
final_data2allwB$correct_pid2_6 <- 0 # dewine 
final_data2allwB$correct_pid2_7 <- 0 # trump 
### add in the correct 
final_data2allwB$correct_pid2_1[final_data2allwB$polknow_ns_pid2_1=="Democrat"] <- 1
final_data2allwB$correct_pid2_2[final_data2allwB$polknow_ns_pid2_2=="Republican"] <- 1
final_data2allwB$correct_pid2_3[final_data2allwB$polknow_ns_pid2_3=="Democrat"] <- 1
final_data2allwB$correct_pid2_5[final_data2allwB$polknow_ns_pid2_5=="Republican"] <- 1
final_data2allwB$correct_pid2_6[final_data2allwB$polknow_ns_pid2_6=="Republican"] <- 1
final_data2allwB$correct_pid2_7[final_data2allwB$polknow_ns_pid2_7=="Republican"] <- 1
### run second loop here 
final_data2allwB$correct_total_pid2 <- final_data2allwB$correct_pid2_1 + final_data2allwB$correct_pid2_2+
  final_data2allwB$correct_pid2_3+final_data2allwB$correct_pid2_5+
  final_data2allwB$correct_pid2_6+final_data2allwB$correct_pid2_7
## vector of positions 
### now, collapse into the vals of interest 
polknow_vec <- c("President", "U.S. Senate", "U.S. House", "OH Senate", "OH House",
                 "OH Governor", "U.S. Supreme Court")

### create nov index 
nov_index <-  names(final_data2allwB)[grepl("correct_pid1",colnames(final_data2allwB))]
polknow1_df <- data.frame()
for (i in 1:length(nov_index)) {
  print(i)
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### get title 
  ###
  testtab <- moe_topline(df = final_data2allwB, variable  =temp_var ,weight = final_weight)
  testtab <- as.data.frame(testtab) 
  ## we only need 1 
  testtab <- subset(testtab, Response==1)
  ## we will want to run round_preserve_sum() on the data 
  testtab[,2] <- round_preserve_sum(testtab[,2], 0)
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,5] <- round_preserve_sum(testtab[,5], 1)
  ### unnest 
  testtab2 <- unnest(testtab, Response)
  testtab2 <- as.data.frame(testtab2)
  colnames(testtab2)[1] <- "Correct"
  
  ## now, get the results subsetted, then save and test in the R markdown 
  reduced_tab <- testtab2[,c(1:3,5)]
  reduced_tab$Position <- polknow_vec[i]
  if(nrow(polknow1_df)==0){
    polknow1_df <- reduced_tab
  }else{
    polknow1_df <- rbind(polknow1_df,reduced_tab )
  }
}
polknow1_df <- polknow1_df[,c(5,1:4)]
### good, now export 
saveRDS(polknow1_df,"polknow/polknow1.rds")

### now the second part 
polknow2_vec <- c("Joseph Biden", "J.D. Vance", "Kamala Harris","Mike Pence", "Mike DeWine","Donald Trump" )
nov_index <-  names(final_data2allwB)[grepl("correct_pid2",colnames(final_data2allwB))]

### run second loop here 
polknow2_df <- data.frame()
for (i in 1:length(nov_index)) {
  print(i)
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### get title 
  ###
  testtab <- moe_topline(df = final_data2allwB, variable  =temp_var ,weight = final_weight)
  testtab <- as.data.frame(testtab) 
  ## we only need 1 
  testtab <- subset(testtab, Response==1)
  ## we will want to run round_preserve_sum() on the data 
  testtab[,2] <- round_preserve_sum(testtab[,2], 0)
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,5] <- round_preserve_sum(testtab[,5], 1)
  ### unnest 
  testtab2 <- unnest(testtab, Response)
  testtab2 <- as.data.frame(testtab2)
  colnames(testtab2)[1] <- "Correct"
  
  ## now, get the results subsetted, then save and test in the R markdown 
  reduced_tab <- testtab2[,c(1:3,5)]
  reduced_tab$Position <- polknow_vec[i]
  if(nrow(polknow2_df)==0){
    polknow2_df <- reduced_tab
  }else{
    polknow2_df <- rbind(polknow2_df,reduced_tab )
  }
}
polknow2_df <- polknow2_df[,c(5,1:4)]
### good, now export 
saveRDS(polknow2_df,"polknow/polknow2.rds")

###### Favorite Politician/Public Figure #####
## we also want to create a simplified version here of three cats
fav_vec <- c("Donald Trump", "Joe Biden", "Mike DeWine","Robert F Kennedy Jr.","Tim Ryan","JD Vance",
             "Mike Pence","Kamala Harris","Ron DeSantis","Sherrod Brown","Tim Scott","Niki Haley",
             "Chris Christie","Vivek Ramaswamy","Asa Hutchinson","Michelle Obama","LeBron James",
             "Frank LaRose","Matt Dolan","Bernie Moreno","Congressional Democrats",
             "Congressional Republicans","US Supreme Court","Ohio Leg. Democrats",
             "Ohio Leg. Republicans","Ohio Supreme Court")
fav_index <- names(final_data2allwB)[grepl("favorability",colnames(final_data2allwB))]
polfav_df <- data.frame()
for (i in 1:length(fav_index)) {
  print(i)
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==fav_index[i]]
  #### fix all of the favorability labels 
  final_data2allwB$temp_var[final_data2allwB$temp_var=="Strongly Favorable"] <- "Favorable"
  final_data2allwB$temp_var[final_data2allwB$temp_var=="Somewhat Favorable"] <- "Favorable"
  final_data2allwB$temp_var[final_data2allwB$temp_var=="Somewhat Unfavorable"] <- "Unfavorable"
  final_data2allwB$temp_var[final_data2allwB$temp_var=="Strongly Unfavorable"] <- "Unfavorable"
  final_data2allwB$temp_var[final_data2allwB$temp_var!="Unfavorable" &
                              final_data2allwB$temp_var!="Favorable"] <- "Neither/unsure"
  ###
  testtab <- moe_topline(df = final_data2allwB, variable  =temp_var ,weight = final_weight)
  testtab <- as.data.frame(testtab)
  testtab <- testtab[,2:6]
  ### change first field 
  testtab$Response <- NA
  testtab[1,6] <- "Favorable"
  testtab[2,6] <- "Not sure/Neutral"
  testtab[3,6] <- "Unfavorable"
  
  ## we will want to run round_preserve_sum() on the data 
  testtab[,1] <- round_preserve_sum(testtab[,1], 0)
  testtab[,2] <- round_preserve_sum(testtab[,2], 0)
  testtab[,4] <- round_preserve_sum(testtab[,4], 1)
  ## now, get the results subsetted, then save and test in the R markdown 
  reduced_tab <- testtab[,c(1,2,4,6)]
  reduced_tab$Person <- fav_vec[i]
  ##switch order 
  reduced_tab <- reduced_tab[,c(5,4,1,2,3)]
  
  if(nrow(polfav_df)==0){
    polfav_df <- reduced_tab
  }else{
    polfav_df <- rbind(polfav_df,reduced_tab )
  }
}
### now save 
saveRDS(polfav_df, "favorability/favorable_tab.rds")
polknow_tab1 <- readRDS("favorability/favorable_tab.rds")
#### Create net favorables ###
### we will want to drop neutral, then make wide 
polknow_tab1 <- subset(polfav_df, Response!="Not sure/Neutral")
polknow_tab1 <- subset(polknow_tab1, select=-c(Frequency, MOE))
polknow_tab1 <- polknow_tab1 %>% spread(Response, Percent)
## net 
polknow_tab1$net_fav <- polknow_tab1$Favorable-polknow_tab1$Unfavorable
saveRDS(polknow_tab1, "favorability/netfavorable_tab.rds")



###### August 8th primary questions ######
## we will do augp_dr_ma1_1 - augp_dr_ma1_7 as barplot; augp_ns_yn1 augp_dr_yn1
testtab <- moe_topline(df = final_data2allwB, variable  =augp_ns_yn1 ,weight = final_weight)
testtab <- as.data.frame(testtab)
testtab <- testtab[,c(1:3,5)]
testtab$Item <- "Voted in Aug. 8th Prim?"

## we will want to run round_preserve_sum() on the data 
testtab[,2] <- round_preserve_sum(testtab[,2], 0)
testtab[,3] <- round_preserve_sum(testtab[,3], 0)
testtab[,4] <- round_preserve_sum(testtab[,4], 1)
## now, get the results subsetted, then save and test in the R markdown 
reduced_tab <- testtab[,c(5,1:4)]
##now the other aug item 
testtab2 <- moe_topline(df = final_data2allwB, variable  =augp_dr_yn1 ,weight = final_weight)
testtab2 <- as.data.frame(testtab2)
testtab2 <- testtab2[,c(1:3,5)]
testtab2$Item <- "How voted in Aug. 8th Prim?"

## we will want to run round_preserve_sum() on the data 
testtab2[,2] <- round_preserve_sum(testtab2[,2], 0)
testtab2[,3] <- round_preserve_sum(testtab2[,3], 0)
testtab2[,4] <- round_preserve_sum(testtab2[,4], 1)
## now, get the results subsetted, then save and test in the R markdown 
reduced_tab2 <- testtab2[,c(5,1:4)]

## now combine 
reduced_tab <- rbind(reduced_tab,reduced_tab2)
##save 
saveRDS(reduced_tab, "augp/vote_behavior_aug.rds")
### The reason for voting a given way 
aug_reason_vec <- c("Const. overreach","Abortion","Personal autonomy","Rural rights","Partisanship","Other",
                    "Don't recall")
aug_index <-  names(final_data2allwB)[grepl("augp_dr_ma1",colnames(final_data2allwB))]
### create the index of values based on string matching ###
final_data2allwB$augp_dr_ma1_1 <- 0
final_data2allwB$augp_dr_ma1_2 <- 0
final_data2allwB$augp_dr_ma1_3 <- 0
final_data2allwB$augp_dr_ma1_4 <- 0
final_data2allwB$augp_dr_ma1_5 <- 0
final_data2allwB$augp_dr_ma1_6 <- 0
final_data2allwB$augp_dr_ma1_7 <- 0
### now code as 1 if present 
final_data2allwB$augp_dr_ma1_1[grepl("Constitutional overreach",final_data2allwB$augp_dr_ma1)] <- 1
final_data2allwB$augp_dr_ma1_2[grepl("Abortion",final_data2allwB$augp_dr_ma1)] <- 1
final_data2allwB$augp_dr_ma1_3[grepl("Personal autonomy",final_data2allwB$augp_dr_ma1)] <- 1
final_data2allwB$augp_dr_ma1_4[grepl("Rural rights",final_data2allwB$augp_dr_ma1)] <- 1
final_data2allwB$augp_dr_ma1_5[grepl("Partisanship",final_data2allwB$augp_dr_ma1)] <- 1
final_data2allwB$augp_dr_ma1_6[grepl("Other",final_data2allwB$augp_dr_ma1)] <- 1
final_data2allwB$augp_dr_ma1_7[grepl("Don't recall",final_data2allwB$augp_dr_ma1)] <- 1
# augp_ns_yn1

aug_index <-  names(final_data2allwB)[grepl("augp_dr_ma1",colnames(final_data2allwB))]
aug_index <- aug_index[2:8]
aug_reason_df <- data.frame()
## now the barplot for the reasons 
for (i in 1:length(aug_index)) {
  print(i)
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==aug_index[i]]
  ### subset if equal to 0 
  temp_df <- subset(final_data2allwB, augp_ns_yn1=="Yes")
  #final_data2allwB$temp_var[final_data2allwB$augp_ns_yn1==1 & is.na(final_data2allwB$temp_var)==T] <- 0 
  ###
  testtab <- moe_topline(df = temp_df, variable  =temp_var ,weight = final_weight)
  testtab <- as.data.frame(testtab,row.names = F) # really only need to keep two; recall the denom 
  testtab <- testtab[,c(1:3,5)]
  # colnames(testtab)[1] <- "Reason"
  ### change first field 
  testtab <- testtab[2,]
  testtab[,1] <- as.character(testtab[,1])
  row.names(testtab) <- aug_reason_vec[i]
  ## we will want to run round_preserve_sum() on the data 
  testtab[,2] <- round_preserve_sum(testtab[,2], 0)
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,4] <- round_preserve_sum(testtab[,4], 1)
  ## now, get the results subsetted, then save and test in the R markdown 
  
  if(nrow(polfav_df)==0){
    aug_reason_df <- testtab
  }else{
    aug_reason_df <- rbind(aug_reason_df,testtab )
  }
}
aug_reason_df$Response <- aug_reason_vec

### good, lets create the plot and table 
temp_barplot <- ggplot(aug_reason_df,aes(Response, Percent)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin=Percent-MOE, ymax=Percent+MOE), width=.2,
                position=position_dodge(.9)) + 
  theme_minimal() + ggtitle(title) + 
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 15)) +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.25)) +
  ggtitle("Reasons for voting in August 8th Primary")
table_temp <- paste0("aug_reason",sep="_","table")
bar_temp <- paste0("aug_reason",sep="_","barplot")

### command 
temp_png_tab <- paste0("augp",sep="/",table_temp,sep=".png")
temp_png_bar <- paste0("augp",sep="/",bar_temp,sep=".png")

png(temp_png_tab, width=7,height=10,units="in", res=400)       # Export PNG as table
p<-tableGrob(aug_reason_df,rows=NULL)
grid.arrange(temp_barplot,p)
#grid::grid.text(title,x = (0.5), y = (0.8))
#grid.table(testtab, rows=NULL)
dev.off() ## finished with this ! 


##### November Primary Interest Questions #####
## these are the novp qs 
nov_index <-  names(final_data2allwB)[grepl("novp",colnames(final_data2allwB))]
nov_q_items <- q_items[grep("novp",q_items$V1), ]
nrow(nov_q_items)==length(nov_index) # if true, good 

for (i in 1:length(nov_index)) {
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  title = nov_q_items$V2[nov_q_items==nov_index[i]]
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
  ###
  testtab <- moe_topline(df = final_data2allwB, variable  =temp_var ,weight = final_weight)
  testtab <- as.data.frame(testtab)
  testtab <- testtab[,c(1,2,3,5)]
  testtab[,2] <- round_preserve_sum(testtab[,2], 0)
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,4] <- round_preserve_sum(testtab[,4], 1)
  #colnames(testtab)[1] <- "Response"
  ### get factor attr
  testtab2 <- unnest(testtab, Response)
  testtab2 <- as.data.frame(testtab2)
  colnames(testtab2)[1] <- "Response"
  ### now get the plot 
  temp_barplot <- ggplot(testtab2 ,aes(x=Response, y=Percent)) +
    geom_bar(stat = "identity", position = "dodge") + 
    geom_errorbar(aes(ymin=Percent-MOE, ymax=Percent+MOE), width=.2,
                  position=position_dodge(.9)) + 
    theme_minimal() + ggtitle(title) + 
    scale_x_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15)) +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.25)) +
    ggtitle(title)
  ### now save to pdf 
  ## create names for plots and tables 
  table_temp <- paste0(temp_name,sep="_","table")
  bar_temp <- paste0(temp_name,sep="_","barplot")
  
  ### command 
  temp_png_tab <- paste0("novp",sep="/",table_temp,sep=".png")
  temp_png_bar <- paste0("novp",sep="/",bar_temp,sep=".png")
  
  png(temp_png_tab, width=7,height=10,units="in", res=400)       # Export PNG as table
  p<-tableGrob(testtab,rows=NULL)
  grid.arrange(temp_barplot,p)
  #grid::grid.text(title,x = (0.5), y = (0.8))
  #grid.table(testtab, rows=NULL)
  dev.off()
}
##### Create November Primary Voting interest variable ####
## These will be an index based upon: How much thought, enthusiasm, plan to vote, and casted a ballot. For 
# our purposes, lets look at thought and enthusiasm, then weight based upon plan to vote. 
table(final_data2allwB$novp_ns_lot1) # None      Not sure Only a little   Quite a lot          Some; go with 
# None and not sure as 0 
table(final_data2allwB$novp_na_ent1) # Not at all enthusiastic     Not so enthusiastic   Somewhat enthusiastic    
# Very enthusiastic 
### index for thoughts here ; 1
final_data2allwB$nov_index1 <- 0
final_data2allwB$nov_index1[final_data2allwB$novp_ns_lot1=="Only a little"] <- 1
final_data2allwB$nov_index1[final_data2allwB$novp_ns_lot1=="Some"] <- 2
final_data2allwB$nov_index1[final_data2allwB$novp_ns_lot1=="Quite a lot"] <- 3
### index for enthusiasm here 
final_data2allwB$nov_index2 <- 0
final_data2allwB$nov_index2[final_data2allwB$novp_na_ent1=="Not so enthusiastic"] <- 1
final_data2allwB$nov_index2[final_data2allwB$novp_na_ent1=="Somewhat enthusiastic"] <- 2
final_data2allwB$nov_index2[final_data2allwB$novp_na_ent1=="Very enthusiastic"] <- 3
### now sum and normalize 
final_data2allwB$nov_interest <- (final_data2allwB$nov_index1+final_data2allwB$nov_index2)/6
table(final_data2allwB$nov_interest) # 190 are most excited
table(final_data2allwB$novp_dk_yn1)
### now weight by plan to vote weight ; we can adjust weights later 
final_data2allwB$vote_plan_weight <- 0
final_data2allwB$vote_plan_weight[final_data2allwB$novp_dk_yn1=="Yes"] <- 1
final_data2allwB$vote_plan_weight[final_data2allwB$novp_dk_yn1=="Don't know"] <- 0.5


### now, lets apply 
final_data2allwB$nov_interest <- final_data2allwB$nov_interest*final_data2allwB$vote_plan_weight
summary(final_data2allwB$nov_interest)



###### Abortion Issue 1 Survey Experiment #### 
### now, lets do the analysis by the treatments of interest 
nov_index <-  names(final_data2allwB)[grepl("novi",colnames(final_data2allwB))]
nov_q_items <- q_items[grep("novi",q_items$V1), ]
nrow(nov_q_items)==length(nov_index) # if true, good 
## create vector 
nov_title_vec <- c("Have you heard much about the November 7th issue election?",
                   "Do you agree with the proposed amendment? -- Referendum lang.",
                   "Do you plan to vote Yes or No on Issue 1? -- Referendum lang.",
                   "Do you agree with the proposed amendment? -- League of Women's voters lang.",
                   "Do you plan to vote Yes or No on Issue 1? -- League of Women's voters lang.")

### run a loop on these; might create a vector for the q_items 
for (i in 1:length(nov_index)) {
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### subset those who do not want to vote 
  temp_df <- subset(final_data2allwB, nov_interest >= 0.5)
  #temp_df <- subset(final_data2allwB, novp_dk_yn1== "Yes" & novp_na_ent1 != "Not at all enthusiastic")
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  title = nov_title_vec[i]
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
  ## subset
  temp_df <- subset(temp_df, temp_var!="")
  ### need to search here for disagree 
  if(str_detect(nov_index[i],"_agr")==T){
    temp_df$temp_var[temp_df$temp_var=="Strongly agree"] <- "Agree"
    temp_df$temp_var[temp_df$temp_var=="Somewhat agree"] <- "Agree"
    #temp_df$temp_var[grep("Don", temp_df$temp_var)] <- "Neither/Don't know"
    temp_df$temp_var[temp_df$temp_var=="Strongly disagree"] <- "Disagree"
    temp_df$temp_var[temp_df$temp_var=="Somewhat disagree"] <- "Disagree"
    temp_df$temp_var[temp_df$temp_var!="Agree" & temp_df$temp_var!="Disagree"] <- "Neither/Don't know"
  }
  
  ###
  testtab <- moe_topline(df = temp_df, variable  =temp_var ,weight = final_weight)
  testtab <- as.data.frame(testtab)
  testtab <- testtab[,c(1,2,3,5)]
  testtab[,2] <- round_preserve_sum(testtab[,2], 0)
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,4] <- round_preserve_sum(testtab[,4], 1)
  #colnames(testtab)[1] <- "Response"
  ### get factor attr
  testtab2 <- unnest(testtab, Response)
  testtab2 <- as.data.frame(testtab2)
  colnames(testtab2)[1] <- "Response"
  ## get rid of empty fields 
  testtab2 <- subset(testtab2, Response!="")
  ### now get the plot 
  temp_barplot <- ggplot(testtab2 ,aes(x=Response, y=Percent)) +
    geom_bar(stat = "identity", position = "dodge") + 
    geom_errorbar(aes(ymin=Percent-MOE, ymax=Percent+MOE), width=.2,
                  position=position_dodge(.9)) + 
    theme_minimal() + ggtitle(title) + 
    scale_x_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15)) +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.25)) +
    ggtitle(title)
  ### now save to pdf 
  ## create names for plots and tables 
  table_temp <- paste0(temp_name,sep="_","table")
  bar_temp <- paste0(temp_name,sep="_","barplot")
  
  ### command 
  temp_png_tab <- paste0("novi",sep="/",table_temp,sep=".png")
  temp_png_bar <- paste0("novi",sep="/",bar_temp,sep=".png")
  
  png(temp_png_tab, width=7,height=10,units="in", res=400)       # Export PNG as table
  p<-tableGrob(testtab2,rows=NULL)
  grid.arrange(temp_barplot,p)
  #grid::grid.text(title,x = (0.5), y = (0.8))
  #grid.table(testtab, rows=NULL)
  dev.off()
}


##### General Marijuana Questions ####
nov_index <-  names(final_data2allwB)[grepl("mjiss",colnames(final_data2allwB))]
nov_q_items <- q_items[grep("mjiss",q_items$V1), ]
nrow(nov_q_items)==length(nov_index) # if true, good 

### run a loop on these; might create a vector for the q_items 
for (i in 1:length(nov_index)) {
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### subset those who do not want to vote 
  temp_df <- subset(final_data2allwB, novp_dk_yn1!= "No")
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  title = nov_q_items$V2[nov_q_items==nov_index[i]]
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
  title <- str_replace(title, "Waley", "Whaley")
  title <- str_replace(title,"right direction", "right track" )
  ## subset
  temp_df <- subset(temp_df, temp_var!="")
  ### need to search here for disagree 
  if(str_detect(unique(temp_df$temp_var)[1],"agree")==T){
    temp_df$temp_var[temp_df$temp_var=="Strongly agree"] <- "Agree"
    temp_df$temp_var[temp_df$temp_var=="Somewhat agree"] <- "Agree"
    #temp_df$temp_var[grep("Don", temp_df$temp_var)] <- "Neither/Don't know"
    temp_df$temp_var[temp_df$temp_var=="Strongly disagree"] <- "Disagree"
    temp_df$temp_var[temp_df$temp_var=="Somewhat disagree"] <- "Disagree"
    temp_df$temp_var[temp_df$temp_var!="Agree" & temp_df$temp_var!="Disagree"] <- "Neither/Don't know"
    
    
    
  }
  
  ###
  testtab <- moe_topline(df = temp_df, variable  =temp_var ,weight = final_weight)
  testtab <- as.data.frame(testtab)
  testtab <- testtab[,c(1,2,3,5)]
  testtab[,2] <- round_preserve_sum(testtab[,2], 0)
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,4] <- round_preserve_sum(testtab[,4], 1)
  #colnames(testtab)[1] <- "Response"
  ### get factor attr
  testtab2 <- unnest(testtab, Response)
  testtab2 <- as.data.frame(testtab2)
  colnames(testtab2)[1] <- "Response"
  ## get rid of empty fields 
  testtab2 <- subset(testtab2, Response!="")
  ### now get the plot 
  temp_barplot <- ggplot(testtab2 ,aes(x=Response, y=Percent)) +
    geom_bar(stat = "identity", position = "dodge") + 
    geom_errorbar(aes(ymin=Percent-MOE, ymax=Percent+MOE), width=.2,
                  position=position_dodge(.9)) + 
    theme_minimal() + ggtitle(title) + 
    scale_x_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15)) +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.25)) +
    ggtitle(title)
  ### now save to pdf 
  ## create names for plots and tables 
  table_temp <- paste0(temp_name,sep="_","table")
  bar_temp <- paste0(temp_name,sep="_","barplot")
  
  ### command 
  temp_png_tab <- paste0("abortion",sep="/",table_temp,sep=".png")
  temp_png_bar <- paste0("abortion",sep="/",bar_temp,sep=".png")
  
  png(temp_png_tab, width=7,height=10,units="in", res=400)       # Export PNG as table
  p<-tableGrob(testtab,rows=NULL)
  grid.arrange(temp_barplot,p)
  #grid::grid.text(title,x = (0.5), y = (0.8))
  #grid.table(testtab, rows=NULL)
  dev.off()
}

##### Abortion related questions #### 
nov_index <-  names(final_data2allwB)[grepl("abortion",colnames(final_data2allwB))]
nov_q_items <- q_items[grep("abortion",q_items$V1), ]
nrow(nov_q_items)==length(nov_index) # if true, good 

### run a loop on these; might create a vector for the q_items 
for (i in 1:length(nov_index)) {
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### subset those who do not want to vote 
  # temp_df <- subset(final_data2allwB, novp_dk_yn1!= "No")
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  title = nov_q_items$V2[nov_q_items==nov_index[i]]
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
  ## subset
  temp_df <- subset(final_data2allwB, novp_dk_yn1!= "No")
  ### need to search here for disagree 
  if(str_detect(unique(temp_df$temp_var)[1],"agree")==T){
    temp_df$temp_var[temp_df$temp_var=="Strongly agree"] <- "Agree"
    temp_df$temp_var[temp_df$temp_var=="Somewhat agree"] <- "Agree"
    #temp_df$temp_var[grep("Don", temp_df$temp_var)] <- "Neither/Don't know"
    temp_df$temp_var[temp_df$temp_var=="Strongly disagree"] <- "Disagree"
    temp_df$temp_var[temp_df$temp_var=="Somewhat disagree"] <- "Disagree"
    temp_df$temp_var[temp_df$temp_var!="Agree" & temp_df$temp_var!="Disagree"] <- "Neither/Don't know"
  }
  if(str_detect(temp_df$temp_var[1],"legal")==T){
    temp_df$temp_var[temp_df$temp_var=="It should always be illegal"] <- "Illegal"
    temp_df$temp_var[temp_df$temp_var=="It should always be legal"] <- "Legal"
    temp_df$temp_var[temp_df$temp_var=="It should be mostly illegal"] <- "Illegal"
    temp_df$temp_var[temp_df$temp_var=="It should be mostly legal"] <- "Legal"
  }
  
  ###
  testtab <- moe_topline(df = temp_df, variable =temp_var ,weight = final_weight)
  testtab <- as.data.frame(testtab)
  testtab <- testtab[,c(1,2,3,5)]
  testtab[,2] <- round_preserve_sum(testtab[,2], 0)
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,4] <- round_preserve_sum(testtab[,4], 1)
  #colnames(testtab)[1] <- "Response"
  ### get factor attr
  testtab2 <- unnest(testtab, Response)
  testtab2 <- as.data.frame(testtab2)
  colnames(testtab2)[1] <- "Response"
  ## get rid of empty fields 
  testtab2 <- subset(testtab2, Response!="")
  ### now get the plot 
  temp_barplot <- ggplot(testtab2 ,aes(x=Response, y=Percent)) +
    geom_bar(stat = "identity", position = "dodge") + 
    geom_errorbar(aes(ymin=Percent-MOE, ymax=Percent+MOE), width=.2,
                  position=position_dodge(.9)) + 
    theme_minimal() + ggtitle(title) + 
    scale_x_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15)) +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.25)) +
    ggtitle(title)
  ### now save to pdf 
  ## create names for plots and tables 
  table_temp <- paste0(temp_name,sep="_","table")
  bar_temp <- paste0(temp_name,sep="_","barplot")
  
  ### command 
  temp_png_tab <- paste0("abortion",sep="/",table_temp,sep=".png")
  temp_png_bar <- paste0("abortion",sep="/",bar_temp,sep=".png")
  
  png(temp_png_tab, width=7,height=10,units="in", res=400)       # Export PNG as table
  p<-tableGrob(testtab,rows=NULL)
  grid.arrange(temp_barplot,p)
  #grid::grid.text(title,x = (0.5), y = (0.8))
  #grid.table(testtab, rows=NULL)
  dev.off()
}
### create folders for pie_charts 
main_dir <- getwd()


output_dir <- file.path(main_dir, "pie_charts")
if (!dir.exists(output_dir)){
  dir.create(output_dir)
  } else {
    print("Dir already exists!")
  }

### abortion pie charts 

for (i in 1:length(nov_index)) {
  print(i)
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### subset those who do not want to vote 
  temp_df <- subset(final_data2allwB, novp_dk_yn1!= "No")
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  title = nov_q_items$V2[i]
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
  ## subset
  temp_df <- subset(temp_df, temp_var!="")
  
  ### need to search here for disagree 
  if(str_detect(temp_df$temp_var[1],"agree")==T){
    temp_df$temp_var[temp_df$temp_var=="Strongly agree"] <- "Agree"
    temp_df$temp_var[temp_df$temp_var=="Somewhat agree"] <- "Agree"
    #temp_df$temp_var[grep("Don", temp_df$temp_var)] <- "Neither/Don't know"
    temp_df$temp_var[temp_df$temp_var=="Strongly disagree"] <- "Disagree"
    temp_df$temp_var[temp_df$temp_var=="Somewhat disagree"] <- "Disagree"
    temp_df$temp_var[temp_df$temp_var!="Agree" & temp_df$temp_var!="Disagree"] <- "Neither/Don't know"
  }
  ### check for legal 
  if(str_detect(temp_df$temp_var[1],"legal")==T){
    temp_df$temp_var[temp_df$temp_var=="It should always be illegal"] <- "Illegal"
    temp_df$temp_var[temp_df$temp_var=="It should always be legal"] <- "Legal"
    temp_df$temp_var[temp_df$temp_var=="It should be mostly illegal"] <- "Illegal"
    temp_df$temp_var[temp_df$temp_var=="It should be mostly legal"] <- "Legal"
  }
  
  ### collapse items 
  testtab <- moe_topline(df = temp_df, variable  =temp_var ,weight = final_weight)
  testtab <- as.data.frame(testtab)
  testtab <- testtab[,c(1,2,3,5)]
  testtab[,2] <- round_preserve_sum(testtab[,2], 0)
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,4] <- round_preserve_sum(testtab[,4], 1)
  #colnames(testtab)[1] <- "Response"
  ### get factor attr
  testtab2 <- unnest(testtab, Response)
  testtab2 <- as.data.frame(testtab2)
  colnames(testtab2)[1] <- "Response"
  ## get rid of empty fields 
  testtab2 <- subset(testtab2, Response!="")
  ## we have the pcts, so we can collapse into bar chart 
  ### create coords 
  testtab2 <- testtab2 %>% 
    mutate(csum = rev(cumsum(rev(Percent))), 
           pos = Percent/2 + lead(csum, 1),
           pos = if_else(is.na(pos), Percent/2, pos))
  ## subset colors 
  onu_color_vec_sub <- onu_color_vec[1:nrow(testtab2)]
  
  ### now get the plot 
  temp_pirchart <- ggplot(data=testtab2 , aes(x="",  y=Percent, fill=Response )) +
    geom_bar(stat="identity", width=1, color=onu_color_vec_sub) + coord_polar("y", start=0) +
    geom_label_repel(data = testtab2,
                     aes(y = pos, label = paste0(Percent, "%")),
                     size = 4.5, nudge_x = 0.25, show.legend = FALSE)+
    scale_fill_manual(values=onu_color_vec_sub) + 
    guides(fill = guide_legend(title = "Response")) +
    theme_void() +
    theme(strip.text.x = element_text(size = 15)) +
    ggtitle(title)
  ### now save to pdf 
  ## create names for plots and tables 
  pie_temp <- paste0(temp_name,sep="_","piechart")
  
  ### command 
  temp_png_pie <- paste0("pie_charts/abortion",sep="/",pie_temp,sep=".png")
  ggsave(temp_png_pie ,plot=temp_pirchart, scale=1,width=7,height=5,units = c("in"),dpi=400,
         bg="white")
  
}


##### Drug attitude questions -- matrix  ##### 
### knowledge
nov_index <-  names(final_data2allwB)[grepl("drugat_ns",colnames(final_data2allwB))]
nov_q_items <- q_items[grep("drugat_ns",q_items$V1), ]
nrow(nov_q_items)==length(nov_index) # if true, good 

### run a loop on these; might create a vector for the q_items 
for (i in 1:length(nov_index)) {
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### subset those who do not want to vote 
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  title = nov_q_items$V2[nov_q_items==nov_index[i]]
  if(nov_index[i]=="mjiss_ns_sup"){
    title="Do you support or oppose Issue 2?"
  }
  
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
  title <- str_replace(title, "Waley", "Whaley")
  title <- str_replace(title,"right direction", "right track" )
  ## subset
  temp_df <- subset(final_data2allwB, temp_var!="")
  ### need to search here for disagree 
  if(str_detect(unique(temp_df$temp_var)[1],"agree")==T){
    temp_df$temp_var[temp_df$temp_var=="Strongly agree"] <- "Agree"
    temp_df$temp_var[temp_df$temp_var=="Somewhat agree"] <- "Agree"
    #temp_df$temp_var[grep("Don", temp_df$temp_var)] <- "Neither/Don't know"
    temp_df$temp_var[temp_df$temp_var=="Strongly disagree"] <- "Disagree"
    temp_df$temp_var[temp_df$temp_var=="Somewhat disagree"] <- "Disagree"
    temp_df$temp_var[temp_df$temp_var!="Agree" & temp_df$temp_var!="Disagree"] <- "Neither/Don't know"
  }
  
  ###
  testtab <- moe_topline(df = temp_df, variable  =temp_var ,weight = final_weight)
  testtab <- as.data.frame(testtab)
  testtab <- testtab[,c(1,2,3,5)]
  testtab[,2] <- round_preserve_sum(testtab[,2], 0)
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,4] <- round_preserve_sum(testtab[,4], 1)
  #colnames(testtab)[1] <- "Response"
  ### get factor attr
  testtab2 <- unnest(testtab, Response)
  testtab2 <- as.data.frame(testtab2)
  colnames(testtab2)[1] <- "Response"
  ## get rid of empty fields 
  testtab2 <- subset(testtab2, Response!="")
  ## rearrange for drug attitude 
  testtab2$index <- 1
  testtab2$index[testtab2$Response=="Somewhat knowledgeable"] <- 2
  testtab2$index[testtab2$Response=="Knowledgeable"] <- 3
  testtab2$index[testtab2$Response=="Very knowledgeable"] <- 4
  ## now arrange 
  testtab2 <- testtab2[order(testtab2$index), ]
  testtab2 <- subset(testtab2, select=-c(index))
  ### now get the plot 
  temp_barplot <- ggplot(testtab2 ,aes(x=Response, y=Percent)) +
    geom_bar(stat = "identity", position = "dodge") + 
    geom_errorbar(aes(ymin=Percent-MOE, ymax=Percent+MOE), width=.2,
                  position=position_dodge(.9)) + 
    theme_minimal() + ggtitle(title) + 
    scale_x_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15)) +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.25)) +
    ggtitle(title)
  ### now save to pdf 
  ## create names for plots and tables 
  table_temp <- paste0(temp_name,sep="_","table")
  bar_temp <- paste0(temp_name,sep="_","barplot")
  
  ### command 
  temp_png_tab <- paste0("drugat",sep="/",table_temp,sep=".png")
  temp_png_bar <- paste0("drugat",sep="/",bar_temp,sep=".png")
  
  png(temp_png_tab, width=7,height=10,units="in", res=400)       # Export PNG as table
  p<-tableGrob(testtab,rows=NULL)
  grid.arrange(temp_barplot,p)
  #grid::grid.text(title,x = (0.5), y = (0.8))
  #grid.table(testtab, rows=NULL)
  dev.off()
}
###### drug legalization ####
nov_index <-  names(final_data2allwB)[grepl("drugat",colnames(final_data2allwB))]
nov_q_items <- q_items[grep("drugat",q_items$V1), ]
nrow(nov_q_items)==length(nov_index) # if true, good 

drugatt_df <- data.frame()
for (i in 1:length(nov_index)) {
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### subset those who do not want to vote 
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  title = nov_q_items$V2[i]
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
  
  ## subset
  temp_df <- subset(final_data2allwB, temp_var!="")
  
  ### reduce the categories 
  if(str_detect(nov_index[i],"know")){
    temp_df$temp_var[temp_df$temp_var!="Not knowledgeable"] <- "Knowledgeable"
  }
  if(str_detect(temp_df$temp_var[1],"agree")==T){
    temp_df$temp_var[temp_df$temp_var=="Strongly agree"] <- "Agree"
    temp_df$temp_var[temp_df$temp_var=="Somewhat agree"] <- "Agree"
    #temp_df$temp_var[grep("Don", temp_df$temp_var)] <- "Neither/Don't know"
    temp_df$temp_var[temp_df$temp_var=="Strongly disagree"] <- "Disagree"
    temp_df$temp_var[temp_df$temp_var=="Somewhat disagree"] <- "Disagree"
    temp_df$temp_var[temp_df$temp_var!="Agree" & temp_df$temp_var!="Disagree"] <- "Neither/Don't know"
  }
  
  ### moe command 
  testtab <- moe_topline(df = temp_df, variable  =temp_var ,weight = final_weight)
  testtab <- as.data.frame(testtab) 
  
  ## we will want to run round_preserve_sum() on the data 
  testtab[,2] <- round_preserve_sum(testtab[,2], 0)
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,5] <- round_preserve_sum(testtab[,5], 1)
  ## now, get the results subsetted, then save and test in the R markdown 
  reduced_tab <- testtab[,c(1:3,5)]
  if(nrow(drugatt_df)==0){
    drugatt_df <- reduced_tab
  }else{
    drugatt_df <- rbind(drugatt_df,reduced_tab )
  }
}
# drugatt_df <- drugatt_df[,c(5,1:4)]
## now change name 
drugatt_df <- unnest(drugatt_df, Response)
drugatt_df <- as.data.frame(drugatt_df)
colnames(drugatt_df)[1] <- "Response"
colnames(drugatt_df)[2] <- "Num"
colnames(drugatt_df)[3] <- "Pct"

## split 
drug_know_df <- drugatt_df[1:24,]
drug_pref_df <- drugatt_df[25:60,]
### create folders 
###### drug legalization ####
nov_index <-  names(final_data2allwB)[grepl("drugat",colnames(final_data2allwB))]
nov_q_items <- q_items[grep("drugat",q_items$V1), ]
nrow(nov_q_items)==length(nov_index) # if true, good 

drugatt_df <- data.frame()
for (i in 1:length(nov_index)) {
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### subset those who do not want to vote 
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  title = nov_q_items$V2[i]
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
  
  ## subset
  temp_df <- subset(final_data2allwB, temp_var!="")
  
  ### reduce the categories 
  if(str_detect(nov_index[i],"know")){
    temp_df$temp_var[temp_df$temp_var!="Not knowledgeable"] <- "Knowledgeable"
  }
  if(str_detect(temp_df$temp_var[1],"agree")==T){
    temp_df$temp_var[temp_df$temp_var=="Strongly agree"] <- "Agree"
    temp_df$temp_var[temp_df$temp_var=="Somewhat agree"] <- "Agree"
    #temp_df$temp_var[grep("Don", temp_df$temp_var)] <- "Neither/Don't know"
    temp_df$temp_var[temp_df$temp_var=="Strongly disagree"] <- "Disagree"
    temp_df$temp_var[temp_df$temp_var=="Somewhat disagree"] <- "Disagree"
    temp_df$temp_var[temp_df$temp_var!="Agree" & temp_df$temp_var!="Disagree"] <- "Neither/Don't know"
  }

  ### moe command 
  testtab <- moe_topline(df = temp_df, variable  =temp_var ,weight = final_weight)
  testtab <- as.data.frame(testtab) 

  ## we will want to run round_preserve_sum() on the data 
  testtab[,2] <- round_preserve_sum(testtab[,2], 0)
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,5] <- round_preserve_sum(testtab[,5], 1)
  ## now, get the results subsetted, then save and test in the R markdown 
  reduced_tab <- testtab[,c(1:3,5)]
  if(nrow(drugatt_df)==0){
    drugatt_df <- reduced_tab
  }else{
    drugatt_df <- rbind(drugatt_df,reduced_tab )
  }
}
# drugatt_df <- drugatt_df[,c(5,1:4)]
## now change name 
drugatt_df <- unnest(drugatt_df, Response)
drugatt_df <- as.data.frame(drugatt_df)
colnames(drugatt_df)[1] <- "Response"
colnames(drugatt_df)[2] <- "Num"
colnames(drugatt_df)[3] <- "Pct"

### create folders 
main_dir <- getwd()


output_dir <- file.path(main_dir, "drugat_tab")
if (!dir.exists(output_dir)){
    dir.create(output_dir)
  } else {
    print("Dir already exists!")
}



## split 
drug_know_df <- drugatt_df[1:24,]
drug_pref_df <- drugatt_df[25:60,]
### good, now export 
saveRDS(drug_know_df,"drugat_tab/drug_know.rds")
saveRDS(drug_pref_df,"drugat_tab/drug_pref.rds")

## make sure primary folder is present 
output_dir <- file.path(main_dir, "primary")
if (!dir.exists(output_dir)){
  dir.create(output_dir)
} else {
  print("Dir already exists!")
}

##### Primary pivot questions #####
nov_index <-  names(final_data2allwB)[grepl("primary",colnames(final_data2allwB))]
nov_index <- nov_index[!grepl("TEXT", nov_index)]
nov_q_items <- q_items[grep("primary",q_items$V1), ]
nov_q_items <- nov_q_items[!grepl("TEXT",nov_q_items$V1 ), ]
nrow(nov_q_items)==length(nov_index) # if true, good 
## #fix primary interest 
final_data2allwB$primary.interest[final_data2allwB$primary.interest=="I do not plan to vote in either the Democratic or Republican primary election"] <-
  "I do not plan to vote in either"
final_data2allwB$primary.interest[final_data2allwB$primary.interest=="Yes, I plan to vote in the Democratic primary election"] <-
  "Yes, I plan to vote in the Democratic primary"
final_data2allwB$primary.interest[final_data2allwB$primary.interest=="Yes, I plan to vote in the Republican primary election"] <-
  "Yes, I plan to vote in the Republican primary"

### run a loop on these; might create a vector for the q_items 
for (i in 1:length(nov_index)) {
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### subset those who do not want to vote 
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  title = nov_q_items$V2[i]
  
  if(nov_index[i]=="mjiss_ns_sup"){
    title="Do you support or oppose Issue 2?"
  }
  
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
  title <- str_replace(title, "Waley", "Whaley")
  title <- str_replace(title,"right direction", "right track" )
  ## subset
  temp_df <- subset(final_data2allwB, temp_var!="")
  ### need to search here for disagree 
  if(str_detect(unique(temp_df$temp_var)[1],"agree")==T){
    temp_df$temp_var[temp_df$temp_var=="Strongly agree"] <- "Agree"
    temp_df$temp_var[temp_df$temp_var=="Somewhat agree"] <- "Agree"
    #temp_df$temp_var[grep("Don", temp_df$temp_var)] <- "Neither/Don't know"
    temp_df$temp_var[temp_df$temp_var=="Strongly disagree"] <- "Disagree"
    temp_df$temp_var[temp_df$temp_var=="Somewhat disagree"] <- "Disagree"
    temp_df$temp_var[temp_df$temp_var!="Agree" & temp_df$temp_var!="Disagree"] <- "Neither/Don't know"
  }
  
  ###
  testtab <- moe_topline(df = temp_df, variable  =temp_var ,weight = final_weight)
  testtab <- as.data.frame(testtab)
  testtab <- testtab[,c(1,2,3,5)]
  testtab[,2] <- round_preserve_sum(testtab[,2], 0)
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,4] <- round_preserve_sum(testtab[,4], 1)
  #colnames(testtab)[1] <- "Response"
  ### get factor attr
  testtab2 <- unnest(testtab, Response)
  testtab2 <- as.data.frame(testtab2)
  colnames(testtab2)[1] <- "Response"
  ## get rid of empty fields 
  testtab2 <- subset(testtab2, Response!="")

  ### now get the plot 
  temp_barplot <- ggplot(testtab2 ,aes(x=Response, y=Percent)) +
    geom_bar(stat = "identity", position = "dodge") + 
    geom_errorbar(aes(ymin=Percent-MOE, ymax=Percent+MOE), width=.2,
                  position=position_dodge(.9)) + 
    theme_minimal() + ggtitle(title) + 
    scale_x_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15)) +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.25)) +
    ggtitle(title)
  ### now save to pdf 
  ## create names for plots and tables 
  table_temp <- paste0(temp_name,sep="_","table")
  bar_temp <- paste0(temp_name,sep="_","barplot")
  
  ### command 
  temp_png_tab <- paste0("primary",sep="/",table_temp,sep=".png")
  temp_png_bar <- paste0("primary",sep="/",bar_temp,sep=".png")
  
  png(temp_png_tab, width=7,height=10,units="in", res=400)       # Export PNG as table
  p<-tableGrob(testtab,rows=NULL)
  grid.arrange(temp_barplot,p)
  #grid::grid.text(title,x = (0.5), y = (0.8))
  #grid.table(testtab, rows=NULL)
  dev.off()
}

##### Now trump related questions ####
nov_index <-  names(final_data2allwB)[grepl("trump",colnames(final_data2allwB))]
nov_index <- nov_index[!grepl("favorability_trump", nov_index)]
nov_q_items <- q_items[grep("trump",q_items$V1), ]
nov_q_items <- nov_q_items[!grepl("favorability_trump",nov_q_items$V1 ), ]
nrow(nov_q_items)==length(nov_index) # if true, good 

### run a loop on these; might create a vector for the q_items 
for (i in 1:length(nov_index)) {
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### subset those who do not want to vote 
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  title = nov_q_items$V2[nov_q_items==nov_index[i]]
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
  ## subset
  temp_df <- subset(final_data2allwB, temp_var!="")
  ### need to search here for disagree 
  if(str_detect(unique(temp_df$temp_var)[1],"likely")==T){
    temp_df$temp_var[temp_df$temp_var=="Very likely"] <- "Likely"
    temp_df$temp_var[temp_df$temp_var=="Likely"] <- "Likely"
    #temp_df$temp_var[grep("Don", temp_df$temp_var)] <- "Neither/Don't know"
    temp_df$temp_var[temp_df$temp_var=="Unlikely"] <- "Unlikely"
    temp_df$temp_var[temp_df$temp_var=="Very unlikely"] <- "Unlikely"
    temp_df$temp_var[temp_df$temp_var!="Likely" & temp_df$temp_var!="Unlikely"] <- "Not sure"
  }
  
  ###
  testtab <- moe_topline(df = temp_df, variable  =temp_var ,weight = final_weight)
  testtab <- as.data.frame(testtab)
  testtab <- testtab[,c(1,2,3,5)]
  testtab[,2] <- round_preserve_sum(testtab[,2], 0)
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,4] <- round_preserve_sum(testtab[,4], 1)
  #colnames(testtab)[1] <- "Response"
  ### get factor attr
  testtab2 <- unnest(testtab, Response)
  testtab2 <- as.data.frame(testtab2)
  colnames(testtab2)[1] <- "Response"
  ## get rid of empty fields 
  testtab2 <- subset(testtab2, Response!="")
  
  ### now get the plot 
  temp_barplot <- ggplot(testtab2 ,aes(x=Response, y=Percent)) +
    geom_bar(stat = "identity", position = "dodge") + 
    geom_errorbar(aes(ymin=Percent-MOE, ymax=Percent+MOE), width=.2,
                  position=position_dodge(.9)) + 
    theme_minimal() + ggtitle(title) + 
    scale_x_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15)) +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.25)) +
    ggtitle(title)
  ### now save to pdf 
  ## create names for plots and tables 
  table_temp <- paste0(temp_name,sep="_","table")
  bar_temp <- paste0(temp_name,sep="_","barplot")
  testtab2$Response <- sapply(lapply(testtab2$Response, strwrap, width=50), paste, collapse="\n")
  ### command 
  temp_png_tab <- paste0("trump",sep="/",table_temp,sep=".png")
  temp_png_bar <- paste0("trump",sep="/",bar_temp,sep=".png")
  
  png(temp_png_tab, width=7,height=10,units="in", res=400)       # Export PNG as table
  p<-tableGrob(testtab2,rows=NULL)
  
  grid.arrange(temp_barplot,p)
  #grid::grid.text(title,x = (0.5), y = (0.8))
  #grid.table(testtab, rows=NULL)
  dev.off()
}

##### Senate Match Ups #####
nov_index <-  names(final_data2allwB)[grepl("Senate",colnames(final_data2allwB))]
nov_index <- nov_index[!grepl("TEXT", nov_index)]
nov_q_items <- q_items[grep("Senate",q_items$V1), ]
nov_q_items <- nov_q_items[!grepl("TEXT",nov_q_items$V1 ), ]
nrow(nov_q_items)==length(nov_index) # if true, good 

### run a loop on these; might create a vector for the q_items 
for (i in 1:length(nov_index)) {
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### subset those who do not want to vote 
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  title = nov_q_items$V2[i]
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
  title <- str_replace(title, "Waley", "Whaley")
  title <- str_replace(title,"right direction", "right track" )
  ## subset
  temp_df <- subset(final_data2allwB, temp_var!="")
  ### need to search here for disagree 
  if(str_detect(unique(temp_df$temp_var)[1],"agree")==T){
    temp_df$temp_var[temp_df$temp_var=="Strongly agree"] <- "Agree"
    temp_df$temp_var[temp_df$temp_var=="Somewhat agree"] <- "Agree"
    #temp_df$temp_var[grep("Don", temp_df$temp_var)] <- "Neither/Don't know"
    temp_df$temp_var[temp_df$temp_var=="Strongly disagree"] <- "Disagree"
    temp_df$temp_var[temp_df$temp_var=="Somewhat disagree"] <- "Disagree"
    temp_df$temp_var[temp_df$temp_var!="Agree" & temp_df$temp_var!="Disagree"] <- "Neither/Don't know"
  }
  
  ###
  testtab <- moe_topline(df = temp_df, variable  =temp_var ,weight = final_weight)
  testtab <- as.data.frame(testtab)
  testtab <- testtab[,c(1,2,3,5)]
  testtab[,2] <- round_preserve_sum(testtab[,2], 0)
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,4] <- round_preserve_sum(testtab[,4], 1)
  #colnames(testtab)[1] <- "Response"
  ### get factor attr
  testtab2 <- unnest(testtab, Response)
  testtab2 <- as.data.frame(testtab2)
  colnames(testtab2)[1] <- "Response"
  ## get rid of empty fields 
  testtab2 <- subset(testtab2, Response!="")
  
  ### now get the plot 
  temp_barplot <- ggplot(testtab2 ,aes(x=Response, y=Percent)) +
    geom_bar(stat = "identity", position = "dodge") + 
    geom_errorbar(aes(ymin=Percent-MOE, ymax=Percent+MOE), width=.2,
                  position=position_dodge(.9)) + 
    theme_minimal() + ggtitle(title) + 
    scale_x_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15)) +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.25)) +
    ggtitle(title)
  ### now save to pdf 
  ## create names for plots and tables 
  table_temp <- paste0(temp_name,sep="_","table")
  bar_temp <- paste0(temp_name,sep="_","barplot")
  
  ### command 
  temp_png_tab <- paste0("senate",sep="/",table_temp,sep=".png")
  temp_png_bar <- paste0("senate",sep="/",bar_temp,sep=".png")
  
  png(temp_png_tab, width=7,height=10,units="in", res=400)       # Export PNG as table
  p<-tableGrob(testtab2,rows=NULL)
  grid.arrange(temp_barplot,p)
  #grid::grid.text(title,x = (0.5), y = (0.8))
  #grid.table(testtab, rows=NULL)
  dev.off()
}

##### Presidential Matchups #####
nov_index <-  names(final_data2allwB)[grepl("potus",colnames(final_data2allwB))]
nov_index <- nov_index[!grepl("TEXT", nov_index)]
nov_index <- nov_index[!grepl("primary", nov_index)]
nov_index <- nov_index[!grepl("matchup6", nov_index)]
nov_q_items <- q_items[grep("potus",q_items$V1), ]
nov_q_items <- nov_q_items[!grepl("TEXT",nov_q_items$V1 ), ]
nov_q_items <- nov_q_items[!grepl("primary",nov_q_items$V1 ), ]
nov_q_items <- nov_q_items[!grepl("potus.matchup6",nov_q_items$V1 ), ]

nrow(nov_q_items)==length(nov_index) # if true, good 

### run a loop on these; might create a vector for the q_items 
for (i in 1:length(nov_index)) {
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### subset those who do not want to vote 
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  #title = nov_q_items$V2[nov_q_items==nov_index[i]]
  title = "Who would you support for president in the following match-ups"
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
  title <- str_replace(title, "Waley", "Whaley")
  title <- str_replace(title,"right direction", "right track" )
  ## subset
  temp_df <- subset(final_data2allwB, temp_var!="")
  ### need to search here for disagree 
  if(str_detect(unique(temp_df$temp_var)[1],"agree")==T){
    temp_df$temp_var[temp_df$temp_var=="Strongly agree"] <- "Agree"
    temp_df$temp_var[temp_df$temp_var=="Somewhat agree"] <- "Agree"
    #temp_df$temp_var[grep("Don", temp_df$temp_var)] <- "Neither/Don't know"
    temp_df$temp_var[temp_df$temp_var=="Strongly disagree"] <- "Disagree"
    temp_df$temp_var[temp_df$temp_var=="Somewhat disagree"] <- "Disagree"
    temp_df$temp_var[temp_df$temp_var!="Agree" & temp_df$temp_var!="Disagree"] <- "Neither/Don't know"
  }
  
  ###
  testtab <- moe_topline(df = temp_df, variable  =temp_var ,weight = final_weight)
  testtab <- as.data.frame(testtab)
  testtab <- testtab[,c(1,2,3,5)]
  testtab[,2] <- round_preserve_sum(testtab[,2], 0)
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,4] <- round_preserve_sum(testtab[,4], 1)
  #colnames(testtab)[1] <- "Response"
  ### get factor attr
  testtab2 <- unnest(testtab, Response)
  testtab2 <- as.data.frame(testtab2)
  colnames(testtab2)[1] <- "Response"
  ## get rid of empty fields 
  testtab2 <- subset(testtab2, Response!="")
  
  ### now get the plot 
  temp_barplot <- ggplot(testtab2 ,aes(x=Response, y=Percent)) +
    geom_bar(stat = "identity", position = "dodge") + 
    geom_errorbar(aes(ymin=Percent-MOE, ymax=Percent+MOE), width=.2,
                  position=position_dodge(.9)) + 
    theme_minimal() + ggtitle(title) + 
    scale_x_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15)) +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.25)) +
    ggtitle(title)
  ### now save to pdf 
  ## create names for plots and tables 
  table_temp <- paste0(temp_name,sep="_","table")
  bar_temp <- paste0(temp_name,sep="_","barplot")
  
  ### command 
  temp_png_tab <- paste0("potus",sep="/",table_temp,sep=".png")
  temp_png_bar <- paste0("potus",sep="/",bar_temp,sep=".png")
  
  png(temp_png_tab, width=7,height=10,units="in", res=400)       # Export PNG as table
  p<-tableGrob(testtab2,rows=NULL)
  grid.arrange(temp_barplot,p)
  #grid::grid.text(title,x = (0.5), y = (0.8))
  #grid.table(testtab, rows=NULL)
  dev.off()
}

##### Most important issues and track #####
nov_index <-  c("topofmind","rightwrongtrack","econ_situation","nat_econ_sit")
nov_q_items <- q_items[q_items$V1 %in% nov_index, ]
nrow(nov_q_items)==length(nov_index) # if true, good 
## fix the titles 
nov_q_items$V2[1] <- 
  "Which one of the following topics is on the top of mind for you when you think about voting"
nov_q_items$V2[4] <- 
  "Over the past year  do you feel the nation's economic situation has gotten better  gotten worse  or stayed about the same"

### run a loop on these; might create a vector for the q_items 
for (i in 1:length(nov_index)) {
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### subset those who do not want to vote 
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  #title = nov_q_items$V2[nov_q_items==nov_index[i]]
  title = nov_q_items$V2[i]
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
  title <- str_replace(title, "Waley", "Whaley")
  title <- str_replace(title,"right direction", "right track" )
  ## subset
  temp_df <- subset(final_data2allwB, temp_var!="")
  ### need to search here for disagree 
  if(str_detect(unique(temp_df$temp_var)[1],"Gotten")==T){
    temp_df$temp_var[temp_df$temp_var=="Gotten somewhat worse"] <- "Worse"
    temp_df$temp_var[temp_df$temp_var=="Gotten much worse"] <- "Worse"
    #temp_df$temp_var[grep("Don", temp_df$temp_var)] <- "Neither/Don't know"
    temp_df$temp_var[temp_df$temp_var=="Gotten much better"] <- "Better"
    temp_df$temp_var[temp_df$temp_var=="Gotten somewhat better"] <- "Better"
    temp_df$temp_var[temp_df$temp_var!="Worse" & temp_df$temp_var!="Better"] <- "About same/Unsure"
  }
  
  ###
  testtab <- moe_topline(df = temp_df, variable  =temp_var ,weight = final_weight)
  testtab <- as.data.frame(testtab)
  testtab <- testtab[,c(1,2,3,5)]
  testtab[,2] <- round_preserve_sum(testtab[,2], 0)
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,4] <- round_preserve_sum(testtab[,4], 1)
  #colnames(testtab)[1] <- "Response"
  ### get factor attr
  testtab2 <- unnest(testtab, Response)
  testtab2 <- as.data.frame(testtab2)
  colnames(testtab2)[1] <- "Response"
  ## get rid of empty fields 
  testtab2 <- subset(testtab2, Response!="")
  
  ### now get the plot 
  temp_barplot <- ggplot(testtab2 ,aes(x=Response, y=Percent)) +
    geom_bar(stat = "identity", position = "dodge") + 
    geom_errorbar(aes(ymin=Percent-MOE, ymax=Percent+MOE), width=.2,
                  position=position_dodge(.9)) + 
    theme_minimal() + ggtitle(title) + 
    scale_x_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15)) +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.25)) +
    ggtitle(title)
  ### now save to pdf 
  ## create names for plots and tables 
  table_temp <- paste0(temp_name,sep="_","table")
  bar_temp <- paste0(temp_name,sep="_","barplot")
  
  ### command 
  temp_png_tab <- paste0("rightwrongtrack",sep="/",table_temp,sep=".png")
  temp_png_bar <- paste0("rightwrongtrack",sep="/",bar_temp,sep=".png")
  
  png(temp_png_tab, width=7,height=10,units="in", res=400)       # Export PNG as table
  p<-tableGrob(testtab2,rows=NULL)
  grid.arrange(temp_barplot,p)
  #grid::grid.text(title,x = (0.5), y = (0.8))
  #grid.table(testtab, rows=NULL)
  dev.off()
}

##### Toxic questions #####
### do the word cloud latter 
nov_index <-  names(final_data2allwB)[grepl("toxic",colnames(final_data2allwB))]
nov_index <- nov_index[!grepl("toxic_txtentr", nov_index)]
nov_q_items <- q_items[grep("toxic",q_items$V1), ]
nov_q_items <- nov_q_items[!grepl("toxic_txtentr",nov_q_items$V1 ), ]


nrow(nov_q_items)==length(nov_index) # if true, good 

### run a loop on these; might create a vector for the q_items 
for (i in 1:length(nov_index)) {
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### subset those who do not want to vote 
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  #title = nov_q_items$V2[nov_q_items==nov_index[i]]
  title = nov_q_items$V2[i]
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
  title <- str_replace(title, "Waley", "Whaley")
  title <- str_replace(title,"right direction", "right track" )
  ## subset
  temp_df <- subset(final_data2allwB, temp_var!="")
  ### need to search here for disagree 
  if(str_detect(unique(temp_df$temp_var)[1],"agree")==T){
    temp_df$temp_var[temp_df$temp_var=="Strongly agree"] <- "Agree"
    temp_df$temp_var[temp_df$temp_var=="Somewhat agree"] <- "Agree"
    #temp_df$temp_var[grep("Don", temp_df$temp_var)] <- "Neither/Don't know"
    temp_df$temp_var[temp_df$temp_var=="Strongly disagree"] <- "Disagree"
    temp_df$temp_var[temp_df$temp_var=="Somewhat disagree"] <- "Disagree"
    temp_df$temp_var[temp_df$temp_var!="Agree" & temp_df$temp_var!="Disagree"] <- "Neither/Don't know"
  }
  
  ###
  testtab <- moe_topline(df = temp_df, variable  =temp_var ,weight = final_weight)
  testtab <- as.data.frame(testtab)
  testtab <- testtab[,c(1,2,3,5)]
  testtab[,2] <- round_preserve_sum(testtab[,2], 0)
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,4] <- round_preserve_sum(testtab[,4], 1)
  #colnames(testtab)[1] <- "Response"
  ### get factor attr
  testtab2 <- unnest(testtab, Response)
  testtab2 <- as.data.frame(testtab2)
  colnames(testtab2)[1] <- "Response"
  ## get rid of empty fields 
  testtab2 <- subset(testtab2, Response!="")
  
  ### now get the plot 
  temp_barplot <- ggplot(testtab2 ,aes(x=Response, y=Percent)) +
    geom_bar(stat = "identity", position = "dodge") + 
    geom_errorbar(aes(ymin=Percent-MOE, ymax=Percent+MOE), width=.2,
                  position=position_dodge(.9)) + 
    theme_minimal() + ggtitle(title) + 
    scale_x_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15)) +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.25)) +
    ggtitle(title)
  ### now save to pdf 
  ## create names for plots and tables 
  table_temp <- paste0(temp_name,sep="_","table")
  bar_temp <- paste0(temp_name,sep="_","barplot")
  
  ### command 
  temp_png_tab <- paste0("toxic",sep="/",table_temp,sep=".png")
  temp_png_bar <- paste0("toxic",sep="/",bar_temp,sep=".png")
  
  png(temp_png_tab, width=7,height=10,units="in", res=400)       # Export PNG as table
  p<-tableGrob(testtab2,rows=NULL)
  grid.arrange(temp_barplot,p)
  #grid::grid.text(title,x = (0.5), y = (0.8))
  #grid.table(testtab, rows=NULL)
  dev.off()
}

##### Screeners/Demographics #####

nov_index <-  c("race","gender","educ","age","pid_think","ind_lean")
nov_q_items <- q_items[q_items$V1 %in% nov_index, ]
nrow(nov_q_items)==length(nov_index) # if true, good 
final_data2allwB$educ[str_detect(final_data2allwB$educ, "Bachel")] <- "Bachelor's degree"
final_data2allwB$educ[str_detect(final_data2allwB$educ, "professional")] <- "Graduate or professional degree"
saveRDS(final_data2allwB, "final_data2allwB.rds")
### make sure folder to save is present 
output_dir <- file.path(main_dir, "demos_screen")
if (!dir.exists(output_dir)){
  dir.create(output_dir)
} else {
  print("Dir already exists!")
}

table(final_data2allwB$educ)
### run a loop on these; might create a vector for the q_items 
for (i in 1:length(nov_index)) {
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### subset those who do not want to vote 
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  #title = nov_q_items$V2[nov_q_items==nov_index[i]]
  title = nov_q_items$V2[i]
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
  title <- str_replace(title, "Waley", "Whaley")
  title <- str_replace(title,"right direction", "right track" )
  ## subset
  temp_df <- subset(final_data2allwB, temp_var!="")
  ### need to search here for disagree 
  if(str_detect(unique(temp_df$temp_var)[1],"agree")==T){
    temp_df$temp_var[temp_df$temp_var=="Strongly agree"] <- "Agree"
    temp_df$temp_var[temp_df$temp_var=="Somewhat agree"] <- "Agree"
    #temp_df$temp_var[grep("Don", temp_df$temp_var)] <- "Neither/Don't know"
    temp_df$temp_var[temp_df$temp_var=="Strongly disagree"] <- "Disagree"
    temp_df$temp_var[temp_df$temp_var=="Somewhat disagree"] <- "Disagree"
    temp_df$temp_var[temp_df$temp_var!="Agree" & temp_df$temp_var!="Disagree"] <- "Neither/Don't know"
  }
  
  ###
  testtab <- moe_topline(df = temp_df, variable  =temp_var ,weight = final_weight)
  testtab <- as.data.frame(testtab)
  testtab <- testtab[,c(1,2,3,5)]
  testtab[,2] <- round_preserve_sum(testtab[,2], 0)
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,4] <- round_preserve_sum(testtab[,4], 1)
  #colnames(testtab)[1] <- "Response"
  ### get factor attr
  testtab2 <- unnest(testtab, Response)
  testtab2 <- as.data.frame(testtab2)
  colnames(testtab2)[1] <- "Response"
  ## get rid of empty fields 
  testtab2 <- subset(testtab2, Response!="")
  
  ### now get the plot 
  temp_barplot <- ggplot(testtab2 ,aes(x=Response, y=Percent)) +
    geom_bar(stat = "identity", position = "dodge") + 
    geom_errorbar(aes(ymin=Percent-MOE, ymax=Percent+MOE), width=.2,
                  position=position_dodge(.9)) + 
    theme_minimal() + ggtitle(title) + 
    scale_x_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15)) +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.25)) +
    ggtitle(title)
  ### now save to pdf 
  ## create names for plots and tables 
  table_temp <- paste0(temp_name,sep="_","table")
  bar_temp <- paste0(temp_name,sep="_","barplot")
  
  ### command 
  temp_png_tab <- paste0("demos_screen",sep="/",table_temp,sep=".png")
  temp_png_bar <- paste0("demos_screen",sep="/",bar_temp,sep=".png")
  testtab2$Response <- sapply(lapply(testtab2$Response, strwrap, width=50), paste, collapse="\n")
  png(temp_png_tab, width=7,height=10,units="in", res=400)       # Export PNG as table
  p<-tableGrob(testtab2,rows=NULL)
  grid.arrange(temp_barplot,p)
  #grid::grid.text(title,x = (0.5), y = (0.8))
  #grid.table(testtab, rows=NULL)
  dev.off()
}

##### Extra Demographics #####

nov_index <-  c("hispanic","income","children","marital","householdsize",
                "sexuality","union","armedservice","current_armedservice","employmentstatus",
                "emplyed_industry","didvote","rep_strong","dem_strong",
                "ideo_1","ideo_treat1","ideo_treat2")
nov_q_items <- q_items[q_items$V1 %in% nov_index, ]
nov_q_items$V1
nrow(nov_q_items)==length(nov_index) # if true, good 

### make sure folder exists 
output_dir <- file.path(main_dir, "demos_extra")
if (!dir.exists(output_dir)){
  dir.create(output_dir)
} else {
  print("Dir already exists!")
}

### run a loop on these; might create a vector for the q_items 
for (i in 1:length(nov_index)) {
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### subset those who do not want to vote 
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  #title = nov_q_items$V2[nov_q_items==nov_index[i]]
  title = nov_q_items$V2[i]
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
  title <- str_replace(title, "Waley", "Whaley")
  title <- str_replace(title,"right direction", "right track" )
  ## subset
  temp_df <- subset(final_data2allwB, temp_var!="")
  ### need to search here for disagree 
  if(str_detect(unique(temp_df$temp_var)[1],"agree")==T){
    temp_df$temp_var[temp_df$temp_var=="Strongly agree"] <- "Agree"
    temp_df$temp_var[temp_df$temp_var=="Somewhat agree"] <- "Agree"
    #temp_df$temp_var[grep("Don", temp_df$temp_var)] <- "Neither/Don't know"
    temp_df$temp_var[temp_df$temp_var=="Strongly disagree"] <- "Disagree"
    temp_df$temp_var[temp_df$temp_var=="Somewhat disagree"] <- "Disagree"
    temp_df$temp_var[temp_df$temp_var!="Agree" & temp_df$temp_var!="Disagree"] <- "Neither/Don't know"
  }
  
  ###
  testtab <- moe_topline(df = temp_df, variable  =temp_var ,weight = final_weight)
  testtab <- as.data.frame(testtab)
  testtab <- testtab[,c(1,2,3,5)]
  testtab[,2] <- round_preserve_sum(testtab[,2], 0)
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,4] <- round_preserve_sum(testtab[,4], 1)
  #colnames(testtab)[1] <- "Response"
  ### get factor attr
  testtab2 <- unnest(testtab, Response)
  testtab2 <- as.data.frame(testtab2)
  colnames(testtab2)[1] <- "Response"
  ## get rid of empty fields 
  testtab2 <- subset(testtab2, Response!="")
  
  ### now get the plot 
  temp_barplot <- ggplot(testtab2 ,aes(x=Response, y=Percent)) +
    geom_bar(stat = "identity", position = "dodge") + 
    geom_errorbar(aes(ymin=Percent-MOE, ymax=Percent+MOE), width=.2,
                  position=position_dodge(.9)) + 
    theme_minimal() + ggtitle(title) + 
    scale_x_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15)) +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.25)) +
    ggtitle(title)
  ### now save to pdf 
  ## create names for plots and tables 
  table_temp <- paste0(temp_name,sep="_","table")
  bar_temp <- paste0(temp_name,sep="_","barplot")
  
  ### command 
  temp_png_tab <- paste0("demos_extra",sep="/",table_temp,sep=".png")
  temp_png_bar <- paste0("demos_extra",sep="/",bar_temp,sep=".png")
  testtab2$Response <- sapply(lapply(testtab2$Response, strwrap, width=50), paste, collapse="\n")
  png(temp_png_tab, width=7,height=10,units="in", res=400)       # Export PNG as table
  p<-tableGrob(testtab2,rows=NULL)
  grid.arrange(temp_barplot,p)
  #grid::grid.text(title,x = (0.5), y = (0.8))
  #grid.table(testtab, rows=NULL)
  dev.off()
}

### good. Let's get these into R markdown then 

## violin plot to check the weights 
### create a plot for the sampling error; let's startify by party 
party_weight_violin<-ggplot(final_data2allwB, aes(x=party_simp, y=final_weight, fill=party_simp)) + 
  geom_violin(trim=FALSE) +   scale_fill_manual(name=NULL,values=c("blue","darkgreen", "red")) + 
  theme_minimal()+ylab("Weights") + 
  theme(axis.title.x = element_blank()) +
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 15))
## save here 
ggsave(party_weight_violin, filename = "party_violin_weight.png", width=6, height = 4, dpi=400,bg="White")


dem_sub <- subset(final_data2allwB, party_simp=="Democrat/lean Democrat")
ind_sub <- subset(final_data2allwB, party_simp=="no lean")
gop_sub <- subset(final_data2allwB, party_simp=="Republican/lean Republican")

## now do quantile here 
quantile(dem_sub$final_weight, seq(0,1,0.01))
quantile(ind_sub$final_weight, seq(0,1,0.01))
quantile(gop_sub$final_weight, seq(0,1,0.01))

## find length of zip codes 
length(unique(final_data2allwB$US.Zip.Code))


resp_by_zcta <- final_data2allwB %>%
  group_by(US.Zip.Code) %>%
  tally()
dim(resp_by_zcta)
zip_csv <- read.csv("zip_code_database.csv")
zip_csv_oh <- subset(zip_csv, state=="OH")
zip_csv_oh <- subset(zip_csv_oh, type=="STANDARD")
## GET POP
sum(zip_csv_oh$irs_estimated_population)
## subset and merge 
zip_csv_oh_samp <- merge(zip_csv_oh,resp_by_zcta, by.x="zip", by.y="US.Zip.Code")
sum(zip_csv_oh_samp$irs_estimated_population)/sum(zip_csv_oh$irs_estimated_population)
nrow(zip_csv_oh)

summary(resp_by_zcta)
quantile(resp_by_zcta$n, seq(0,1,by=0.05))
