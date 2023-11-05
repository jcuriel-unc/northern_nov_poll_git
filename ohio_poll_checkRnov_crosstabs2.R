################################################################################
################## November 2023 Northern Poll -- Cross Tabs  ###################################
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
library(Hmisc)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

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
col_vec_party = c("blue","darkgreen","red")
## read in the data for the results 

##### Political Knowledge #####


### now, collapse into the vals of interest 
polknow_vec <- c("President", "U.S. Senate", "U.S. House", "OH Senate", "OH House",
                 "OH Governor", "U.S. Supreme Court")
### now grab the index of vals 
nov_index <-  names(final_data2allwB)[grepl("correct_pid1",colnames(final_data2allwB))]
#nov_index <- nov_index[grepl("correct",nov_index)]
length(polknow_vec)==length(nov_index) # if true, good 
polknow1_df <- data.frame()
for (i in 1:length(nov_index)) {
  print(i)
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### get title 
  temp_name <- colnames(final_data2allwB)[i]
  #title = final_data2lbl[temp_name]
  #title <-  strwrap(title, width = 60, simplify = FALSE)
  #title <- sapply(title, paste, collapse = "\n")
  ## collapse not sure and other 
  # final_data2allwB$temp_var[final_data2allwB$temp_var==3] <- 4
  ### party cross tab 
  testtab <-   moe_crosstab(df = final_data2allwB,
                                  x =party_simp , y = temp_var , weight = final_weight, format = "long")
  testtab <- unnest(testtab, temp_var)
  testtab <- as.data.frame(testtab) 
  colnames(testtab)[2] <- "Response"
  ## we only need 1 
  testtab <- subset(testtab, Response==1)
  ## we will want to run round_preserve_sum() on the data 
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,4] <- round_preserve_sum(testtab[,4], 1)
  testtab[,5] <- round_preserve_sum(testtab[,5], 0)
  ## now, get the results subsetted, then save and test in the R markdown 
  reduced_tab <- testtab[,c(1:5)]
  colnames(reduced_tab)[1] <- "Party"
  reduced_tab$Response <- "Correct"
  reduced_tab$Position <- polknow_vec[i]
  if(nrow(polknow1_df)==0){
    polknow1_df <- reduced_tab
  }else{
    polknow1_df <- rbind(polknow1_df,reduced_tab )
  }
}
### make folder
main_dir <- getwd()
output_dir <- file.path(main_dir, "party_tabs/polknow")
if (!dir.exists(output_dir)){
  dir.create(output_dir)
} else {
  print("Dir already exists!")
}

### save data
saveRDS(polknow1_df, "party_tabs/polknow/polknow1_party.rds")

### run second pid 
### now grab the index of vals 
nov_index <-  names(final_data2allwB)[grepl("correct_pid2",colnames(final_data2allwB))]
#nov_index <- nov_index[grepl("correct",nov_index)]
polknow2_vec <- c("Joseph Biden", "J.D. Vance", "Kamala Harris","Mike Pence", "Mike DeWine","Donald Trump" )
length(polknow2_vec)==length(nov_index) # if true, good 

polknow2_df <- data.frame()
for (i in 1:length(nov_index)) {
  print(i)
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### get title 
  temp_name <- colnames(final_data2allwB)[i]
  #title = final_data2lbl[temp_name]
  #title <-  strwrap(title, width = 60, simplify = FALSE)
  #title <- sapply(title, paste, collapse = "\n")
  ## collapse not sure and other 
  # final_data2allwB$temp_var[final_data2allwB$temp_var==3] <- 4
  ### party cross tab 
  testtab <-   moe_crosstab(df = final_data2allwB,
                            x =party_simp , y = temp_var , weight = final_weight, format = "long")
  testtab <- unnest(testtab, temp_var)
  testtab <- as.data.frame(testtab) 
  colnames(testtab)[2] <- "Response"
  ## we only need 1 
  testtab <- subset(testtab, Response==1)
  ## we will want to run round_preserve_sum() on the data 
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,4] <- round_preserve_sum(testtab[,4], 1)
  testtab[,5] <- round_preserve_sum(testtab[,5], 0)
  ## now, get the results subsetted, then save and test in the R markdown 
  reduced_tab <- testtab[,c(1:5)]
  colnames(reduced_tab)[1] <- "Party"
  reduced_tab$Response <- "Correct"
  reduced_tab$Position <- polknow2_vec[i]
  if(nrow(polknow2_df)==0){
    polknow2_df <- reduced_tab
  }else{
    polknow2_df <- rbind(polknow1_df,reduced_tab )
  }
}
### save data
saveRDS(polknow2_df, "party_tabs/polknow/polknow2_party.rds")

#### age 
nov_index <-  names(final_data2allwB)[grepl("correct_pid1",colnames(final_data2allwB))]
#nov_index <- nov_index[grepl("correct",nov_index)]
length(polknow_vec)==length(nov_index) # if true, good 

polknow1_df <- data.frame()
for (i in 1:length(nov_index)) {
  print(i)
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### get title 
  temp_name <- colnames(final_data2allwB)[i]
  #title = final_data2lbl[temp_name]
  #title <-  strwrap(title, width = 60, simplify = FALSE)
  #title <- sapply(title, paste, collapse = "\n")
  ## collapse not sure and other 
  # final_data2allwB$temp_var[final_data2allwB$temp_var==3] <- 4
  ### party cross tab 
  testtab <-   moe_crosstab(df = final_data2allwB,
                            x =age_simp , y = temp_var , weight = final_weight, format = "long")
  testtab <- unnest(testtab, temp_var)
  testtab <- as.data.frame(testtab) 
  colnames(testtab)[2] <- "Response"
  ## we only need 1 
  testtab <- subset(testtab, Response==1)
  ## we will want to run round_preserve_sum() on the data 
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,4] <- round_preserve_sum(testtab[,4], 1)
  testtab[,5] <- round_preserve_sum(testtab[,5], 0)
  ## now, get the results subsetted, then save and test in the R markdown 
  reduced_tab <- testtab[,c(1:5)]
  colnames(reduced_tab)[1] <- "Age"
  reduced_tab$Response <- "Correct"
  reduced_tab$Position <- polknow_vec[i]
  if(nrow(polknow1_df)==0){
    polknow1_df <- reduced_tab
  }else{
    polknow1_df <- rbind(polknow1_df,reduced_tab )
  }
}
### create folder
output_dir <- file.path(main_dir, "age_tabs/polknow")
if (!dir.exists(output_dir)){
  dir.create(output_dir)
} else {
  print("Dir already exists!")
}

### save data
saveRDS(polknow1_df, "age_tabs/polknow/polknow1_party.rds")

### run second pid 
### now grab the index of vals 
nov_index <-  names(final_data2allwB)[grepl("correct_pid2",colnames(final_data2allwB))]
#nov_index <- nov_index[grepl("correct",nov_index)]
length(polknow2_vec)==length(nov_index) # if true, good 

polknow2_df <- data.frame()
for (i in 1:length(nov_index)) {
  print(i)
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### get title 
  temp_name <- colnames(final_data2allwB)[i]
  #title = final_data2lbl[temp_name]
  #title <-  strwrap(title, width = 60, simplify = FALSE)
  #title <- sapply(title, paste, collapse = "\n")
  ## collapse not sure and other 
  # final_data2allwB$temp_var[final_data2allwB$temp_var==3] <- 4
  ### party cross tab 
  testtab <-   moe_crosstab(df = final_data2allwB,
                            x =age_simp , y = temp_var , weight = final_weight, format = "long")
  testtab <- unnest(testtab, temp_var)
  testtab <- as.data.frame(testtab) 
  colnames(testtab)[2] <- "Response"
  ## we only need 1 
  testtab <- subset(testtab, Response==1)
  ## we will want to run round_preserve_sum() on the data 
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,4] <- round_preserve_sum(testtab[,4], 1)
  testtab[,5] <- round_preserve_sum(testtab[,5], 0)
  ## now, get the results subsetted, then save and test in the R markdown 
  reduced_tab <- testtab[,c(1:5)]
  colnames(reduced_tab)[1] <- "Age"
  reduced_tab$Response <- "Correct"
  reduced_tab$Position <- polknow2_vec[i]
  if(nrow(polknow2_df)==0){
    polknow2_df <- reduced_tab
  }else{
    polknow2_df <- rbind(polknow1_df,reduced_tab )
  }
}
### save data
saveRDS(polknow2_df, "age_tabs/polknow/polknow2_party.rds")


###### Favorability of public figures ##### 
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
  ### fix to get rid of the modifiers 
  final_data2allwB$temp_var[final_data2allwB$temp_var=="Strongly Favorable"] <- "Favorable"
  final_data2allwB$temp_var[final_data2allwB$temp_var=="Somewhat Favorable"] <- "Favorable"
  final_data2allwB$temp_var[final_data2allwB$temp_var=="Somewhat Unfavorable"] <- "Unfavorable"
  final_data2allwB$temp_var[final_data2allwB$temp_var=="Strongly Unfavorable"] <- "Unfavorable"
  final_data2allwB$temp_var[final_data2allwB$temp_var!="Unfavorable" &
                              final_data2allwB$temp_var!="Favorable"] <- "Neither/unsure"
  #title = final_data2lbl[temp_name]
  #title <-  strwrap(title, width = 60, simplify = FALSE)
  #title <- sapply(title, paste, collapse = "\n")
  ## collapse not sure and other 
  # final_data2allwB$temp_var[final_data2allwB$temp_var==3] <- 4
  ###
  testtab <-   moe_crosstab(df = final_data2allwB,
                            x =party_simp , y = temp_var , weight = final_weight, format = "long")
  testtab <- unnest(testtab, temp_var)
  testtab <- as.data.frame(testtab) 
  colnames(testtab)[2] <- "Response"
  ### create a new n field 
  testtab$Num <- testtab$n*(testtab$pct/100)
  ### now subset 
  testtab <- testtab[,c(1,2,3,4,6)]
  
  ## we will want to run round_preserve_sum() on the data 
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,4] <- round_preserve_sum(testtab[,4], 1)
  testtab[,5] <- round_preserve_sum(testtab[,5], 0)
  ## now, get the results subsetted, then save and test in the R markdown 
  testtab$Person <- fav_vec[i]
  ##switch order 
  testtab <- testtab[,c(6,1:5)]
  
  if(nrow(polfav_df)==0){
    polfav_df <- testtab
  }else{
    polfav_df <- rbind(polfav_df,testtab )
  }
}
### folder
output_dir <- file.path(main_dir, "party_tabs/favorability")
if (!dir.exists(output_dir)){
  dir.create(output_dir)
} else {
  print("Dir already exists!")
}

### now save 
saveRDS(polfav_df, "party_tabs/favorability/favorable_tab.rds")

### favorability by age 
polfav_df <- data.frame()
for (i in 1:length(fav_index)) {
  print(i)
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==fav_index[i]]
  ### fix to get rid of the modifiers 
  final_data2allwB$temp_var[final_data2allwB$temp_var=="Strongly Favorable"] <- "Favorable"
  final_data2allwB$temp_var[final_data2allwB$temp_var=="Somewhat Favorable"] <- "Favorable"
  final_data2allwB$temp_var[final_data2allwB$temp_var=="Somewhat Unfavorable"] <- "Unfavorable"
  final_data2allwB$temp_var[final_data2allwB$temp_var=="Strongly Unfavorable"] <- "Unfavorable"
  final_data2allwB$temp_var[final_data2allwB$temp_var!="Unfavorable" &
                              final_data2allwB$temp_var!="Favorable"] <- "Neither/unsure"
  #title = final_data2lbl[temp_name]
  #title <-  strwrap(title, width = 60, simplify = FALSE)
  #title <- sapply(title, paste, collapse = "\n")
  ## collapse not sure and other 
  # final_data2allwB$temp_var[final_data2allwB$temp_var==3] <- 4
  ###
  testtab <-   moe_crosstab(df = final_data2allwB,
                            x =age_simp , y = temp_var , weight = final_weight, format = "long")
  testtab <- unnest(testtab, temp_var)
  testtab <- as.data.frame(testtab) 
  colnames(testtab)[2] <- "Response"
  ### create a new n field 
  testtab$Num <- testtab$n*(testtab$pct/100)
  ### now subset 
  testtab <- testtab[,c(1,2,3,4,6)]
  
  ## we will want to run round_preserve_sum() on the data 
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,4] <- round_preserve_sum(testtab[,4], 1)
  testtab[,5] <- round_preserve_sum(testtab[,5], 0)
  ## now, get the results subsetted, then save and test in the R markdown 
  testtab$Person <- fav_vec[i]
  ##switch order 
  testtab <- testtab[,c(6,1:5)]
  colnames(testtab)[2] <- "Age"
  
  if(nrow(polfav_df)==0){
    polfav_df <- testtab
  }else{
    polfav_df <- rbind(polfav_df,testtab )
  }
}
## folder
output_dir <- file.path(main_dir, "age_tabs/favorability")
if (!dir.exists(output_dir)){
  dir.create(output_dir)
} else {
  print("Dir already exists!")
}

### ave to age folder 
saveRDS(polfav_df, "age_tabs/favorability/favorable_tab.rds")

##### Primary preferences by age #####
nov_index <-  names(final_data2allwB)[grepl("primary",colnames(final_data2allwB))]
nov_index <- nov_index[!grepl("_TEXT", nov_index)]
nov_q_items <- q_items[grep("primary",q_items$V1), ]
nov_q_items <- nov_q_items[!grepl("TEXT",nov_q_items$V1), ]

nrow(nov_q_items)==length(nov_index) # if true, good 

### run a loop on these; might create a vector for the q_items 
for (i in 1:length(nov_index)) {
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### subset those who do not want to vote 
  temp_df <- final_data2allwB
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  title = nov_q_items$V2[i]
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
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
  ## run crosstab here 
  ### run moe here 
  testtab_party <-   moe_crosstab(df = temp_df,
                                  x =age_simp , y = temp_var , weight = final_weight, format = "long")
  testtab_party <- as.data.frame(testtab_party)
  testtab_party <- testtab_party[,c(1,2,3,4,5)]
  ### create a new n field 
  testtab_party$Num <- testtab_party$n*(testtab_party$pct/100)
  ### now subset 
  testtab_party <- testtab_party[,c(1,2,3,4,6)]
  testtab_party[,3] <- round_preserve_sum(testtab_party[,3],0)
  testtab_party[,4] <- round_preserve_sum(testtab_party[,4],1)
  testtab_party[,5] <- round_preserve_sum(testtab_party[,5],0)
  
  
  #colnames(testtab)[1] <- "Response"
  ### get factor attr
  testtab2 <- unnest(testtab_party, temp_var)
  testtab2 <- as.data.frame(testtab2)
  colnames(testtab2)[2] <- "Response"
  colnames(testtab2)[1] <- "Age"
  
  ## get rid of empty fields 
  testtab2 <- subset(testtab2, Response!="")
  ### now get the plot 
  temp_party_plot<-
    ggplot(testtab2,aes(x = pct, y = Response, xmin = (pct - moe), xmax = (pct + moe), color = Age)) +
    geom_pointrange(position = position_dodge(width = 0.2)) + xlim(0,100) +
    scale_y_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15)) + scale_color_manual(name=NULL,values=col_vec_party) +
    theme_minimal() + theme(axis.title = element_blank()) + ggtitle(title)
  ### now save to pdf 
  ## create names for plots and tables 
  table_temp <- paste0(temp_name,sep="_","table")
  bar_temp <- paste0(temp_name,sep="_","barplot")
  testtab2$Response <- sapply(lapply(testtab2$Response, strwrap, width=30), paste, collapse="\n")
  ### folder 
  output_dir <- file.path(main_dir, "age_tabs/primary")
  if (!dir.exists(output_dir)){
    dir.create(output_dir)
  } else {
    print("Dir already exists!")
  }
  ### command 
  temp_png_tab <- paste0("age_tabs/primary",sep="/",table_temp,sep=".png")
  temp_png_bar <- paste0("age_tabs/primary",sep="/",bar_temp,sep=".png")
  
  png(temp_png_tab, width=7,height=14,units="in", res=400)       # Export PNG as table
  p<-tableGrob(testtab2,rows=NULL)
  grid.arrange(temp_party_plot,p)
  #grid::grid.text(title,x = (0.5), y = (0.8))
  #grid.table(testtab, rows=NULL)
  dev.off()
}

##### Senate match ups by age #####
nov_index <-  names(final_data2allwB)[grepl("Senate",colnames(final_data2allwB))]
nov_index <- nov_index[!grepl("TEXT", nov_index)]
nov_q_items <- q_items[grep("Senate",q_items$V1), ]
nov_q_items <- nov_q_items[!grepl("TEXT",nov_q_items$V1), ]

nrow(nov_q_items)==length(nov_index) # if true, good 

### run a loop on these; might create a vector for the q_items 
for (i in 1:length(nov_index)) {
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### subset those who do not want to vote 
  temp_df <- final_data2allwB
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  title = nov_q_items$V2[i]
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
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
  ## run crosstab here 
  ### run moe here 
  testtab_party <-   moe_crosstab(df = temp_df,
                                  x =age_simp , y = temp_var , weight = final_weight, format = "long")
  testtab_party <- as.data.frame(testtab_party)
  testtab_party <- testtab_party[,c(1,2,3,4,5)]
  ### create a new n field 
  testtab_party$Num <- testtab_party$n*(testtab_party$pct/100)
  ### now subset 
  testtab_party <- testtab_party[,c(1,2,3,4,6)]
  testtab_party[,3] <- round_preserve_sum(testtab_party[,3],0)
  testtab_party[,4] <- round_preserve_sum(testtab_party[,4],1)
  testtab_party[,5] <- round_preserve_sum(testtab_party[,5],0)
  
  
  #colnames(testtab)[1] <- "Response"
  ### get factor attr
  testtab2 <- unnest(testtab_party, temp_var)
  testtab2 <- as.data.frame(testtab2)
  colnames(testtab2)[2] <- "Response"
  colnames(testtab2)[1] <- "Age"
  
  ## get rid of empty fields 
  testtab2 <- subset(testtab2, Response!="")
  ### now get the plot 
  temp_party_plot<-
    ggplot(testtab2,aes(x = pct, y = Response, xmin = (pct - moe), xmax = (pct + moe), color = Age)) +
    geom_pointrange(position = position_dodge(width = 0.2)) + xlim(0,100) +
    scale_y_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15)) + scale_color_manual(name=NULL,values=col_vec_party) +
    theme_minimal() + theme(axis.title = element_blank()) + ggtitle(title)
  ### now save to pdf 
  ## create names for plots and tables 
  table_temp <- paste0(temp_name,sep="_","table")
  bar_temp <- paste0(temp_name,sep="_","barplot")
  testtab2$Response <- sapply(lapply(testtab2$Response, strwrap, width=30), paste, collapse="\n")
  #### folder 
  output_dir <- file.path(main_dir, "age_tabs/senate")
  if (!dir.exists(output_dir)){
    dir.create(output_dir)
  } else {
    print("Dir already exists!")
  }
  ### command 
  temp_png_tab <- paste0("age_tabs/senate",sep="/",table_temp,sep=".png")
  temp_png_bar <- paste0("age_tabs/senate",sep="/",bar_temp,sep=".png")
  
  png(temp_png_tab, width=7,height=14,units="in", res=400)       # Export PNG as table
  p<-tableGrob(testtab2,rows=NULL)
  grid.arrange(temp_party_plot,p)
  #grid::grid.text(title,x = (0.5), y = (0.8))
  #grid.table(testtab, rows=NULL)
  dev.off()
}

### senate by party 
for (i in 1:length(nov_index)) {
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### subset those who do not want to vote 
  temp_df <- final_data2allwB
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  title = nov_q_items$V2[i]
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
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
  ## run crosstab here 
  ### run moe here 
  testtab_party <-   moe_crosstab(df = temp_df,
                                  x =party_simp , y = temp_var , weight = final_weight, format = "long")
  testtab_party <- as.data.frame(testtab_party)
  testtab_party <- testtab_party[,c(1,2,3,4,5)]
  ### create a new n field 
  testtab_party$Num <- testtab_party$n*(testtab_party$pct/100)
  ### now subset 
  testtab_party <- testtab_party[,c(1,2,3,4,6)]
  testtab_party[,3] <- round_preserve_sum(testtab_party[,3],0)
  testtab_party[,4] <- round_preserve_sum(testtab_party[,4],1)
  testtab_party[,5] <- round_preserve_sum(testtab_party[,5],0)
  
  
  #colnames(testtab)[1] <- "Response"
  ### get factor attr
  testtab2 <- unnest(testtab_party, temp_var)
  testtab2 <- as.data.frame(testtab2)
  colnames(testtab2)[2] <- "Response"
  colnames(testtab2)[1] <- "Party"
  
  ## get rid of empty fields 
  testtab2 <- subset(testtab2, Response!="")
  ### now get the plot 
  temp_party_plot<-
    ggplot(testtab2,aes(x = pct, y = Response, xmin = (pct - moe), xmax = (pct + moe), color = Party)) +
    geom_pointrange(position = position_dodge(width = 0.2)) + xlim(0,100) +
    scale_y_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15)) + scale_color_manual(name=NULL,values=col_vec_party) +
    theme_minimal() + theme(axis.title = element_blank()) + ggtitle(title)
  ### now save to pdf 
  ## create names for plots and tables 
  table_temp <- paste0(temp_name,sep="_","table")
  bar_temp <- paste0(temp_name,sep="_","barplot")
  testtab2$Response <- sapply(lapply(testtab2$Response, strwrap, width=30), paste, collapse="\n")
  #### folder 
  output_dir <- file.path(main_dir, "party_tabs/senate")
  if (!dir.exists(output_dir)){
    dir.create(output_dir)
  } else {
    print("Dir already exists!")
  }
  ### command 
  temp_png_tab <- paste0("party_tabs/senate",sep="/",table_temp,sep=".png")
  temp_png_bar <- paste0("party_tabs/senate",sep="/",bar_temp,sep=".png")
  
  png(temp_png_tab, width=7,height=14,units="in", res=400)       # Export PNG as table
  p<-tableGrob(testtab2,rows=NULL)
  grid.arrange(temp_party_plot,p)
  #grid::grid.text(title,x = (0.5), y = (0.8))
  #grid.table(testtab, rows=NULL)
  dev.off()
}

##### Presidential matchups #####
## age
nov_index <-  names(final_data2allwB)[grepl("potus",colnames(final_data2allwB))]
nov_index <- nov_index[!grepl("TEXT", nov_index)]
nov_index <- nov_index[!grepl("primary", nov_index)]

nov_q_items <- q_items[grep("potus",q_items$V1), ]
nov_q_items <- nov_q_items[!grepl("TEXT",nov_q_items$V1), ]
nov_q_items <- nov_q_items[!grepl("primary",nov_q_items$V1), ]
nov_q_items <- subset(nov_q_items, V1 != "potus matchup6")
nrow(nov_q_items)==length(nov_index) # if true, good 

### run a loop on these; might create a vector for the q_items 
for (i in 1:length(nov_index)) {
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### subset those who do not want to vote 
  temp_df <- final_data2allwB
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  title = nov_q_items$V2[i]
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
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
  ## run crosstab here 
  ### run moe here 
  testtab_party <-   moe_crosstab(df = temp_df,
                                  x =age_simp , y = temp_var , weight = final_weight, format = "long")
  testtab_party <- as.data.frame(testtab_party)
  testtab_party <- testtab_party[,c(1,2,3,4,5)]
  ### create a new n field 
  testtab_party$Num <- testtab_party$n*(testtab_party$pct/100)
  ### now subset 
  testtab_party <- testtab_party[,c(1,2,3,4,6)]
  testtab_party[,3] <- round_preserve_sum(testtab_party[,3],0)
  testtab_party[,4] <- round_preserve_sum(testtab_party[,4],1)
  testtab_party[,5] <- round_preserve_sum(testtab_party[,5],0)
  
  
  #colnames(testtab)[1] <- "Response"
  ### get factor attr
  testtab2 <- unnest(testtab_party, temp_var)
  testtab2 <- as.data.frame(testtab2)
  colnames(testtab2)[2] <- "Response"
  colnames(testtab2)[1] <- "Age"
  
  ## get rid of empty fields 
  testtab2 <- subset(testtab2, Response!="")
  ### now get the plot 
  temp_party_plot<-
    ggplot(testtab2,aes(x = pct, y = Response, xmin = (pct - moe), xmax = (pct + moe), color = Age)) +
    geom_pointrange(position = position_dodge(width = 0.2)) + xlim(0,100) +
    scale_y_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15)) + scale_color_manual(name=NULL,values=col_vec_party) +
    theme_minimal() + theme(axis.title = element_blank()) + ggtitle(title)
  ### now save to pdf 
  ## create names for plots and tables 
  table_temp <- paste0(temp_name,sep="_","table")
  bar_temp <- paste0(temp_name,sep="_","barplot")
  testtab2$Response <- sapply(lapply(testtab2$Response, strwrap, width=30), paste, collapse="\n")
  ### folder create 
  output_dir <- file.path(main_dir, "age_tabs/potus")
  if (!dir.exists(output_dir)){
    dir.create(output_dir)
  } else {
    print("Dir already exists!")
  }
  ### command 
  temp_png_tab <- paste0("age_tabs/potus",sep="/",table_temp,sep=".png")
  temp_png_bar <- paste0("age_tabs/potus",sep="/",bar_temp,sep=".png")
  
  png(temp_png_tab, width=7,height=14,units="in", res=400)       # Export PNG as table
  p<-tableGrob(testtab2,rows=NULL)
  grid.arrange(temp_party_plot,p)
  #grid::grid.text(title,x = (0.5), y = (0.8))
  #grid.table(testtab, rows=NULL)
  dev.off()
}
## by party results 
for (i in 1:length(nov_index)) {
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### subset those who do not want to vote 
  temp_df <- final_data2allwB
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  title = nov_q_items$V2[i]
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
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
  ## run crosstab here 
  ### run moe here 
  testtab_party <-   moe_crosstab(df = temp_df,
                                  x =party_simp , y = temp_var , weight = final_weight, format = "long")
  testtab_party <- as.data.frame(testtab_party)
  testtab_party <- testtab_party[,c(1,2,3,4,5)]
  ### create a new n field 
  testtab_party$Num <- testtab_party$n*(testtab_party$pct/100)
  ### now subset 
  testtab_party <- testtab_party[,c(1,2,3,4,6)]
  testtab_party[,3] <- round_preserve_sum(testtab_party[,3],0)
  testtab_party[,4] <- round_preserve_sum(testtab_party[,4],1)
  testtab_party[,5] <- round_preserve_sum(testtab_party[,5],0)
  
  
  #colnames(testtab)[1] <- "Response"
  ### get factor attr
  testtab2 <- unnest(testtab_party, temp_var)
  testtab2 <- as.data.frame(testtab2)
  colnames(testtab2)[2] <- "Response"
  colnames(testtab2)[1] <- "Party"
  
  ## get rid of empty fields 
  testtab2 <- subset(testtab2, Response!="")
  ### now get the plot 
  temp_party_plot<-
    ggplot(testtab2,aes(x = pct, y = Response, xmin = (pct - moe), xmax = (pct + moe), color = Party)) +
    geom_pointrange(position = position_dodge(width = 0.2)) + xlim(0,100) +
    scale_y_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15)) + scale_color_manual(name=NULL,values=col_vec_party) +
    theme_minimal() + theme(axis.title = element_blank()) + ggtitle(title)
  ### now save to pdf 
  ## create names for plots and tables 
  table_temp <- paste0(temp_name,sep="_","table")
  bar_temp <- paste0(temp_name,sep="_","barplot")
  testtab2$Response <- sapply(lapply(testtab2$Response, strwrap, width=30), paste, collapse="\n")
  ### folder create 
  output_dir <- file.path(main_dir, "party_tabs/potus")
  if (!dir.exists(output_dir)){
    dir.create(output_dir)
  } else {
    print("Dir already exists!")
  }
  ### command 
  temp_png_tab <- paste0("party_tabs/potus",sep="/",table_temp,sep=".png")
  temp_png_bar <- paste0("party_tabs/potus",sep="/",bar_temp,sep=".png")
  
  png(temp_png_tab, width=7,height=14,units="in", res=400)       # Export PNG as table
  p<-tableGrob(testtab2,rows=NULL)
  grid.arrange(temp_party_plot,p)
  #grid::grid.text(title,x = (0.5), y = (0.8))
  #grid.table(testtab, rows=NULL)
  dev.off()
}

##### Most pressing issues #####
###age
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
  temp_df <- final_data2allwB
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  title = nov_q_items$V2[i]
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
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
  if(str_detect(unique(temp_df$temp_var)[1],"Gotten")==T){
    temp_df$temp_var[temp_df$temp_var=="Gotten somewhat worse"] <- "Worse"
    temp_df$temp_var[temp_df$temp_var=="Gotten much worse"] <- "Worse"
    #temp_df$temp_var[grep("Don", temp_df$temp_var)] <- "Neither/Don't know"
    temp_df$temp_var[temp_df$temp_var=="Gotten much better"] <- "Better"
    temp_df$temp_var[temp_df$temp_var=="Gotten somewhat better"] <- "Better"
    temp_df$temp_var[temp_df$temp_var!="Worse" & temp_df$temp_var!="Better"] <- "About same/Unsure"
  }
  ## run crosstab here 
  ### run moe here 
  testtab_party <-   moe_crosstab(df = temp_df,
                                  x =age_simp , y = temp_var , weight = final_weight, format = "long")
  testtab_party <- as.data.frame(testtab_party)
  testtab_party <- testtab_party[,c(1,2,3,4,5)]
  ### create a new n field 
  testtab_party$Num <- testtab_party$n*(testtab_party$pct/100)
  ### now subset 
  testtab_party <- testtab_party[,c(1,2,3,4,6)]
  testtab_party[,3] <- round_preserve_sum(testtab_party[,3],0)
  testtab_party[,4] <- round_preserve_sum(testtab_party[,4],1)
  testtab_party[,5] <- round_preserve_sum(testtab_party[,5],0)
  
  
  #colnames(testtab)[1] <- "Response"
  ### get factor attr
  testtab2 <- unnest(testtab_party, temp_var)
  testtab2 <- as.data.frame(testtab2)
  colnames(testtab2)[2] <- "Response"
  colnames(testtab2)[1] <- "Age"
  
  ## get rid of empty fields 
  testtab2 <- subset(testtab2, Response!="")
  ### now get the plot 
  temp_party_plot<-
    ggplot(testtab2,aes(x = pct, y = Response, xmin = (pct - moe), xmax = (pct + moe), color = Age)) +
    geom_pointrange(position = position_dodge(width = 0.2)) + xlim(0,100) +
    scale_y_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15)) + scale_color_manual(name=NULL,values=col_vec_party) +
    theme_minimal() + theme(axis.title = element_blank()) + ggtitle(title)
  ### now save to pdf 
  ## create names for plots and tables 
  table_temp <- paste0(temp_name,sep="_","table")
  bar_temp <- paste0(temp_name,sep="_","barplot")
  testtab2$Response <- sapply(lapply(testtab2$Response, strwrap, width=30), paste, collapse="\n")
  ### folder create 
  output_dir <- file.path(main_dir, "age_tabs/rightwrongtrack")
  if (!dir.exists(output_dir)){
    dir.create(output_dir)
  } else {
    print("Dir already exists!")
  }
  ### command 
  temp_png_tab <- paste0("age_tabs/rightwrongtrack",sep="/",table_temp,sep=".png")
  temp_png_bar <- paste0("age_tabs/rightwrongtrack",sep="/",bar_temp,sep=".png")
  
  png(temp_png_tab, width=7,height=14,units="in", res=400)       # Export PNG as table
  p<-tableGrob(testtab2,rows=NULL)
  grid.arrange(temp_party_plot,p)
  #grid::grid.text(title,x = (0.5), y = (0.8))
  #grid.table(testtab, rows=NULL)
  dev.off()
}
## by party results 
for (i in 1:length(nov_index)) {
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### subset those who do not want to vote 
  temp_df <- final_data2allwB
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  title = nov_q_items$V2[i]
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
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
  if(str_detect(unique(temp_df$temp_var)[1],"Gotten")==T){
    temp_df$temp_var[temp_df$temp_var=="Gotten somewhat worse"] <- "Worse"
    temp_df$temp_var[temp_df$temp_var=="Gotten much worse"] <- "Worse"
    #temp_df$temp_var[grep("Don", temp_df$temp_var)] <- "Neither/Don't know"
    temp_df$temp_var[temp_df$temp_var=="Gotten much better"] <- "Better"
    temp_df$temp_var[temp_df$temp_var=="Gotten somewhat better"] <- "Better"
    temp_df$temp_var[temp_df$temp_var!="Worse" & temp_df$temp_var!="Better"] <- "About same/Unsure"
  }
  ## run crosstab here 
  ### run moe here 
  testtab_party <-   moe_crosstab(df = temp_df,
                                  x =party_simp , y = temp_var , weight = final_weight, format = "long")
  testtab_party <- as.data.frame(testtab_party)
  testtab_party <- testtab_party[,c(1,2,3,4,5)]
  ### create a new n field 
  testtab_party$Num <- testtab_party$n*(testtab_party$pct/100)
  ### now subset 
  testtab_party <- testtab_party[,c(1,2,3,4,6)]
  testtab_party[,3] <- round_preserve_sum(testtab_party[,3],0)
  testtab_party[,4] <- round_preserve_sum(testtab_party[,4],1)
  testtab_party[,5] <- round_preserve_sum(testtab_party[,5],0)
  
  
  #colnames(testtab)[1] <- "Response"
  ### get factor attr
  testtab2 <- unnest(testtab_party, temp_var)
  testtab2 <- as.data.frame(testtab2)
  colnames(testtab2)[2] <- "Response"
  colnames(testtab2)[1] <- "Party"
  
  ## get rid of empty fields 
  testtab2 <- subset(testtab2, Response!="")
  ### now get the plot 
  temp_party_plot<-
    ggplot(testtab2,aes(x = pct, y = Response, xmin = (pct - moe), xmax = (pct + moe), color = Party)) +
    geom_pointrange(position = position_dodge(width = 0.2)) + xlim(0,100) +
    scale_y_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15)) + scale_color_manual(name=NULL,values=col_vec_party) +
    theme_minimal() + theme(axis.title = element_blank()) + ggtitle(title)
  ### now save to pdf 
  ## create names for plots and tables 
  table_temp <- paste0(temp_name,sep="_","table")
  bar_temp <- paste0(temp_name,sep="_","barplot")
  testtab2$Response <- sapply(lapply(testtab2$Response, strwrap, width=30), paste, collapse="\n")
  ### folder create 
  output_dir <- file.path(main_dir, "party_tabs/rightwrongtrack")
  if (!dir.exists(output_dir)){
    dir.create(output_dir)
  } else {
    print("Dir already exists!")
  }
  ### command 
  temp_png_tab <- paste0("party_tabs/rightwrongtrack",sep="/",table_temp,sep=".png")
  temp_png_bar <- paste0("party_tabs/rightwrongtrack",sep="/",bar_temp,sep=".png")
  
  png(temp_png_tab, width=7,height=14,units="in", res=400)       # Export PNG as table
  p<-tableGrob(testtab2,rows=NULL)
  grid.arrange(temp_party_plot,p)
  #grid::grid.text(title,x = (0.5), y = (0.8))
  #grid.table(testtab, rows=NULL)
  dev.off()
}

###### Toxic politics #####
nov_index <-  names(final_data2allwB)[grepl("toxic",colnames(final_data2allwB))]
nov_index <- nov_index[!grepl("txtentr", nov_index)]
nov_q_items <- q_items[grep("toxic",q_items$V1), ]
nov_q_items <- nov_q_items[!grepl("txtentr",nov_q_items$V1), ]
nrow(nov_q_items)==length(nov_index) # if true, good 

### run a loop on these; might create a vector for the q_items 
for (i in 1:length(nov_index)) {
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### subset those who do not want to vote 
  temp_df <- final_data2allwB
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  title = nov_q_items$V2[i]
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
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
  if(str_detect(unique(temp_df$temp_var)[1],"well")==T){
    temp_df$temp_var[temp_df$temp_var=="Not too well"] <- "Not well"
    temp_df$temp_var[temp_df$temp_var=="Not too well at all"] <- "Not well"
    #temp_df$temp_var[grep("Don", temp_df$temp_var)] <- "Neither/Don't know"
    temp_df$temp_var[temp_df$temp_var=="Very well"] <- "Well"
    temp_df$temp_var[temp_df$temp_var=="Somewhat well"] <- "Well"
    temp_df$temp_var[temp_df$temp_var!="Well" & temp_df$temp_var!="Not well"] <- "Not sure"
  }
  ## run crosstab here 
  ### run moe here 
  testtab_party <-   moe_crosstab(df = temp_df,
                                  x =age_simp , y = temp_var , weight = final_weight, format = "long")
  testtab_party <- as.data.frame(testtab_party)
  testtab_party <- testtab_party[,c(1,2,3,4,5)]
  ### create a new n field 
  testtab_party$Num <- testtab_party$n*(testtab_party$pct/100)
  ### now subset 
  testtab_party <- testtab_party[,c(1,2,3,4,6)]
  testtab_party[,3] <- round_preserve_sum(testtab_party[,3],0)
  testtab_party[,4] <- round_preserve_sum(testtab_party[,4],1)
  testtab_party[,5] <- round_preserve_sum(testtab_party[,5],0)
  
  
  #colnames(testtab)[1] <- "Response"
  ### get factor attr
  testtab2 <- unnest(testtab_party, temp_var)
  testtab2 <- as.data.frame(testtab2)
  colnames(testtab2)[2] <- "Response"
  colnames(testtab2)[1] <- "Age"
  
  ## get rid of empty fields 
  testtab2 <- subset(testtab2, Response!="")
  ### now get the plot 
  temp_party_plot<-
    ggplot(testtab2,aes(x = pct, y = Response, xmin = (pct - moe), xmax = (pct + moe), color = Age)) +
    geom_pointrange(position = position_dodge(width = 0.2)) + xlim(0,100) +
    scale_y_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15)) + scale_color_manual(name=NULL,values=col_vec_party) +
    theme_minimal() + theme(axis.title = element_blank()) + ggtitle(title)
  ### now save to pdf 
  ## create names for plots and tables 
  table_temp <- paste0(temp_name,sep="_","table")
  bar_temp <- paste0(temp_name,sep="_","barplot")
  testtab2$Response <- sapply(lapply(testtab2$Response, strwrap, width=30), paste, collapse="\n")
  ### folder create 
  output_dir <- file.path(main_dir, "age_tabs/toxic")
  if (!dir.exists(output_dir)){
    dir.create(output_dir)
  } else {
    print("Dir already exists!")
  }
  ### command 
  temp_png_tab <- paste0("age_tabs/toxic",sep="/",table_temp,sep=".png")
  temp_png_bar <- paste0("age_tabs/toxic",sep="/",bar_temp,sep=".png")
  
  png(temp_png_tab, width=7,height=14,units="in", res=400)       # Export PNG as table
  p<-tableGrob(testtab2,rows=NULL)
  grid.arrange(temp_party_plot,p)
  #grid::grid.text(title,x = (0.5), y = (0.8))
  #grid.table(testtab, rows=NULL)
  dev.off()
}
## by party results 
for (i in 1:length(nov_index)) {
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### subset those who do not want to vote 
  temp_df <- final_data2allwB
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  title = nov_q_items$V2[i]
  title <-  strwrap(title, width = 60, simplify = FALSE)
  title <- sapply(title, paste, collapse = "\n")
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
  if(str_detect(unique(temp_df$temp_var)[1],"well")==T){
    temp_df$temp_var[temp_df$temp_var=="Not too well"] <- "Not well"
    temp_df$temp_var[temp_df$temp_var=="Not too well at all"] <- "Not well"
    #temp_df$temp_var[grep("Don", temp_df$temp_var)] <- "Neither/Don't know"
    temp_df$temp_var[temp_df$temp_var=="Very well"] <- "Well"
    temp_df$temp_var[temp_df$temp_var=="Somewhat well"] <- "Well"
    temp_df$temp_var[temp_df$temp_var!="Well" & temp_df$temp_var!="Not well"] <- "Not sure"
  }
  ## run crosstab here 
  ### run moe here 
  testtab_party <-   moe_crosstab(df = temp_df,
                                  x =party_simp , y = temp_var , weight = final_weight, format = "long")
  testtab_party <- as.data.frame(testtab_party)
  testtab_party <- testtab_party[,c(1,2,3,4,5)]
  ### create a new n field 
  testtab_party$Num <- testtab_party$n*(testtab_party$pct/100)
  ### now subset 
  testtab_party <- testtab_party[,c(1,2,3,4,6)]
  testtab_party[,3] <- round_preserve_sum(testtab_party[,3],0)
  testtab_party[,4] <- round_preserve_sum(testtab_party[,4],1)
  testtab_party[,5] <- round_preserve_sum(testtab_party[,5],0)
  
  
  #colnames(testtab)[1] <- "Response"
  ### get factor attr
  testtab2 <- unnest(testtab_party, temp_var)
  testtab2 <- as.data.frame(testtab2)
  colnames(testtab2)[2] <- "Response"
  colnames(testtab2)[1] <- "Party"
  
  ## get rid of empty fields 
  testtab2 <- subset(testtab2, Response!="")
  ### now get the plot 
  temp_party_plot<-
    ggplot(testtab2,aes(x = pct, y = Response, xmin = (pct - moe), xmax = (pct + moe), color = Party)) +
    geom_pointrange(position = position_dodge(width = 0.2)) + xlim(0,100) +
    scale_y_discrete(labels = function(x) 
      stringr::str_wrap(x, width = 15)) + scale_color_manual(name=NULL,values=col_vec_party) +
    theme_minimal() + theme(axis.title = element_blank()) + ggtitle(title)
  ### now save to pdf 
  ## create names for plots and tables 
  table_temp <- paste0(temp_name,sep="_","table")
  bar_temp <- paste0(temp_name,sep="_","barplot")
  testtab2$Response <- sapply(lapply(testtab2$Response, strwrap, width=30), paste, collapse="\n")
  ### folder create 
  output_dir <- file.path(main_dir, "party_tabs/toxic")
  if (!dir.exists(output_dir)){
    dir.create(output_dir)
  } else {
    print("Dir already exists!")
  }
  ### command 
  temp_png_tab <- paste0("party_tabs/toxic",sep="/",table_temp,sep=".png")
  temp_png_bar <- paste0("party_tabs/toxic",sep="/",bar_temp,sep=".png")
  
  png(temp_png_tab, width=7,height=14,units="in", res=400)       # Export PNG as table
  p<-tableGrob(testtab2,rows=NULL)
  grid.arrange(temp_party_plot,p)
  #grid::grid.text(title,x = (0.5), y = (0.8))
  #grid.table(testtab, rows=NULL)
  dev.off()
}

##### Fix previous toplines (toxic; trump; missing titles ) #####

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
  if(str_detect(unique(temp_df$temp_var)[1],"well")==T){
    temp_df$temp_var[temp_df$temp_var=="Not too well"] <- "Not well"
    temp_df$temp_var[temp_df$temp_var=="Not too well at all"] <- "Not well"
    #temp_df$temp_var[grep("Don", temp_df$temp_var)] <- "Neither/Don't know"
    temp_df$temp_var[temp_df$temp_var=="Very well"] <- "Well"
    temp_df$temp_var[temp_df$temp_var=="Somewhat well"] <- "Well"
    temp_df$temp_var[temp_df$temp_var!="Well" & temp_df$temp_var!="Not well"] <- "Not sure"
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
  title = nov_q_items$V2[i]
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
  testtab2 <- subset(testtab2, Percent > 1)
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
###### Summary table on what people think of other party members ##### 
## for democrats, is toxic_ns_well2
## for GOP, it is toxic_ns_well3
## we want these for the other parties, and therefore subset thees 
nov_index_gop <-  names(final_data2allwB)[grepl("toxic_ns_well3",colnames(final_data2allwB))]
nov_index_dem <-  names(final_data2allwB)[grepl("toxic_ns_well2",colnames(final_data2allwB))]

### create vector in order 
toxic_trait_vec <- c("Lazy","Immoral","Dishonest","Unintelligent","Hardworking","Moral",
                     "Honest","Open-minded","Intelligent","Close-minded","Tolerant")


#nov_index <- nov_index[grepl("correct",nov_index)]
length(polknow_vec)==length(nov_index) # if true, good 

### get gop thoughts on democrats 
toxic_other_party_gop_on_dem <- data.frame()
for (i in 1:length(nov_index_dem)) {
  print(i)
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index_dem[i]]
  temp_gop <- subset(final_data2allwB, party_simp=="Republican/lean Republican")
  ### get title 
  temp_name <- colnames(final_data2allwB)[i]
  #title = final_data2lbl[temp_name]
  if(str_detect(unique(temp_gop$temp_var)[1],"well")==T){
    temp_gop$temp_var[temp_gop$temp_var=="Not too well"] <- "Not well"
    temp_gop$temp_var[temp_gop$temp_var=="Not too well at all"] <- "Not well"
    #temp_gop$temp_var[grep("Don", temp_gop$temp_var)] <- "Neither/Don't know"
    temp_gop$temp_var[temp_gop$temp_var=="Very well"] <- "Well"
    temp_gop$temp_var[temp_gop$temp_var=="Somewhat well"] <- "Well"
    temp_gop$temp_var[temp_gop$temp_var!="Well" & temp_gop$temp_var!="Not well"] <- "Not sure"
  }

  ### party cross tab 
  testtab <-   moe_topline(df = temp_gop, variable  =temp_var ,weight = final_weight)
  testtab <- unnest(testtab, Response)
  testtab <- as.data.frame(testtab) 
  testtab <- testtab[,c(1,2,3,5)]
  ## change name 
  colnames(testtab)[1] <- "Response"
  testtab[,2] <- round_preserve_sum(testtab[,2], 0)
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,4] <- round_preserve_sum(testtab[,4], 1)
  ### we only want those that fall under the category of "well"
  testtab <- subset(testtab,Response=="Well" )
  testtab$Trait <- toxic_trait_vec[i]
  ## now, get the results subsetted, then save and test in the R markdown 
  reduced_tab <- testtab[,c(5,2:4)]
  if(nrow(toxic_other_party_gop_on_dem)==0){
    toxic_other_party_gop_on_dem <- reduced_tab
  }else{
    toxic_other_party_gop_on_dem <- rbind(toxic_other_party_gop_on_dem,reduced_tab )
  }
}
### save data
## now sort on pct 
toxic_other_party_gop_on_dem <- toxic_other_party_gop_on_dem[order(-toxic_other_party_gop_on_dem$Percent),]
output_dir <- file.path(main_dir, "party_tabs/toxic_other")
if (!dir.exists(output_dir)){
  dir.create(output_dir)
} else {
  print("Dir already exists!")
}
saveRDS(toxic_other_party_gop_on_dem, "party_tabs/toxic_other/toxic_other_party_gop_on_dem.rds")

### now for Democrats thoughts on GOP 
toxic_other_party_dem_on_gop <- data.frame()
for (i in 1:length(nov_index_dem)) {
  print(i)
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index_gop[i]]
  temp_gop <- subset(final_data2allwB, party_simp=="Democrat/lean Democrat")
  ### get title 
  temp_name <- colnames(final_data2allwB)[i]
  #title = final_data2lbl[temp_name]
  if(str_detect(unique(temp_gop$temp_var)[1],"well")==T){
    temp_gop$temp_var[temp_gop$temp_var=="Not too well"] <- "Not well"
    temp_gop$temp_var[temp_gop$temp_var=="Not too well at all"] <- "Not well"
    #temp_gop$temp_var[grep("Don", temp_gop$temp_var)] <- "Neither/Don't know"
    temp_gop$temp_var[temp_gop$temp_var=="Very well"] <- "Well"
    temp_gop$temp_var[temp_gop$temp_var=="Somewhat well"] <- "Well"
    temp_gop$temp_var[temp_gop$temp_var!="Well" & temp_gop$temp_var!="Not well"] <- "Not sure"
  }
  
  ### party cross tab 
  testtab <-   moe_topline(df = temp_gop, variable  =temp_var ,weight = final_weight)
  testtab <- unnest(testtab, Response)
  testtab <- as.data.frame(testtab) 
  testtab <- testtab[,c(1,2,3,5)]
  ## change name 
  colnames(testtab)[1] <- "Response"
  testtab[,2] <- round_preserve_sum(testtab[,2], 0)
  testtab[,3] <- round_preserve_sum(testtab[,3], 0)
  testtab[,4] <- round_preserve_sum(testtab[,4], 1)
  ### we only want those that fall under the category of "well"
  testtab <- subset(testtab,Response=="Well" )
  testtab$Trait <- toxic_trait_vec[i]
  ## now, get the results subsetted, then save and test in the R markdown 
  reduced_tab <- testtab[,c(5,2:4)]
  if(nrow(toxic_other_party_dem_on_gop)==0){
    toxic_other_party_dem_on_gop <- reduced_tab
  }else{
    toxic_other_party_dem_on_gop <- rbind(toxic_other_party_dem_on_gop,reduced_tab )
  }
}
## now sort on pct then save  
toxic_other_party_dem_on_gop <- toxic_other_party_dem_on_gop[order(-toxic_other_party_dem_on_gop$Percent),]
saveRDS(toxic_other_party_dem_on_gop, "party_tabs/toxic_other/toxic_other_party_dem_on_gop.rds")

## test combine 
test_combine <- cbind(toxic_other_party_gop_on_dem,toxic_other_party_dem_on_gop)
test_combine <- test_combine[,c(1,3,5,7)]
colnames(test_combine) <- c("Republicans think Democrats are...", "% GOP", 
                            "Democrats think Republicans are...","% Dem")
###### Read in and create net pos and neg tables for the favorability ##### 
polknow_tab1 <- readRDS("favorability/favorable_tab.rds")
### we will want to drop neutral, then make wide 
polknow_tab1 <- subset(polknow_tab1, Response!="Not sure/Neutral")
polknow_tab1 <- subset(polknow_tab1, select=-c(Frequency, MOE))
polknow_tab1 <- polknow_tab1 %>% spread(Response, Percent)
## net 
polknow_tab1$net_fav <- polknow_tab1$Favorable-polknow_tab1$Unfavorable
saveRDS(polknow_tab1, "favorability/netfavorable_tab.rds")


### by party 
polknow_tab1party <- readRDS("party_tabs/favorability/favorable_tab.rds")
polknow_tab1party <- subset(polknow_tab1party, Response!="Neither/unsure")
polknow_tab1party <- subset(polknow_tab1party, select=-c(Num, moe))
polknow_tab1party <- polknow_tab1party %>% spread(Response, pct)
## net 
polknow_tab1party$net_fav <- polknow_tab1party$Favorable-polknow_tab1party$Unfavorable
##save 
saveRDS(polknow_tab1party, "party_tabs/favorability/netfavorable_tab.rds")


#### now, do the txtentry thing here; word cloud and dendrograms #####

# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#### functions for text ####
###necessary functions for text analysis that are user defined 
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
removeNumPunct<- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myStopWords<- c(setdiff(stopwords('english'), c("r","big")), "use", "see",
                "used", "via", "amp")
##second stem step
stemCompletion2<-function(x, dictionary){
  x<-unlist(strsplit(as.character(x), " "))
  x<- x[x != ""]
  x<-stemCompletion(x, dictionary=dictionary)
  x<-paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}
wordFreq <- function(corpus, word){
  results<-lapply(corpus,
                  function(x) {grep(as.character(x), 
                                    pattern=paste0("\\<", word))}
  )
  sum(unlist(results))
}
replaceWord <- function(corpus, oldword, newword){
  tm_map(corpus, content_transformer(gsub),
         pattern=oldword, replacement=newword)
}
##cleaning profile from Kelsey
cleaning.profile <- list(removePunctuation=T,stripWhitespace=T,removeNumbers=T,
                         tolower=T, removePunctuation=T,
                         removePunctuation.PlainTextDocument=T,
                         stopwords=T,weighting=weightTf,stem=T)
#####
### create a vector source 
docs <- Corpus(VectorSource(final_data2allwB$toxic_txtentr))
class(docs) # good, a simple corpus 
## clean the txt 
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
## further cleaning 
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
## remove words noted in what Rob wrote 
docs <- tm_map(docs, removeWords, c("people", "one","will","believe","open","taking",
                                    "going","want","still","like")) 
# people/one/will/believe/open/taking/going/want/still/like


# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
docs <- replaceWord(docs, "republicans", "republican")

### term matrix 
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
### can now create the wordcloud 
### this is good enough for the general I think 
png("toxic_txt/t_wordcloud.png", width=7,height=7,units="in", res=400)# Export PNG as table
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4)) 
plot.new()
text(x=0.5, y=0.5, "Perceived greatest threats to democracy")
set.seed(1234)
#title("Perceived greatest threats to democracy")
wordcloud(words = d$word, freq = d$freq, min.freq = 5,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
dev.off()

### create a loop by party 
if (!dir.exists("party_tabs/toxic_txt")){
  dir.create("party_tabs/toxic_txt")
} else {
  print("Dir already exists!")
}
party_vec <- sort(unique(final_data2allwB$party_simp))
party_name_vec <- c("Democrats", "Independents","Republicans")
for (i in 1:length(party_vec)) {
  temp_dfw <- subset(final_data2allwB,party_simp==party_vec[i] )
  docs <- Corpus(VectorSource(temp_dfw$toxic_txtentr))
  class(docs) # good, a simple corpus 
  ## clean the txt 
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  ## further cleaning 
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("blabla1", "blabla2"))
  ## remove words noted in what Rob wrote 
  docs <- tm_map(docs, removeWords, c("people", "one","will","believe","open","taking",
                                      "going","want","still","like"))
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  docs <- replaceWord(docs, "republicans", "republican")
  
  ### term matrix 
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d, 10)
  ### can now create the wordcloud 
  ### create title 
  title_new <- paste0("party_tabs/toxic_txt/", party_name_vec[i],sep=".png")
  ### this is good enough for the general I think 
  png(title_new, width=7,height=7,units="in", res=400)# Export PNG as table
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4)) 
  plot.new()
  text(x=0.5, y=0.5, paste0("Perceived greatest threats to democracy for ", party_name_vec[i]))
  set.seed(1234)
  #title("Perceived greatest threats to democracy")
  wordcloud(words = d$word, freq = d$freq, min.freq = 5,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  dev.off()
  
}
### word cloud by age 
if (!dir.exists("age_tabs/toxic_txt")){
  dir.create("age_tabs/toxic_txt")
} else {
  print("Dir already exists!")
}
age_vec <- sort(unique(final_data2allwB$age_simp))
party_name_vec <- c("Democrats", "Independents","Republicans")
for (i in 1:length(party_vec)) {
  temp_dfw <- subset(final_data2allwB,age_simp==age_vec[i] )
  docs <- Corpus(VectorSource(temp_dfw$toxic_txtentr))
  class(docs) # good, a simple corpus 
  ## clean the txt 
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  ## further cleaning 
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
  ## remove words noted in what Rob wrote 
  docs <- tm_map(docs, removeWords, c("people", "one","will","believe","open","taking",
                                      "going","want","still","like"))
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  docs <- replaceWord(docs, "republicans", "republican")
  
  ### term matrix 
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d, 10)
  ### can now create the wordcloud 
  ### create title 
  title_new <- paste0("age_tabs/toxic_txt/", age_vec[i],sep=".png")
  ### this is good enough for the general I think 
  png(title_new, width=7,height=7,units="in", res=400)# Export PNG as table
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4)) 
  plot.new()
  text(x=0.5, y=0.5, paste0("Perceived greatest threats to democracy for ", age_vec[i]))
  set.seed(1234)
  #title("Perceived greatest threats to democracy")
  wordcloud(words = d$word, freq = d$freq, min.freq = 5,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  dev.off()
  
}


##### Testing for Rmarkdown internal code #####
## now get the weighted ave then bind 
pid1_wt_ave <- final_data2allwB %>%
  dplyr::summarise(correct_pid1_total = weighted.mean(correct_total_pid1,final_weight))
pid1_wt_range <- wtd.quantile(final_data2allwB$correct_total_pid1,final_data2allwB$final_weight,
                              probs=c(0.25,0.5,0.75))
pid1_wt_range <- as.data.frame(pid1_wt_range)
pid1_wt_range <- t(pid1_wt_range)
pid1_wt_range <- as.data.frame(pid1_wt_range)
pid1_wt_range$Category <- "Institutions"
#pid1_wt_range$item <- "% correct"
colnames(pid1_wt_range) <- c("25th percentile", "Median", "75th percentile", "Category")
### repeat for the second 
pid2_wt_range <- wtd.quantile(final_data2allwB$correct_total_pid2,final_data2allwB$final_weight,
                              probs=c(0.25,0.5,0.75))
pid2_wt_range <- as.data.frame(pid2_wt_range)
pid2_wt_range <- t(pid2_wt_range)
pid2_wt_range <- as.data.frame(pid2_wt_range)
pid2_wt_range$Category <- "People"
#pid2_wt_range$item <- "% correct"
colnames(pid2_wt_range) <- c("25th percentile", "Median", "75th percentile", "Category")

## bind 
pid_df <- rbind(pid1_wt_range,pid2_wt_range)
pid_df <- pid_df[,c(4,1:3)]
pid_df[1,2:4] <- round((pid_df[1,2:4]/7)*100,1)
pid_df[2,2:4] <- round((pid_df[2,2:4]/6)*100,1)

### test and see if I can do the dplyr on this 
pid1_party <- final_data2allwB %>%
  group_by(party_simp) %>%
  dplyr::summarise(lower_bound=wtd.quantile(correct_total_pid2,final_weight,
                                            probs=c(0.25)),
                   median=wtd.quantile(correct_total_pid2,final_weight,
               probs=c(0.5)),
               upper_bound=wtd.quantile(correct_total_pid2,final_weight,
                            probs=c(0.75)))
## test rounding 
pid1_party[,2:4] <- round((pid1_party[,2:4]/7)*100,1)
