################################################################################
################## November 2023 Northern Poll -- Issue Deepdive  ###################################
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
library(ggpubr)
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

final_data2allwB <- readRDS("final_data2allwB.rds")

### look at marijuana usage overall 
table(final_data2allwB$mjiss_ns_use)
table(final_data2allwB$mjiss_nc_yn) # 399 say yes, 251 no, and 18 not comfortable 
399/(399+251+18) # that's 59.7% ; reports results in alphabetical order 
table(final_data2allwB$age) # Let's break this into age groups and see how these compare 


##### Plot the Y/N vote based upon interest in enthusiasm ####
nov_index <-  names(final_data2allwB)[grepl("novp",colnames(final_data2allwB))]
nov_q_items <- q_items[grep("novp",q_items$V1), ]
nrow(nov_q_items)==length(nov_index) # if true, good 
nov_q_items$V2

summary(final_data2allwB$nov_interest)
## get seq. 
novint_seq <- sort(unique(final_data2allwB$nov_interest))

### now, create a plot for the weighted pct by group of interest 
nov_vote_df <-  moe_crosstab(df = final_data2allwB,
                                x =nov_interest , y = mjiss_ns_sup, weight = final_weight, format = "long")
nov_vote_df <- as.data.frame(nov_vote_df)
### get factor attr
nov_vote_df <- unnest(nov_vote_df, mjiss_ns_sup)
nov_vote_df <- as.data.frame(nov_vote_df)

## subset to support only 
nov_vote_df <- subset(nov_vote_df,mjiss_ns_sup =="Support")
nov_vote_df <- subset(nov_vote_df, n > 1)

### now get the plot 
temp_int_plot<- ggplot(nov_vote_df, aes(x=nov_interest, y=pct)) + 
  geom_line() + 
  geom_point(aes(size=n)) + 
  geom_ribbon(aes(ymin=pct-moe, ymax=pct+moe), alpha=0.3) + theme_minimal() + 
  labs(y = "Support for Issue 2", x = "Interest in turning in November election.",
       caption=paste0("Interest created via an index of enthusiasm to vote in November election 
                      and level of thought about election. Weighted by plan to vote, with intent 
                      not to vote 0, not sure at 0.5, and yes at 1.
                      Shaded gray area reflects 95% confidence interval."),
       title="Support for Ohio marijuana legalization by interest \n in November issue elections") +
  geom_text(aes(x=nov_interest+0.005,y=pct+10,label=round(pct,1)),size=3) + scale_size(guide="none")
  
temp_int_plot
ggsave("nov_interest_and_issue2support.png" ,temp_int_plot, 
       scale=1,width=9,height=6,units = c("in"), dpi=400,bg="white")

### now, let's do support for issue 1 by treatment ###

### now, create a plot for the weighted pct by group of interest 
### referendum version
temp_dfA <- subset(final_data2allwB, novisA_dk_yn1 != "")
nov_vote_iss1dfA <-  moe_crosstab(df = temp_dfA,
                             x =nov_interest , y = novisA_dk_yn1, weight = final_weight, format = "long")
nov_vote_iss1dfA <- as.data.frame(nov_vote_iss1dfA)
### get factor attr
nov_vote_iss1dfA <- unnest(nov_vote_iss1dfA, novisA_dk_yn1)
nov_vote_iss1dfA <- as.data.frame(nov_vote_iss1dfA)

## subset to support only 
nov_vote_iss1dfA <- subset(nov_vote_iss1dfA,novisA_dk_yn1 =="Yes")
nov_vote_iss1dfA <- subset(nov_vote_iss1dfA, n > 1)
## lwv version
### now, create a plot for the weighted pct by group of interest
temp_dfB <- subset(final_data2allwB, novisB_dk_yn1 != "")
nov_vote_iss1dfB <-  moe_crosstab(df = temp_dfB,
                             x =nov_interest , y = novisB_dk_yn1, weight = final_weight, format = "long")
nov_vote_iss1dfB <- as.data.frame(nov_vote_iss1dfB)
### get factor attr
nov_vote_iss1dfB <- unnest(nov_vote_iss1dfB, novisB_dk_yn1)
nov_vote_iss1dfB <- as.data.frame(nov_vote_iss1dfB)

## subset to support only 
nov_vote_iss1dfB <- subset(nov_vote_iss1dfB,novisB_dk_yn1 =="Yes")
nov_vote_iss1dfB <- subset(nov_vote_iss1dfB, n > 1)

### now, combine the two 
nov_vote_iss1dfA$item <- "Actual ballot language"
nov_vote_iss1dfB$item <- "LWV language"
### change col name 
colnames(nov_vote_iss1dfA)[2] <- "support"
colnames(nov_vote_iss1dfB)[2] <- "support"
total_novissue1df <- rbind(nov_vote_iss1dfA,nov_vote_iss1dfB)

### now plot the results like for issue 2 
### now get the plot 
issue1_aint_plot<- ggplot(nov_vote_iss1dfA, aes(x=nov_interest, y=pct)) + 
  geom_line() + 
  geom_point(aes(size=n), col="darkred") + 
  geom_ribbon(aes(ymin=pct-moe, ymax=pct+moe), alpha=0.3, fill="red") + theme_minimal() + ylim(0,150) +
  labs(y = "Support for Issue 1", x = "Interest in turning in November election.",
       caption=paste0("Interest created via an index of enthusiasm to vote in November election 
                      and level of thought about election. Weighted by plan to vote, with intent 
                      not to vote 0, not sure at 0.5, and yes at 1.
                      Shaded gray area reflects 95% confidence interval."),
       title="Support for Ohio Right to Make Reproductive Decisions
       in November issue elections with Actual Ballot Language.") +
  geom_text(aes(x=nov_interest+0.005,y=pct+10,label=round(pct,1)),size=3) + scale_size(guide="none")
issue1_aint_plot
### now B 
issue1_bint_plot<- ggplot(nov_vote_iss1dfB, aes(x=nov_interest, y=pct)) + 
  geom_line() + 
  geom_point(aes(size=n), col="blue") + 
  geom_ribbon(aes(ymin=pct-moe, ymax=pct+moe), alpha=0.3, fill="lightblue") + theme_minimal() + ylim(0,150)+
  labs(y = "Support for Issue 1", x = "Interest in turning in November election.",
       caption=paste0("Interest created via an index of enthusiasm to vote in November election 
                      and level of thought about election. Weighted by plan to vote, with intent 
                      not to vote 0, not sure at 0.5, and yes at 1.
                      Shaded gray area reflects 95% confidence interval."),
       title="Support for Ohio Right to Make Reproductive Decisions
       in November issue elections with \n League of Women Voters Language.") +
  geom_text(aes(x=nov_interest+0.005,y=pct+10,label=round(pct,1)),size=3) + scale_size(guide="none")
issue1_bint_plot

### now combine them 
combined_plots <- ggarrange(issue1_aint_plot, issue1_bint_plot, 
                            ncol = 1, nrow = 2)
ggsave("issue1support_plots.png" ,plot=combined_plots, scale=1,width=6,height=11,units = c("in"),dpi=400,
       bg="white")

### if we were to weight, only 20.5 pct of Ohioans report having taken marijuana


###### Plots for Issues -- topline and cross tabs #######

###### Abortion Issue 1 Survey Experiment #### 
### now, lets do the analysis by the treatments of interest 
## create vector 
### now let's run the crosstab by party, and then gender. 
## party 
### correct party 
col_vec_party = c("blue","darkgreen","red")
## read in the data for the results 

nov_index <-  names(final_data2allwB)[grepl("novi",colnames(final_data2allwB))]
nov_q_items <- q_items[grep("novi",q_items$V1), ]
nrow(nov_q_items)==length(nov_index) # if true, good 
## create vector 
nov_title_vec <- c("Have you heard much about the November 7th issue election?",
                   "Do you agree with the proposed amendment? -- Referendum lang.",
                   "Do you plan to vote Yes or No on Issue 1? -- Referendum lang.",
                   "Do you agree with the proposed amendment? -- League of Women's voters lang.",
                   "Do you plan to vote Yes or No on Issue 1? -- League of Women's voters lang.")
### make sure folders exist 
main_dir <- getwd()
block_vec <- c("party_tabs","age_tabs","gender_tabs","toxic_txt")
for (i in 1:length(block_vec)) {
  sub_dir <- block_vec[i]
  output_dir <- file.path(main_dir, sub_dir)
  if (!dir.exists(output_dir)){
    dir.create(output_dir)
  } else {
    print("Dir already exists!")
  }
}

### run a loop on these; might create a vector for the q_items 
for (i in 1:length(nov_index)) {
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### subset those who do not want to vote 
  temp_df <- subset(final_data2allwB, nov_interest >= 0.5)
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  title = nov_title_vec[i]
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
  testtab_party$Num <- testtab_party$n*(testtab_party$pct/100)
  ### now subset 
  testtab_party <- testtab_party[,c(1,2,3,4,6)]
  testtab_party[,3] <- round_preserve_sum(testtab_party[,3],0)
  testtab_party[,4] <- round_preserve_sum(testtab_party[,4],1)
  testtab_party[,5] <- round_preserve_sum(testtab_party[,5],0)
  
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
  
  ### make folder
  if (!dir.exists("party_tabs/novi")){
    dir.create("party_tabs/novi")
  } else {
    print("Dir already exists!")
  }
  
  ### command 
  temp_png_tab <- paste0("party_tabs/novi",sep="/",table_temp,sep=".png")
  temp_png_bar <- paste0("party_tabs/novi",sep="/",bar_temp,sep=".png")
  
  png(temp_png_tab, width=7,height=10,units="in", res=400)       # Export PNG as table
  p<-tableGrob(testtab2,rows=NULL)
  grid.arrange(temp_party_plot,p)
  #grid::grid.text(title,x = (0.5), y = (0.8))
  #grid.table(testtab, rows=NULL)
  dev.off()
}

##### Crosstabs by gender ####
table(final_data2allwB$gender) ## out of balance 

for (i in 1:length(nov_index)) {
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### subset those who do not want to vote 
  temp_df <- subset(final_data2allwB, nov_interest >= 0.5)
  temp_df <- subset(temp_df, gender=="Female" | gender=="Male")
  ### get title 
  temp_name <- colnames(final_data2allwB)[colnames(final_data2allwB)==nov_index[i]]
  title = nov_title_vec[i]
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
  ## run crosstab here 
  ### run moe here 
  testtab_party <-   moe_crosstab(df = temp_df,
                                  x =gender , y = temp_var , weight = final_weight, format = "long")
  testtab_party <- as.data.frame(testtab_party)
  testtab_party$Num <- testtab_party$n*(testtab_party$pct/100)
  ### now subset 
  testtab_party <- testtab_party[,c(1,2,3,4,6)]
  testtab_party[,3] <- round_preserve_sum(testtab_party[,3],0)
  testtab_party[,4] <- round_preserve_sum(testtab_party[,4],1)
  testtab_party[,5] <- round_preserve_sum(testtab_party[,5],0)
  
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
  ### folder 
  ### make folder
  if (!dir.exists("gender_tabs/novi")){
    dir.create("gender_tabs/novi")
  } else {
    print("Dir already exists!")
  }
  ### command 
  temp_png_tab <- paste0("gender_tabs/novi",sep="/",table_temp,sep=".png")
  temp_png_bar <- paste0("gender_tabs/novi",sep="/",bar_temp,sep=".png")
  
  png(temp_png_tab, width=7,height=10,units="in", res=400)       # Export PNG as table
  p<-tableGrob(testtab2,rows=NULL)
  grid.arrange(temp_party_plot,p)
  #grid::grid.text(title,x = (0.5), y = (0.8))
  #grid.table(testtab, rows=NULL)
  dev.off()
}

###### Marijuana Redux -- topline and margins #####
### now let's go with the cross tabs from the other script 
nov_index <-  names(final_data2allwB)[grepl("mjiss",colnames(final_data2allwB))]
## subset further 
nov_index <- nov_index[1:7]
nov_q_items <- q_items[grep("mjiss",q_items$V1), ]
nov_q_items <- nov_q_items[1:7,]
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
  ### make sure folder exists 
  ### make folder
  if (!dir.exists("age_tabs/mjiss")){
    dir.create("age_tabs/mjiss")
  } else {
    print("Dir already exists!")
  }
  ### command 
  temp_png_tab <- paste0("age_tabs/mjiss",sep="/",table_temp,sep=".png")
  temp_png_bar <- paste0("age_tabs/mjiss",sep="/",bar_temp,sep=".png")
  
  png(temp_png_tab, width=7,height=10,units="in", res=400)       # Export PNG as table
  p<-tableGrob(testtab2,rows=NULL)
  grid.arrange(temp_party_plot,p)
  #grid::grid.text(title,x = (0.5), y = (0.8))
  #grid.table(testtab, rows=NULL)
  dev.off()
}

## create marijuana by use measure 
table(final_data2allwB$mjiss_nc_yn)
final_data2allwB$tried_marijuana <- "Yes"
final_data2allwB$tried_marijuana[final_data2allwB$mjiss_nc_yn!="Yes"] <- "No"

### not sure if I will do much given that; 

###### Pie charts of interest #####

## start with issue 1  ; we want to collapse to 3; combine neutrals with dont know
nov_index <-  names(final_data2allwB)[grepl("novi",colnames(final_data2allwB))]
nov_q_items <- q_items[grep("novi",q_items$V1), ]
nrow(nov_q_items)==length(nov_index) # if true, good 

### onu_color_vec creation
library(RColorBrewer)
library(colorspace)
pal_ext <- (brewer.pal(8,"Set2"))
onu_color_vec <- c("#f26b27",pal_ext )

## create vector 
nov_title_vec <- c("Have you heard much about the November 7th issue election?",
                   "Do you agree with the proposed amendment? -- Referendum lang.",
                   "Do you plan to vote Yes or No on Issue 1? -- Referendum lang.",
                   "Do you agree with the proposed amendment? -- League of Women's voters lang.",
                   "Do you plan to vote Yes or No on Issue 1? -- League of Women's voters lang.")
for (i in 1:length(nov_index)) {
  print(i)
  final_data2allwB$temp_var <- final_data2allwB[colnames(final_data2allwB)==nov_index[i]]
  ### subset those who do not want to vote 
  temp_df <- subset(final_data2allwB, nov_interest >= 0.5)
  
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
  
  ###folder
  ### make folder
  if (!dir.exists("pie_charts/novi")){
    dir.create("pie_charts/novi")
  } else {
    print("Dir already exists!")
  }
  
  ### command 
  temp_png_pie <- paste0("pie_charts/novi",sep="/",pie_temp,sep=".png")
  ggsave(temp_png_pie ,plot=temp_pirchart, scale=1,width=7,height=5,units = c("in"),dpi=400,
         bg="white")
  
}
### now abortion 

## start with issue 1  ; we want to collapse to 3; combine neutrals with dont know
nov_index <-  names(final_data2allwB)[grepl("abortion",colnames(final_data2allwB))]
nov_q_items <- q_items[grep("abortion",q_items$V1), ]
nrow(nov_q_items)==length(nov_index) # if true, good 

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
  
  ## folder
  if (!dir.exists("pie_charts/abortion")){
    dir.create("pie_charts/abortion")
  } else {
    print("Dir already exists!")
  }
  
  ### command 
  temp_png_pie <- paste0("pie_charts/abortion",sep="/",pie_temp,sep=".png")
  ggsave(temp_png_pie ,plot=temp_pirchart, scale=1,width=7,height=5,units = c("in"),dpi=400,
         bg="white")
  
}
