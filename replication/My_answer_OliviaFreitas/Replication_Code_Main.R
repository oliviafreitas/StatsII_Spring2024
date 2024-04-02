############################################################################################################################################
# The Cultural Origins of Populism ----------------------------
# Yotam Margalit, Shir Raviv and Omer Solodoch  ---------------------------
# This R file contains the code necessary to replicate the analysis in the main text ----------------------------
############################################################################################################################################
#### Clear environment and return memory to the operating system ---------
rm(list = ls())
gc()

#### Installing and loading needed packages -----------
# Load or install the required packages for this analysis
packages_required <- c(
  'stargazer', 'lmtest', 'haven', 'expss', 'dplyr', 'base','broom',
  'psych', 'srvyr','forcats','broom','rmarkdown', 'ggplot2',
  'ivmte', 'ggpubr', 'writexl', 'rio', 'patchwork',
  'Lahman', 'cregg', 'xtable', 'ggmosaic'
)

# Function to check and install any missing packages
install_and_load_packages <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package)
      library(package, character.only = TRUE)
    }
  }
}

# Apply the function to the list of required packages
install_and_load_packages(packages_required)


#### Set Working Directory  ############
# Set the working directory where the data is stored and where to save outputs

#setwd("") # Set your working directory to the top-level directory here ("JOP Replication")

setwd("C:/Users/User/Documents/GitHub/StatsII_Spring2024/replication/My_answer_OliviaFreitas")



plotpath<-"figures/"

#### Data ---------
ess_data <- readRDS("ESS_69_newvars.rds")


##### Figure (1) Socio-demographic Characteristics, By Voting  ------------------------------------#####
calculate_mean <- function(var, data) {
  var_name <- as.name(var)
  data %>%
    dplyr::filter(!is.na(!!var_name)) %>%
    as_survey_design(weights = c(fullweight)) %>%
    dplyr::group_by(cntry,vote_RightPop_factor) %>%
    dplyr::summarize(n = survey_mean(!!var_name, na.rm = FALSE, vartype = "ci"))
}

older55.pop_descriptive <- calculate_mean("older55", ess_data)
Rural.pop_descriptive <- calculate_mean("Rural_area", ess_data)
low_education.pop_descriptive <- calculate_mean("low_education", ess_data)
below_median_income.pop_descriptive <- calculate_mean("below_2median_income", ess_data)
econ_inse_obj.pop_descriptive <- calculate_mean("econ_inse_obj", ess_data)
very_relig.pop_descriptive <- calculate_mean("very_relig", ess_data)

fig1_demographics <- bind_rows(
  older55.pop_descriptive,
  Rural.pop_descriptive,
  low_education.pop_descriptive,
  below_median_income.pop_descriptive,
  econ_inse_obj.pop_descriptive,
  very_relig.pop_descriptive,
  .id="Characteristic")

fig1_demographics$Characteristic<-factor(fig1_demographics$Characteristic, levels=c(1,2,3,4,5,6),
                                         labels=c(
                                           "Older (55+)",
                                           "Rural residents",
                                           "Low education",
                                           "Low income",
                                           "Unemp. / insecure jobs",
                                           "Religious"
                                         ))

fig1_demographics$vote_RightPop_factor<-factor(fig1_demographics$vote_RightPop_factor)
fig1_demographics$vote_RightPop_factor <- relevel(fig1_demographics$vote_RightPop_factor, ref = "Right Wing Populist Parties")

figure1<-fig1_demographics %>%
  mutate(Characteristic = fct_reorder(Characteristic, desc(n))) %>%
  ggplot()+aes(x=Characteristic,y=n,fill=vote_RightPop_factor,colour=vote_RightPop_factor)+
  geom_col(alpha=0.9,width=0.5, position = position_dodge(width=0.7))+
  geom_errorbar(aes(ymin = n, 
                    ymax = n_upp), position = position_dodge(width=0.7),
                width = 0.5, lwd=0.4, color="gray40") +
  scale_fill_manual(values = c("#A20021", "#97C1D1"))+
  scale_colour_manual(values = c("#A20021", "#97C1D1"))+
  scale_y_continuous(labels=scales::percent, limits = c(0, 0.8)) +
  labs(x = " ", y = "",  fill = "", color = "") + coord_flip()+
  theme_minimal() + facet_wrap(facets=~cntry,nrow=2)+
  theme(legend.position = "bottom",  
         strip.text.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        plot.background = element_rect(fill = "white", colour = "white"),
        plot.margin = unit(c(0, 1, 0.5, 0), "cm"),
        panel.spacing = unit(1, "lines"))
figure1

ggsave(figure1,
       filename = paste0(plotpath, "fig1.tif"),
       device = "tiff",
       height = 5, width = 11, dpi = 600)


##### Figure (2) Share of Voters Whose Characteristics Match Each Storyline, by Vote ------------------------------------#####
calculate_mean <- function(var, data) {
  var_name <- as.name(var)
  data %>%
    dplyr::filter(!is.na(!!var_name)) %>%
    as_survey_design(weights = c(fullweight)) %>%
    dplyr::group_by(cntry,vote_RightPop_factor) %>%
    dplyr::summarize(n = survey_mean(!!var_name, na.rm = FALSE, vartype = "ci"))
}

White_Native_img_undermined_cult_pop <- calculate_mean("White_Native_img_undermined_cult", ess_data)
older55_authoritarian_pop <- calculate_mean("older55_authoritarian", ess_data)
rural_have_no_voice2_pop <- calculate_mean("rural_have_no_voice2", ess_data)
social_status25_respect2_pop <- calculate_mean("social_status25_respect2", ess_data)
distant_people_local_noturban_pop <- calculate_mean("distant_people_local_noturban", ess_data)

fig2_culturalstories <- bind_rows( White_Native_img_undermined_cult_pop,
                              older55_authoritarian_pop,
                              rural_have_no_voice2_pop,
                              distant_people_local_noturban_pop,
                              social_status25_respect2_pop,
                              .id="Characteristic")


fig2_culturalstories$Characteristic<-factor(fig2_culturalstories$Characteristic, 
                                       levels=c(1,2,3,4,5),
                                       labels=c("Ethno-Cultural Estrangement",
                                                "Intergenerational Backlash",
                                                "Rural Resentment",
                                                "Community Disintegration",
                                                "Social Status Anxiety"))

fig2_culturalstories<-fig2_culturalstories%>% 
  ungroup(cntry)%>% 
  add_row(Characteristic = "Community Disintegration", 
          cntry = "United Kingdom",
          vote_RightPop_factor="Right Wing Populist Parties",
          n=0, n_low=0,  n_upp=0)

fig2_culturalstories<-fig2_culturalstories%>% 
  add_row(Characteristic = "Social Status Anxiety", 
          cntry = "United Kingdom",
          vote_RightPop_factor="Right Wing Populist Parties",
          n=0, n_low=0,  n_upp=0)


fig2_culturalstories<-fig2_culturalstories%>% 
  add_row(Characteristic = "Community Disintegration", 
          cntry = "Germany",
          vote_RightPop_factor="Right Wing Populist Parties",
          n=0, n_low=0,  n_upp=0)


fig2_culturalstories<-fig2_culturalstories%>% 
  add_row(Characteristic = "Social Status Anxiety", 
          cntry = "Germany",
          vote_RightPop_factor="Right Wing Populist Parties",
          n=0, n_low=0,  n_upp=0)


fig2_culturalstories$cntry <- gsub("United Kingdom", "UK", fig2_culturalstories$cntry)

fig2_culturalstories$vote_RightPop_factor<-factor(fig2_culturalstories$vote_RightPop_factor)
fig2_culturalstories$vote_RightPop_factor <- relevel(fig2_culturalstories$vote_RightPop_factor, ref = "Right Wing Populist Parties")

figure2<-fig2_culturalstories %>%
  filter(cntry!="Spain")%>%
  mutate(Characteristic = fct_reorder(Characteristic, desc(n))) %>%
  ggplot()+aes(x=Characteristic,y=n,label=n,fill=vote_RightPop_factor,colour=vote_RightPop_factor)+
  geom_col(alpha=0.9,width=0.5, position = position_dodge(width=0.7))+
  geom_errorbar(aes(ymin = n, 
                    ymax = n_upp), position = position_dodge(width=0.7),
                width = 0.5, lwd=0.4, color="gray40") +
  scale_fill_manual(values = c("#A20021", "#97C1D1"))+
  scale_colour_manual(values = c("#A20021", "#97C1D1"))+
  scale_y_continuous(labels=scales::percent) +
  labs(x = " ", y = "", subtitle = "", title="", 
       fill = "", color = "") +  coord_flip()+
  theme_minimal() +facet_wrap(facets=~cntry,nrow=2)+
  theme(legend.position = "bottom",  
         strip.text.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        plot.background = element_rect(fill = "white", colour = "white"),
        plot.margin = unit(c(0, 1, 0.5, 0), "cm"),
        panel.spacing = unit(1, "lines"))
figure2
ggsave(figure2,
       filename = paste0(plotpath, "fig2.tif"),
       device = "tiff",
       height = 5, width = 11, dpi = 600)



##### Figure (3) Cultural Predictors of Voting for Right-Wing Populist Parties------------------------------------#####

coef_popvote_cultstories_bycntry2018 <- ess_data %>%
  filter(Wave %in% c("2018")) %>%
  group_by(cntry)%>%
  do(tidy(lm(vote_choice_PopList_RightPop ~                
               White_Native_img_undermined_cult+
               older55_authoritarian+
               rural_have_no_voice2,
             weights = fullweight, data = .)))


coef_popvote_cultstories_bycntry2016 <- ess_data %>%
  filter(Wave %in% c("2016")) %>%
  group_by(cntry)%>%
  do(tidy(lm(vote_choice_PopList_RightPop ~                
               White_Native_img_undermined_cult+
               older55_authoritarian+
               rural_have_no_voice2,
             weights = fullweight, data = .)))

coef_popvote_cultstories_bycntry2014 <- ess_data %>%
  filter(Wave %in% c("2014")) %>%
  group_by(cntry)%>%
  do(tidy(lm(vote_choice_PopList_RightPop ~                
               White_Native_img_undermined_cult+
               older55_authoritarian+
               rural_have_no_voice2,
             weights = fullweight, data = .)))

coef_popvote_cultstories_bycntry2012 <- ess_data %>%
  filter(Wave %in% c("2012")) %>%
  group_by(cntry)%>%
  do(tidy(lm(vote_choice_PopList_RightPop ~                
               White_Native_img_undermined_cult+
               older55_authoritarian+
               distant_people_local_noturban+
               social_status25_respect2,
             weights = fullweight, data = .)))


fig3_coef <- bind_rows( coef_popvote_cultstories_bycntry2018,
                                              coef_popvote_cultstories_bycntry2016,
                                              coef_popvote_cultstories_bycntry2014,
                                              coef_popvote_cultstories_bycntry2012,
                                              .id="Wave")

fig3_coef$Wave<-factor(fig3_coef$Wave, 
                                             levels=c(1,2,3,4),
                                             labels=c("2018",
                                                      "2016",
                                                      "2014",
                                                      "2012"))


fig3_coef<-fig3_coef%>% 
  filter(term %in% c("White_Native_img_undermined_cult", 
                     "older55_authoritarian", "rural_have_no_voice2",
                     "distant_people_local_noturban", "social_status25_respect2")) %>%
  mutate(term=case_when(
    term=="White_Native_img_undermined_cult"~"Ethno-Cultural Estrangement",
    term=="older55_authoritarian"~"Intergenerational Backlash",
    term=="rural_have_no_voice2"~"Rural Resentment",
    term=="social_status25_respect2"~"Social Status Anxiety",
    term=="distant_people_local_noturban"~"Community Disintegration"
  )) 

fig_3<-
  fig3_coef %>%  
  ggplot()+aes(term, estimate, fill=term, color=term, shape=Wave)+ 
  geom_hline(yintercept=0, linetype=2, lwd=0.9, size=1.3, colour = "#ACBBBF", alpha=0.9) +
  geom_errorbar(stat = "identity", alpha = 0.5, 
                position = position_dodge(width = 0.7),
                aes(ymin=estimate - 1.96*std.error, ymax=estimate + 1.96*std.error),
                lwd=0.8, width = 0)+
  geom_errorbar(stat = "identity", alpha = 0.8, 
                position = position_dodge(width = 0.7),
                aes(ymin=estimate - 1.64*std.error, ymax=estimate + 1.64*std.error), 
                lwd=0.6, width=0) +
  scale_colour_manual(values = c("#24416B","#2F7275","#80ABC6","#E84F5C","#96051A"))+
  scale_fill_manual(values = c("#24416B","#2F7275","#80ABC6","#E84F5C","#96051A"))+
  geom_point(stat = "identity", alpha = 0.7,  
             size = 1, position = position_dodge(width = 0.7)) + 
  guides(color = FALSE, fill = FALSE)+
  coord_flip() + 
  labs(x = "", y = " ", subtitle = "", shape="") +
  facet_wrap(facets=~cntry,nrow=2)+theme_minimal() +
  theme(legend.position = "bottom",  
         strip.text.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        plot.background = element_rect(fill = "white", colour = "white"),
        plot.margin = unit(c(0, 1, 0.5, 0), "cm"),
        panel.spacing = unit(1, "lines"))
fig_3

ggsave(fig_3,
       filename = paste0(plotpath, "fig3.tif"),
       device = "tiff",
       height = 5, width = 11, dpi = 600)


##### Figure (4) The Overlap between Cultural and Economic Concerns  ------------------------------------

# Select relevant columns from the original dataset
fig4_sumcult<-ess_data %>%
  dplyr::select(idno, Wave, cntry, fullweight, vote_choice_PopList_RightPop, vote_RightPop_factor, White_Native_img_undermined_cult, 
                older55_authoritarian, rural_have_no_voice2, social_status25_respect2, distant_people_local_noturban,
                unemployed_lookingforjob, production_worker, below_2median_income, difficult_subj )

# Calculate the sum of cultural stories for each respondent
fig4_sumcult<-fig4_sumcult %>%
  rowwise() %>%
  mutate(sum_stories_cult = sum(White_Native_img_undermined_cult, 
                                older55_authoritarian, 
                                rural_have_no_voice2,
                                social_status25_respect2, distant_people_local_noturban, na.rm = TRUE))

# Categorize respondents based on whether they have cultural concerns
fig4_sumcult$culturalconcern<- as.numeric (with(fig4_sumcult, ifelse ((sum_stories_cult!=0),1,0)))
fig4_sumcult$culturalconcern[is.na(fig4_sumcult$sum_stories_cult) ] <- NA


# Calculate the sum of economic stories for each respondent
fig4_sumcult<-fig4_sumcult %>%
  rowwise() %>%
  mutate(sum_stories_econ= sum(difficult_subj, 
                               below_2median_income, 
                               production_worker,
                               unemployed_lookingforjob, na.rm = TRUE))

# Categorize respondents based on whether they have economic concerns
fig4_sumcult$economicisequrity<- as.numeric (with(fig4_sumcult, ifelse ((sum_stories_econ!=0),1,0)))
fig4_sumcult$economicisequrity[is.na(fig4_sumcult$sum_stories_econ) ] <- NA

# Create a combined variable to capture both cultural and economic concerns
fig4_sumcult<-fig4_sumcult %>%
  mutate(cult_econ=case_when(economicisequrity==1 & culturalconcern==1 ~"Economic & Cultural concerns",
                             economicisequrity==1 & culturalconcern==0 ~"Economic concern",
                             economicisequrity==0 & culturalconcern==1 ~"Cultural concern",
                             economicisequrity==0 & culturalconcern==0 ~"Neither" ))
fig4_sumcult$cult_econ<-factor(fig4_sumcult$cult_econ, 
                                   levels=c(       "Neither",
                                                   "Economic concern",
                                                   "Economic & Cultural concerns",
                                                   "Cultural concern" ))


fig4_popshare <- fig4_sumcult %>%
  filter(Wave %in% c("2012","2014","2016", "2018")) %>%
  filter(cntry !="Spain") %>%
  as_survey(weights = c(fullweight)) %>%
  dplyr::group_by(cntry, vote_RightPop_factor, cult_econ) %>%
  dplyr::summarize(n = survey_total() ) %>%
  dplyr::ungroup(cntry, vote_RightPop_factor) %>%
  dplyr::group_by(cntry, vote_RightPop_factor) %>%
  dplyr::mutate(freq = n / sum(n))



fig4_popshare$cult_econ<-factor(fig4_popshare$cult_econ, 
                                         levels=c(       "Neither",
                                                         "Economic concern",
                                                         "Economic & Cultural concerns",
                                                         "Cultural concern" ))


fig4_popshare$vote_RightPop_factor<-factor(fig4_popshare$vote_RightPop_factor, 
                                                                 levels=c(  "Right Wing Populist Parties" ,
                                                                            "Other Parties"   ))
fig4_popshare$cntry
fig4_pop_cultecon <- fig4_popshare %>%
  arrange(vote_RightPop_factor,cult_econ, freq) 


fig4_pop_cultecon$cntry2<-factor(fig4_pop_cultecon$cntry, 
                                      levels=c( "Poland","Hungary",
                                               "Sweden","Germany",
                                                "Netherlands",
                                                "Switzerland",
                                                "United Kingdom", "Denmark","France","Italy"
                                      ))


figure4<- 
  fig4_pop_cultecon %>%
  ggplot() +
  aes(x = cntry2,
      fill = cult_econ,
      colour = cult_econ,
      weight = freq) +
  geom_bar(position = "fill", width=0.7) +
  hrbrthemes::theme_ipsum_rc() +
  scale_fill_manual(values = c("#DFEEEA", "#A7C4BC","#5E8B7E", "#2F5D62"))+
  scale_colour_manual(values = c("#DFEEEA", "#A7C4BC","#5E8B7E", "#2F5D62"))+
  labs(x = "", y = "", 
       fill = "", color = "") +  coord_flip()+  
  scale_y_continuous(labels=scales::percent) +
  facet_grid(vars(), vars(vote_RightPop_factor))+theme_minimal()+
  theme(legend.position = "bottom",  
         strip.text.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        plot.background = element_rect(fill = "white", colour = "white"),
        plot.margin = unit(c(0, 1, 0.5, 0), "cm"),
        panel.spacing = unit(1, "lines"))+
  guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE))

figure4

ggsave(figure4,
       filename = paste0(plotpath, "fig4.tif"),
       device = "tiff",
       height = 4, width = 9, dpi = 600)

#-------------------------------------------------------------------------------------------

# To understand whether the relationship between two variables changes based on their combined influence, let's explore interaction effects.
# Specifically, we will focus on the interplay between "economic insecurity" and "cultural concern" in shaping voting behavior.

# Load necessary libraries 
library(ggplot2)

# Create an interaction term
fig4_sumcult$interaction <- fig4_sumcult$economicisequrity * fig4_sumcult$culturalconcern

# Fit a linear regression model with the interaction term
model_with_interaction <- lm(vote_choice_PopList_RightPop ~ economicisequrity + culturalconcern + interaction, data = fig4_sumcult)

# Predicted vote
predicted_vote <- predict(model_with_interaction, newdata = fig4_sumcult)

# Create a data frame
df <- data.frame(economic_insecurity = fig4_sumcult$economicisequrity,
                 predicted_vote = predicted_vote,
                 cultural_concern = fig4_sumcult$culturalconcern)

# Scatter plot
ggplot(df, aes(x = economic_insecurity, y = predicted_vote, color = cultural_concern)) +
  geom_point() +
  labs(x = "Economic Insecurity", y = "Predicted Voting Choice",
       title = "Interaction Effect: Economic Insecurity vs. Voting Choice",
       color = "Cultural Concern") +
  theme_minimal()
