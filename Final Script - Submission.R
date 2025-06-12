#SET UP

#Install and load required packages

install.packages("haven")
install.packages("Rtools")
install.packages("dplyr")
install.packages("logistf") 
install.packages("missForest")
install.packages("ggplot2")
install.packages("car")

library(dplyr)
library(rlang)
library(haven)
library(logistf)
library(missForest)
library(ggplot2)
library(car)
library(tidyr)
library(tidyverse)
#Set working directory

setwd("C:/Users/green/OneDrive/Documents/DClinPsy/Thesis/NCDS");

ncds_0to16 <- read_spss("Age0-16_Sweeps0-3/ncds0123.sav");
ncds_age23 <- read_spss("Age23_Sweep4/ncds4.sav");
ncds_age23 <- read_spss("Age23_Sweep4/ncds4.sav");
ncds_age33mi <- read_spss("Age33_Sweep5/ncds5cmi.sav");
ncds_age33mc <- read_spss("Age33_Sweep5/ncds5mc.sav");
ncds_age33p <- read_spss("Age33_Sweep5/ncds5p.sav");
ncds_age42 <- read_spss("Age42_Sweep6/ncds6.sav")

#Checking the data is there

head(ncds_0to16,5)
head(ncds_age23, 5)
head(ncds_age33mi, 5)
head(ncds_age42, 5)

#For potential brain injury we want n2571, n2572, n2575, n2576, 2577, n2578, n2579
#3 = unconsciousness, #4=skull fracture
#just checking
ncds_0to16$n2571
table(ncds_0to16$n2571)

#Now, looking at all those with a potential brain injury
#First, create a temporary dataframe with a column of those who had unconsciousness only
#Second, add a column of those who had fractures only


tbi_unconc_frac <- ncds_0to16 %>%
  mutate(
    tbi_unconc = ifelse(n2571 == 3 | n2572 == 3 | n2575 == 3 | 
                          n2576 == 3 | n2577 == 3 | n2578 == 3 | n2579 == 3, 1, 0),
    tbi_frac = ifelse(n2571 == 4 | n2572 == 4 | n2575 == 4 | 
                        n2576 == 4 | n2577 == 4 | n2578 == 4 | n2579 == 4, 1, 0)
  ) %>%
  select(ncdsid, tbi_unconc, tbi_frac)

colnames(tbi_unconc_frac)

#making the NAs zero
#then checking

tbi_unconc_frac[is.na(tbi_unconc_frac)] <- 0

table(tbi_unconc_frac$tbi_unconc)
table(tbi_unconc_frac$tbi_frac)

#showing the number of participants with brain injury

count_rows_with_ones1 <- sum(rowSums(tbi_unconc_frac[, c("tbi_unconc", "tbi_frac")], na.rm = TRUE) > 0)
print(count_rows_with_ones1)

#BASELINE DF CREATION

#The beginnings of a df with just desired variables. First, ncds_0to16 data
#Sex = n622
#SES 11age = n1230 "financial hardship", n858 "free school meals"
#16age = n2441 "financial hardship", n2440 "free school meals"
#Brain injury 7age = n280 "concussion or head injury with unconsciousness"
#11age = n1282 "accident causing unconsciousness"
#16age = n2571, n2572, n2575, n2576, n2577, n2578, n2579 injury causing "unconsciousness" or "skull fracture" and A&E/Hospital admission
#Rutters A [age16, mh] = n2517, n2518, n2519, n2518, n2519, n2520,	n2521, n2522, n2523, 
#n2524, n2525, n2526, n2527, n2528, n2529, n2530, n2531, n2532, n2533,	n2534

my_study0to16v2 <- ncds_0to16[, c("ncdsid", "n622", "n1230", "n858", "n2441", "n2440",
                                  "n280", "n1282","n2571", "n2572", "n2575", "n2576", "n2577", "n2578", "n2579",
                                  "n2517", "n2518", "n2519", "n2520",	"n2521", "n2522", "n2523", "n2524", "n2525",
                                  "n2526", "n2527", "n2528", "n2529", "n2530", "n2531", "n2532", "n2533",	"n2534")]

colnames(my_study0to16v2)

#Next, making an age 23 df with only data wanted
#Malaise Inventory = n6016, n6017, n6018, n6019, n6020, n6021, n6022, n6023, n6024,
#n6025, n6026, n6027, n6028, n6029, n6030, n6031, n6032, n6033, n6034, n6035, 
#n6036, n6037, n6038, n6039


my_study23v2 <- ncds_age23 %>% select(c("ncdsid", 
                                        "n6016", "n6017", "n6018", "n6019", "n6020", "n6021",
                                        "n6022", "n6023", "n6024", "n6025", "n6026", "n6027", 
                                        "n6028", "n6029", "n6030", "n6031", "n6032", "n6033", 
                                        "n6034", "n6035", "n6036", "n6037", "n6038", "n6039"))

colnames(my_study23v2)

#Next, making an age 33 df with only data wanted
#Malaise = n504238, n504239, n504240, n504241, n504242, n504243, n504244, n504245, 
#n504246, n504247, n504248, n504249, n504250, n504251, n504252, n504253, n504254, 
#n504255, n504256, n504257, n504258, n504259, n504260, n504261

my_study33v2 <- ncds_age33mi %>% select(c("ncdsid",
                                          "n504238", "n504239", "n504240", "n504241", "n504242",
                                          "n504243", "n504244", "n504245", "n504246", "n504247",
                                          "n504248", "n504249", "n504250", "n504251", "n504252", 
                                          "n504253", "n504254", "n504255", "n504256", "n504257", 
                                          "n504258", "n504259", "n504260", "n504261"))
colnames(my_study33v2)

#Now, making an age 42 df with only data wanted
#Ethnicity = ethnic
#GHQ-12 = ghq1, ghq2, ghq3, ghq4, ghq5, ghq6, ghq7, ghq8, ghq9, ghq10, ghq11, ghq12
#Malaise =  mal01, mal02, mal03, mal04, mal05, mal06, mal07, mal08, mal09, mal10, 
#mal11, mal12, mal13, mal14, mal15, mal16, mal17, mal18, mal19, mal20, mal21, 
#mal22, mal23, mal24

my_study42v2 <- ncds_age42 %>% select(c("ncdsid", "ethnic",
                                        "ghq1", "ghq2", "ghq3", "ghq4", "ghq5", "ghq6", "ghq7", "ghq8",
                                        "ghq9", "ghq10", "ghq11", "ghq12",
                                        "mal01", "mal02", "mal03", "mal04", "mal05", "mal06", "mal07", "mal08", 
                                        "mal09", "mal10", "mal11", "mal12", "mal13", "mal14", "mal15", "mal16", 
                                        "mal17", "mal18", "mal19", "mal20", "mal21", "mal22", "mal23", "mal24"))

colnames(my_study42v2)


#Next, perform the left join on df1 and selected columns from df2

combined_all2 <- my_study0to16v2 %>%
  left_join(my_study23v2, by = "ncdsid") %>%
  left_join(my_study33v2, by = "ncdsid") %>%
  left_join(my_study42v2, by = "ncdsid") %>%
  left_join(tbi_unconc_frac, by = "ncdsid")

colnames(combined_all2)

#Great, now we have a baseline df
#Let's check for any duplicate IDs

any(duplicated(combined_all2$ncdsid))  #FALSE

#DEMOGRAPHICS

#Sex
# [1] male = 9595; [2] female = 8959; NA = 4

combined_all2 %>% 
  count(n622)

#Sex and BI
#Broad definition of TBI in that it includes someone who has either unconc or frac

combined_all2 %>%
  filter(tbi_unconc == 1 | tbi_frac == 1) %>%
  count(n622, name = "count")

combined_all2 %>%
  filter(tbi_unconc == 0 & tbi_frac == 0) %>% 
  count(n622, name = "count")

#BI males = 240, females = 121 [total = 361]
#nonBI males = 9355, females = 8838, na = 4 [total = 18197]
#361 + 18197 = 18558

#Ethnicity
# [1] British = 10941; [2] Irish = 44; [3] White Other = 163; [4] White & Black Caribbean = 17;
# [5] White & Black African = 2; [6] White & Asian = 9; [7] Other mixed race = 5; [8] Indian = 46;
# [9] Pakistani = 16; [11] Other Asian = 13; [12] Caribbean = 37; [13] African = 4; [14] Other Black = 18
# [15] Chinese = 2; [16] Other ethnic group = 68; [98] Dont know = 1; [99] Not answered = 1; NA = 7171

combined_all2 %>% 
  count(ethnic)

#Ethnicity and BI
combined_all2 %>%
  filter(tbi_unconc == 1 | tbi_frac == 1) %>%  
  count(ethnic, name = "count")

#Count ethnicity with BI - 
# [1] British = 254; [2] Irish = 1; [3] White Other = 7; [8] Indian = 1; 
# [12] Caribbean = 1; [16] Other ethnic group = 5; NA = 92

combined_all2 %>%
  filter(tbi_unconc == 0 & tbi_frac == 0) %>%  
  count(ethnic, name = "count")


#Count ethnicity without BI
# [1] British = 10687; [2] Irish = 43; [3] White Other = 156; [4] White & Black Caribbean = 17;
# [5] White & Black African = 2; [6] White & Asian = 9; [7] Other mixed race = 5; 
# [8] Indian = 45; [9] Pakistani = 16; [11] Other Asian = 13; [12] Caribbean = 36;
# [13] African = 4; [14] Other Black = 18; [15] Chinese= 2; [16] Other ethnic group = 63;
# [98] Dont know = 1; [99] Not answered = 1; NA = 7079


#Free School Meals (age 11)
# [1] Yes = 1434; [2] No = 12520; NA = 4604

combined_all2 %>% 
  count(n858)

#Count FSM with BI
combined_all2 %>%
  filter(tbi_unconc == 1 | tbi_frac == 1) %>% 
  count(n858, name = "count")

# [1] Yes = 38; [2] No = 267; NA = 56

#Count FSM without BI
combined_all2 %>%
  filter(tbi_unconc == 0 & tbi_frac == 0) %>% 
  count(n858, name = "count")

# [1] Yes = 1396; [2] No = 12253; NA = 4548

#Create a tbi_any column for the model
combined_all2 <- combined_all2 %>%
  mutate(tbi_any = ifelse(tbi_unconc == 1 | tbi_frac == 1, 1, 0))

table(combined_all2$tbi_any)
colnames(combined_all2)


#IMPUTATION
#As the counts were low in some of the areas, imputation of the variables is needed for the model

table(is.na(combined_all2$n858))

original_ethnic <- combined_all2$ethnic
original_fsm <- combined_all2$n858
original_n622 <- combined_all2$n622

impute_data <- combined_all2[, c("n622", "ethnic", "n858" )]

impute_data$ethnic <- as.factor(impute_data$ethnic)
impute_data$fsm <- as.factor(impute_data$n858)
impute_data$tbi_n622 <- as.factor(impute_data$n622)

impute_data_clean <- impute_data %>%
  zap_labels() %>% 
  mutate(across(everything(), ~ as.character(.))) %>%  
  mutate(across(everything(), ~ as.factor(.))) 

impute_data_clean_df <- as.data.frame(impute_data_clean)
set.seed(123)
imputed <- missForest(impute_data_clean_df)$ximp

combined_all2$fsm_imp <- imputed$fsm
combined_all2$ethnic_imp <- imputed$ethnic
combined_all2$n622_imp <- imputed$tbi_n622

table(is.na(combined_all2$ethnic_imp))
head(combined_all2$fsm)
colnames(combined_all2)

#Counts of Imputed Results

#Count sex (imputed) with BI
combined_all2 %>%
  filter(tbi_unconc == 1 | tbi_frac == 1) %>% 
  count(n622_imp, name = "count")

# [1] Male = 240, [2] Female = 121

#Count sex (imputed) without BI
combined_all2 %>%
  filter(tbi_unconc == 0 & tbi_frac == 0) %>% 
  count(n622_imp, name = "count")

# [1] Male = 9359, [2] Female = 8838

#Count ethnicity (imputed) with BI
combined_all2 %>%
  filter(tbi_unconc == 1 | tbi_frac == 1) %>% 
  count(ethnic_imp, name = "count")

#Count ethnicity with BI - 
# [1] British = 254; [2] Irish = 1; [3] White Other = 7; [4] = 18; [8] Indian = 1; 
# [12] Caribbean = 1; [15] = 62; [16] Other ethnic group = 5; 98 = 9, 99 = 3

#Count ethnicity (imputed) without BI
combined_all2 %>%
  filter(tbi_unconc == 0 & tbi_frac == 0) %>% 
  count(ethnic_imp, name = "count")

# [1] British = 10687; [2] Irish = 43; [3] White Other = 156; [4] = 2946; [5] = 2;
# [6] = 9; [7] = 5; [8] Indian = 45;  [9] = 16; [11] = 13; # [12] Caribbean = 36; [13] = 4; [14] = 18;
# [15] = 3590; [16] Other ethnic group = 63; [98] = 334, [99] = 230.

#Count fsm (imputed) with BI
combined_all2 %>%
  filter(tbi_unconc == 1 | tbi_frac == 1) %>% 
  count(fsm_imp, name = "count")

# [1] Yes = 38, [2] No = 323

#Count fsm (imputed) without BI
combined_all2 %>%
  filter(tbi_unconc == 0 & tbi_frac == 0) %>% 
  count(gen_knowledge_imp, name = "count")

# [1] Yes = 1396, [2] No = 16801

#Count fsm (imputed) with BI
combined_all2 %>%
  filter(tbi_unconc == 1 | tbi_frac == 1) %>% 
  count(fsm_imp, name = "count")


# PLOT DATA
# Before imputation vs after

ethnicity_palette <- c("Original" = "#a6cee3", "Imputed" = "#fdbf6f")
fsm_palette <- c("Original" = "#6a3d9a", "Imputed" = "#b2df8a")
sex_palette <- c("Original" = "#ff7f00", "Imputed" = "#cab2d6")

ethnic_labels <- c( 
  "British", "Irish", "White Other", "White & Black Caribbean", "White & Black African", 
  "White & Asian", "Other Mixed Race", "Indian", "Pakistani", "Other Asian", "Caribbean", "African", "Other Black", "Chinese",
  "Other Ethnic Group"
)

sex_labels <- c(
  "Male", "Female"
)

fsm_labels <- c(
  "Yes", "No")


# Free School Meals plot

plot_data_fsm <- data.frame(
  fsm = c(as.character(original_fsm), 
          as.character(imputed$fsm)),
  Source = rep(c("Original", "Imputed"), each = nrow(imputed))
)

myplot_fsm1 <- ggplot(plot_data_fsm, aes(x = factor(fsm, levels = c(1:13, NA)), fill = Source)) +
  geom_bar(position = "dodge") +
  scale_x_discrete(labels = fsm_labels) +
  labs(
    title = "Comparison of Accommodation (Original vs Imputed)",
    x = "Accommodation",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = c(fsm_palette))

ggsave("plots/fsm_comparison_plot1.png", plot = myplot_fsm1, width = 8, height = 6)

myplot_fsm2 <- ggplot(plot_data_fsm, aes(x = factor(fsm, levels = c(1:13, NA)), fill = Source)) +
  geom_bar(aes(y = ..prop.., group = Source), position = "dodge") +
  scale_x_discrete(labels = fsm_labels) +
  labs(
    title = "Proportion of Accommodation (Original vs Imputed)",
    x = "Accommodation",
    y = "Proportion"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c(fsm_palette))

ggsave("plots/fsm_comparison_plot2.png", plot = myplot_fsm2, width = 8, height = 6)

# Ethnicity plot

plot_data_eth <- data.frame(
  ethnic = c(as.character(original_ethnic), 
             as.character(imputed$ethnic)),
  Source = rep(c("Original", "Imputed"), each = nrow(imputed))
)

myplot_eth1 <- ggplot(plot_data_eth, aes(x = factor(ethnic, levels = c(1:15, NA)), fill = Source)) +
  geom_bar(position = "dodge") +
  scale_x_discrete(labels = ethnic_labels) +
  labs(
    title = "Comparison of Ethnicity (Original vs Imputed)",
    x = "Ethnicity",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = c(ethnicity_palette))

ggsave("plots/ethnicity_comparison_plot1.png", plot = myplot_eth1, width = 8, height = 6)

#Plot of proportions before and after to check the impute data proportioned it sufficiently
myplot_eth2 <- ggplot(plot_data_eth, aes(x = factor(ethnic, levels = c(1:13, NA)), fill = Source)) +
  geom_bar(aes(y = ..prop.., group = Source), position = "dodge") +
  scale_x_discrete(labels = ethnic_labels) +
  labs(
    title = "Proportion of Ethnicity (Original vs Imputed)",
    x = "Ethnicity",
    y = "Proportion"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c(ethnicity_palette))

ggsave("plots/ethnicity_comparison_plot2.png", plot = myplot_eth2, width = 8, height = 6)

# Sex plot

plot_data_sex <- data.frame(
  sex = c(as.character(original_n622), 
          as.character(imputed$n622)),
  Source = rep(c("Original", "Imputed"), each = nrow(imputed))
)

myplot_sex1 <- ggplot(plot_data_sex, aes(x = factor(sex, levels = c(1:2, NA)), fill = Source)) +
  geom_bar(position = "dodge") +
  scale_x_discrete(labels = sex_labels) +
  labs(
    title = "Comparison of Sex (Original vs Imputed)",
    x = "Sex",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = c(sex_palette))

ggsave("plots/sex_comparison_plot1.png", plot = myplot_sex1, width = 8, height = 6)

myplot_sex2 <- ggplot(plot_data_sex, aes(x = factor(sex, levels = c(1:2, NA)), fill = Source)) +
  geom_bar(aes(y = ..prop.., group = Source), position = "dodge") +
  scale_x_discrete(labels = sex_labels) +
  labs(
    title = "Proportion of Sex (Original vs Imputed)",
    x = "Sex",
    y = "Proportion"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c(sex_palette))

ggsave("plots/sex_comparison_plot2.png", plot = myplot_sex2, width = 8, height = 6)


#UNADJUSTED ASSOCIATIONS

#Will run the unadjusted models with Rutter's A (RSa), Malaise Inventory (MAL) and General Health Questionnaire (GHQ-12)

#RSa
# Note: Only for age 16
RSa_list2 <- combined_all2 %>%
  select("ncdsid", "n2517", "n2518", "n2519", "n2520", "n2521", "n2522", "n2523", "n2524", "n2525",
         "n2526", "n2527", "n2528", "n2529", "n2530", "n2531", "n2532", "n2533", "n2534") %>%
  mutate(across(c("n2517", "n2518", "n2519", "n2520", "n2521", "n2522", "n2523", "n2524", "n2525",
                  "n2526", "n2527", "n2528", "n2529", "n2530", "n2531", "n2532", "n2533", "n2534"), ~ case_when(
                    . == 1 ~ 1,
                    . == 2 ~ 2,
                    . == 3 ~ 3,
                    . == -1 ~ NA_real_,
                    . == 0 ~ NA_real_,
                    TRUE ~ NA_real_
                  ))) %>%
  mutate(RSa_Total2 = rowSums(select(., c("n2517", "n2518", "n2519", "n2520", "n2521", "n2522", "n2523", 
                                          "n2524", "n2525", "n2526", "n2527", "n2528", "n2529", "n2530", "n2531", 
                                          "n2532", "n2533", "n2534")), na.rm = TRUE))



print(RSa_list2$RSa_Total2)

colnames(RSa_list2)

combined_all2 <- combined_all2 %>%
  left_join(RSa_list2 %>% select(ncdsid, RSa_Total2), by = "ncdsid")

colnames(combined_all2)

RSa_unadj_mod2 <- lm(RSa_Total2 ~ tbi_any, data = combined_all2)
summary(RSa_unadj_mod2)
confint(RSa_unadj_mod2)


#GHQ-12
# Note: Only for age 42
GHQ_list2 <- combined_all2 %>%
  select("ncdsid", "ghq1", "ghq2", "ghq3", "ghq4", "ghq5", "ghq6", "ghq7", "ghq8",
         "ghq9", "ghq10", "ghq11", "ghq12") 

colnames(GHQ_list2)
GHQ_list2 <- GHQ_list2 %>%
  mutate(across(c("ghq1", "ghq2", "ghq3", "ghq4", "ghq5", "ghq6", "ghq7", "ghq8",
                  "ghq9", "ghq10", "ghq11", "ghq12"), ~ case_when(
                    . == 1 ~ 0,
                    . == 2 ~ 0,
                    . == 3 ~ 1,
                    . == 4 ~ 1,
                    . %in% c(8, 9) ~ NA_real_,
                    TRUE ~ NA_real_
                  ))) %>%
  mutate(GHQ_Total2 = rowSums(across(c("ghq1", "ghq2", "ghq3", "ghq4", "ghq5", "ghq6", "ghq7", "ghq8",
                                       "ghq9", "ghq10", "ghq11", "ghq12")), na.rm = TRUE))

colnames(GHQ_list2)

table(GHQ_list2$GHQ_Total2)

combined_all2 <- combined_all2 %>%
  left_join(GHQ_list2 %>% select(ncdsid, GHQ_Total2), by = "ncdsid")

colnames(combined_all2)

ghq_glm_unadj2 <- lm(GHQ_Total2 ~ tbi_any, data = combined_all2)
summary(ghq_glm_unadj2)
confint(ghq_glm_unadj2)

#MAL
# Note: MAL is available for ages 23, 33, and 42

#23

MAL_23_list2 <- combined_all2 %>%
  select("ncdsid", "n6016", "n6017", "n6018", "n6019", "n6020", "n6021", "n6022", 
         "n6023", "n6024", "n6025", "n6026", "n6027", "n6028", "n6029", 
         "n6030", "n6031", "n6032", "n6033", "n6034", "n6035", "n6036", 
         "n6037", "n6038", "n6039") 

colnames(MAL_23_list2)

MAL_23_list2 <- MAL_23_list2 %>%
  mutate(across(c("n6016", "n6017", "n6018", "n6019", "n6020", "n6021", "n6022", 
                  "n6023", "n6024", "n6025", "n6026", "n6027", "n6028", "n6029", 
                  "n6030", "n6031", "n6032", "n6033", "n6034", "n6035", "n6036", 
                  "n6037", "n6038", "n6039"), ~ case_when(
                    . == 1 ~ 1,  # Yes 
                    . == 2 ~ 0,  # No 
                    . %in% c(8, 9) ~ NA_real_,  # Don't know & Not answered 
                    TRUE ~ NA_real_
                  ))) %>%
  mutate(MAL_23_Total2 = rowSums(across(c("n6016", "n6017", "n6018", "n6019", "n6020", "n6021", "n6022", 
                                          "n6023", "n6024", "n6025", "n6026", "n6027", "n6028", "n6029", 
                                          "n6030", "n6031", "n6032", "n6033", "n6034", "n6035", "n6036", 
                                          "n6037", "n6038", "n6039")), na.rm = TRUE))

colnames(MAL_23_list2)
table(MAL_23_list2$MAL_23_Total2)

combined_all2 <- combined_all2 %>%
  left_join(MAL_23_list2 %>% select(ncdsid, MAL_23_Total2), by = "ncdsid")

colnames(combined_all2)

MAL_glm_23_unadj2 <- lm(MAL_23_Total2 ~ tbi_any, data = combined_all2)
summary(MAL_glm_23_unadj2)
confint(MAL_glm_23_unadj2)

#33

MAL_33_list2 <- combined_all2 %>%
  select("ncdsid", "n504238", "n504239", "n504240", "n504241", "n504242", "n504243", "n504244", "n504245",
         "n504246", "n504247", "n504248", "n504249", "n504250", "n504251", "n504252", "n504253", "n504254",
         "n504255", "n504256", "n504257", "n504258", "n504259", "n504260", "n504261") 

colnames(MAL_33_list2)

MAL_33_list2 <- MAL_33_list2 %>%
  mutate(across(c("n504238", "n504239", "n504240", "n504241", "n504242", "n504243", "n504244", "n504245",
                  "n504246", "n504247", "n504248", "n504249", "n504250", "n504251", "n504252", "n504253", "n504254",
                  "n504255", "n504256", "n504257", "n504258", "n504259", "n504260", "n504261"), ~ case_when(
                    . == 1 ~ 1,  # Yes ??? 1
                    . == 2 ~ 0,  # No ??? 0
                    . %in% c(8, 9) ~ NA_real_,  # Don't know & Not answered ??? NA
                    TRUE ~ NA_real_
                  ))) %>%
  mutate(MAL_33_Total2 = rowSums(across(c("n504238", "n504239", "n504240", "n504241", "n504242", "n504243", "n504244", "n504245",
                                          "n504246", "n504247", "n504248", "n504249", "n504250", "n504251", "n504252", "n504253", "n504254",
                                          "n504255", "n504256", "n504257", "n504258", "n504259", "n504260", "n504261")), na.rm = TRUE))

colnames(MAL_33_list2)
table(MAL_33_list2$MAL_33_Total2)

combined_all2 <- combined_all2 %>%
  left_join(MAL_33_list2 %>% select(ncdsid, MAL_33_Total2), by = "ncdsid")

colnames(combined_all2)

MAL_glm_33_unadj2 <- lm(MAL_33_Total2 ~ tbi_any, data = combined_all2)
summary(MAL_glm_33_unadj2)
confint(MAL_glm_33_unadj2)

#42

MAL_42_list2 <- combined_all2 %>%
  select("ncdsid", "mal01", "mal02", "mal03", "mal04", "mal05", "mal06", "mal07", "mal08", 
         "mal09", "mal10", "mal11", "mal12", "mal13", "mal14", "mal15", "mal16", 
         "mal17", "mal18", "mal19", "mal20", "mal21", "mal22", "mal23", "mal24") 

colnames(MAL_42_list2)

MAL_42_list2 <- MAL_42_list2 %>%
  mutate(across(c("mal01", "mal02", "mal03", "mal04", "mal05", "mal06", "mal07", "mal08", 
                  "mal09", "mal10", "mal11", "mal12", "mal13", "mal14", "mal15", "mal16", 
                  "mal17", "mal18", "mal19", "mal20", "mal21", "mal22", "mal23", "mal24"), ~ case_when(
                    . == 1 ~ 1,  # Yes ??? 1
                    . == 2 ~ 0,  # No ??? 0
                    . %in% c(8, 9) ~ NA_real_,  # Don't know & Not answered ??? NA
                    TRUE ~ NA_real_
                  ))) %>%
  mutate(MAL_42_Total2 = rowSums(across(c("mal01", "mal02", "mal03", "mal04", "mal05", "mal06", "mal07", "mal08", 
                                          "mal09", "mal10", "mal11", "mal12", "mal13", "mal14", "mal15", "mal16", 
                                          "mal17", "mal18", "mal19", "mal20", "mal21", "mal22", "mal23", "mal24")), na.rm = TRUE))


colnames(MAL_42_list2)
table(MAL_42_list2$MAL_42_Total2)

combined_all2 <- combined_all2 %>%
  left_join(MAL_42_list2 %>% select(ncdsid, MAL_42_Total2), by = "ncdsid")

colnames(combined_all2)

MAL_glm_42_unadj2 <- lm(MAL_42_Total2 ~ tbi_any, data = combined_all2)
summary(MAL_glm_42_unadj2)
confint(MAL_glm_42_unadj2)


#ADJUSTED ASSOCIATIONS

# Will run the adjusted models with Rutter's A (RSa), Malaise Inventory (MAL) and General Health Questionnaire (GHQ-12)
# Will look at sex, ethnicity, and free school meals

#RSa
# Given how small the counts were in some of the variables, it was decided to group some categories
# After adjusting the variables to factors (as opposed to numeric)

combined_all2 <- combined_all2 %>%
  mutate(
    ethnicity_group = case_when(
      ethnic_imp %in% c(98, 99) ~ NA_character_,
      ethnic_imp == 1 ~ "British",
      ethnic_imp %in% c(2, 3) ~ "White Other",
      ethnic_imp %in% c(4, 5, 6, 7) ~ "Mixed Race",
      ethnic_imp %in% c(8, 9, 11, 15) ~ "Asian",
      ethnic_imp %in% c(12, 13, 14) ~ "Black",
      ethnic_imp == 16 ~ "Other Ethnic Group",
      TRUE ~ NA_character_
    )
  )

table(combined_all2$ethnicity_group)   

# To decide which to set as the reference before reallocating the class
# Sex = 1 (male)
# Ethnic = 1 (British)
# FSM = 2 (Middle)

sex_counts <- table(combined_all2$n622)
sex_percentages <- sex_counts / sum(sex_counts) * 100
data.frame(
  Count = sex_counts,
  Percentage = sex_percentages
)

ethnic_counts <- table(combined_all2$ethnicity_group)
ethnic_percentages <- ethnic_counts / sum(ethnic_counts) * 100
data.frame(
  Count = ethnic_counts,
  Percentage = ethnic_percentages
)

fsm_counts <- table(combined_all2$fsm)
fsm_percentages <- fsm_counts / sum(fsm_counts) * 100
data.frame(
  Count = fsm_counts,
  Percentage = fsm_percentages
)

combined_all2$n622 <- relevel(factor(combined_all2$n622), ref = "1")
combined_all2$ethnicity_group <- relevel(factor(combined_all2$ethnicity_group), ref = "British")
combined_all2$fsm_imp <- relevel(factor(combined_all2$fsm_imp), ref = "2")

sapply(combined_all2[c("ethnicity_group", "n622", "fsm_imp")], class)




RSa_adj_Mod <- lm(RSa_Total2 ~ tbi_any + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(RSa_adj_Mod)
confint(RSa_adj_Mod)


#Model assumptions

   vif(RSa_adj_Mod) #no multicollinearity assumption met
   durbinWatsonTest(RSa_adj_Mod) # independent errors assumption met
   png("linearity.png", width = 800, height = 600)  
      plot(RSa_adj_Mod, which = 1)
      dev.off() #linearity assumption met
    png("homoscedasticity.png", width = 800, height = 600)
      plot(RSa_adj_Mod$fitted.values, rstandard(RSa_adj_Mod),
           xlab = "Fitted values", ylab = "Standardised residuals",
           main = "Homoscedasticity check")
      abline(h = 0, col = "red")
      dev.off() #no major violation, but some clustering due to categorical predictors
    png("residuals.png", width = 800, height = 600)  
      hist(residuals(RSa_adj_Mod))
      dev.off() #skewed left (negatively skewed), not a symmetrical bell shape, some kurtosis (peakedness),
         #Linear models are robust to non-normality, in large sample sizes & aim to estimating coefficients and assessing model fit rather than making strong inferences  
    
    
#GHQ-12

#Model 2
ghq_glm_adj2 <- lm(GHQ_Total2 ~ tbi_any + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(ghq_glm_adj2)
confint(ghq_glm_adj2)


#MAL

#23

#Model 2
MAL_glm_23_adj2 <- lm(MAL_23_Total2 ~ tbi_any + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(MAL_glm_23_adj2)
confint(MAL_glm_23_adj2)


#33

#Model 2
MAL_glm_33_adj2 <- lm(MAL_33_Total2 ~ tbi_any + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(MAL_glm_33_adj2)
confint(MAL_glm_33_adj2)


#42

#Model 3
MAL_glm_42_adj2 <- lm(MAL_42_Total2 ~ tbi_any + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(MAL_glm_42_adj2)
confint(MAL_glm_42_adj2)

#SUBSCALES

#MAL has two sub-scales (psychological and somatic), so will look at those

#MAL

#23
#Psychological = n6017, n6018, n6020, n6021, n6022, n6023, n6024, n6025, n6027, n6028,
#n6029, n6030, n6031,  n6034, n6035
#Somatic = n6016, n6019, n6026, n6032, n6033, n6036, n6037, n6038

MAL_23_list2 <- MAL_23_list2 %>%
  rowwise() %>%
  mutate(
    mal_psych_23 = sum(c_across(c("n6017", "n6018", "n6020", "n6021", "n6022", "n6023", "n6024", "n6025", "n6027", "n6028",
                                  "n6029", "n6030", "n6031",  "n6034", "n6035")), na.rm = TRUE)
  ) %>%
  ungroup()

table(MAL_23_list2$mal_psych_23)

MAL_23_list2 <- MAL_23_list2 %>%
  rowwise() %>%
  mutate(
    mal_somatic_23 = sum(c_across(c("n6016", "n6019", "n6026", "n6032", "n6033",
                                    "n6036", "n6037", "n6038")), na.rm = TRUE)
  ) %>%
  ungroup()

table(MAL_23_list2$mal_somatic_23)

colnames(MAL_23_list2)

#Join into combined_all2 df

combined_all2 <- combined_all2 %>%
  left_join(MAL_23_list2 %>% select(ncdsid, mal_psych_23, mal_somatic_23), by = "ncdsid")


#Unadjusted associations subscales
glm_psych_23_model <- lm(mal_psych_23 ~ tbi_any, data = combined_all2)
summary(glm_psych_23_model)
confint(glm_psych_23_model)

glm_som_23_model <- lm(mal_somatic_23 ~ tbi_any, data = combined_all2)
summary(glm_som_23_model)
confint(glm_som_23_model)


#Adjusted associations sub-scales: Model 2
glm_psych_23_adj2 <- lm(mal_psych_23 ~ tbi_any + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(glm_psych_23_adj2)
confint(glm_psych_23_adj2)

glm_som_23_adj2 <- lm(mal_somatic_23 ~ tbi_any + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(glm_som_23_adj2)
confint(glm_som_23_adj2)

#33
#Psychological = n504239, n504240,  n504242, n504243, n504244, n504245, n504246, n504247, n504249, n504250, n504251, n504252, n504253, n504256, n504257
#Somatic = n504238, n504241, n504248, n504254, n504255, n504258, n504259, n504260

MAL_33_list2 <- MAL_33_list2 %>%
  rowwise() %>%
  mutate(
    mal_psych_33 = sum(c_across(c("n504239", "n504240",  "n504242", "n504243", "n504244", 
                                  "n504245", "n504246", "n504247", "n504249", "n504250", "n504251", "n504252", "n504253",
                                  "n504256", "n504257")), na.rm = TRUE)
  ) %>%
  ungroup()

table(MAL_33_list2$mal_psych_33)

MAL_33_list2 <- MAL_33_list2 %>%
  rowwise() %>%
  mutate(
    mal_somatic_33 = sum(c_across(c("n504238", "n504241", "n504248", "n504254", "n504255",
                                    "n504258", "n504259", "n504260")), na.rm = TRUE)
  ) %>%
  ungroup()

table(MAL_33_list2$mal_somatic_33)

colnames(MAL_33_list2)


#Join into combined_all2 df

combined_all2 <- combined_all2 %>%
  left_join(MAL_33_list2 %>% select(ncdsid, mal_psych_33, mal_somatic_33), by = "ncdsid")


#Unadjusted associations subscales
glm_psych_33_model <- lm(mal_psych_33 ~ tbi_any, data = combined_all2)
summary(glm_psych_33_model)
confint(glm_psych_33_model)

glm_som_33_model <- glm(mal_somatic_33 ~ tbi_any, data = combined_all2)
summary(glm_som_33_model)
confint(glm_som_33_model)

#Adjusted associations sub-scales: Model 2
glm_psych_33_adj2 <- lm(mal_psych_33 ~ tbi_any + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(glm_psych_33_adj2)
confint(glm_psych_33_adj2)

glm_som_33_adj2 <- lm(mal_somatic_33 ~ tbi_any + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(glm_som_33_adj2)
confint(glm_som_33_adj2)


#42
#Psychological = mal02, mal03,  mal05, mal06, mal07, mal08, mal09, mal10, mal12, mal13, mal14, mal15, mal16, mal19, mal20
#Somatic = mal01, mal04, mal11, mal17, mal18, mal21, mal22, mal23

MAL_42_list2 <- MAL_42_list2 %>%
  rowwise() %>%
  mutate(
    mal_psych = sum(c_across(c("mal02", "mal03", "mal05", "mal06", "mal07", "mal08", "mal09", "mal10", 
                               "mal12", "mal13", "mal14", "mal15", "mal16", "mal19", "mal20")), na.rm = TRUE)
  ) %>%
  ungroup()

table(MAL_42_list2$mal_psych)

MAL_42_list2 <- MAL_42_list2 %>%
  rowwise() %>%
  mutate(
    mal_somatic = sum(c_across(c("mal01", "mal04", "mal11", "mal17", "mal18", "mal21", "mal22", 
                                 "mal23")), na.rm = TRUE)
  ) %>%
  ungroup()

table(MAL_42_list2$mal_somatic)

colnames(MAL_42_list2)

#Join into combined_all2 df

combined_all2 <- combined_all2 %>%
  left_join(MAL_42_list2 %>% select(ncdsid, mal_psych, mal_somatic), by = "ncdsid")

colnames(combined_all2)

#Unadjusted associations subscales
glm_psych_model <- lm(mal_psych ~ tbi_any, data = combined_all2)
summary(glm_psych_model)
confint(glm_psych_model)

glm_som_model <- lm(mal_somatic ~ tbi_any,data = combined_all2)
summary(glm_som_model)
confint(glm_som_model)


#Adjusted associations sub-scales: Model 2 
glm_psych_adj2 <- lm(mal_psych ~ tbi_any + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(glm_psych_adj2)
confint(glm_psych_adj2)

glm_som_adj2 <- lm(mal_somatic ~ tbi_any + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(glm_som_adj2)
confint(glm_som_adj2)


#SENSITIVITY AND SPECIFICITY

#Severity 1: none, mild and severe based on having episodes of unconc and frac

tbi_sense2 <- combined_all2 %>% 
  transmute(
    ncdsid, 
    tbi_unconc = ifelse(n2571 == 3 | n2572 == 3 | n2575 == 3 | 
                          n2576 == 3 | n2577 == 3 | n2578 == 3 | n2579 == 3, 1, 0),
    tbi_frac   = ifelse(n2571 == 4 | n2572 == 4 | n2575 == 4 | 
                          n2576 == 4 | n2577 == 4 | n2578 == 4 | n2579 == 4, 1, 0)
  )

# Step 2: Count the total number of injuries for each person
tbi_sense2 <- tbi_sense2 %>%
  mutate(tbi_count = rowSums(tbi_sense2[, c("tbi_unconc", "tbi_frac")], na.rm = TRUE))

#Step 3: Categorize severity based on T0 = no TBI history
#T1 = either unconsciousness or fracture, 
#T2 = both unconsciousness and fracture
tbi_sense2 <- tbi_sense2 %>%
  mutate(tbi_severity = case_when(
    tbi_count == 0 ~ "T0",
    tbi_count == 1 ~ "T1",
    tbi_count > 1 ~ "T2"
  ))

# Step 4: Check the distribution of severity categories
table(tbi_sense2$tbi_severity)

head(tbi_sense2)

combined_all2 <- combined_all2 %>%
  left_join(tbi_sense2 %>% select(ncdsid, tbi_severity), by = "ncdsid")

combined_all2 %>% 
  count(tbi_severity)

# [1] T0 = 18197, [2] T1 unc or frac = 338, [3] T2 unc and frac = 23

#Severity 2: none, mild and severe based on number of counts for unconc and frac

tbi_sense3 <- combined_all2 %>%
  transmute(
    ncdsid, 
    tbi_events_count = rowSums(across(c(n2571, n2572, n2575, n2576, n2577, n2578, n2579),
                                      ~ .x %in% c(3, 4)), na.rm = TRUE)
  )

colnames(tbi_sense3)

tbi_sense3 <- tbi_sense3 %>%
  mutate(
    tbi_freq_cat = case_when(
      tbi_events_count == 0 ~ "None",
      tbi_events_count == 1 ~ "Single",
      tbi_events_count >= 2 ~ "Multiple"
    )
  )

table(tbi_sense3$tbi_freq_cat)

colnames(tbi_sense3)

combined_all2 <- combined_all2 %>%
  left_join(tbi_sense3 %>% select(ncdsid, tbi_freq_cat), by = "ncdsid")

colnames(combined_all2)

combined_all2 %>% 
  count(tbi_freq_cat)

# [1] multiple = 35, [2] none = 18197, [3] single = 326

#Will run analyses with these alternative sensitivity ratings, though note: counts are rather low
#Need to set reference categories first, otherwise R uses alphabetical by default

combined_all2$tbi_severity <- relevel(as.factor(combined_all2$tbi_severity), ref = "T0")

combined_all2$tbi_freq_cat <- relevel(as.factor(combined_all2$tbi_freq_cat), ref = "None")

#RSa

mal_glm_16_sense1 <- lm(RSa_Total2 ~ tbi_freq_cat, data = combined_all2)
summary(mal_glm_16_sense1)
confint(mal_glm_16_sense1)

mal_glm_16_sev1 <- lm(RSa_Total2 ~ tbi_severity, data = combined_all2)
summary(mal_glm_16_sev1)
confint(mal_glm_16_sev1)

#Model 2

mal_glm_16_sense2 <- lm(RSa_Total2 ~ tbi_freq_cat + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(mal_glm_16_sense2)
confint(mal_glm_16_sense2)

mal_glm_16_sev2 <- lm(RSa_Total2 ~ tbi_severity + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(mal_glm_16_sev2)
confint(mal_glm_16_sev2)

#23

mal_glm_23_sense1 <- lm(MAL_23_Total2 ~ tbi_freq_cat, data = combined_all2)
summary(mal_glm_23_sense1)
confint(mal_glm_23_sense1)

mal_glm_23_sev1 <- lm(MAL_23_Total2 ~ tbi_severity, data = combined_all2)
summary(mal_glm_23_sev1)
confint(mal_glm_23_sev1)

#Model 2

mal_glm_23_sense2 <- lm(MAL_23_Total2 ~ tbi_freq_cat  + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(mal_glm_23_sense2)
confint(mal_glm_23_sense2)

mal_glm_23_sev2 <- lm(MAL_23_Total2 ~ tbi_severity  + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(mal_glm_23_sev2)
confint(mal_glm_23_sev2)

#33

mal_glm_33_sense1 <- lm(MAL_33_Total2 ~ tbi_freq_cat, data = combined_all2)
summary(mal_glm_33_sense1)
confint(mal_glm_33_sense1)

mal_glm_33_sev1 <- lm(MAL_33_Total2 ~ tbi_severity, data = combined_all2)
summary(mal_glm_33_sev1)
confint(mal_glm_33_sev1)

#Model 2

mal_glm_33_sense2 <- lm(MAL_33_Total2 ~ tbi_freq_cat + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(mal_glm_33_sense2)
confint(mal_glm_33_sense2)

mal_glm_33_sev2 <- lm(MAL_33_Total2 ~ tbi_severity + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(mal_glm_33_sev2)
confint(mal_glm_33_sev2)

#42

mal_glm_42_sense1 <- lm(MAL_42_Total2 ~ tbi_freq_cat, data = combined_all2)
summary(mal_glm_42_sense1)
confint(mal_glm_42_sense1)

mal_glm_42_sev1 <- lm(MAL_42_Total2 ~ tbi_severity, data = combined_all2)
summary(mal_glm_42_sev1)
confint(mal_glm_42_sev1)


#Model 2
mal_glm_42_sense2 <- lm(MAL_42_Total2 ~ tbi_freq_cat + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(mal_glm_42_sense2)
confint(mal_glm_42_sense2)

mal_glm_42_sev2 <- lm(MAL_42_Total2 ~ tbi_severity + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(mal_glm_42_sev2)
confint(mal_glm_42_sev2)

#GHQ-12

mal_glm_ghq_sense1 <- lm(GHQ_Total2 ~ tbi_freq_cat, data = combined_all2)
summary(mal_glm_ghq_sense1)
confint(mal_glm_ghq_sense1)

mal_glm_ghq_sev1 <- lm(GHQ_Total2 ~ tbi_severity, data = combined_all2)
summary(mal_glm_ghq_sev1)
confint(mal_glm_ghq_sev1)

#Model 2
mal_glm_ghq_sense2 <- lm(GHQ_Total2 ~ tbi_freq_cat + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(mal_glm_ghq_sense2)
confint(mal_glm_ghq_sense2)

mal_glm_ghq_sev2 <- lm(GHQ_Total2 ~ tbi_severity + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(mal_glm_ghq_sev2)
confint(mal_glm_ghq_sev2)

#TBI definition

# For potential brain injury in 7 & 11 years:
# yr11: 1=home, 2=school, 3=RTA, 4=elsewhere, 5=no, 6=unsure, -1=NA
combined_all2$n1282 

TBI_11_bi2 <- ifelse(combined_all2$n1282 %in% c(1, 2, 3, 4), 1,
                     ifelse(combined_all2$n1282 == 5, 0, NA))
table(TBI_11_bi, useNA = "ifany")

# yr7: 1 = don't know, 2 = yes, 3 = no.
combined_all2$n280 

TBI_7_bi <- ifelse(combined_all2$n280 %in% c(2), 1,
                   ifelse(combined_all2$n280 == 3, 0, NA))
table(TBI_7_bi, useNA = "ifany")


## adding the lifetime variables into it
combined_all2$tbi_childhood <- apply(
  cbind(TBI_7_bi, TBI_11_bi, combined_all2$tbi_any),
  1,
  function(row) {
    if (any(row == 1, na.rm = TRUE)) {
      return(1)
    } else if (all(is.na(row))) {
      return(NA)
    } else if (all(row %in% c(0, NA))) {
      return(0)
    } else {
      return(NA)
    }
  }
)

colnames(combined_all2)
table(combined_all2$tbi_childhood, useNA = "ifany")

# RSa
# unadjusted
RSa_unadj_def <- glm(RSa_Total2 ~ tbi_childhood, data = combined_all2)
summary(RSa_unadj_def)
confint(RSa_unadj_def)

# adjusted
RSa_adj_def2 <- lm(RSa_Total2 ~ tbi_childhood + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(RSa_adj_def2)
confint(RSa_adj_def2)


# GHQ
# Unadjusted
ghq_glm_def <- lm(GHQ_Total2 ~ tbi_childhood, data = combined_all2)
summary(ghq_glm_def)
confint(ghq_glm_def)

# Adjusted
ghq_glm_def2 <- lm(GHQ_Total2 ~ tbi_childhood + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(ghq_glm_def2)
confint(ghq_glm_def2)


# 23
# Unadjusted
MAL_glm_23_def <- lm(MAL_23_Total2 ~ tbi_childhood, data = combined_all2)
summary(MAL_glm_23_def)
confint(MAL_glm_23_def)

# Adjusted
MAL_glm_23_def2 <- lm(MAL_23_Total2 ~ tbi_childhood + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(MAL_glm_23_def2)
confint(MAL_glm_23_def2)


# 33
# Unadjusted

MAL_glm_33_def <- glm(MAL_33_Total2 ~ tbi_childhood, data = combined_all2)
summary(MAL_glm_33_def)
confint(MAL_glm_33_def)

# Adjusted
MAL_glm_33_def2 <- lm(MAL_33_Total2 ~ tbi_childhood  + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(MAL_glm_33_def2)
confint(MAL_glm_33_def2)


# 42
# Unadjusted
MAL_glm_42_def <- lm(MAL_42_Total2 ~ tbi_childhood, data = combined_all2)
summary(MAL_glm_42_def)
confint(MAL_glm_42_def)

# Adjusted
MAL_glm_42_def2 <- lm(MAL_42_Total2 ~ tbi_childhood  + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(MAL_glm_42_def2)
confint(MAL_glm_42_def2)


# TBI definition: follow-up severity
# As TBI definition has broadened, decided to also see the severity with this more inclusive definition

combined_all2$TBI_total_count <- rowSums(
  cbind(TBI_7_bi, TBI_11_bi, combined_all2$tbi_any),
  na.rm = TRUE
)

combined_all2$TBI_2plus <- ifelse(
  combined_all2$TBI_total_count >= 2, 1,
  ifelse(is.na(combined_all2$TBI_total_count), NA, 0)
)

table(combined_all2$TBI_2plus)

combined_all2$TBI_def_sev <- with(combined_all2, ifelse(
  is.na(TBI_total_count), NA,
  ifelse(TBI_total_count == 0, "T0",
         ifelse(TBI_total_count == 1, "T1", "T2")
  )
))

table(combined_all2$TBI_def_sev)

combined_all2$TBI_def_sev <- factor(combined_all2$TBI_def_sev, levels = c("T0", "T1", "T2"))

levels(combined_all2$TBI_def_sev)

# Now to run in the model

# RSa
# unadjusted
RSa_unadj_def3 <- lm(RSa_Total2 ~ TBI_def_sev, data = combined_all2)
summary(RSa_unadj_def3)
confint(RSa_unadj_def3)

# Adjusted
RSa_adj_def4 <- lm(RSa_Total2 ~ TBI_def_sev + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(RSa_adj_def4)
confint(RSa_adj_def4)


# GHQ
# Unadjusted
ghq_glm_def3 <- lm(GHQ_Total2 ~ TBI_def_sev, data = combined_all2)
summary(ghq_glm_def3)
confint(ghq_glm_def3)

# Adjusted
ghq_glm_def4 <- lm(GHQ_Total2 ~ TBI_def_sev + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(ghq_glm_def4)
confint(ghq_glm_def4)


# 23
# Unadjusted
MAL_glm_23_def3 <- lm(MAL_23_Total2 ~ TBI_def_sev, data = combined_all2)
summary(MAL_glm_23_def3)
confint(MAL_glm_23_def3)

# Adjusted
MAL_glm_23_def4 <- lm(MAL_23_Total2 ~ TBI_def_sev + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(MAL_glm_23_def4)
confint(MAL_glm_23_def4)


# 33
# Unadjusted

MAL_glm_33_def3 <- lm(MAL_33_Total2 ~ TBI_def_sev, data = combined_all2)
summary(MAL_glm_33_def3)
confint(MAL_glm_33_def3)

# Adjusted
MAL_glm_33_def4 <- lm(MAL_33_Total2 ~ TBI_def_sev + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(MAL_glm_33_def4)
confint(MAL_glm_33_def4)


# 42
# Unadjusted
MAL_glm_42_def3 <- lm(MAL_42_Total2 ~ TBI_def_sev, data = combined_all2)
summary(MAL_glm_42_def3)
confint(MAL_glm_42_def3)

# Adjusted
MAL_glm_42_def4 <- lm(MAL_42_Total2 ~ TBI_def_sev + n622 + ethnicity_group + fsm_imp, data = combined_all2)
summary(MAL_glm_42_def4)
confint(MAL_glm_42_def4)