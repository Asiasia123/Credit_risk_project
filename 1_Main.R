#---- Analysis: basic data ----


#---- Libraries used in project ----

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(DT)
library(tidyverse) 
library(gmodels) #crossTable

#----Data collection----
getwd()

mig_data <- read_csv("C:\\Dataset.csv")
spec(mig_data)
summary(mig_data)


#----Data wrangling----

## ----checking NA and zero values----
any(is.na(mig_data)) # result: FALSE (no missing values)
amount_of_zeros <- colSums(mig_data == 0)
print(amount_of_zeros)

## ----checking unique values in few columns----
unique_values <- list(
  PERIOD = unique(mig_data$PERIOD),
  BRANCH = unique(mig_data$BRANCH),
  RATING = unique(mig_data$RATING),
  PD = unique(mig_data$PD),
  LGD = unique(mig_data$LGD))
print(unique_values)

## ----checking duplicates----
mig_dupl <- mig_data %>%
  group_by(CUSTOMER_ID) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))
# It looks like one customer can have few credits

## ----columns adjustment

MD <- mig_data %>%
  mutate(PERIOD = as.numeric(str_extract(PERIOD, "\\d+"))) %>%
  mutate(IRB = ifelse(APPROACH == "AIRB", 3, 4)) %>%
  mutate(CustType = ifelse(RATING %in% c("PC6+","PC6","PC6-","PC5+","PC5","PC5-",
                                          "PC4+","PC4","PC4-","PC3+","PC3","PC3-",
                                          "PC2+","PC2","PC2-","PC1+","PC1","PC1-"), 5,  # non-defaulted
                                          ifelse(RATING %in% c("PC0+","PC0","PC0-"), 6, # defaulted
                                                 7)))                                   # unrated

## ----checking if RATING and PD value match----
MD_PD <- MD %>%
  filter(CustType %in% c(5, 6) ) # only Rated Customers

MD_PD <- data.frame(MD_PD[c("RATING", "PD")])

MD_PD_spread <- MD_PD %>%
  count(RATING, PD) %>%
  spread(PD, n, fill = 0)  # values partly match, calibration possible 

MD_PD_spread$PD_calib <- as.numeric(names(MD_PD_spread)[-1][max.col(MD_PD_spread[-1],

                                                                                                                               ties.method = "first")])
MD_all <- MD %>%
  left_join(select(MD_PD_spread, RATING, PD_calib), by = "RATING")%>%
  mutate(PD_calib = if_else(is.na(PD_calib), PD, PD_calib)) %>%
  mutate(EL_counted = round(EAD_AMOUNT*LGD*PD_calib, 1)) %>% # counting EL from equation (more accurate)
  mutate(LGD_counted = ifelse(EL_counted > 0 & PD_calib > 0 & EAD_AMOUNT > 0,
                              EL_counted / (PD_calib * EAD_AMOUNT), LGD)) %>%
  mutate(EAD_counted = ifelse(EL_counted > 0 & PD_calib > 0 & LGD_counted > 0,
                              EL_counted / (PD_calib * LGD), EAD_AMOUNT))%>%
  mutate(EL_counted = round(EAD_AMOUNT*LGD*PD_calib, 1)) %>% # 2% higher sum than before
  mutate(Risk_Weight =  ifelse(is.na(RWA/EAD_counted), 0, (RWA/EAD_counted)))

## ----quick look on data ----
any(is.na(mig_data)) # result: FALSE (no missing values)
amount_of_zeros <- colSums(MD_all == 0)
print(amount_of_zeros) # PD of zero value reduced from 9.2% to 0.08%

CrossTable(MD_all$BRANCH)
CrossTable(MD_all$PERIOD)
CrossTable(MD_all$PD)
CrossTable(MD_all$IRB)
CrossTable(MD_all$CustType)
CrossTable(y = MD_all$PERIOD, x = MD_all$RATING, 
           prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
CrossTable(y = MD_all$PERIOD, x = MD_all$CustType, 
           prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
CrossTable(y = MD_all$IRB, x = MD_all$CustType, 
           prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)


#----Analysis: migration of rating----

MIG <- MD %>%
    select(CUSTOMER_ID, PERIOD, RATING)

MIG <- MIG %>%
  group_by(CUSTOMER_ID) %>%
  mutate(CustFloat = case_when(
                  all(is.na(RATING[PERIOD == 1])) & all(!is.na(RATING[PERIOD == 2])) ~ "Newbie",
                  all(!is.na(RATING[PERIOD == 1])) & all(is.na(RATING[PERIOD == 2])) ~ "Ender",
                  any(RATING[PERIOD == 1] != RATING[PERIOD == 2]) ~ "Mover",
                  TRUE ~ "Stayer" )) %>%
  ungroup() %>%
  distinct(CUSTOMER_ID, PERIOD, RATING, CustFloat, .keep_all = TRUE)

MIG_MS <- filter(MIG, CustFloat=="Mover"| CustFloat=="Stayer")

matrix <- table(MIG_MS$RATING[MIG_MS$PERIOD == 1], MIG_MS$RATING[MIG_MS$PERIOD == 2])
print(matrix)

MIG_table <- as.data.frame(as.table(matrix))
MIG_table$RATING_from <- rownames(matrix)[MIG_table$Var1]
MIG_table$RATING_to <- colnames(matrix)[MIG_table$Var2]
MIG_table$Var1 <- NULL
MIG_table$Var2 <- NULL

MIG_matrix <- MIG_table %>%
  pivot_wider(names_from = RATING_to, values_from = Freq, values_fill = 0)

MIG_matrix <- MIG_matrix %>%
  arrange(desc(RATING_from))

MIG_matrix <- MIG_matrix[, c(1, rev(seq_along(MIG_matrix)[-1]))]

MD_all <- MD_all %>% 
  left_join(select(MIG, CUSTOMER_ID, CustFloat), by = "CUSTOMER_ID")


