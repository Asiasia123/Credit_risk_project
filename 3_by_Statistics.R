#----Analysis: Statistics----

## Exposure by credit rating
by_CR_OE <- MD_all %>%
  group_by(RATING) %>%
  summarise(
    P1_sum_OE_EURm = round(sum(ifelse(PERIOD == 1, ORIGINAL_EXPOSURE, 0))/1000000, digits = 1),
    P2_sum_OE_EURm = round(sum(ifelse(PERIOD == 2, ORIGINAL_EXPOSURE, 0))/1000000, digits = 1))

## Exposure by branch
by_BR_stat <- MD_all %>%
  group_by(BRANCH) %>%
  summarise(
    P1_sum_OE_EURm = round(sum(ifelse(PERIOD == 1, ORIGINAL_EXPOSURE, 0))/1000000, digits = 1),
    P2_sum_OE_EURm = round(sum(ifelse(PERIOD == 2, ORIGINAL_EXPOSURE, 0))/1000000, digits = 1),
    P1_sum_EL_counted = round(sum(ifelse(PERIOD == 1, EL_counted, 0))/1000000, digits = 1),
    P2_sum_EL_counted = round(sum(ifelse(PERIOD == 2, EL_counted, 0))/1000000, digits = 1),
    P1_sum_EAD_counted = round(sum(ifelse(PERIOD == 1, EAD_counted, 0))/1000000, digits = 1),
    P2_sum_EAD_counted = round(sum(ifelse(PERIOD == 2, EAD_counted, 0))/1000000, digits = 1),
    P1_sum_RWA = round(sum(ifelse(PERIOD == 1, RWA, 0))/1000000, digits = 1),
    P2_sum_RWA = round(sum(ifelse(PERIOD == 2, RWA, 0))/1000000, digits = 1))

# Risk weight by branch
by_BR_Risk_Weight <- MD_all %>% # different result than using range PD
    filter(RWA>0)%>%
    filter(EAD_counted>0)%>%
    group_by(BRANCH) %>%
    summarise(P1_mean_Risk_Weight = round(mean(ifelse(PERIOD == 1, Risk_Weight, 0))*100, 2),
              P2_mean_Risk_Weight = round(mean(ifelse(PERIOD == 2, Risk_Weight, 0))*100, 2))

# AIRB by branch
by_BR_AIRB <- MD_all %>%
  filter(APPROACH=="AIRB")%>%
  group_by(BRANCH) %>%
  summarise(P1_sum_OE_EURm = round(sum(ifelse(PERIOD == 1, ORIGINAL_EXPOSURE, 0))/1000000, digits = 1),
            P2_sum_OE_EURm = round(sum(ifelse(PERIOD == 2, ORIGINAL_EXPOSURE, 0))/1000000, digits = 1))

# FIRB by branch
by_BR_FIRB <- MD_all %>%
  filter(APPROACH=="FIRB")%>%
  group_by(BRANCH) %>%
  summarise(P1_sum_OE_EURm = round(sum(ifelse(PERIOD == 1, ORIGINAL_EXPOSURE, 0))/1000000, digits = 1),
            P2_sum_OE_EURm = round(sum(ifelse(PERIOD == 2, ORIGINAL_EXPOSURE, 0))/1000000, digits = 1))

# Defaulted customers by branch
by_BR_DEF <- MD_all %>%
  filter(CustType==6) %>%
  group_by(BRANCH) %>%
  summarise(
    P1_sum_OE_EURm = round(sum(ifelse(PERIOD == 1, ORIGINAL_EXPOSURE, 0))/1, digits = 1),
    P2_sum_OE_EURm = round(sum(ifelse(PERIOD == 2, ORIGINAL_EXPOSURE, 0))/1, digits = 1))

# Stayer / mover in OE
by_CustFloat_OE <- MD_all %>%
  group_by(CustFloat) %>%
  summarise(
    P1_sum_OE_EURm = round(sum(ifelse(PERIOD == 1, ORIGINAL_EXPOSURE, 0))/1, digits = 1),
    P2_sum_OE_EURm = round(sum(ifelse(PERIOD == 2, ORIGINAL_EXPOSURE, 0))/1, digits = 1))

## count OE on excluded (11% of obser.) from CR stat (=>it's a 4,5% of OE)
by_EXCL_OE <- MD_all %>%
  filter(EAD_counted>0)%>%
  filter(PD_calib>0)%>%
  filter(LGD_counted>0) %>%
    summarise(
    P1_sum_OE_EURm = round(sum(ifelse(PERIOD == 1, ORIGINAL_EXPOSURE, 0))/1000000, digits = 1),
    P2_sum_OE_EURm = round(sum(ifelse(PERIOD == 2, ORIGINAL_EXPOSURE, 0))/1000000, digits = 1))

# newbie by branch
by_BR_newbie <- MD_all %>%
  filter(CustFloat=="Newbie" | CustFloat=="Ender") %>%
  group_by(BRANCH) %>%
  summarise(
    P1_sum_OE_Ender = round(sum(ifelse(PERIOD == 1, ORIGINAL_EXPOSURE, 0))/1, digits = 1),
    P2_sum_OE_Newbie = round(sum(ifelse(PERIOD == 2, ORIGINAL_EXPOSURE, 0))/1, digits = 1))

# Stayer, Mover, Newbie - count unique
by_Custid_CustFloat <- MD_all %>%
  filter(PERIOD==2) %>%  
  group_by(CustFloat) %>%
  summarise(length(unique(CUSTOMER_ID)))
