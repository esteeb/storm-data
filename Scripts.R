
data <- read.csv("repdata_data_StormData.csv.bz2")


test$EVTYPE <- data$EVTYPE %>%
  toupper()%>%
  {gsub("TSTM*", "THUNDERSTORM", . , ignore.case = TRUE)}%>%
  {gsub("THUNDERSTORM*", "THUNDERSTORM", ., ignore.case = TRUE)}%>%
  {gsub("HURRICANE*", "HURRICANE", . , ignore.case = TRUE  )}

test2 <- data$EVTYPE %>%
  toupper()

test2[grep("THUNDERSTORM*", test2)] <- "THUNDERSTORM"
test2[grep("TSTM",test2)] <- "THUNDERSTORM"
test2[grep("SUMMARY.*", test2)] <- "SUMMARY"
test2[grep("TROPICAL STORM", test2)] <- "TROPICAL STORM"
test2[grep("HURRICANE", test2)] <- "HURRICANE"

# Convert *EXP columns to numeric multiples
prop_dmg_exp <- data$PROPDMGEXP

prop_dmg_exp[grep("K", prop_dmg_exp, ignore.case = TRUE)] <- 1000
prop_dmg_exp[grep("B", prop_dmg_exp, ignore.case = TRUE)] <- 1000000
prop_dmg_exp[grep("M", prop_dmg_exp, ignore.case = TRUE)] <- 1000000000
prop_dmg_exp[grep("[^0-9]", prop_dmg_exp)] <- 1

prop_dmg_exp <- as.numeric(prop_dmg_exp)
prop_dmg_exp[is.na(prop_dmg_exp)] <- 0

data$PROPDMGEXP <- prop_dmg_exp

crop_dmg_exp <- data$CROPDMGEXP

crop_dmg_exp[grep("K", crop_dmg_exp)] <- 1000
crop_dmg_exp[grep("K", crop_dmg_exp)] <- 1000000
crop_dmg_exp[grep("K", crop_dmg_exp)] <- 1000000000
crop_dmg_exp[grep("[^0-9]", crop_dmg_exp)] <- 1

crop_dmg_exp <- as.numeric(crop_dmg_exp)
crop_dmg_exp[is.na(crop_dmg_exp)] <- 0 

data$CROPDMGEXP <- crop_dmg_exp

proc_data_damage <- data%>%
  select(EVTYPE, PROPDMG, PROPDMGEXP,
                CROPDMG, CROPDMGEXP)%>%
  dplyr::group_by(EVTYPE)%>%
  mutate(Act.Crop.Dmg = CROPDMG * CROPDMGEXP,
         Act.Prop.Dmg = PROPDMG * PROPDMGEXP
  )