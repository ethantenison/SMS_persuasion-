# Grace Hopper 2020 SMS analysis
#Adi Tantravahi 02/28/2020

# ----------
# Setup
# ----------


# Read in data
rand_data <- read.csv("data/randomization.csv", na.strings=c("","NA"))
sms_data <- read.csv("data/text_message_data.csv", na.strings=c("","NA"))
survey_data <- read.csv("data/survey_data.csv", na.strings=c("","NA"))
turnout_data <- read.csv("turnout_data.csv",  na.strings=c("","NA")))

# check for duplicates in data
sum(duplicated(turnout_data))
turnout_data <- turnout_data[!duplicated(turnout_data$ai_id),] #delete duplicate id's

sum(duplicated(survey_data$phone_number)) # check for duplicates, there are none

sum(duplicated(sms_data))
sms_data <- subset(sms_data, message_direction == "outbound") ## we subset because their inbound text doesnt matter

sum(duplicated(rand_data)) # there are none

# Combine turnout data
df <- merge(rand_data, turnout_data, by = "ai_id")
# Add survey data
df <- merge(df, survey_data, by = "phone_number")
## Add sms data
df <- merge(df, sms_data, by = "phone_number")

# ----------
# SMS Data
# ----------

## We need to make sure everyone was sent the right texts
# The messages field is kind of messy unfortunately
table(df$message_text)


# ----------
# Clean data
# ----------

# Clean up marital status variable
df$marital_status_bin <- NA
df$marital_status_bin[df$marital_status == "unmarried" | 
                        df$marital_status == "separated"] <- "Unmarried" ## changed l to t
df$marital_status_bin[df$marital_status == "married"] <- "Married"
#df <- df[!is.na(df$marital_status_bin),] ## omit all na values in marital_status_bin
#sum(is.na(df$marital_status_bin)) ## check for na's in marital status bin


# Bin age variable
## I don't really know the best way to do this
### changing this to be catagorical so 1 2 3 
df$age_bin[df$age < 30] <- 1
df$age_bin[df$age >= 30 & df$age < 55] <- 2
df$age_bin[df$age >= 55] <- 3

#df <- df[!is.na(df$age_bin),] ## omit all na values in age_bin
#sum(is.na(df$age_bin)) ## check for na's in age_bin

#clean race variable
# there is both caucasian and white. turn all caucasian to white
df$race[df$race == "caucasian"] <- "white"

df <- df[!duplicated(df$phone_number),]

# ----------
# Balance
# ----------

## Check for balance across assignment
## We need to double-check treatment assignment balance by age, race, and marital status
bal <- glm(sms_treat ~ race + age_bin + marital_status_bin, 
           data = df, family = "binomial")
summary(bal)

# ----------
# Results
# ----------

# Estimate Turnout
## Control for race, marital status, age (binned), gender, and
## support for hopper
turnout_model <- glm(turnout ~ sms_treat + race + marital_status_bin + 
                       age_bin + support_hopper, 
                     data = df, family = "binomial")
summary(turnout_model)


# Estimate Persuasion
## Control for race, marital status, age (binned), gender, and 
## whether they ended up voting in the election later
persuasion_model <- glm(support_hopper ~ sms_treat + race + marital_status_bin + 
                          age_bin + turnout,
                        data = df, family = "binomial")
summary(persuasion_model)

# ----------
# Subgroup Effects
# ----------

try <- na.omit(df)
## I wonder if the treatment was especially effective at motivating turnout among any subgroups. 
## write a function to check a given subgroup for differences in treatment effectiveness
subgroup_effects <- function(dataset = try, subgroup,treatment = "sms_treat",outcome = "turnout"){
        
        model <- glm(outcome ~ treatment + subgroup, data = dataset, family = "binomial")
        return(summary(model))
}

# Then let's check subgroup effects on turnout by gender & race
subgroup_effects(subgroup = "gender")



subgroup_effects(subgroup = "race")



