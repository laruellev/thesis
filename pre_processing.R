setwd("C:/Users/larue/Downloads/BAM/Thesis/analysis")

library(dplyr)
library(stargazer)
library(tidyr)
library(stringr)
library(readr)

# IMPORT DATA
df <- read.csv("Consumer survey on the price of sustainable fasion_June 9, 2023_16.16.csv")

# use only the actual survey responses, not the preview responses
df_survey <- df[df$Status == "IP Address",]

# use only complete survey responses, not unfinished ones
df_survey <- df_survey[df_survey$Finished == "True", ]


# remove columns with survey metadata
col <- colnames(df_survey)
metadata <- c("StartDate",
              "EndDate",
              "Status",
              "IPAddress",
              "Progress",
              "Duration..in.seconds.",
              "Finished",
              "RecordedDate",
              "ResponseId",
              "RecipientLastName",
              "RecipientFirstName",
              "RecipientEmail",
              "ExternalReference",
              "LocationLatitude",
              "LocationLongitude",
              "DistributionChannel",
              "UserLanguage",
              "Q_RecaptchaScore")

df_responses <- df_survey[, !names(df_survey) %in% metadata]

# removing intermediate elements
rm(col)
rm(metadata)

# start indexing the responses at 1 again
df_responses_new <- df_responses
rownames(df_responses_new) <- 1:nrow(df_responses_new)
df_responses <- df_responses_new

# correct typo question 9
df_responses$Q9.a.[df_responses$Q9.a. == "€100 / 32 washes / 4.57% donated / carbon neutral"] <- "€100 / 32 washes / 4.75% donated / carbon neutral"

# remove intermediate steps
rm(df)
rm(df_survey)
rm(df_responses_new)

# DATA FORMATTING TO MAKE IT USABLE FOR THE PARTWORTH MODEL

# BLOCKS
blocks <- read.table("blocks.txt", sep = "\t")

# remove rows containing block number
blocks <- 
  blocks |> 
  filter(!row_number() %in% seq(1, 46, 5))

# remove rows containing headers
blocks <- 
  blocks |> 
  filter(!row_number() %in% seq(1, 37, 4))

# separate into different columns
blocks_sep <- blocks |> 
  separate(V1, 
           into = c("profile", "other"), 
            sep = "^\\s*\\S+\\K\\s+")
blocks_sep <- blocks_sep |> 
  separate(other, 
           into = c("price", "other"), 
           sep = "^\\s*\\S+\\K\\s+")
blocks_sep <- blocks_sep |> 
  separate(other, 
           into = c("washes", "other"), 
           sep = "^\\s*\\S+\\K\\s+")
blocks_sep <- blocks_sep |> 
  separate(other, 
           into = c("donations", "emissions"), 
           sep = "^\\s*\\S+\\K\\s+")

# reassigning block number 
block_nr <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9, 9, 10, 10, 10)
blocks_sep$block <- block_nr

# reorder the columns to have the block nr as the first column
blocks <- blocks_sep[, c(6, 1, 2, 3, 4, 5)]

# replace "none" by "no donations" in donations
blocks$donations[blocks$donations == "none"] <- "no donations"
blocks$donations[blocks$donations == "2%"] <- "2% donated"
blocks$donations[blocks$donations == "4.75%"] <- "4.75% donated"
blocks$emissions[blocks$emissions == "37 kilotons/year"] <- "37 ktons"
blocks$emissions[blocks$emissions == "72.6 kilotons/year"] <- "72.6 ktons"
blocks$washes <- paste(blocks$washes, "washes")

# give the profiles the same format as was given in the survey
blocks$concat <- paste(blocks$price, blocks$washes, blocks$donations, blocks$emissions,
                       sep = " / ")

# remove intermediate steps 
rm(blocks_sep)
rm(block_nr)

write.csv(blocks, 'C:/Users/larue/Downloads/BAM/Thesis/analysis/blocks.csv', row.names = FALSE)

# RESPONSES

# add respondent ID & make it the first column 
df_responses$ID <- seq.int(nrow(df_responses))
df_responses <- df_responses |> 
  select(ID, everything())

# separate by question
Q1 <- df_responses |> select(1, 2, 3)
Q2 <- df_responses |> select(1, 4, 5)
Q3 <- df_responses |> select(1, 6, 7)
Q4 <- df_responses |> select(1, 8, 9)
Q5 <- df_responses |> select(1, 10, 11)
Q6 <- df_responses |> select(1, 13, 14)
Q7 <- df_responses |> select(1, 15, 16)
Q8 <- df_responses |> select(1, 17, 18)
Q9 <- df_responses |> select(1, 19, 20)
Q10 <- df_responses |> select(1, 21, 22)
Q_pers <- df_responses |> select(1, 23, 24, 25)


# match the attributes to the consumer choice
Q1 <- merge(blocks, Q1, by.x = "concat", by.y = "Q1.a.", all.x = FALSE, all.y = TRUE)
Q1 <- Q1[Q1$block == 1,]
Q1 <- Q1 |> 
  select(ID, everything())
Q1 <- Q1[, !names(Q1) %in% c("concat")]


Q1 <- Q1[order(Q1$ID), ]
Q1_new <- Q1
rownames(Q1_new) <- 1:nrow(Q1_new)
Q1 <- Q1_new

colnames(Q1)[8] <- "buy"

Q2 <- merge(blocks, Q2, by.x = "concat", by.y = "Q2.a.", all.x = FALSE, all.y = TRUE)
Q2 <- Q2[Q2$block == 2,]
Q2 <- Q2 |> 
  select(ID, everything())
Q2 <- Q2[, !names(Q2) %in% c("concat")]

Q2 <- Q2[order(Q2$ID), ]
Q2_new <- Q2
rownames(Q2_new) <- 1:nrow(Q2_new)
Q2 <- Q2_new

colnames(Q2)[8] <- "buy"

Q3 <- merge(blocks, Q3, by.x = "concat", by.y = "Q3.a.", all.x = FALSE, all.y = TRUE)
Q3 <- Q3[Q3$block == 3,]
Q3 <- Q3 |> 
  select(ID, everything())
Q3 <- Q3[, !names(Q3) %in% c("concat")]

Q3 <- Q3[order(Q3$ID), ]
Q3_new <- Q3
rownames(Q3_new) <- 1:nrow(Q3_new)
Q3 <- Q3_new

colnames(Q3)[8] <- "buy"

Q4 <- merge(blocks, Q4, by.x = "concat", by.y = "Q4.a.", all.x = FALSE, all.y = TRUE)
Q4 <- Q4[Q4$block == 4,]
Q4 <- Q4 |> 
  select(ID, everything())
Q4 <- Q4[, !names(Q4) %in% c("concat")]

Q4 <- Q4[order(Q4$ID), ]
Q4_new <- Q4
rownames(Q4_new) <- 1:nrow(Q4_new)
Q4 <- Q4_new

colnames(Q4)[8] <- "buy"

Q5 <- merge(blocks, Q5, by.x = "concat", by.y = "Q5.a.", all.x = FALSE, all.y = TRUE)
Q5 <- Q5[Q5$block == 5,]
Q5 <- Q5 |> 
  select(ID, everything())
Q5 <- Q5[, !names(Q5) %in% c("concat")]

Q5 <- Q5[order(Q5$ID), ]
Q5_new <- Q5
rownames(Q5_new) <- 1:nrow(Q5_new)
Q5 <- Q5_new

colnames(Q5)[8] <- "buy"

Q6 <- merge(blocks, Q6, by.x = "concat", by.y = "Q6.a.", all.x = FALSE, all.y = TRUE)
Q6 <- Q6[Q6$block == 6,]
Q6 <- Q6 |> 
  select(ID, everything())
Q6 <- Q6[, !names(Q6) %in% c("concat")]

Q6 <- Q6[order(Q6$ID), ]
Q6_new <- Q6
rownames(Q6_new) <- 1:nrow(Q6_new)
Q6 <- Q6_new

colnames(Q6)[8] <- "buy"

Q7 <- merge(blocks, Q7, by.x = "concat", by.y = "Q7.a.", all.x = FALSE, all.y = TRUE)
Q7 <- Q7[Q7$block == 7,]
Q7 <- Q7 |> 
  select(ID, everything())
Q7 <- Q7[, !names(Q7) %in% c("concat")]

Q7 <- Q7[order(Q7$ID), ]
Q7_new <- Q7
rownames(Q7_new) <- 1:nrow(Q7_new)
Q7 <- Q7_new

colnames(Q7)[8] <- "buy"

Q8 <- merge(blocks, Q8, by.x = "concat", by.y = "Q8.a.", all.x = FALSE, all.y = TRUE)
Q8 <- Q8[Q8$block == 8,]
Q8 <- Q8 |> 
  select(ID, everything())
Q8 <- Q8[, !names(Q8) %in% c("concat")]

Q8 <- Q8[order(Q8$ID), ]
Q8_new <- Q8
rownames(Q8_new) <- 1:nrow(Q8_new)
Q8 <- Q8_new

colnames(Q8)[8] <- "buy"

Q9 <- merge(blocks, Q9, by.x = "concat", by.y = "Q9.a.", all.x = FALSE, all.y = TRUE)
Q9 <- Q9[Q9$block == 9,]
Q9 <- Q9 |> 
  select(ID, everything())
Q9 <- Q9[, !names(Q9) %in% c("concat")]

Q9 <- Q9[order(Q9$ID), ]
Q9_new <- Q9
rownames(Q9_new) <- 1:nrow(Q9_new)
Q9 <- Q9_new

colnames(Q9)[8] <- "buy"
  
Q10 <- merge(blocks, Q10, by.x = "concat", by.y = "Q10.a.", all.x = FALSE, all.y = TRUE)
Q10 <- Q10[Q10$block == 10,]
Q10 <- Q10 |> 
  select(ID, everything())
Q10 <- Q10[, !names(Q10) %in% c("concat")]

Q10 <- Q10[order(Q10$ID), ]
Q10_new <- Q10
rownames(Q10_new) <- 1:nrow(Q10_new)
Q10 <- Q10_new

colnames(Q10)[8] <- "buy"

rm(
  Q1_new,
  Q2_new,
  Q3_new,
  Q4_new,
  Q5_new,
  Q6_new,
  Q7_new,
  Q8_new,
  Q9_new,
  Q10_new
)


df_choices <-  rbind(Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10)

rm(
  Q1,
  Q2,
  Q3,
  Q4,
  Q5,
  Q6,
  Q7,
  Q8,
  Q9,
  Q10
)

# the data frame contains all the preferred choices
df_choices$choice <- 1

# create a data frame with all the possible choices
# blocks$concat <- NULL

# amount of respondents
resp <- as.numeric(nrow(df_responses))
block <- as.numeric(nrow(blocks))

# create new dataframe with all respondents choices AND no choices
df_all <- do.call("rbind", replicate(resp, blocks, simplify = FALSE))

# add respondent ID
ID <- as.integer(rep(1:resp, each = block))
df_all$ID <- ID

rm(ID)

df_all <- df_all |> 
  select(ID, everything())

colnames(df_all)[1] <- "ID_all"
colnames(df_all)[2] <- "block_all"
colnames(df_all)[4] <- "price_all"
colnames(df_all)[5] <- "washes_all"
colnames(df_all)[6] <- "donations_all"
colnames(df_all)[7] <- "emissions_all"
colnames(df_all)[8] <- "concat_all"

df_all$block_all <- as.integer(df_all$block_all)

# merge the full dataframe with the choice dataframe
df_choices$concat <- paste(df_choices$price, df_choices$washes, df_choices$donations, df_choices$emissions,
                       sep = " / ")

df_choices2 <- df_choices |> select(1, 2, 9, 8, 10)

df_conjoint <- merge(x = df_all, 
                     y = df_choices2, 
                     by.x = c("ID_all", "block_all", "concat_all"), 
                     by.y = c("ID", "block", "concat"), 
                     all.x = TRUE, all.y = FALSE)

df_conjoint$choice[is.na(df_conjoint$choice)] <- 0
colnames(df_conjoint)[1] <- "respondent_id"
colnames(df_conjoint)[2] <- "block"

table(df_conjoint$choice)

# remove intermediate steps
rm(
  df_all, 
  df_choices, 
  df_choices2
)
blocks$concat <- NULL
df_conjoint$concat_all <- NULL

# reorder and reindex dataframe 
df_conjoint <- df_conjoint |> 
  arrange(respondent_id, block)

table(df_conjoint$choice)

# check where the missing 1 are
with(rle(df_conjoint$choice), {
  ok <- values == 0 & lengths > 4
  ends <- cumsum(lengths)
  starts <- ends - lengths + 1
  data.frame(starts, ends)[ok, ]
})

# fix them based on df_responses
df_conjoint[741, 8] <- 1
df_conjoint[2870, 8] <- 1

table(df_conjoint$choice)

# create csv file
#write.csv(df_conjoint, 'C:/Users/larue/Downloads/BAM/Thesis/analysis/conjoint_data.csv', row.names = FALSE)

# create csv file with personal data
colnames(Q_pers)[2] <- "age"
colnames(Q_pers)[3] <- "income"
colnames(Q_pers)[4] <- "sustainability"

#write.csv(Q_pers, 'C:/Users/larue/Downloads/BAM/Thesis/analysis/personalia.csv', row.names = FALSE)

# create a buy dataframe 
len <- nrow(df_responses)*10
num <- rep(1:len, each = 3)

df_conjoint$num <- num

df_buy <- df_conjoint |> filter(buy == "Yes")
df_buy <- df_conjoint[df_conjoint$num %in% df_buy$num, ]

rm(len, num)

df_buy$num <- NULL

df_buy[is.na(df_buy)] <- 0
df_buy$buy <- case_when(df_buy$buy == "Yes" ~ 1,
                        df_buy$buy == 0 ~ 0)
df_buy$choice <- NULL

#write.csv(df_buy, 'C:/Users/larue/Downloads/BAM/Thesis/analysis/df_buy.csv', row.names = FALSE)
