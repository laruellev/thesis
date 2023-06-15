setwd("C:/Users/larue/Downloads/BAM/Thesis/analysis")
library(dplyr)
library(stargazer)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(corrplot)
library(Hmisc)

# install_version("mlogit", version = "1.0-1", repos = "http://cran.us.r-project.org")

library(mlogit)

library(cbcTools)

# import data -----------------------------------------------------------------
df_cat <- read.csv("conjoint_data.csv")
df_perso <- read.csv("personalia.csv")

colnames(df_cat)[4] <- "price"
colnames(df_cat)[5] <- "washes"
colnames(df_cat)[6] <- "donations"
colnames(df_cat)[7] <- "emissions"

# sanity check: n_choice should be equal to 1000
table(df_cat$choice)

# merge the responses and the personalia --------------------------------------
df_all <- merge(x = df_cat, 
                y = df_perso, 
                by.x = c("respondent_id"), 
                by.y = c("ID"), 
                all.x = TRUE, all.y = FALSE)

df_all <- df_all |> 
  select(c("age", "income", "sustainability", "respondent_id"), everything())

# replace the buy choice by 0 or 1 --------------------------------------------
df_all$buy <- case_when(df_all$buy == "Yes" ~ 1,
                        df_all$buy == "No" ~ 0)

# rename columns --------------------------------------------------------------
colnames(df_all)[7] <- "price"
colnames(df_all)[8] <- "washes"
colnames(df_all)[9] <- "donations"
colnames(df_all)[10] <- "emissions"

# personalia ------------------------------------------------------------------
df_perso$sustainability <- 
  case_when(df_perso$sustainability == "Far below average" ~ 1,
            df_perso$sustainability == "Somewhat below average" ~ 2,
            df_perso$sustainability == "Average" ~ 3,
            df_perso$sustainability == "Somewhat above average" ~ 4,
            df_perso$sustainability == "Far above average" ~ 5)

# RColorBrewer::display.brewer.all() # to check plot colors

df_income <- df_perso[df_perso$income == "Yes", ]
df_no_income <- df_perso[df_perso$income == "No", ]

plt_income <- ggplot(data = df_income,
              aes(x = sustainability, fill = age)) +
  geom_histogram(binwidth = 1, aes(fill = age), color = "black") +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(title = "Respondents with income stream", x = "Sustainability level", y = "Amount of respondents")

plt_no_income <- ggplot(data = df_no_income,
                     aes(x = sustainability, fill = age)) +
  geom_histogram(binwidth = 1, aes(fill = age), color = "black") +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(title = "Respondents without income stream", x = "Sustainability level", y = "Amount of respondents")

grid.arrange(plt_income, plt_no_income, ncol = 2)

#ggsave("resp_personalia.pdf", 
#       plot = grid.arrange(plt_income, plt_no_income, ncol = 2),
#       path = "C:/Users/larue/Downloads/BAM/Thesis/analysis/results",
#       dpi = "retina",
#       bg = "white",
#       width = 10.64,
#       height = 6.27)

# remove unnecessary elements
rm(plt_income,
   plt_no_income)

df_perso$income <- case_when(
  df_perso$income == "Yes" ~ 1,
  df_perso$income == "No" ~ 0
)

df_perso$age <- case_when(
  df_perso$age == "18 - 24" ~ 24,
  df_perso$age == "25 - 34" ~ 34, 
  df_perso$age == "35 - 44" ~ 44
)

df_perso$ID <- NULL

# correlation matrix
# http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software

corr2 <- rcorr(as.matrix(df_perso))

#pdf(file = "C:/Users/larue/Downloads/BAM/Thesis/analysis/results/corrplot.pdf",
#    width = 8,
#    height = 8)

corrplot.mixed(corr2$r,
               lower = "number",
               upper = "color",
               lower.col = "black",
)

corrplot(corr2$r, type="upper", order="hclust", 
         p.mat = corr2$P, sig.level = 0.05, insig = "blank")

#dev.off()

rm(
  df_income,
  df_no_income,
  corr2
)


# correlation matrix ----------------------------------------------------------
df_all$price <- case_when(
  df_all$price == "€35" ~ 35,
  df_all$price == "€100" ~ 100,
  df_all$price == "€165" ~ 165,
)

df_all$washes <- case_when(
  df_all$washes == "7 washes" ~ 7,
  df_all$washes == "32 washes" ~ 32,
  df_all$washes == "57 washes" ~ 57,
)

df_all$donations <- case_when(
  df_all$donations == "2% donated" ~ 0.02,
  df_all$donations == "4.75% donated" ~ 0.0475,
  df_all$donations == "no donations" ~ 0,
)

df_all$emissions <- case_when(
  df_all$emissions == "carbon neutral" ~ 0,
  df_all$emissions == "37 ktons" ~ 37000,
  df_all$emissions == "72.6 ktons" ~ 72600
)

df_corr <- df_all |> select(7:11)
corr <- rcorr(as.matrix(df_corr))

#pdf(file = "C:/Users/larue/Downloads/BAM/Thesis/analysis/results/corrplot_rslt.pdf",
#    width = 8,
#    height = 8)

corrplot.mixed(corr$r,
               lower = "number",
               upper = "color",
               lower.col = "black",
)

#dev.off()

rm(corr,
   df_corr)

# top 3 most selected profile --------------------------------------------------
df_most_selected <- 
  df_all |> 
  group_by(profile) |> 
  summarise(Freq = sum(choice))

df_most_selected <- 
  df_most_selected[order(df_most_selected$Freq, decreasing = TRUE),]
row.names(df_most_selected) <- NULL

top3 <- head(df_most_selected, 3)
rm(df_most_selected)

blocks <- read.csv("blocks.csv")
blocks$concat <- NULL

top3 <- blocks[blocks$profile %in% top3$profile, ]
stargazer(top3, summary = FALSE)

# https://rpubs.com/tmk1221/CBC ------------------------------------------------
# counts analysis 

# check that attributes are somewhat balanced 
blocks |> group_by(price) |> count()
blocks |> group_by(washes) |> count()
blocks |> group_by(donations) |> count()
blocks |> group_by(emissions) |> count()

# counts of how many times each attribute is picked 
xtabs(choice ~ price, data = df_all) # sanity check: more attributes for cheaper price
xtabs(choice ~ washes, data = df_all)
xtabs(choice ~ donations, data = df_all)
xtabs(choice ~ emissions, data = df_all)

# fitting choice models

df_num <- df_cat
df_num$price <- case_when(
  df_num$price == "€35" ~ 35,
  df_num$price == "€100" ~ 100,
  df_num$price == "€165" ~ 165
)
  
df_mlogit_cat <- mlogit.data(
  data = df_cat, 
  choice = "choice", 
  shape = "long", 
  varying = 4:7, 
  id.var = "respondent_id", 
  alt.levels = paste("pos", 1:3)
)

mdl_cat <- mlogit(
  data = df_mlogit_cat,
  choice ~ 0 + 
    price + washes + donations + emissions
) # model where all the variables are categorical 

df_mlogit_num <- mlogit.data(
  data = df_num, 
  choice = "choice", 
  shape = "long", 
  varying = 4:7, 
  id.var = "respondent_id", 
  alt.levels = paste("pos", 1:3)
)

mdl_num <- mlogit(
  data = df_mlogit_num,
  choice ~ 0 + 
    price + washes + donations + emissions
) # model where all the variables are numerical

summary(mdl_cat) # all attributes are categorical var
summary(mdl_num) # all attributes are numeric var

lrtest(mdl_cat, mdl_num) #check which model fits the data better
# null hypothesis: mdl_num fits better than mdl_cat
# large p-value: fail to reject the null hypothesis
# -> both model have equal fit / use mdl_num

# willingness to pay
coef(mdl_num)["washes57 washes"]/(-coef(mdl_num)["price"])
coef(mdl_num)["washes7 washes"]/(-coef(mdl_num)["price"])

coef(mdl_num)["donations4.75% donated"]/(-coef(mdl_num)["price"])
coef(mdl_num)["donationsno donations"]/(-coef(mdl_num)["price"])

coef(mdl_num)["emissions72.6 ktons"]/(-coef(mdl_num)["price"])
coef(mdl_num)["emissionscarbon neutral"]/(-coef(mdl_num)["price"])

# preference share simulator
comp_universe <- blocks[c(3, 22, 5, 7, 18, 30, 13, 27, 11), ]
comp_universe$block <- NULL
comp_universe$profile <- NULL
comp_universe$concat <- NULL
comp_universe$price <- case_when(
  comp_universe$price == "€35" ~ 35,
  comp_universe$price == "€100" ~ 100,
  comp_universe$price == "€165" ~ 165
)

predict.mnl <- function(model, data) {
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
  utility <- data.model%*%model$coef
  share <- round(exp(utility)/sum(exp(utility)),2)
  cbind(share, data)
}

predict.mnl(mdl_num, comp_universe)

# sensitivity plot
sensitivity.plot <- function(model, attrib, base.data, competitor.data) {
  data <- rbind(base.data, competitor.data)
  base.share <- predict.mnl(model, data)[1, 1]
  share <- NULL
  for (a in seq_along(attrib)) {
    for (i in attrib[[a]]) {
      data[1,] <- base.data
      data[1,a] <- i
      share <- c(share, predict.mnl(model, data)[1, 1])
    }
  }
  data.frame(level=unlist(attrib), share=share, increase=share-base.share)
}

base_data <- blocks[21, ]
base_data$block <- NULL
base_data$profile <- NULL
base_data$concat <- NULL
base_data$price <- case_when(
  base_data$price == "€35" ~ 35,
  base_data$price == "€100" ~ 100,
  base_data$price == "€165" ~ 165
)

competitor_data <- comp_universe

attrib <- list(price = c(35, 100, 165),
               washes = c("7 washes", "32 washes", "57 washes"),
               donations = c("no donations", "2% donated", "4.75% donated"),
               emissions = c("carbon neutral", "37 ktons", "72.6 ktons"))

tradeoff <- sensitivity.plot(mdl_num, attrib, base_data, comp_universe)

barplot(tradeoff$increase, horiz=FALSE, names.arg=tradeoff$level,
        ylab="Change in Share for Baseline Product")

# groups analysis --------------------------------------------------------------
df_perso <- read.csv("personalia.csv")
df_perso$sustainability <- 
  case_when(df_perso$sustainability == "Far below average" ~ 1,
            df_perso$sustainability == "Somewhat below average" ~ 2,
            df_perso$sustainability == "Average" ~ 3,
            df_perso$sustainability == "Somewhat above average" ~ 4,
            df_perso$sustainability == "Far above average" ~ 5)

df_buy <- read.csv("df_buy.csv")

df_buy$price <- case_when(
  df_buy$price_all == "€35" ~ 35,
  df_buy$price_all == "€100" ~ 100,
  df_buy$price_all == "€165" ~ 165,
)

df_buy$washes <- case_when(
  df_buy$washes_all == "7 washes" ~ 7,
  df_buy$washes_all == "32 washes" ~ 32,
  df_buy$washes_all == "57 washes" ~ 57,
)

df_buy$donations <- case_when(
  df_buy$donations_all == "2% donated" ~ 0.02,
  df_buy$donations_all == "4.75% donated" ~ 0.0475,
  df_buy$donations_all == "no donations" ~ 0,
)

df_buy$emissions <- case_when(
  df_buy$emissions_all == "carbon neutral" ~ 0,
  df_buy$emissions_all == "37 ktons" ~ 37000,
  df_buy$emissions_all == "72.6 ktons" ~ 72600
)

df_buy$price_all <- NULL
df_buy$emissions_all <- NULL
df_buy$donations_all <- NULL
df_buy$washes_all <- NULL

df_buy |> group_by(buy) |> count() # how many people would end up buying 

group1 <- df_perso |> filter(sustainability < 3) |> filter(income == "Yes")
group2 <- df_perso |> filter(sustainability > 3) |> filter(income == "Yes")
group3 <- df_perso |> filter(sustainability < 3) |> filter(income == "No")
group4 <- df_perso |> filter(sustainability > 3) |> filter(income == "No")

df_group1 <- df_buy[df_buy$respondent_id %in% group1$ID, ]
df_group2 <- df_buy[df_buy$respondent_id %in% group2$ID, ]
df_group3 <- df_buy[df_buy$respondent_id %in% group3$ID, ]
df_group4 <- df_buy[df_buy$respondent_id %in% group4$ID, ]

df_corr_1 <- df_group1 |> select(4:8)
df_corr_2 <- df_group2 |> select(4:8)
df_corr_3 <- df_group3 |> select(4:8)
df_corr_4 <- df_group4 |> select(4:8)

corr_group1 <- rcorr(as.matrix(df_corr_1))
corr_group2 <- rcorr(as.matrix(df_corr_2))
corr_group3 <- rcorr(as.matrix(df_corr_3))
corr_group4 <- rcorr(as.matrix(df_corr_4))

m1.1 <- corr_group1[["r"]]
m1.2 <- corr_group1[["P"]]
corr1_r <- m1.1[2:5, 1]
corr1_p <- m1.2[2:5, 1]
group1_rslt <- rbind(corr1_r, corr1_p)

m2.1 <- corr_group2[["r"]]
m2.2 <- corr_group2[["P"]]
corr2_r <- m2.1[2:5, 1]
corr2_p <- m2.2[2:5, 1]
group2_rslt <- rbind(corr2_r, corr2_p)

m3.1 <- corr_group3[["r"]]
m3.2 <- corr_group3[["P"]]
corr3_r <- m3.1[2:5, 1]
corr3_p <- m3.2[2:5, 1]
group3_rslt <- rbind(corr3_r, corr3_p)

m4.1 <- corr_group4[["r"]]
m4.2 <- corr_group4[["P"]]
corr4_r <- m4.1[2:5, 1]
corr4_p <- m4.2[2:5, 1]
group4_rslt <- rbind(corr4_r, corr4_p)

rslts <- rbind(
  group1_rslt,
  group2_rslt,
  group3_rslt, 
  group4_rslt
)

stargazer(rslts, type = "text", summary = FALSE)

rm(df_corr_1,
   df_corr_2, 
   df_corr_3, 
   df_corr_4, 
   m1.1, 
   m1.2, 
   m2.1, 
   m2.2, 
   m3.1, 
   m3.2, 
   m4.1, 
   m4.2, 
   corr_group1, corr_group2, corr_group3, corr_group4,
   group1_rslt, group2_rslt, group3_rslt, group4_rslt,
   corr1_r, corr1_p, 
   corr2_r, corr2_p,
   corr3_r, corr3_p,
   corr4_r, corr4_p)
