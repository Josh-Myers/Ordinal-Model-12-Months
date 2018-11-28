# dem and averaging 12 mth

# ordinal RS - pass = pass both (type A and pass DP)
#            - Mild = fail one test (fail DP and type A | type B or C tymp and pass DP)
#            - Severe = fail both tests

#import----
library(plyr)
library(tidyr)
library(tidyverse)
library(cowplot)
library(devtools)
#install_github('Josh-Myers/MyersMisc')
library(MyersMisc)
theme_set(theme_bw(base_size = 10))

abs.2 = readRDS('abs.2.rds')
abs.24 = readRDS('abs.24.rds')

abs.2 = select(abs.2, -abs226)

# number of infants who attended follow ups
abs.2 = abs.2[!is.na(abs.2$rs),]
abs.24 = abs.24[!is.na(abs.24$rs),]

number_infants = as.data.frame(unique(abs.2$sub.id)) # 220 infants attended follow ups

# Table 1: missing data
tymp_results = summary(abs.2$tymp)
dpoae_results = summary(abs.2$dpoae)
abs_missing = sum(is.na(abs.2$abs250)) # 63 missing

# dataset with NA (CNT) of rs and abs removed
# first set rs = cnt to NA 
abs.2$rs[abs.2$rs == 'CNT'] = NA
abs.24$rs[abs.24$rs == 'CNT'] = NA
abs.2$rs = droplevels(abs.2$rs)
abs.24$rs = droplevels(abs.24$rs)

abs.2 = abs.2[!is.na(abs.2$rs) & !is.na(abs.2$abs250),]
abs.24 = abs.24[!is.na(abs.24$rs) & !is.na(abs.24$abs226),] # final dataset had 358 observations

# Table 2: characteristics
age.wks = summary(abs.2$age.wks)
sex = summary(abs.2$gender)
eth = summary(abs.2$ethnicity)
rs = summary(abs.2$rs)

# do any demographics have missing data?
sum(is.na(abs.2$ear))
sum(is.na(abs.2$gender))
sum(is.na(abs.2$ethnicity)) # none - no need to impute

# reference standard plots

# 1 mean rs plots normal/Mild/Severe ----
summary(abs.24$rs)
rs.plot.df = abs.24
rs.plot.df$rs = factor(rs.plot.df$rs, levels = c('Pass', 'Mild', 'Severe'))
rs.plot.df$rs <- revalue(rs.plot.df$rs, c("Pass" = "Normal, n = 232", 
                                "Mild" = "Mild, n = 54",
                                "Severe" = "Severe, n = 72"))

rs.plot.df <- select(rs.plot.df, rs, starts_with('abs'))
colnames(rs.plot.df) <- c("rs", "226.00", "257.33", "280.62", "297.30", "324.21", "343.49", "363.91", "385.55", "408.48", "432.77", "458.50",
                      "471.94", "500.00", "514.65", "545.25", "561.23", "577.68", "594.60", "629.96", "648.42", "667.42", "686.98",
                      "707.11", "727.83", "749.15", "771.11", "793.70", "816.96", "840.90", "865.54", "890.90", "917.00", "943.87",
                      "971.53", "1000.00", "1029.30", "1059.46", "1090.51", "1122.46", "1155.35", "1189.21", "1224.05", "1259.92", 
                      "1296.84", "1334.84", "1373.95", "1414.21", "1455.65", "1498.31", "1542.21", "1587.40", "1633.92", "1681.79",
                      "1731.07", "1781.80", "1834.01", "1887.75", "1943.06", "2000.00", "2058.60", "2118.93", "2181.02", "2244.92",
                      "2310.71", "2378.41", "2448.11", "2519.84", "2593.68", "2669.68", "2747.91", "2828.43", "2911.31", "2996.61",
                      "3084.42", "3174.80", "3267.83", "3363.59", "3462.15", "3563.59", "3668.02", "3775.50", "3886.13", 
                      "4000.00", "4117.21", "4237.85", "4362.03", "4489.85", "4621.41", "4756.83", "4896.21", "5039.68", "5187.36",
                      "5339.36", "5495.81", "5656.85", "5822.61", "5993.23", "6168.84", "6349.60", "6535.66", "6727.17", "6924.29",
                      "7127.19", "7336.03", "7550.99", "7772.26", "8000.00")

# group by rs and create mean
rs.plot.df <- group_by(rs.plot.df, rs)
abs.mean <- summarise_all(rs.plot.df, funs(mean))
abs.mean <- gather(abs.mean, Frequency, absorbance, 2:108)

abs.rs.plot <- ggplot(abs.mean, aes(x=as.numeric(Frequency), y=absorbance, group=rs, colour=rs)) +
  theme_bw() +
  scale_colour_manual(values = c("Normal, n = 232" = "#00BA38", "Mild, n = 54" = "#619CFF", "Severe, n = 72" = "#F8766D")) +
  geom_line()  +
  xlab("Frequency, Hz") +
  ylab("Absorbance") +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(0, 1)) +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) + 
  theme(legend.title=element_blank(), legend.position="right") 
print(abs.rs.plot)

# 2 tymp / dpoae plots (mean pass both, fail DP, type C/Pass DP, type C/fail DP, fail both)

tymp.dp.plot.df = abs.24
summary(tymp.dp.plot.df$rs.all.possible)
tymp.dp.plot.df$rs.all.possible = factor(tymp.dp.plot.df$rs.all.possible, levels = c("Pass both", "Type A and fail DPOAEs", "Type C and pass DPOAEs", "Type C and fail DPOAEs",
                                                               "Type B and pass DPOAEs", "Fail both"))
tymp.dp.plot.df$rs.all.possible <- revalue(tymp.dp.plot.df$rs.all.possible, c("Pass both"="Pass both, n = 232", 
                                                        "Type A and fail DPOAEs" = "Type A and fail DPOAEs, n = 11",
                                                        "Type C and pass DPOAEs" = "Type C and pass DPOAEs, n = 9", 
                                                        "Type C and fail DPOAEs" = "Type C and fail DPOAEs, n = 6", 
                                                        "Type B and pass DPOAEs" = "Type B and pass DPOAEs, n = 28", 
                                                        "Fail both" = "Fail both, n = 72"))

tymp.dp.plot.df <- select(tymp.dp.plot.df, rs.all.possible, starts_with('abs'))
colnames(tymp.dp.plot.df) <- c("rs", "226.00", "257.33", "280.62", "297.30", "324.21", "343.49", "363.91", "385.55", "408.48", "432.77", "458.50",
                    "471.94", "500.00", "514.65", "545.25", "561.23", "577.68", "594.60", "629.96", "648.42", "667.42", "686.98",
                    "707.11", "727.83", "749.15", "771.11", "793.70", "816.96", "840.90", "865.54", "890.90", "917.00", "943.87",
                    "971.53", "1000.00", "1029.30", "1059.46", "1090.51", "1122.46", "1155.35", "1189.21", "1224.05", "1259.92", 
                    "1296.84", "1334.84", "1373.95", "1414.21", "1455.65", "1498.31", "1542.21", "1587.40", "1633.92", "1681.79",
                    "1731.07", "1781.80", "1834.01", "1887.75", "1943.06", "2000.00", "2058.60", "2118.93", "2181.02", "2244.92",
                    "2310.71", "2378.41", "2448.11", "2519.84", "2593.68", "2669.68", "2747.91", "2828.43", "2911.31", "2996.61",
                    "3084.42", "3174.80", "3267.83", "3363.59", "3462.15", "3563.59", "3668.02", "3775.50", "3886.13", 
                    "4000.00", "4117.21", "4237.85", "4362.03", "4489.85", "4621.41", "4756.83", "4896.21", "5039.68", "5187.36",
                    "5339.36", "5495.81", "5656.85", "5822.61", "5993.23", "6168.84", "6349.60", "6535.66", "6727.17", "6924.29",
                    "7127.19", "7336.03", "7550.99", "7772.26", "8000.00")


# Group by rs and create mean
tymp.dp.plot.df <- group_by(tymp.dp.plot.df, rs)
abs.mean <- summarise_all(tymp.dp.plot.df, funs(mean))
abs.mean <- gather(abs.mean, Frequency, absorbance, 2:108)

# label with the numbers in each group
abs.rs.all.plot <- ggplot(abs.mean) +
  theme_bw() +
  scale_colour_manual(values = c("Pass both, n = 232" = "#00BA38", 
                                 "Type A and fail DPOAEs, n = 11" = "#B79F00", 
                                 "Type C and pass DPOAEs, n = 9" = "#00BFC4", 
                                 "Type C and fail DPOAEs, n = 6" = "#619CFF", 
                                 "Type B and pass DPOAEs, n = 28" = "#F564E3", 
                                 "Fail both, n = 72" = "#F8766D")) +
  geom_line(aes(x = as.numeric(Frequency), y = absorbance, group=rs, colour = rs))  +
  xlab("Frequency, Hz") +
  ylab("Absorbance") +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(0, 1)) +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme(legend.title=element_blank() ) +
  #legend.position=c(0.02,0.98)
  #legend.text=element_text(size=12), 
  theme(legend.position="right")
print(abs.rs.all.plot)

# Multiplot
abs.plots <- plot_grid(abs.rs.plot, abs.rs.all.plot, nrow=2, ncol=1, align = "v", labels = c("A", "B"))
ggsave("abs.plots.jpeg", abs.plots, height=6, width=8, dpi=500)

# Create the 90% normal range for the example plots

norm.24 = abs.24
norm.24 = select(norm.24, rs, starts_with('abs'))
norm.24 <- group_by(norm.24, rs)
abs.median <- summarise_all(norm.24, funs(median))
abs.05 <- summarise_all(norm.24, funs(quantile(., probs = (0.05))))
abs.95 <- summarise_all(norm.24, funs(quantile(., probs = (0.95))))
abs.90 <- rbind(abs.median, abs.05, abs.95)
abs.90 <- data.frame(abs.90)
colnames(abs.90) <- c("rs", "226.00", "257.33", "280.62", "297.30", "324.21", "343.49", "363.91", "385.55", "408.48", "432.77", "458.50",
                       "471.94", "500.00", "514.65", "545.25", "561.23", "577.68", "594.60", "629.96", "648.42", "667.42", "686.98",
                       "707.11", "727.83", "749.15", "771.11", "793.70", "816.96", "840.90", "865.54", "890.90", "917.00", "943.87",
                       "971.53", "1000.00", "1029.30", "1059.46", "1090.51", "1122.46", "1155.35", "1189.21", "1224.05", "1259.92", 
                       "1296.84", "1334.84", "1373.95", "1414.21", "1455.65", "1498.31", "1542.21", "1587.40", "1633.92", "1681.79",
                       "1731.07", "1781.80", "1834.01", "1887.75", "1943.06", "2000.00", "2058.60", "2118.93", "2181.02", "2244.92",
                       "2310.71", "2378.41", "2448.11", "2519.84", "2593.68", "2669.68", "2747.91", "2828.43", "2911.31", "2996.61",
                       "3084.42", "3174.80", "3267.83", "3363.59", "3462.15", "3563.59", "3668.02", "3775.50", "3886.13", 
                       "4000.00", "4117.21", "4237.85", "4362.03", "4489.85", "4621.41", "4756.83", "4896.21", "5039.68", "5187.36",
                       "5339.36", "5495.81", "5656.85", "5822.61", "5993.23", "6168.84", "6349.60", "6535.66", "6727.17", "6924.29",
                       "7127.19", "7336.03", "7550.99", "7772.26", "8000.00")

stats.col <- c("median", "median", "median", "five", "five", "five", "ninety5", "ninety5", "ninety5")
abs.90 <- cbind.data.frame(abs.90, stats.col)
abs.90.long <- gather(abs.90, Frequency, absorbance, 2:108)
abs.90.long <- spread(abs.90.long, stats.col, absorbance)
abs.90.long$Frequency <- as.numeric(abs.90.long$Frequency)
abs.90.long$rs = as.character(abs.90.long$rs)

# filter normal
abs.90.long = filter(abs.90.long, rs == 'Pass')

abs.90.plot <- ggplot(abs.90.long, aes(x=Frequency, y=median, ymin=five, ymax=ninety5)) +
  geom_ribbon(linetype=0, alpha = 0.4) +
  scale_fill_grey(start=0.6, end=0.2) +
  geom_line(size=0.8)  +
  xlab("Frequency, Hz") +
  ylab("Absorbance") +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(0, 1)) +
  theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), 
        legend.position=c(0.03, 0.97)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) 
#theme(legend.position="none")
print(abs.90.plot)

# modeling

#compare wai 1 only and wai 1 include birth type * age interaction, then rcs also - choose model with lowest AIC

### NEED TO DO A DEMOGRAPHICS MODEL AND COMPARE

#intro----
# GOF test is not the classic hosmer lemeshaw which arbitrarily binned observations - this is their new method
# Hosmer, D. W.; Hosmer, T.; le Cessie, S. & Lemeshow, S. A comparison of goodness-of-fit tests for the logistic regression model. Statistics in Medicine, 1997
library(plyr)
library(dplyr)
library(rms)
#library(pROC)
library(ggplot2)

# munge ----
summary(wai.2.df$rs)
wai.2.df$id.res = as.numeric(as.character(wai.2.df$id.res))
wai.2.df$age.12.mth.in.weeks = as.numeric(as.character(wai.2.df$age.12.mth.in.weeks))

# check levels are correct for rs (pass should be first)
levels(wai.2.df$rs)
wai.2.df$rs = relevel(wai.2.df$rs, "Pass") # now they are right
str(wai.2.df$rs)

summary(as.factor(wai.2.df$ear))
summary(wai.2.df$rs)
tapply(wai.2.df$rs, wai.2.df$ear, summary)

# number of infants who attended follow-up appts (and their gender)
one.ear.all <- group_by(wai.2.c, id.res)
one.ear.all <- sample_n(one.ear.all, 1) 
summary(one.ear.all$gender)

# select one ear of each infant to report the demographics
one.ear <- group_by(wai.2.df, id.res)
one.ear <- sample_n(one.ear, 1) # 211 infants included in study sample
summary(one.ear$maternal.ethnicity)
sum(is.na(one.ear$maternal.ethnicity))
summary(one.ear$gender)
sum(is.na(one.ear$gender))
summary(one.ear$age.12.mth.in.weeks)
sum(is.na(one.ear$age.12.mth.in.weeks))

# 3 NA for ethnicity - need to impute
wai.2.df$maternal.ethnicity[is.na(wai.2.df$maternal.ethnicity)] = "Caucasian" # this is what MICE did
# library(mice)
# summary(wai.2.df$age.12.mth.in.weeks)
# wai.2.df$age.12.mth.in.weeks = as.numeric(wai.2.df$age.12.mth.in.weeks)
# imp.eth = mice(wai.2.df)
# wai.2.df.imp = mice::complete(imp.eth)
# summary(wai.2.df.imp$maternal.ethnicity)
# summary(wai.2.df$maternal.ethnicity)
# # the 3 missing were imputed as caucasian
# wai.2.df = wai.2.df.imp

# MV Model
# consider freqs proposed by Myers 6mth - found to have normal, mild, severe relationship
# check PO assumption
#rs.int = as.integer(wai.2.df$rs)
#sf <- function(y) {
#  c('Y>=Normal'=qlogis(mean(y >= 1)), 'Y>=Mild'=qlogis(mean(y >= 2)),
#    'Y>=Severe'=qlogis(mean(y >= 3)))
#                  }
#s <- summary(rs.int ~ abs2000, fun=sf, data = wai.2.df)
#s
# couldn't plot cause of "Inf"  - ?need bigger sample size????
# if use PCA see if it works using the PCs
#plot(s, which=1:3, pch=1:3, xlab='logit', vnames='names', main='', width.factor=1.5) 

ord.plot = plot.xmean.ordinaly(rs ~ abs1000 + abs1414 + abs2000 + abs2828 + abs4000 + abs5657, cr=F, topcats=2, subn = F, data = wai.2.df)
# this looks really good except for abs2828 
# if use pca need to do the plot for the PCs
# Harrell fig 14.2, p 334 -  Solid line is simple stratified mean, dotted line is PO, "C" is CR
# need to check whether trend on solid line is monotonic, and then which is closest to line PO or CR?
tiff("ord.plot.tiff", width = 9, height = 6 , units = 'in', res = 1200)
par(mfrow=c(2,3))
par(mar=c(5,4,2,2)+0.1)
My.plot.xmean.ordinaly(rs ~ abs1000, cr=F, topcats=2, subn = F, data = wai.2.df, xlab = "Reference Standard", ylab = "Absorbance 1000 Hz")
My.plot.xmean.ordinaly(rs ~ abs1414, cr=F, topcats=2, subn = F, data = wai.2.df, xlab = "Reference Standard", ylab = "Absorbance 1414 Hz")
My.plot.xmean.ordinaly(rs ~ abs2000, cr=F, topcats=2, subn = F, data = wai.2.df, xlab = "Reference Standard", ylab = "Absorbance 2000 Hz")
My.plot.xmean.ordinaly(rs ~ abs2828, cr=F, topcats=2, subn = F, data = wai.2.df, xlab = "Reference Standard", ylab = "Absorbance 2828 Hz")
My.plot.xmean.ordinaly(rs ~ abs4000, cr=F, topcats=2, subn = F, data = wai.2.df, xlab = "Reference Standard", ylab = "Absorbance 4000 Hz")
My.plot.xmean.ordinaly(rs ~ abs5657, cr=F, topcats=2, subn = F, data = wai.2.df, xlab = "Reference Standard", ylab = "Absorbance 5657 Hz")
dev.off()

#Y2 = as.numeric(wai.2.df$rs)

#cr0 <- lrm(Y2==1 ~ abs707 + abs1000 + abs1414 + abs2000 + abs2828 + abs4000 + abs5657, data = wai.2.df, x=TRUE, y=TRUE)
# Use the update function to save repeating model right-
# hand side.  An indicator variable for Y=1 is the
# response variable below
#cr1 <- update(cr0, Y2==2 ~ ., subset = Y2>=2)
#plot.lrm.partial(cr0, cr1, center=TRUE) 
##############################################################################
### Modeling Absorbance
## Need to do UV AUCs as well - compare AUC - is MV better?
# MV modeling the variables directly - more interpretable
abs.df = select(wai.2.df, ear:maternal.ethnicity, abs1000:abs5657)

# set all other to "Asian"
abs.df$maternal.ethnicity[abs.df$maternal.ethnicity == "African"] <- "Asian"
abs.df$maternal.ethnicity[abs.df$maternal.ethnicity == "ATSI"] <- "Asian"
abs.df$maternal.ethnicity[abs.df$maternal.ethnicity == "Oceanian"] <- "Asian"
abs.df$maternal.ethnicity[abs.df$maternal.ethnicity == "South American"] <- "Asian"
abs.df$maternal.ethnicity = drop.levels(abs.df$maternal.ethnicity)

raw.dd <- datadist(abs.df)
options(datadist="raw.dd")
f <- lrm(rs ~ abs1000 + abs1414 + abs2000 + abs2828 + abs4000 + abs5657, data = abs.df, x = T, y = T)
#p <- pentrace(f, seq(0, 5, by=.005)) # penalty was 0 - no need
#p$penalty
#f.pca.penal <- update(f.pca, penalty=p$penalty)
#f.pca.penal
#f.pca.penal$penalty

# now bootcov or robcov
#b = bootcov(f, wai.2.df$id.res)
#b
#bootplot(b)
r = robcov(f, wai.2.df$id.res) # I think robcov might be better - it may cause confusion to use bootstrap twice
r                                # and robcov seems to have more literature

gamma = (398.70 - 6) / 398.70
aic.mv = AIC(r)
vif(r)
resid(r, "gof")
anova(r)

options(prType = 'plain') # change to "latex" if want latex output
latex(r, file = "") # replace "abs" with: \textit{A}  and paste into Rsweave file then compile pdf

r.pred <- predict(r, type = "fitted")
# need predict lp (not fitted) for the multiclass roc
# BUT don't use proc to calculate auc - it uses different method than rms - use MRME package if needed
# b.pred.lp = predict(b)
# b.pca.roc <- multiclass.roc(wai.2.df$rs, b.pred.lp, levels = c("Pass", "Mild", "Severe"), 
#                             print.auc=TRUE, plot=TRUE, print.thres = TRUE, col="purple", main="PCA fit")

validate(r, B = 500)
cal1 = calibrate(r, B = 500, kint = 1) # calibrate for Y >= Mild
# it wouldn't let me set riskdist to "F" so I used the scat1d.opts - and set to 0 to supress the distribution of predictions in margin
plot(cal1, scat1d.opts=list(nhistSpike=0, side=1, frac=0.00, tck=0), subtitles = F, xlab = "Predicted Probability", ylab = "Actual Probability")
cal2 = calibrate(r, B = 500, kint = 2) # calibrate for Y >= Severe
plot(cal2, scat1d.opts=list(nhistSpike=0, side=1, frac=0.00, tck=0), subtitles = F, xlab = "Predicted Probability", ylab = "Actual Probability")

tiff("calPlot.tiff", width = 10, height = 5 , units = 'in', res = 1200)
par(mar=c(5,5,2,2)+0.1)
layout(matrix(c(1, 2), 1, 2, byrow = TRUE), widths = c(5,5), heights = c(5))
#Mild Plot
plot(cal1, scat1d.opts=list(nhistSpike=0, side=1, frac=0.00, tck=0), subtitles = F, legend = F, xlab = "Predicted Probability", ylab = "Actual Probability")
legend("bottomright", inset = 0.05, box.lty=0, legend=c("Apparent", "Bias-corrected", "Ideal"),
       col=c("Black", "black", "black"), lty=c(3,1,2), cex=0.8)
mtext("A", 2, adj = 5, las = 1, padj = -10.4, font = 2, cex = 1.3)
#Severe Plot
plot(cal2, scat1d.opts=list(nhistSpike=0, side=1, frac=0.00, tck=0), subtitles = F, legend = F, xlab = "Predicted Probability", ylab = "Actual Probability")
legend("bottomright", inset = 0.05, box.lty=0, legend=c("Apparent", "Bias-corrected", "Ideal"),
       col=c("Black", "black", "black"), lty=c(3,1,2), cex=0.8)
mtext("B", 2, adj = 5.5, las = 1, padj = -10.4, font = 2, cex = 1.3)
dev.off()

n <- sum(r$freq)
quantile(predict(r, type='fitted'), c(100/n, 1-100/n), na.rm=TRUE)

# resid(b, 'score.binary' , pl=TRUE) # they look ok - not too far from horizontal dashed line
# resid(b, 'partial' , pl=TRUE , label.curves=FALSE)
#plot(Predict(b, fun = plogis))

# library(mRMRe) # use this for auc because proc uses a different method than rms 
# rs.ordered = ordered(training$rs)
# auc.train = correlate(rs.ordered, pred.train.penal, "cindex")
# auc.train # this is the same as rms!
# 
# pred.test.penal <- predict(f, testing, type="lp")
# # roc.test.penal <- multiclass.roc(testing$rs, pred.test.penal) 
# # roc.test.penal 
# 
# rs.ordered.test = ordered(testing$rs)
# auc.test = correlate(rs.ordered.test, pred.test.penal, "cindex")
# auc.test 

plot(Predict(r, fun = plogis,  kint = 1))
plot(Predict(r, fun = plogis, kint = 2))

val <- validate(r, B=200)
full <- val[[1]]
train <- val[[12]]
test <- val[[23]]
## write a function to convert Dxy to AUC
dxy.to.auc <- function(x) {0.5*(x+1)}

#use fn to convert dxy to auc for full model, train and test sets
auc.full <- dxy.to.auc(full)
auc.train <- dxy.to.auc(train)
auc.test <- dxy.to.auc(test)
opt <- auc.train - auc.test
opt.cor <- auc.full - opt
auc.df <- c(auc.full, auc.train, auc.test, opt, opt.cor)
auc.df.names <- c("full", "train", "test", "opt", "opt.cor")
auc.res <- cbind.data.frame(auc.df.names, auc.df)

# y <- as.numeric(df$rs) # needs to be numeric
# y <- replace(y, y==1, 0)
# y <- replace(y, y==2, 1)
# cal.plot <- MyCalPlot(b.pred, y, smooth = T, logistic.cal = F, pl=T, riskdist = "predicted", statloc = F, 
#                       legendloc=c(0.8,0.1), cex = 1)

### Demographics model
f.dem <- lrm(rs ~ ear + gender + maternal.ethnicity + abs1000 + abs1414 + abs2000 + abs2828 + abs4000 + abs5657, data = abs.df, x = T, y = T)

r.dem = robcov(f.dem, wai.2.df$id.res) # I think robcov might be better - it may cause confusion to use bootstrap twice
r.dem                               # and robcov seems to have more literature

gamma.dem = (399.59 - 9) / 399.59
aic.dem = AIC(r.dem) # model without dems has lower AIC so use that one

# predict class membership
pred.ind = predict(r, type = "fitted.ind")
pred.ind = as.data.frame(round(pred.ind, digits =2))
# use some function to select the class with the highest prob (drop the other 2)

max.pred = as.data.frame(max.col(pred.ind, ties.method = "last"))
summary(as.factor(max.pred$`max.col(pred.ind, ties.method = "last")`))

max.pred <- max.col(pred.ind, "last")
value <- pred.ind[cbind(1:nrow(pred.ind), max.pred)]
cluster <- names(pred.ind)[max.pred]
pred <- data.frame(value, cluster)
pred$cluster = as.character(pred$cluster)
pred$cluster[pred$cluster == "rs=Pass"] = "Pass"
pred$cluster[pred$cluster == "rs=Mild"] = "Mild"
pred$cluster[pred$cluster == "rs=Severe"] = "Severe"
pred$cluster = factor(pred$cluster, levels = c("Pass", "Mild", "Severe"))

# report the class and prob on the graph in the app

## compare to rs label 
pred.compare = cbind(pred, wai.2.df$rs)

cont.tab = table(pred.compare$cluster, pred.compare$`wai.2.df$rs`) # columns are the original label (the truth), and rows are the max predictions
# explore any that the model said was normal, but RS said Severe

# what is the median and range of predictions?
pred.compare %>% 
  group_by(cluster) %>% 
  summarise(mean = mean(value)) 

pred.compare %>% 
  group_by(cluster) %>% 
  summarise(min = min(value)) 

pred.compare %>% 
  group_by(cluster) %>% 
  summarise(max = max(value)) 

# example
eg = filter(wai.2.df, id.res==66, ear=="R") 
eg.prob = predict(r, eg, type = "fitted.ind")
eg.prob
eg.prob = "Probability of mild dysfunction = 0.53"
eg.abs = filter(wai.24.df, id.res==66, ear=="R") 
eg.abs = dplyr::select(eg.abs, abs226:abs8000)
eg.abs.long <- gather(eg.abs, Frequency, Absorbance, abs226:abs8000)
eg.abs.long = cbind.data.frame(FREQ.num, eg.abs.long)
eg.abs.long = eg.abs.long[,c(1,3)]
names(eg.abs.long) = c("Frequency", "Absorbance")

eg.plot.1 = ggplot(eg.abs.long) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  geom_line(aes(x= Frequency, y=Absorbance), data = eg.abs.long, colour="red") +
  geom_ribbon(data=abs.90.long, aes(x = Frequency, ymin = five, ymax = ninety5, linetype=NA), alpha = 0.2, show.legend = F) +
  #scale_fill_manual(values="grey20") + 
  xlab("Frequency, Hz") +
  ylab("Absorbance") +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(0, 1)) +
  theme(legend.text=element_text(size=10), legend.justification=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(plot.title = element_text(vjust=2)) +
  annotate("text", x = 250, y = c(0.92, 0.85), label = c("S66, right", "Prob Mild = 0.53"), hjust = 0) 
eg.plot.1

eg = filter(wai.2.df, id.res==125, ear=="L") 
eg.prob = predict(r, eg, type = "fitted")
eg.prob
eg.prob = "Probability of severe dysfunction = 0.80"
eg.abs = filter(wai.24.df, id.res==125, ear=="L") 
eg.abs = dplyr::select(eg.abs, abs226:abs8000)
eg.abs.long <- gather(eg.abs, Frequency, Absorbance, abs226:abs8000)
eg.abs.long = cbind.data.frame(FREQ.num, eg.abs.long)
eg.abs.long = eg.abs.long[,c(1,3)]
names(eg.abs.long) = c("Frequency", "Absorbance")

eg.plot.2 = ggplot(eg.abs.long) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  geom_line(aes(x= Frequency, y=Absorbance), data = eg.abs.long, colour="blue") +
  geom_ribbon(data=abs.90.long, aes(x = Frequency, ymin = five, ymax = ninety5, linetype=NA), alpha = 0.2, show.legend = F) +
  #scale_fill_manual(values="grey20") + 
  xlab("Frequency, Hz") +
  ylab("Absorbance") +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(0, 1)) +
  theme(legend.text=element_text(size=10), legend.justification=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(plot.title = element_text(vjust=2)) +
  annotate("text", x = 250, y = c(0.92, 0.85), label = c("S125, left", "Prob Severe = 0.80"), hjust = 0) 
eg.plot.2

eg.plots <- plot_grid(eg.plot.1, eg.plot.2, nrow=2, ncol=1, align = "v", labels = c("A", "B"))
ggsave("eg.plots.tiff", eg.plots, height=6, width=6, dpi=1200)

## demographics model
f.dem <- lrm(rs ~ ear + gender + maternal.ethnicity + abs1000 + abs1414 + abs2000 + abs2828 + abs4000 + abs5657, data = abs.df, x = T, y = T)
p <- pentrace(f.dem, seq(0, 5, by=.005)) # penalty was 0 - no need
#p$penalty
#f.pca.penal <- update(f.dem, penalty=p$penalty)
#f.pca.penal
#f.pca.penal$penalty

# now bootcov or robcov
b.dem = bootcov(f.dem, wai.2.df$id.res)
b.dem
bootplot(b.dem)

r.dem = robcov(f.dem, wai.2.df$id.res) # I think robcov might be better - it may cause confusion to use bootstrap twice
r.dem                            # and robcov seems to have more literature

gamma.dem = (290.28 - 9) / 290.28
aic.dem = AIC(r.dem)

###########
## nonlinear model
f.nl <- lrm(rs ~ rcs(abs1000, 5) + rcs(abs1414, 5) + rcs(abs2000, 5) + rcs(abs2828, 5) + rcs(abs4000, 5) + rcs(abs5657, 5), data = abs.df, x = T, y = T)
#p <- pentrace(f.nl, seq(0, 5, by=.005)) # penalty was 0 - no need
#p$penalty
#f.pca.penal <- update(f.pca, penalty=p$penalty)
#f.pca.penal
#f.pca.penal$penalty

r.nl = robcov(f.nl, wai.2.df$id.res) # I think robcov might be better - it may cause confusion to use bootstrap twice
r.nl                                # and robcov seems to have more literature

gamma.nl = (429.76 - 24) / 429.76
aic.nl = AIC(r.nl)

#### UV analyses
uv.dd = datadist(wai.2.df)
options(datadist="uv.dd")

f.250 = lrm(rs ~ abs250, data = wai.2.df, x = T, y = T)
f.354 = lrm(rs ~ abs354, data = wai.2.df, x = T, y = T)
f.500 = lrm(rs ~ abs500, data = wai.2.df, x = T, y = T)
f.707 = lrm(rs ~ abs707, data = wai.2.df, x = T, y = T)
f.1000 = lrm(rs ~ abs1000, data = wai.2.df, x = T, y = T)
f.1414 = lrm(rs ~ abs1414, data = wai.2.df, x = T, y = T)
f.2000 = lrm(rs ~ abs2000, data = wai.2.df, x = T, y = T)
f.2828 = lrm(rs ~ abs2828, data = wai.2.df, x = T, y = T)
f.4000 = lrm(rs ~ abs4000, data = wai.2.df, x = T, y = T)
f.5657 = lrm(rs ~ abs5657, data = wai.2.df, x = T, y = T)
f.8000 = lrm(rs ~ abs8000, data = wai.2.df, x = T, y = T)

r.250 = robcov(f.250, wai.2.df$id.res) 
r.354 = robcov(f.354, wai.2.df$id.res) 
r.500 = robcov(f.500, wai.2.df$id.res) 
r.707 = robcov(f.707, wai.2.df$id.res) 
r.1000 = robcov(f.1000, wai.2.df$id.res) 
r.1414 = robcov(f.1414, wai.2.df$id.res) 
r.2000 = robcov(f.2000, wai.2.df$id.res) 
r.2828 = robcov(f.2828, wai.2.df$id.res) 
r.4000 = robcov(f.4000, wai.2.df$id.res) 
r.5657 = robcov(f.5657, wai.2.df$id.res) 
r.8000 = robcov(f.8000, wai.2.df$id.res) 

aic.250 = AIC(r.250)
aic.354 = AIC(r.354)
aic.500 = AIC(r.500)
aic.707 = AIC(r.707)
aic.1000 = AIC(r.1000)
aic.1414 = AIC(r.1414)
aic.2000 = AIC(r.2000)
aic.2828 = AIC(r.2828)
aic.4000 = AIC(r.4000)
aic.5657 = AIC(r.5657)
aic.8000 = AIC(r.8000)

## function to convert Dxy to AUC
dxy.to.auc <- function(x) {0.5*(x+1)}
dxy.250 = r.250$stats[[7]]
auc.250 <- dxy.to.auc(dxy.250)

dxy.354 = r.354$stats[[7]]
auc.354 <- dxy.to.auc(dxy.354)

dxy.500 = r.500$stats[[7]]
auc.500 <- dxy.to.auc(dxy.500)

dxy.707 = r.707$stats[[7]]
auc.707 <- dxy.to.auc(dxy.707)

dxy.1000 = r.1000$stats[[7]]
auc.1000 <- dxy.to.auc(dxy.1000)

dxy.1414 = r.1414$stats[[7]]
auc.1414 <- dxy.to.auc(dxy.1414)

dxy.2000 = r.2000$stats[[7]]
auc.2000 <- dxy.to.auc(dxy.2000)

dxy.2828 = r.2828$stats[[7]]
auc.2828 <- dxy.to.auc(dxy.2828)

dxy.4000 = r.4000$stats[[7]]
auc.4000 <- dxy.to.auc(dxy.4000)

dxy.5657 = r.5657$stats[[7]]
auc.5657 <- dxy.to.auc(dxy.5657)

dxy.8000 = r.8000$stats[[7]]
auc.8000 <- dxy.to.auc(dxy.8000)

lr_chi2.250 = r.250$stats[[3]]
lr_chi2.354 = r.354$stats[[3]]
lr_chi2.500 = r.500$stats[[3]]
lr_chi2.707 = r.707$stats[[3]]
lr_chi2.1000 = r.1000$stats[[3]]
lr_chi2.1414 = r.1414$stats[[3]]
lr_chi2.2000 = r.2000$stats[[3]]
lr_chi2.2828 = r.2828$stats[[3]]
lr_chi2.4000 = r.4000$stats[[3]]
lr_chi2.5657 = r.5657$stats[[3]]
lr_chi2.8000 = r.8000$stats[[3]]

### Table with frequency models as rows and LRchi^2, AUC, AIC 
stats.250 = cbind.data.frame(lr_chi2.250, auc.250, aic.250)
stats.354 = cbind.data.frame(lr_chi2.354, auc.354, aic.354)
stats.500 = cbind.data.frame(lr_chi2.500, auc.500, aic.500)
stats.707 = cbind.data.frame(lr_chi2.707, auc.707, aic.707)
stats.1000 = cbind.data.frame(lr_chi2.1000, auc.1000, aic.1000)
stats.1414 = cbind.data.frame(lr_chi2.1414, auc.1414, aic.1414)
stats.2000 = cbind.data.frame(lr_chi2.2000, auc.2000, aic.2000)
stats.2828 = cbind.data.frame(lr_chi2.2828, auc.2828, aic.2828)
stats.4000 = cbind.data.frame(lr_chi2.4000, auc.4000, aic.4000)
stats.5657 = cbind.data.frame(lr_chi2.5657, auc.5657, aic.5657)
stats.8000 = cbind.data.frame(lr_chi2.8000, auc.8000, aic.8000)

names(stats.250) = c("LR Chi^2", "AUC", "AIC")
names(stats.354) = c("LR Chi^2", "AUC", "AIC")
names(stats.500) = c("LR Chi^2", "AUC", "AIC")
names(stats.707) = c("LR Chi^2", "AUC", "AIC")
names(stats.1000) = c("LR Chi^2", "AUC", "AIC")
names(stats.1414) = c("LR Chi^2", "AUC", "AIC")
names(stats.2000) = c("LR Chi^2", "AUC", "AIC")
names(stats.2828) = c("LR Chi^2", "AUC", "AIC")
names(stats.4000) = c("LR Chi^2", "AUC", "AIC")
names(stats.5657) = c("LR Chi^2", "AUC", "AIC")
names(stats.8000) = c("LR Chi^2", "AUC", "AIC")

uv.table = rbind.data.frame(stats.250, stats.354, stats.500, stats.707, stats.1000, stats.1414, stats.2000, stats.2828, stats.4000, 
                            stats.5657, stats.8000)
uv.table$Frequency = c(250, 354, 500, 707, 1000, 1414, 2000, 2828, 4000, 5657, 8000)
uv.table = uv.table[,c(4,1,2,3)]
write.csv(uv.table, file = "UV.table.csv")


