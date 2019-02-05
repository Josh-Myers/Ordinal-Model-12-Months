# 12-months ordinal model

#install_github('Josh-Myers/MyersMisc')
library(MyersMisc)
library(plyr)
library(tidyr)
library(tidyverse)
library(cowplot)
library(devtools)
library(rms)
theme_set(theme_bw(base_size = 10))

# load data
abs.2 = readRDS('abs.2.rds')
abs.24 = readRDS('abs.24.rds')
abs.2 = select(abs.2, -abs226)

# total number recruited
total_n = distinct(abs.2, sub.id, .keep_all = TRUE)

# number of infants who attended follow ups
abs.2 = filter(abs.2, !is.na(rs))
abs.24 = filter(abs.24, !is.na(rs))

number_infants = distinct(abs.2, sub.id, .keep_all = TRUE) # 220 infants attended follow ups

# Table 1: missing data
tymp_results = summary(abs.2$tymp)
dpoae_results = summary(abs.2$dpoae)
abs_missing = sum(is.na(abs.2$abs250)) # 63 missing

# dataset with NA (CNT) of rs and abs removed (the study sample)
# first set rs = cnt to NA 
abs.2$rs[abs.2$rs == 'CNT'] = NA
abs.24$rs[abs.24$rs == 'CNT'] = NA
abs.2$rs = droplevels(abs.2$rs)
abs.24$rs = droplevels(abs.24$rs)

# number of infants in study sample
abs.2 = filter(abs.2, !is.na(rs) & !is.na(abs250)) 
abs.24 = filter(abs.24, !is.na(rs) & !is.na(abs226)) # final dataset had 358 observations
number_infants_study_sample = distinct(abs.2, sub.id, .keep_all = TRUE) # and 186 infants 

# Table 2: characteristics
age.wks = summary(number_infants_study_sample$age.wks)
sex = summary(number_infants_study_sample$gender) 
eth = summary(number_infants_study_sample$ethnicity)
rs = summary(abs.2$rs)
rs.by.ear = tapply(abs.2$rs, abs.2$ear, summary)

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
ggsave("fig.1.abs.plots.jpeg", abs.plots, height=6, width=8, dpi=500)

# Create the 90% normal range (1/24 octave) for the example plots
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

# plot to check it looks right
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

# save 90% range for the app
#saveRDS(abs.90.long, "twelveMth90range.rds")

# need the 90% range for 1/2 octave for the app
norm.2 = abs.2
norm.2 = select(norm.2, rs, starts_with('abs'))
norm.2 <- group_by(norm.2, rs) 
abs.median.2 <- summarise_all(norm.2, funs(median))
abs.05.2 <- summarise_all(norm.2, funs(quantile(., probs = (0.05))))
abs.95.2 <- summarise_all(norm.2, funs(quantile(., probs = (0.95))))
abs.90.2 <- rbind(abs.median.2, abs.05.2, abs.95.2)
abs.90.2 <- data.frame(abs.90.2)
colnames(abs.90.2) <- c("rs", "250", "354", "500", "707", "1000", "1414", "2000", "2828", "4000", "5657", "8000.00")

abs.90.2 <- cbind.data.frame(abs.90.2, stats.col)
abs.90.long.2 <- gather(abs.90.2, Frequency, absorbance, 2:12)
abs.90.long.2 <- spread(abs.90.long.2, stats.col, absorbance)
abs.90.long.2$Frequency <- as.numeric(abs.90.long.2$Frequency)
abs.90.long.2$rs = as.character(abs.90.long.2$rs)

# filter normal
abs.90.long.2 = filter(abs.90.long.2, rs == 'Pass')

# plot to check it looks right
abs.90.plot.2 <- ggplot(abs.90.long.2, aes(x=Frequency, y=median, ymin=five, ymax=ninety5)) +
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
print(abs.90.plot.2)

# save 90% range for the app
#saveRDS(abs.90.long.2, "twelveMth90range.rds")

# modeling
summary(abs.2$rs)
abs.2$rs = factor(abs.2$rs, levels = c('Pass', 'Mild', 'Severe'))

# check assumption of ordinality 
# The solid lines are the simple stratified means, and the dashed lines are the expected values if the assumption of proportional odds is met. 
abs.2.copy = abs.2
abs.2.copy$rs = as.character(abs.2.copy$rs)
abs.2.copy$rs[abs.2.copy$rs=='Pass'] = 'Normal'
abs.2.copy$rs = factor(abs.2.copy$rs, levels = c('Normal', 'Mild', 'Severe'))
ord.plot = plot.xmean.ordinaly(rs ~ abs1000 + abs1414 + abs2000 + abs2828 + abs4000 + abs5657, cr=F, topcats=2, subn = F, data = abs.2.copy)

jpeg("fig.2.ord.plots.jpeg", width = 9, height = 6 , units = 'in', res = 500)
par(mfrow=c(2,3))
par(mar=c(5,4,2,2)+0.1)
MyersMisc:::My.plot.xmean.ordinaly(rs ~ abs1000, cr=F, topcats=2, subn = F, data = abs.2.copy, xlab = "Reference Standard", ylab = "Absorbance 1000 Hz")
MyersMisc:::My.plot.xmean.ordinaly(rs ~ abs1414, cr=F, topcats=2, subn = F, data = abs.2.copy, xlab = "Reference Standard", ylab = "Absorbance 1414 Hz")
MyersMisc:::My.plot.xmean.ordinaly(rs ~ abs2000, cr=F, topcats=2, subn = F, data = abs.2.copy, xlab = "Reference Standard", ylab = "Absorbance 2000 Hz")
MyersMisc:::My.plot.xmean.ordinaly(rs ~ abs2828, cr=F, topcats=2, subn = F, data = abs.2.copy, xlab = "Reference Standard", ylab = "Absorbance 2828 Hz")
MyersMisc:::My.plot.xmean.ordinaly(rs ~ abs4000, cr=F, topcats=2, subn = F, data = abs.2.copy, xlab = "Reference Standard", ylab = "Absorbance 4000 Hz")
MyersMisc:::My.plot.xmean.ordinaly(rs ~ abs5657, cr=F, topcats=2, subn = F, data = abs.2.copy, xlab = "Reference Standard", ylab = "Absorbance 5657 Hz")
dev.off()

# set all other to "Asian"
abs.2$ethnicity[abs.2$ethnicity == "African"] <- "Asian"
abs.2$ethnicity[abs.2$ethnicity == "Oceanian"] <- "Asian"
abs.2$ethnicity[abs.2$ethnicity == "South American"] <- "Asian"
abs.2$ethnicity = droplevels(abs.2$ethnicity)

raw.dd <- datadist(abs.2)
options(datadist="raw.dd")
f <- lrm(rs ~ abs1000 + abs1414 + abs2000 + abs2828 + abs4000 + abs5657, data = abs.2, x = T, y = T)
#p <- pentrace(f, seq(0, 5, by=.005)) # penalty was 0 - no need
#p$penalty
#f.pca.penal <- update(f.pca, penalty=p$penalty)
#f.pca.penal
#f.pca.penal$penalty

# now robust SE can use bootcov or robcov - I went with robcov
# I think robcov might be better - it may cause confusion to use bootstrap twice and robcov seems to have more literature
# b = bootcov(f, wai.2.df$id.res)
# b
# bootplot(b)
r = robcov(f, abs.2$sub.id) 
r                                

# shrinkage coefficient
gamma = (298.92 - 6) / 298.92
aic.mv = AIC(r)
vif(r)
resid(r, "gof")
anova(r)
plot(anova(r))
options(prType = 'plain') # change to "latex" if want latex output
latex(r, file = "") # replace "abs" with: \textit{A}  and paste into Rsweave file then compile pdf
saveRDS(r, 'twelveMthModel.rds')
r.pred <- predict(r, type = "fitted")
# need predict lp (not fitted) for the multiclass roc
# BUT don't use proc to calculate auc - it uses different method than rms - use MRME package if needed
# b.pred.lp = predict(b)
# b.pca.roc <- multiclass.roc(wai.2.df$rs, b.pred.lp, levels = c("Pass", "Mild", "Severe"), 
#                             print.auc=TRUE, plot=TRUE, print.thres = TRUE, col="purple", main="PCA fit")

cal1 = calibrate(r, B = 500, kint = 1) # calibrate for Y >= Mild
# it wouldn't let me set riskdist to "F" so I used the scat1d.opts - and set to 0 to supress the distribution of predictions in margin
plot(cal1, scat1d.opts=list(nhistSpike=0, side=1, frac=0.00, tck=0), subtitles = F, xlab = "Predicted Probability", ylab = "Actual Probability")
cal2 = calibrate(r, B = 500, kint = 2) # calibrate for Y >= Severe
plot(cal2, scat1d.opts=list(nhistSpike=0, side=1, frac=0.00, tck=0), subtitles = F, xlab = "Predicted Probability", ylab = "Actual Probability")

jpeg("fig.3.cal.plots.jpeg", width = 10, height = 5 , units = 'in', res = 500)
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

# pred.test.penal <- predict(f, testing, type="lp")
# # roc.test.penal <- multiclass.roc(testing$rs, pred.test.penal) 
# # roc.test.penal 
# 
# rs.ordered.test = ordered(testing$rs)
# auc.test = correlate(rs.ordered.test, pred.test.penal, "cindex")
# auc.test 

plot(Predict(r, fun = plogis,  kint = 1))
plot(Predict(r, fun = plogis, kint = 2))

val <- validate(r, B=500)
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

# c-index for levels of RS
fitted.pred = as.data.frame(predict(r, type = 'fitted'))
mild.pred = fitted.pred[,1]
sev.pred = fitted.pred[,2]

y.mild = as.numeric(abs.2$rs)
y.mild = replace(y.mild, y.mild==1, 0) # make 0 normal
y.mild = replace(y.mild, y.mild==2, 1) # make mild==1
y.mild = replace(y.mild, y.mild==3, 1) # make severe==1
cal.plot.mild.train = val.prob(mild.pred, y.mild) # c-index = 0.919

y.severe = as.numeric(abs.2$rs)
y.severe = replace(y.severe, y.severe==1, 0) # make 0 normal
y.severe = replace(y.severe, y.severe==2, 0) # make mild==0
y.severe = replace(y.severe, y.severe==3, 1) # make severe==1
cal.plot.severe.train = val.prob(sev.pred, y.severe) # c-index = 0.958

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
pred.compare = cbind(pred, abs.2$rs)

cont.tab = table(pred.compare$cluster, pred.compare$`abs.2$rs`) # columns are the original label (the truth), and rows are the max predictions
# explore any that the model said was normal, but RS said Severe

# what is the median and range of predictions?
detach(package:plyr)
mean = pred.compare %>% 
  group_by(cluster) %>% 
  summarise(mean = mean(value)) 
mean

min = pred.compare %>% 
  group_by(cluster) %>% 
  summarise(min = min(value)) 
min

max = pred.compare %>% 
  group_by(cluster) %>% 
  summarise(max = max(value)) 
max

# predict class membership P >= j|X using 0.5 cutoff
pred.fit = predict(r, type = "fitted")
pred.fit = as.data.frame(round(pred.fit, digits =2))
names(pred.fit) = c("mild", "severe")
pred.fit$rs = abs.2$rs
pred.fit$label = NA

pred.fit$label = with(pred.fit, 
                      ifelse(severe > 0.5, "Severe",
                             ifelse(severe < 0.5 & mild < 0.5, "Normal", "Mild")))
pred.fit$label = factor(pred.fit$label, levels = c('Normal', 'Mild', 'Severe'))

cont.tab2 = table(pred.fit$label, pred.fit$rs) # columns are the original label (the truth), and rows are the max predictions

# example
eg1 = filter(abs.2, sub.id==534, ear=="L") 
eg.prob.ind1 = round(predict(r, eg1, type = "fitted.ind"), 2)
eg.prob.ind1
eg.prob.fit1 = round(predict(r, eg1, type = "fitted"), 2)
eg.prob.fit1
prob.ind.norm1 = eg.prob.ind1[1]
prob.ind.mild1 = eg.prob.ind1[2]
prob.ind.sev1 = eg.prob.ind1[3]
prob.fit.mild1 = eg.prob.fit1[1]
prob.fit.sev1 = eg.prob.fit1[2]
eg.abs1 = filter(abs.24, sub.id==534, ear=="L") 
eg.abs1 = dplyr::select(eg.abs1, abs226:abs8000)
freq.num = c(226.00, 257.33, 280.62, 297.30, 324.21, 343.49, 363.91, 385.55, 408.48, 432.77, 458.50,
                  471.94, 500.00, 514.65, 545.25, 561.23, 577.68, 594.60, 629.96, 648.42, 667.42, 686.98,
                  707.11, 727.83, 749.15, 771.11, 793.70, 816.96, 840.90, 865.54, 890.90, 917.00, 943.87,
                  971.53, 1000.00, 1029.30, 1059.46, 1090.51, 1122.46, 1155.35, 1189.21, 1224.05, 1259.92, 
                  1296.84, 1334.84, 1373.95, 1414.21, 1455.65, 1498.31, 1542.21, 1587.40, 1633.92, 1681.79,
                  1731.07, 1781.80, 1834.01, 1887.75, 1943.06, 2000.00, 2058.60, 2118.93, 2181.02, 2244.92,
                  2310.71, 2378.41, 2448.11, 2519.84, 2593.68, 2669.68, 2747.91, 2828.43, 2911.31, 2996.61,
                  3084.42, 3174.80, 3267.83, 3363.59, 3462.15, 3563.59, 3668.02, 3775.50, 3886.13, 
                  4000.00, 4117.21, 4237.85, 4362.03, 4489.85, 4621.41, 4756.83, 4896.21, 5039.68, 5187.36,
                  5339.36, 5495.81, 5656.85, 5822.61, 5993.23, 6168.84, 6349.60, 6535.66, 6727.17, 6924.29,
                  7127.19, 7336.03, 7550.99, 7772.26, 8000.00)
names(eg.abs1) = freq.num
eg.abs.long1 <- gather(eg.abs1, Frequency, Absorbance, 1:107)
eg.abs.long1$Frequency = as.numeric(eg.abs.long1$Frequency)

eg.plot.1 = ggplot(eg.abs.long1) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  geom_line(aes(x= Frequency, y=Absorbance), data = eg.abs.long1, colour="blue") +
  geom_ribbon(data=abs.90.long, aes(x = Frequency, ymin = five, ymax = ninety5, linetype=NA), alpha = 0.2, show.legend = F) +
  xlab("Frequency, Hz") +
  ylab("Absorbance") +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(0, 1)) +
  theme(legend.text=element_text(size=10), legend.justification=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(plot.title = element_text(vjust=2)) +
  annotate("text", x = 250, y = c(0.90), label = ("Left ear of a 55-week-old male"), hjust = 0) +
  annotate("text", x = 250, y = c(0.80), label = ("'ME' >= ~'mild'~ 0.73"), parse=TRUE, hjust=0) +
  annotate("text", x = 250, y = c(0.70), label = ("'ME' >= ~'severe'~ 0.26"), parse=TRUE, hjust=0) +
  annotate("text", x = 250, y = c(0.60), label = paste("Normal = ",  prob.ind.norm1), parse=F, hjust=0) +
  annotate("text", x = 250, y = c(0.50), label = paste("Mild = ",  prob.ind.mild1), parse=F, hjust=0) +
  annotate("text", x = 250, y = c(0.40), label = paste("Severe = ",  prob.ind.sev1), parse=F, hjust=0) 
eg.plot.1

eg2 = filter(abs.2, sub.id==416, ear=="R") 
eg.prob.2 = round(predict(r, eg2, type='fitted'), 2)
eg.prob.ind2 = round(predict(r, eg2, type = "fitted.ind"), 2)
eg.prob.ind2
prob.ind.norm2 = eg.prob.ind2[1]
prob.ind.mild2 = eg.prob.ind2[2]
prob.ind.sev2 = eg.prob.ind2[3]
eg.abs2 = filter(abs.24, sub.id==416, ear=="R") 
eg.abs2 = dplyr::select(eg.abs2, abs226:abs8000)
names(eg.abs2) = freq.num
eg.abs.long2 <- gather(eg.abs2, Frequency, Absorbance, 1:107)
eg.abs.long2$Frequency = as.numeric(eg.abs.long2$Frequency)

eg.plot.2 = ggplot(eg.abs.long2) +
  scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
  geom_line(aes(x= Frequency, y=Absorbance), data = eg.abs.long2, colour="red") +
  geom_ribbon(data=abs.90.long, aes(x = Frequency, ymin = five, ymax = ninety5, linetype=NA), alpha = 0.2, show.legend = F) +
  xlab("Frequency, Hz") +
  ylab("Absorbance") +
  scale_y_continuous(expand=c(0, 0), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1), limits=c(0, 1)) +
  theme(legend.text=element_text(size=10), legend.justification=c(0,1)) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  theme(plot.title = element_text(vjust=2)) +
  annotate("text", x = 250, y = c(0.90), label = ("Right ear of a 53-week-old male"), hjust = 0) +
  annotate("text", x = 250, y = c(0.80), label = ("'ME' >= ~'mild'~ 0.98"), parse=TRUE, hjust=0) +
  annotate("text", x = 250, y = c(0.70), label = ("'ME' >= ~'severe'~ 0.89"), parse=TRUE, hjust=0) +
  annotate("text", x = 250, y = c(0.60), label = paste("Normal = ",  prob.ind.norm2), parse=F, hjust=0) +
  annotate("text", x = 250, y = c(0.50), label = paste("Mild = ",  prob.ind.mild2), parse=F, hjust=0) +
  annotate("text", x = 250, y = c(0.40), label = paste("Severe = ",  prob.ind.sev2), parse=F, hjust=0) 

eg.plot.2

eg.plots <- plot_grid(eg.plot.1, eg.plot.2, nrow=2, ncol=1, align = "v", labels = c("A", "B"))
ggsave("fig.4.eg.plots.jpeg", eg.plots, height=6, width=6, dpi=500)

# Demographics model
f.dem <- lrm(rs ~ ear + gender + ethnicity + abs1000 + abs1414 + abs2000 + abs2828 + abs4000 + abs5657, data = abs.2, x = T, y = T)
r.dem = robcov(f.dem, abs.2$sub.id) 
r.dem                  
gamma.dem = (302.03 - 9) / 302.03
aic.dem = AIC(r.dem) # model without dems has lower AIC so use that one

# nonlinear model
f.nl <- lrm(rs ~ rcs(abs1000, 5) + rcs(abs1414, 5) + rcs(abs2000, 5) + rcs(abs2828, 5) + rcs(abs4000, 5) + rcs(abs5657, 5), data = abs.2, x = T, y = T)
r.nl = robcov(f.nl, abs.2$sub.id) 
r.nl                                
gamma.nl = (324.50 - 24) / 324.50
aic.nl = AIC(r.nl)

# UV models
f.250 = lrm(rs ~ abs250, data = abs.2, x = T, y = T)
f.354 = lrm(rs ~ abs354, data = abs.2, x = T, y = T)
f.500 = lrm(rs ~ abs500, data = abs.2, x = T, y = T)
f.707 = lrm(rs ~ abs707, data = abs.2, x = T, y = T)
f.1000 = lrm(rs ~ abs1000, data = abs.2, x = T, y = T)
f.1414 = lrm(rs ~ abs1414, data = abs.2, x = T, y = T)
f.2000 = lrm(rs ~ abs2000, data = abs.2, x = T, y = T)
f.2828 = lrm(rs ~ abs2828, data = abs.2, x = T, y = T)
f.4000 = lrm(rs ~ abs4000, data = abs.2, x = T, y = T)
f.5657 = lrm(rs ~ abs5657, data = abs.2, x = T, y = T)
f.8000 = lrm(rs ~ abs8000, data = abs.2, x = T, y = T)

r.250 = robcov(f.250, abs.2$sub.id) 
r.354 = robcov(f.354, abs.2$sub.id) 
r.500 = robcov(f.500, abs.2$sub.id) 
r.707 = robcov(f.707, abs.2$sub.id) 
r.1000 = robcov(f.1000, abs.2$sub.id) 
r.1414 = robcov(f.1414, abs.2$sub.id) 
r.2000 = robcov(f.2000, abs.2$sub.id) 
r.2828 = robcov(f.2828, abs.2$sub.id) 
r.4000 = robcov(f.4000, abs.2$sub.id) 
r.5657 = robcov(f.5657, abs.2$sub.id) 
r.8000 = robcov(f.8000, abs.2$sub.id) 

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

##  convert Dxy to AUC
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