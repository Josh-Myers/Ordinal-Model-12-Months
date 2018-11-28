# dem and averaging 12 mth

# ordinal RS - pass = pass both (type A and pass DP)
#            - Mild = fail one test (fail DP and type A | type B or C tymp and pass DP)
#            - Severe = fail both tests

#import----
library(tidyverse)
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



# missing data----
# total number of infants who attended appts
ss <- cbind.data.frame(tymp.dp, wai.24)

ss <- ss[,-c(8, 9, 331, 332)]
summary(as.factor(ss$tymp))
# 59 + 279 + 25 + 129 = 492 ears 
# 59 cnt = 0.12 proportion
summary(ss$dpoae)
# 65 + 316 + 111 = 492 ears
# 21 cnt = 0.06
summary(as.factor(ss$rs.1)) 
# 77 + 62 + 97 + 256 = 492 ears 
# 77/492 cnt = 0.16
summary(ss$rs) 
# 47 + 64 + 104 + 277 = 492
# 47/492 cnt = 0.10
# extra 30 ears able to be classified - only results for one test used if other test missing

# for wai results CNT have been put as NA. need to put one col of wai results as CNT if got results or cnt for other tests (means CNT wai)
wai.cnt <- wai.24

wai.cnt$abs226 <- ifelse(!is.na(ss$tymp) & is.na(wai.cnt$abs226), "CNT", 
                         ifelse(!is.na(ss$dpoae) & is.na(wai.cnt$abs226), "CNT", wai.cnt$abs226))
check.cnt <- cbind.data.frame(ss[, 1:6], wai.cnt$abs226)
summary(as.factor(wai.cnt$abs226)) 
# so 74 CNT for wai 
wai.res <- na.omit(wai.24$abs226) # shows how many CNT for WAI

# However, this is better, apply CNT function over entire wai data frame - so can rm na will rm all that did not show up                    
cnt.fn <- function(x) {
  ifelse(!is.na(ss$tymp) & is.na(x), "CNT", 
         ifelse(!is.na(ss$dpoae) & is.na(x), "CNT", x))
}
# join demographics rep 

#rename variables
names(wai.24)[names(wai.24)=="Ear"] <- "ear"
names(wai.24)[names(wai.24)=="ID Res"] <- "id.res"

names(wai.3)[names(wai.3)=="Ear"] <- "ear"
names(wai.3)[names(wai.3)=="ID Res"] <- "id.res"

names(wai.2)[names(wai.2)=="Ear"] <- "ear"
names(wai.2)[names(wai.2)=="ID Res"] <- "id.res"

names(wai.1)[names(wai.1)=="Ear"] <- "ear"
names(wai.1)[names(wai.1)=="ID Res"] <- "id.res"

demographicsRep <- demographicsRep[order(demographicsRep$id.res),]
wai.2 <- wai.2[order(wai.2$id.res),]
wai.3 <- wai.3[order(wai.3$id.res),]
wai.24 <- wai.24[order(wai.24$id.res),]
# check if idres is identical both data frames
setdiff(demographicsRep$id.res, wai.2$id.res) # should be 0

wai.24.only <- wai.24[,-c(1, 2, 324, 325)] 
wai.24.only <- apply(wai.24.only, 2, cnt.fn)
wai.24.b <- cbind.data.frame(ss[,1:7], wai.24.only, demographicsRep)
wai.24.b <- wai.24.b[complete.cases(wai.24.b[,8:328]),]
wai.24.c <- wai.24.b[,-c(3:5,329)]
str(wai.24.c$rs)
wai.24.c$rs <- relevel(wai.24.c$rs, "Pass")
wai.24.c$rs = droplevels(wai.24.c$rs)
# check if id.res matches
check.2 <- wai.24.b[,c(1, 329)] # should be exactly the same

# add demographics
wai.3.only <- wai.3[,-c(1, 2, 52)] 
wai.3.only <- apply(wai.3.only, 2, cnt.fn)
wai.3.b <- cbind.data.frame(ss[,1:6], wai.3.only, demographicsRep)
wai.3.b <- wai.3.b[complete.cases(wai.3.b[,7:55]),]
wai.3.c <- wai.3.b[,-c(3:5,56)]
str(wai.3.c$rs)
wai.3.c$rs <- relevel(wai.3.c$rs, "Pass")

wai.2.only <- wai.2[,-c(1, 2, 37)] 
wai.2.only <- apply(wai.2.only, 2, cnt.fn)
wai.2.b <- cbind.data.frame(ss[,1:6], wai.2.only, demographicsRep)
wai.2.b <- wai.2.b[complete.cases(wai.2.b[,7:40]),]
wai.2.c <- wai.2.b[,-c(3:5,41)]
str(wai.2.c$rs)
wai.2.c$rs <- relevel(wai.2.c$rs, "Pass")

wai.1.only <- wai.1[,-c(1, 2, 22)] 
wai.1.only <- apply(wai.1.only, 2, cnt.fn)
wai.1.b <- cbind.data.frame(ss[,1:6], wai.1.only, demographicsRep)
wai.1.b <- wai.1.b[complete.cases(wai.1.b[,7:25]),]
wai.1.c <- wai.1.b[,-c(3:5, 26)]
str(wai.1.c$rs)
wai.1.c$rs <- relevel(wai.1.c$rs, "Pass")

# then need to change CNT back to NA then change wai to numeric
na.fn <- function(x) {
  ifelse(x == "CNT", NA, x) 
}

wai.24.c <- apply(wai.24.c, 2, na.fn)
wai.24.c <- as.data.frame(wai.24.c)

num.fn <- function(x) {
  as.numeric(as.character(x))
}

# function to return df with NA omitted based on desired columns
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

wai.24.c.only <- wai.24.c[,c(5:325)]
wai.24.t <- apply(wai.24.c.only, 2, num.fn)
wai.24.t <- as.data.frame(wai.24.t)
wai.24.i <- wai.24.c[,c(1:4, 326:328)]
wai.24.i$id.res = as.numeric(as.character(wai.24.i$id.res))
wai.24.df <- cbind.data.frame(wai.24.i, wai.24.t)
str(wai.24.df$rs)
wai.24.df$rs <- relevel(wai.24.df$rs, "Pass")
wai.24.df = completeFun(wai.24.df, c(3,4,8))
wai.24.df = drop.levels(wai.24.df)

wai.3.c <- apply(wai.3.c, 2, na.fn)
wai.3.c <- as.data.frame(wai.3.c)
wai.3.c.only <- wai.3.c[,c(4:52)]
wai.3.t <- apply(wai.3.c.only, 2, num.fn)
wai.3.t <- as.data.frame(wai.3.t)
wai.3.df <- cbind.data.frame(wai.24.i, wai.3.t)
str(wai.3.df$rs)
wai.3.df$rs <- relevel(wai.3.df$rs, "Pass")
wai.3.df = completeFun(wai.3.df, c(3,4,8))
wai.3.df = drop.levels(wai.3.df)

wai.2.c <- apply(wai.2.c, 2, na.fn)
wai.2.c <- as.data.frame(wai.2.c)
wai.2.c.only <- wai.2.c[,c(4:37)]
wai.2.t <- apply(wai.2.c.only, 2, num.fn)
wai.2.t <- as.data.frame(wai.2.t)
wai.2.df <- cbind.data.frame(wai.24.i, wai.2.t)
str(wai.2.df$rs)
wai.2.df$rs <- relevel(wai.2.df$rs, "Pass")
wai.2.df = completeFun(wai.2.df, c(3,4,8))
wai.2.df = drop.levels(wai.2.df)

# wai.1.c <- apply(wai.1.c, 2, na.fn)
# wai.1.c <- as.data.frame(wai.1.c)
# wai.1.c.only <- wai.1.c[,c(4:22)]
# wai.1.t <- apply(wai.1.c.only, 2, num.fn)
# wai.1.t <- as.data.frame(wai.1.t)
# wai.1.df <- cbind.data.frame(wai.24.i, wai.1.t)
# str(wai.1.df$rs)
# wai.1.df$rs <- relevel(wai.1.df$rs, "Pass")
# wai.1.df = na.omit(wai.1.df)
# wai.1.df = drop.levels(wai.1.df)

# age range and do age plot----
dem <- filter(wai.2.c, ear=="R" )
dem <- select(dem, gender:test.time.12.in.min)
dem$age.12.mth.in.weeks <- as.numeric(as.character(dem$age.12.mth.in.weeks))
dem$test.time.12.in.min <- as.numeric(as.character(dem$test.time.12.in.min))
summary(dem$test.time.12.in.min) # no need to report it took ~5-10 min
summary(dem$age.12.mth.in.weeks) # one is 7 - this is in months set to NA
#dem$age.12.mth.in.weeks[dem$age.12.mth.in.weeks==7] <- NA
dem$age.12.mth.in.weeks[dem$age.12.mth.in.weeks < 40] = NA
hist(dem$age.12.mth.in.weeks)
sum(is.na(dem$age.12.mth.in.weeks))
# 43-70 weeks = 10-16 mths
# percentage of ethnicity
summary(dem$maternal.ethnicity)
# 1 african, 1 sth am, 22 asian, 17 atsi, 121 cauc, 4 oceanian
# comine african, sth am and ocean with asian = 28 asian, 120 cauc, 17 atsi
# = asian 17%, atsi 10%, cauc 73%
# plot age 
#age plot 
# theme_set(theme_bw() + theme(legend.key = element_blank()) + theme(panel.grid.major = element_blank(), 
#             panel.grid.minor = element_blank()) + theme(panel.border=element_rect(colour = "black")))

age.plot <- ggplot(dem, aes(x=gender, y=age.12.mth.in.weeks)) + 
  scale_y_continuous(expand=c(0, 0), breaks = c(40, 50, 60, 70), limits = c(40, 70), 
                     labels = c(40, 50, 60, 70)) +
  scale_x_discrete(breaks=c("F","M"), labels=c("Female", "Male")) +
  geom_boxplot() +
  ylab("Age, weeks") +
  xlab("Gender") +
  theme(legend.position="none") 
print(age.plot)

ggsave("age.plot.tiff", age.plot, height=6, width=5, dpi=1200)

# misc notes----
#there was still one extra in wai.1 - there was a retest that was labelled as test - fixed now but below shows how to do it
# threshold <- 3
# Filter(function (elem) length(which(wai.1$ID.Res == elem)) >= threshold, wai.1$ID.Res)
# #579 was the offender I have changed this in the excel file

# my cal plot function----
# hack validtate.plot.default for my 2 panel plot to match with val.prob - actuall val.prob didn't work because had a space under hist
# had to make a hack of val.prob.ci which is steyerbergs hack of val.prob
MyCalPlot <-
  function (p, y, logit, group, weights = rep(1, length(y)), normwt = FALSE, 
            pl = TRUE, smooth = TRUE, logistic.cal = TRUE, xlab = "Predicted Probability", 
            ylab = "Actual Probability", lim = c(0, 1), m, g, cuts, emax.lim = c(0, 
                                                                                 1), legendloc = lim[1] + c(0.55 * diff(lim), 0.27 * diff(lim)), 
            statloc = c(0, 0.99), riskdist = "calibrated", cex = 0.7, 
            mkh = 0.02, connect.group = FALSE, connect.smooth = TRUE, 
            g.group = 4, evaluate = 100, nmin = 0) 
  {
    if (missing(p)) 
      p <- plogis(logit)
    else logit <- qlogis(p)
    if (length(p) != length(y)) 
      stop("lengths of p or logit and y do not agree")
    names(p) <- names(y) <- names(logit) <- NULL
    Spi <- function(p, y) {
      z <- sum((y - p) * (1 - 2 * p))/sqrt(sum((1 - 2 * p) * 
                                                 (1 - 2 * p) * p * (1 - p)))
      P <- 2 * pnorm(-abs(z))
      c(Z = z, P = P)
    }
    if (!missing(group)) {
      if (length(group) == 1 && is.logical(group) && group) 
        group <- rep("", length(y))
      if (!is.factor(group)) 
        group <- if (is.logical(group) || is.character(group)) 
          as.factor(group)
      else cut2(group, g = g.group)
      names(group) <- NULL
      nma <- !(is.na(p + y + weights) | is.na(group))
      ng <- length(levels(group))
    }
    else {
      nma <- !is.na(p + y + weights)
      ng <- 0
    }
    logit <- logit[nma]
    y <- y[nma]
    p <- p[nma]
    if (ng > 0) {
      group <- group[nma]
      weights <- weights[nma]
      return(val.probg(p, y, group, evaluate, weights, normwt, 
                       nmin))
    }
    if (length(unique(p)) == 1) {
      P <- mean(y)
      Intc <- qlogis(P)
      n <- length(y)
      D <- -1/n
      L01 <- -2 * sum(y * logit - logb(1 + exp(logit)), na.rm = TRUE)
      L.cal <- -2 * sum(y * Intc - logb(1 + exp(Intc)), na.rm = TRUE)
      U.chisq <- L01 - L.cal
      U.p <- 1 - pchisq(U.chisq, 1)
      U <- (U.chisq - 1)/n
      Q <- D - U
      spi <- unname(Spi(p, y))
      stats <- c(0, 0.5, 0, D, 0, 1, U, U.chisq, U.p, Q, mean((y - 
                                                                 p[1])^2), Intc, 0, rep(abs(p[1] - P), 2), spi)
      names(stats) <- c("Dxy", "C (ROC)", "R2", "D", "D:Chi-sq", 
                        "D:p", "U", "U:Chi-sq", "U:p", "Q", "Brier", "Intercept", 
                        "Slope", "Emax", "Eavg", "S:z", "S:p")
      return(stats)
    }
    i <- !is.infinite(logit)
    nm <- sum(!i)
    if (nm > 0) 
      warning(paste(nm, "observations deleted from logistic calibration due to probs. of 0 or 1"))
    f.fixed <- lrm.fit(logit[i], y[i], initial = c(0, 1), maxit = 1L)
    f.recal <- lrm.fit(logit[i], y[i])
    stats <- f.fixed$stats
    n <- stats["Obs"]
    predprob <- seq(emax.lim[1], emax.lim[2], by = 5e-04)
    lt <- f.recal$coef[1] + f.recal$coef[2] * qlogis(predprob)
    calp <- plogis(lt)
    emax <- max(abs(predprob - calp))
    Sm <- lowess(p, y, iter = 0)
    cal.smooth <- approx(Sm, xout = p, ties = mean)$y
    eavg <- mean(abs(p - cal.smooth))
    if (pl) {
      plot(0.5, 0.5, xlim = lim, ylim = lim, type = "n", xlab = xlab, 
           ylab = ylab)
      abline(0, 1, lty = 2)
      lt <- 2
      leg <- "Ideal"
      marks <- -1
      if (logistic.cal) {
        lt <- c(lt, 1)
        leg <- c(leg, "Logistic calibration")
        marks <- c(marks, -1)
      }
      if (smooth) {
        if (connect.smooth) {
          lines(Sm, lty = 1)
          lt <- c(lt, 3)
          marks <- c(marks, -1)
        }
        else {
          points(Sm)
          lt <- c(lt, 0)
          marks <- c(marks, 1)
        }
        leg <- c(leg, "Actual")
      }
      if (!missing(m) | !missing(g) | !missing(cuts)) {
        if (!missing(m)) 
          q <- cut2(p, m = m, levels.mean = TRUE, digits = 7)
        else if (!missing(g)) 
          q <- cut2(p, g = g, levels.mean = TRUE, digits = 7)
        else if (!missing(cuts)) 
          q <- cut2(p, cuts = cuts, levels.mean = TRUE, 
                    digits = 7)
        means <- as.numeric(levels(q))
        prop <- tapply(y, q, function(x) mean(x, na.rm = TRUE))
        points(means, prop, pch = 2)
        if (connect.group) {
          lines(means, prop)
          lt <- c(lt, 1)
        }
        else lt <- c(lt, 0)
        leg <- c(leg, "Grouped observations")
        marks <- c(marks, 2)
      }
    }
    lr <- stats["Model L.R."]
    p.lr <- stats["P"]
    D <- (lr - 1)/n
    L01 <- -2 * sum(y * logit - logb(1 + exp(logit)), na.rm = TRUE)
    U.chisq <- L01 - f.recal$deviance[2]
    p.U <- 1 - pchisq(U.chisq, 2)
    U <- (U.chisq - 2)/n
    Q <- D - U
    Dxy <- stats["Dxy"]
    C <- stats["C"]
    R2 <- stats["R2"]
    B <- mean((p - y)^2)
    spi <- unname(Spi(p, y))
    stats <- c(Dxy, C, R2, D, lr, p.lr, U, U.chisq, p.U, Q, B, 
               f.recal$coef, emax, spi)
    names(stats) <- c("Dxy", "C (ROC)", "R2", "D", "D:Chi-sq", 
                      "D:p", "U", "U:Chi-sq", "U:p", "Q", "Brier", "Intercept", 
                      "Slope", "Emax", "S:z", "S:p")
    stats <- c(stats, c(Eavg = eavg))
    if (pl) {
      logit <- seq(-7, 7, length = 200)
      prob <- plogis(logit)
      pred.prob <- f.recal$coef[1] + f.recal$coef[2] * logit
      pred.prob <- plogis(pred.prob)
      if (logistic.cal) 
        lines(prob, pred.prob, lty = 1)
      lp <- legendloc
      if (!is.logical(lp)) {
        if (!is.list(lp)) 
          lp <- list(x = lp[1], y = lp[2])
        legend(lp, leg, lty = c(2,1), pch = marks, cex = cex, 
               bty = "n")
      }
      if (!is.logical(statloc)) {
        dostats <- c(1, 2, 3, 4, 7, 10, 11, 12, 13, 14, 15, 
                     16)
        leg <- format(names(stats)[dostats])
        leg <- paste(leg, ":", format(stats[dostats]), sep = "")
        if (!is.list(statloc)) 
          statloc <- list(x = statloc[1], y = statloc[2])
        text(statloc, paste(format(names(stats[dostats])), 
                            collapse = "\n"), adj = c(0, 1), cex = cex)
        text(statloc$x + 0.225 * diff(lim), statloc$y, paste(format(round(stats[dostats], 
                                                                          3)), collapse = "\n"), adj = c(1, 1), cex = cex)
      }
      if (is.character(riskdist)) {
        if (riskdist == "calibrated") {
          x <- f.recal$coef[1] + f.recal$coef[2] * qlogis(p)
          x <- plogis(x)
          x[p == 0] <- 0
          x[p == 1] <- 1
        }
        else x <- p
        bins <- seq(lim[1], lim[2], length = 101)
        x <- x[x >= lim[1] & x <= lim[2]]
        f <- table(cut(x, bins))
        j <- f > 0
        bins <- (bins[-101])[j]
        f <- f[j]
        f <- lim[1] + 0.15 * diff(lim) * f/max(f)
        segments(bins, 0, bins, f)
      }
    }
    stats
  }
# my val plot function----
# hack val.prob for my plot - set line type and legend etc

MyValPlot <-
  function (x, xlab, ylab, xlim, ylim, legend = TRUE, subtitles = TRUE, 
            scat1d.opts = NULL, ...) 
  {
    at <- attributes(x)
    if (missing(ylab)) 
      ylab <- if (at$model == "lr") 
        "Actual Probability"
    else paste("Observed", at$yvar.name)
    if (missing(xlab)) {
      if (at$model == "lr") {
        xlab <- paste("Predicted Pr{", at$yvar.name, sep = "")
        if (at$non.slopes == 1) {
          xlab <- if (at$lev.name == "TRUE") 
            paste(xlab, "}", sep = "")
          else paste(xlab, "=", at$lev.name, "}", sep = "")
        }
        else xlab <- paste(xlab, ">=", at$lev.name, "}", 
                           sep = "")
      }
      else xlab <- paste("Predicted", at$yvar.name)
    }
    p <- x[, "predy"]
    p.app <- x[, "calibrated.orig"]
    p.cal <- x[, "calibrated.corrected"]
    if (missing(xlim) & missing(ylim)) 
      xlim <- ylim <- range(c(p, p.app, p.cal), na.rm = TRUE)
    else {
      if (missing(xlim)) 
        xlim <- range(p)
      if (missing(ylim)) 
        ylim <- range(c(p.app, p.cal, na.rm = TRUE))
    }
    plot(p, p.app, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, 
         type = "n", ...)
    predicted <- at$predicted
    err <- NULL
    if (length(predicted)) {
      s <- !is.na(p + p.cal)
      err <- predicted - approx(p[s], p.cal[s], xout = predicted, 
                                ties = mean)$y
      cat("\nn=", n <- length(err), "   Mean absolute error=", 
          round(mae <- mean(abs(err), na.rm = TRUE), 3), "   Mean squared error=", 
          round(mean(err^2, na.rm = TRUE), 5), "\n0.9 Quantile of absolute error=", 
          round(quantile(abs(err), 0.9, na.rm = TRUE), 3), 
          "\n\n", sep = "")
      if (subtitles) 
        title(sub = paste("Mean absolute error=", round(mae, 
                                                        3), " n=", n, sep = ""), cex = 0.65, adj = 1)
      do.call("scat1d", c(list(x = predicted), scat1d.opts))
    }
    lines(p, p.app, lty = 3)
    lines(p, p.cal, lty = 1)
    abline(a = 0, b = 1, lty = 2)
    if (subtitles) 
      title(sub = paste("B=", at$B, "repetitions,", at$method), 
            adj = 0)
    if (!(is.logical(legend) && !legend)) {
      if (is.logical(legend)) 
        legend <- list(x = xlim[1] + 0.55 * diff(xlim), y = ylim[1] + 
                         0.32 * diff(ylim))
      legend(legend, c("Ideal", "Apparent", "Bias-corrected"), 
             lty = c(2, 3, 1), bty = "n")
    }
    invisible(err)
  }
# my val.prb ci.2 function----
# hack of steyerberg's cal.plot function - which is an adapataion of the val.prob fn from rms
# main thing was to get all hist bins poining up to match plot(calibrate) - I had 2 panel cal plot - needed to match the bottom histograms
# couldn't do that with val.prob because no way to control space under hist plot - could do that with this function
# changed degree to 1 to match val.prob which is what I really wanted to use in the first place
# how to install the package
# library(githubinstall)
# githubinstall("CalibrationCurves")
library(CalibrationCurves)

My.val.prob.ci.2 <-
  function (p, y, logit, group, weights = rep(1, length(y)), normwt = F, 
            pl = T, smooth = c("loess", "rcs", F), CL.smooth = "fill", 
            CL.BT = F, lty.smooth = 1, col.smooth = "black", lwd.smooth = 1, 
            nr.knots = 5, logistic.cal = F, lty.log = 1, col.log = "black", 
            lwd.log = 1, xlab = "Predicted Probability", ylab = "Observed proportion", 
            xlim = c(-0.02, 1), ylim = c(-0.15, 1), m, g, cuts, emax.lim = c(0, 
                                                                             1), legendloc = c(0.5, 0.27), statloc = c(0, 0.85), dostats = T, 
            cl.level = 0.95, method.ci = "pepe", roundstats = 2, riskdist = "predicted", 
            cex = 0.75, cex.leg = 0.75, connect.group = F, connect.smooth = T, 
            g.group = 4, evaluate = 100, nmin = 0, d0lab = "0", d1lab = "1", 
            cex.d01 = 0.7, dist.label = 0.04, line.bins = -0.05, dist.label2 = 0.03, 
            cutoff, las = 1, length.seg = 1, y.intersp = 1, lty.ideal = 1, 
            col.ideal = "grey", lwd.ideal = 1, ...) 
  {
    if (smooth[1] == F) {
      smooth <- "F"
    }
    smooth <- match.arg(smooth)
    if (!missing(p)) 
      if (any(!(p >= 0 | p <= 1))) {
        stop("Probabilities can not be > 1 or < 0.")
      }
    if (missing(p)) 
      p <- 1/(1 + exp(-logit))
    else logit <- log(p/(1 - p))
    if (!all(c(0, 1) %in% y)) {
      stop("The vector with the binary outcome can only contain the values 0 and 1.")
    }
    if (length(p) != length(y)) 
      stop("lengths of p or logit and y do not agree")
    names(p) <- names(y) <- names(logit) <- NULL
    if (!missing(group)) {
      if (length(group) == 1 && is.logical(group) && group) 
        group <- rep("", length(y))
      if (!is.factor(group)) 
        group <- if (is.logical(group) || is.character(group)) 
          as.factor(group)
      else cut2(group, g = g.group)
      names(group) <- NULL
      nma <- !(is.na(p + y + weights) | is.na(group))
      ng <- length(levels(group))
    }
    else {
      nma <- !is.na(p + y + weights)
      ng <- 0
    }
    logit <- logit[nma]
    y <- y[nma]
    p <- p[nma]
    if (ng > 0) {
      group <- group[nma]
      weights <- weights[nma]
      return(val.probg(p, y, group, evaluate, weights, normwt, 
                       nmin))
    }
    y <- y[order(p)]
    logit <- logit[order(p)]
    p <- p[order(p)]
    if (length(p) > 5000 & smooth == "loess") {
      warning("Number of observations > 5000, RCS is recommended.", 
              immediate. = T)
    }
    if (length(p) > 1000 & CL.BT == T) {
      warning("Number of observations is > 1000, this could take a while...", 
              immediate. = T)
    }
    if (length(unique(p)) == 1) {
      P <- mean(y)
      Intc <- log(P/(1 - P))
      n <- length(y)
      D <- -1/n
      L01 <- -2 * sum(y * logit - log(1 + exp(logit)), na.rm = T)
      L.cal <- -2 * sum(y * Intc - log(1 + exp(Intc)), na.rm = T)
      U.chisq <- L01 - L.cal
      U.p <- 1 - pchisq(U.chisq, 1)
      U <- (U.chisq - 1)/n
      Q <- D - U
      stats <- c(0, 0.5, 0, D, 0, 1, U, U.chisq, U.p, Q, mean((y - 
                                                                 p[1])^2), Intc, 0, rep(abs(p[1] - P), 2))
      names(stats) <- c("Dxy", "C (ROC)", "R2", "D", "D:Chi-sq", 
                        "D:p", "U", "U:Chi-sq", "U:p", "Q", "Brier", "Intercept", 
                        "Slope", "Emax", "Eavg", "ECI")
      return(stats)
    }
    i <- !is.infinite(logit)
    nm <- sum(!i)
    if (nm > 0) 
      warning(paste(nm, "observations deleted from logistic calibration due to probs. of 0 or 1"))
    i.2 <- i
    f.or <- lrm(y[i] ~ logit[i])
    f <- lrm.fit(logit[i], y[i])
    cl.slope <- confint(f, level = cl.level)[2, ]
    f2 <- lrm.fit(offset = logit[i], y = y[i])
    cl.interc <- confint(f2, level = cl.level)
    stats <- f$stats
    cl.auc <- ci.auc(y, p, cl.level, method.ci)
    n <- stats["Obs"]
    predprob <- seq(emax.lim[1], emax.lim[2], by = 5e-04)
    lt <- f$coef[1] + f$coef[2] * log(predprob/(1 - predprob))
    calp <- 1/(1 + exp(-lt))
    emax <- max(abs(predprob - calp))
    if (pl) {
      plot(0.5, 0.5, xlim = xlim, ylim = ylim, type = "n", 
           xlab = xlab, ylab = ylab, las = las, ...)
      clip(0, 1, 0, 1)
      abline(0, 1, lty = lty.ideal, col = col.ideal, lwd = lwd.ideal)
      do.call("clip", as.list(par()$usr))
      lt <- lty.ideal
      lw.d <- lwd.ideal
      all.col <- col.ideal
      leg <- "Ideal"
      marks <- -1
      if (logistic.cal) {
        lt <- c(lt, lty.log)
        lw.d <- c(lw.d, lwd.log)
        all.col <- c(all.col, col.log)
        leg <- c(leg, "Logistic calibration")
        marks <- c(marks, -1)
      }
      if (smooth != "F") {
        all.col <- c(all.col, col.smooth)
      }
      if (smooth == "loess") {
        Sm <- loess(y ~ p, degree = 1)
        Sm <- data.frame(Sm$x, Sm$fitted)
        Sm.01 <- Sm
        if (connect.smooth == T & CL.smooth != "fill") {
          clip(0, 1, 0, 1)
          lines(Sm, lty = lty.smooth, lwd = lwd.smooth, 
                col = col.smooth)
          do.call("clip", as.list(par()$usr))
          lt <- c(lt, lty.smooth)
          lw.d <- c(lw.d, lwd.smooth)
          marks <- c(marks, -1)
        }
        else if (connect.smooth == F & CL.smooth != "fill") {
          clip(0, 1, 0, 1)
          points(Sm, col = col.smooth)
          do.call("clip", as.list(par()$usr))
          lt <- c(lt, 0)
          lw.d <- c(lw.d, 1)
          marks <- c(marks, 1)
        }
        if (CL.smooth == T | CL.smooth == "fill") {
          to.pred <- seq(min(p), max(p), length = 200)
          if (CL.BT == T) {
            cat("Bootstrap samples are being generated.\n\n\n")
            res.BT <- replicate(2000, BT.samples(y, p, 
                                                 to.pred))
            CL.BT <- apply(res.BT, 1, quantile, c(0.025, 
                                                  0.975))
            colnames(CL.BT) <- to.pred
            if (CL.smooth == "fill") {
              clip(0, 1, 0, 1)
              polygon(x = c(to.pred, rev(to.pred)), y = c(CL.BT[2, 
                                                                ], rev(CL.BT[1, ])), col = rgb(177, 177, 
                                                                                               177, 177, maxColorValue = 255), border = NA)
              if (connect.smooth == T) {
                lines(Sm, lty = lty.smooth, lwd = lwd.smooth, 
                      col = col.smooth)
                lt <- c(lt, lty.smooth)
                lw.d <- c(lw.d, lwd.smooth)
                marks <- c(marks, -1)
              }
              else if (connect.smooth == F) {
                points(Sm, col = col.smooth)
                lt <- c(lt, 0)
                lw.d <- c(lw.d, 1)
                marks <- c(marks, 1)
              }
              do.call("clip", as.list(par()$usr))
              leg <- c(leg, "Actual")
            }
            else {
              clip(0, 1, 0, 1)
              lines(to.pred, CL.BT[1, ], lty = 2, lwd = 1, 
                    col = col.smooth)
              clip(0, 1, 0, 1)
              lines(to.pred, CL.BT[2, ], lty = 2, lwd = 1, 
                    col = col.smooth)
              do.call("clip", as.list(par()$usr))
              leg <- c(leg, "Actual", 
                       "CL flexible")
              lt <- c(lt, 2)
              lw.d <- c(lw.d, 1)
              all.col <- c(all.col, col.smooth)
              marks <- c(marks, -1)
            }
          }
          else {
            Sm.0 <- loess(y ~ p, degree = 2)
            cl.loess <- predict(Sm.0, type = "fitted", 
                                se = T)
            clip(0, 1, 0, 1)
            if (CL.smooth == "fill") {
              polygon(x = c(Sm.0$x, rev(Sm.0$x)), y = c(cl.loess$fit + 
                                                          cl.loess$se.fit * 1.96, rev(cl.loess$fit - 
                                                                                        cl.loess$se.fit * 1.96)), col = rgb(177, 
                                                                                                                            177, 177, 177, maxColorValue = 255), border = NA)
              if (connect.smooth == T) {
                lines(Sm, lty = lty.smooth, lwd = lwd.smooth, 
                      col = col.smooth)
                lt <- c(lt, lty.smooth)
                lw.d <- c(lw.d, lwd.smooth)
                marks <- c(marks, -1)
              }
              else if (connect.smooth == F) {
                points(Sm, col = col.smooth)
                lt <- c(lt, 0)
                lw.d <- c(lw.d, 1)
                marks <- c(marks, 1)
              }
              do.call("clip", as.list(par()$usr))
              leg <- c(leg, "Actual")
            }
            else {
              lines(Sm.0$x, cl.loess$fit + cl.loess$se.fit * 
                      1.96, lty = 2, lwd = 1, col = col.smooth)
              lines(Sm.0$x, cl.loess$fit - cl.loess$se.fit * 
                      1.96, lty = 2, lwd = 1, col = col.smooth)
              do.call("clip", as.list(par()$usr))
              leg <- c(leg, "Actual", 
                       "CL flexible")
              lt <- c(lt, 2)
              lw.d <- c(lw.d, 1)
              all.col <- c(all.col, col.smooth)
              marks <- c(marks, -1)
            }
          }
        }
        else {
          leg <- c(leg, "Actual")
        }
        cal.smooth <- approx(Sm.01, xout = p)$y
        eavg <- mean(abs(p - cal.smooth))
        ECI <- mean((p - cal.smooth)^2) * 100
      }
      if (smooth == "rcs") {
        par(lwd = lwd.smooth, bty = "n", col = col.smooth)
        if (!is.numeric(nr.knots)) {
          stop("Nr.knots must be numeric.")
        }
        if (nr.knots == 5) {
          tryCatch(rcspline.plot(p, y, model = "logistic", 
                                 nk = 5, show = "prob", statloc = "none", add = T, 
                                 showknots = F, xrange = c(min(na.omit(p)), 
                                                           max(na.omit(p))), lty = lty.smooth), error = function(e) {
                                                             warning("The number of knots led to estimation problems, nk will be set to 4.", 
                                                                     immediate. = T)
                                                             tryCatch(rcspline.plot(p, y, model = "logistic", 
                                                                                    nk = 4, show = "prob", statloc = "none", 
                                                                                    add = T, showknots = F, xrange = c(min(na.omit(p)), 
                                                                                                                       max(na.omit(p))), lty = lty.smooth), error = function(e) {
                                                                                                                         warning("Nk 4 also led to estimation problems, nk will be set to 3.", 
                                                                                                                                 immediate. = T)
                                                                                                                         rcspline.plot(p, y, model = "logistic", nk = 3, 
                                                                                                                                       show = "prob", statloc = "none", add = T, 
                                                                                                                                       showknots = F, xrange = c(min(na.omit(p)), 
                                                                                                                                                                 max(na.omit(p))), lty = lty.smooth)
                                                                                                                       })
                                                           })
        }
        else if (nr.knots == 4) {
          tryCatch(rcspline.plot(p, y, model = "logistic", 
                                 nk = 4, show = "prob", statloc = "none", add = T, 
                                 showknots = F, xrange = c(min(na.omit(p)), 
                                                           max(na.omit(p))), lty = lty.smooth), error = function(e) {
                                                             warning("The number of knots led to estimation problems, nk will be set to 3.", 
                                                                     immediate. = T)
                                                             rcspline.plot(p, y, model = "logistic", nk = 3, 
                                                                           show = "prob", statloc = "none", add = T, 
                                                                           showknots = F, xrange = c(min(na.omit(p)), 
                                                                                                     max(na.omit(p))), lty = lty.smooth)
                                                           })
        }
        else if (nr.knots == 3) {
          tryCatch(rcspline.plot(p, y, model = "logistic", 
                                 nk = 3, show = "prob", statloc = "none", add = T, 
                                 showknots = F, xrange = c(min(na.omit(p)), 
                                                           max(na.omit(p))), lty = lty.smooth), error = function(e) {
                                                             stop("Nk = 3 led to estimation problems.")
                                                           })
        }
        else {
          stop(paste("Number of knots = ", nr.knots, sep = "", 
                     ", only 5 >= nk >=3 is allowed."))
        }
        par(lwd = 1, bty = "o", col = "black")
        leg <- c(leg, "Flexible calibration (RCS)", "CL flexible")
        lt <- c(lt, lty.smooth, 2)
        lw.d <- c(lw.d, rep(lwd.smooth, 2))
        all.col <- c(all.col, col.smooth)
        marks <- c(marks, -1, -1)
      }
      if (!missing(m) | !missing(g) | !missing(cuts)) {
        if (!missing(m)) 
          q <- cut2(p, m = m, levels.mean = T, digits = 7)
        else if (!missing(g)) 
          q <- cut2(p, g = g, levels.mean = T, digits = 7)
        else if (!missing(cuts)) 
          q <- cut2(p, cuts = cuts, levels.mean = T, digits = 7)
        means <- as.single(levels(q))
        prop <- tapply(y, q, function(x) mean(x, na.rm = T))
        points(means, prop, pch = 2, cex = 1)
        ng <- tapply(y, q, length)
        og <- tapply(y, q, sum)
        ob <- og/ng
        se.ob <- sqrt(ob * (1 - ob)/ng)
        g <- length(as.single(levels(q)))
        for (i in 1:g) lines(c(means[i], means[i]), c(prop[i], 
                                                      min(1, prop[i] + 1.96 * se.ob[i])), type = "l")
        for (i in 1:g) lines(c(means[i], means[i]), c(prop[i], 
                                                      max(0, prop[i] - 1.96 * se.ob[i])), type = "l")
        if (connect.group) {
          lines(means, prop)
          lt <- c(lt, 1)
          lw.d <- c(lw.d, 1)
        }
        else lt <- c(lt, 0)
        lw.d <- c(lw.d, 0)
        leg <- c(leg, "Grouped observations")
        marks <- c(marks, 2)
      }
    }
    lr <- stats["Model L.R."]
    p.lr <- stats["P"]
    D <- (lr - 1)/n
    L01 <- -2 * sum(y * logit - logb(1 + exp(logit)), na.rm = TRUE)
    U.chisq <- L01 - f$deviance[2]
    p.U <- 1 - pchisq(U.chisq, 2)
    U <- (U.chisq - 2)/n
    Q <- D - U
    Dxy <- stats["Dxy"]
    C <- stats["C"]
    R2 <- stats["R2"]
    B <- sum((p - y)^2)/n
    Bmax <- mean(y) * (1 - mean(y))^2 + (1 - mean(y)) * mean(y)^2
    Bscaled <- 1 - B/Bmax
    stats <- c(Dxy, C, R2, D, lr, p.lr, U, U.chisq, p.U, Q, B, 
               f2$coef[1], f$coef[2], emax, Bscaled)
    names(stats) <- c("Dxy", "C (ROC)", "R2", "D", "D:Chi-sq", 
                      "D:p", "U", "U:Chi-sq", "U:p", "Q", "Brier", "Intercept", 
                      "Slope", "Emax", "Brier scaled")
    if (smooth == "loess") 
      stats <- c(stats, c(Eavg = eavg), c(ECI = ECI))
    if (!missing(cutoff)) {
      arrows(x0 = cutoff, y0 = 0.1, x1 = cutoff, y1 = -0.025, 
             length = 0.15)
    }
    if (pl) {
      if (min(p) > plogis(-7) | max(p) < plogis(7)) {
        lrm.fit.1 <- lrm(y[i.2] ~ qlogis(p[i.2]))
        if (logistic.cal) 
          lines(p[i.2], plogis(lrm.fit.1$linear.predictors), 
                lwd = lwd.log, lty = lty.log, col = col.log)
      }
      else {
        logit <- seq(-7, 7, length = 200)
        prob <- 1/(1 + exp(-logit))
        pred.prob <- f$coef[1] + f$coef[2] * logit
        pred.prob <- 1/(1 + exp(-pred.prob))
        if (logistic.cal) 
          lines(prob, pred.prob, lty = lty.log, lwd = lwd.log, 
                col = col.log)
      }
      lp <- legendloc
      if (!is.logical(lp)) {
        if (!is.list(lp)) 
          lp <- list(x = lp[1], y = lp[2])
        legend(lp, leg, lty = lt, pch = marks, cex = cex.leg, 
               bty = "n", lwd = lw.d, col = all.col, y.intersp = y.intersp)
      }
      if (!is.logical(statloc)) {
        if (dostats[1] == T) {
          stats.2 <- paste("Calibration\n", "...intercept: ", 
                           sprintf(paste("%.", roundstats, "f", sep = ""), 
                                   stats["Intercept"]), " (", sprintf(paste("%.", 
                                                                            roundstats, "f", sep = ""), cl.interc[1]), 
                           " to ", sprintf(paste("%.", roundstats, "f", 
                                                 sep = ""), cl.interc[2]), ")", "\n", "...slope: ", 
                           sprintf(paste("%.", roundstats, "f", sep = ""), 
                                   stats["Slope"]), " (", sprintf(paste("%.", 
                                                                        roundstats, "f", sep = ""), cl.slope[1]), 
                           " to ", sprintf(paste("%.", roundstats, "f", 
                                                 sep = ""), cl.slope[2]), ")", "\n", "Discrimination\n", 
                           "...c-statistic: ", sprintf(paste("%.", roundstats, 
                                                             "f", sep = ""), stats["C (ROC)"]), " (", 
                           sprintf(paste("%.", roundstats, "f", sep = ""), 
                                   cl.auc[2]), " to ", sprintf(paste("%.", roundstats, 
                                                                     "f", sep = ""), cl.auc[3]), ")", sep = "")
          text(statloc[1], statloc[2], stats.2, pos = 4, 
               cex = cex)
        }
        else {
          dostats <- dostats
          leg <- format(names(stats)[dostats])
          leg <- paste(leg, ":", format(stats[dostats], 
                                        digits = roundstats), sep = "")
          if (!is.list(statloc)) 
            statloc <- list(x = statloc[1], y = statloc[2])
          text(statloc, paste(format(names(stats[dostats])), 
                              collapse = "\n"), adj = 0, cex = cex)
          text(statloc$x + (xlim[2] - xlim[1])/3, statloc$y, 
               paste(format(round(stats[dostats], digits = roundstats)), 
                     collapse = "\n"), adj = 1, cex = cex)
        }
      }
      if (is.character(riskdist)) {
        if (riskdist == "calibrated") {
          x <- f$coef[1] + f$coef[2] * log(p/(1 - p))
          x <- 1/(1 + exp(-x))
          x[p == 0] <- 0
          x[p == 1] <- 1
        }
        else x <- p
        bins <- seq(0, min(1, max(xlim)), length = 101)
        x <- x[x >= 0 & x <= 1]
        f0 <- table(cut(x[y == 0], bins))
        f1 <- table(cut(x[y == 1], bins))
        j0 <- f0 > 0
        j1 <- f1 > 0
        bins0 <- (bins[-101])[j0]
        bins1 <- (bins[-101])[j1]
        f0 <- f0[j0]
        f1 <- f1[j1]
        maxf <- max(f0, f1)
        f0 <- (0.1 * f0)/maxf
        f1 <- (0.1 * f1)/maxf
        segments(bins1, line.bins, bins1, length.seg * f1 + 
                   line.bins)
        segments(bins0, line.bins, bins0, length.seg * f0 + 
                   line.bins)
        lines(c(min(bins0, bins1) - 0.01, max(bins0, bins1) + 
                  0.01), c(line.bins, line.bins))
        text(max(bins0, bins1) + dist.label, line.bins + 
               dist.label2, d1lab, cex = cex.d01)
        text(max(bins0, bins1) + dist.label, line.bins - 
               dist.label2, d0lab, cex = cex.d01)
      }
    }
    if (dostats == T) {
      cat(paste("\n\n A ", cl.level * 100, "% confidence interval is given for the calibration intercept, calibration slope and c-statistic. \n\n", 
                sep = ""))
    }
    stats
  }

# mean plots----
# mean abs normal/Mild/Severe 
#frequencies vector (for use later to make the x axis numberic - geom_ribbon requires continuous x axis) 
FREQ.num <- c(226.00, 257.33, 280.62, 297.30, 324.21, 343.49, 363.91, 385.55, 408.48, 432.77, 458.50, 471.94, 500.00, 
              514.65, 545.25, 561.23, 577.68, 594.60, 629.96, 648.42, 667.42, 686.98, 707.11, 727.83, 749.15, 771.11, 
              793.70, 816.96, 840.90, 865.54, 890.90, 917.00, 943.87, 971.53, 1000.00, 1029.30, 1059.46, 1090.51, 1122.46, 
              1155.35, 1189.21, 1224.05, 1259.92, 1296.84, 1334.84, 1373.95, 1414.21, 1455.65, 1498.31, 1542.21, 1587.40, 
              1633.92, 1681.79, 1731.07, 1781.80, 1834.01, 1887.75, 1943.06, 2000.00, 2058.60, 2118.93, 2181.02, 2244.92, 
              2310.71, 2378.41, 2448.11, 2519.84, 2593.68, 2669.68, 2747.91, 2828.43, 2911.31, 2996.61, 3084.42, 3174.80, 
              3267.83, 3363.59, 3462.15, 3563.59, 3668.02, 3775.50, 3886.13, 4000.00, 4117.21, 4237.85, 4362.03, 4489.85, 
              4621.41, 4756.83, 4896.21, 5039.68, 5187.36, 5339.36, 5495.81, 5656.85, 5822.61, 5993.23, 6168.84, 6349.60, 
              6535.66, 6727.17, 6924.29, 7127.19, 7336.03, 7550.99, 7772.26, 8000.00)
FREQ.num <- rep(FREQ.num, each=3)
#make df for abs, mag and pha

data1 = wai.24.df
#data1 = na.omit(data1)
summary(data1$rs)
data1$rs = factor(data1$rs, levels = c("Pass", "Mild", "Severe"))
data1$rs <- revalue(data1$rs, c("Pass" = "Normal, n = 253", 
                                "Mild" = "Mild, n = 69",
                                "Severe" = "Severe, n = 85"))

abs.24 <- data1[,c(3, 8:114)]
colnames(abs.24) <- c("rs", "226.00", "257.33", "280.62", "297.30", "324.21", "343.49", "363.91", "385.55", "408.48", "432.77", "458.50",
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

mag.24 <- wai.24.df[,c(3, 115:221)]
colnames(mag.24) <- c("rs", "226.00", "257.33", "280.62", "297.30", "324.21", "343.49", "363.91", "385.55", "408.48", "432.77", "458.50",
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

pha.24 <- wai.24.df[,c(3, 222:328)]
colnames(pha.24) <- c("rs", "226.00", "257.33", "280.62", "297.30", "324.21", "343.49", "363.91", "385.55", "408.48", "432.77", "458.50",
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

# Group by  category BP
library(dplyr)
abs.24 <- group_by(abs.24, rs)
mag.24 <- group_by(mag.24, rs)
pha.24 <- group_by(pha.24, rs)

# create median and IQR
abs.mean <- summarise_all(abs.24, funs(mean))
mag.mean <- summarise_all(mag.24, funs(mean))
pha.mean <- summarise_all(pha.24, funs(mean))

library(tidyr)
abs.mean <- gather(abs.mean, Frequency, absorbance, 2:108)
mag.mean <- gather(mag.mean, Frequency, magnitude, 2:108)
pha.mean <- gather(pha.mean, Frequency, phase, 2:108)

#I set the theme permanently to black and white with a black border and no gridlines
#need to change if want to add gridlines but can do this on case-by-case basis
#the code for the figures has this but it is not necessary because I have set it permanently
## I had an issue where line size and alb were showing up in the legend because I had those inside the aes() - need to put outside aes: http://stackoverflow.com/questions/14794599/how-the-change-line-width-in-ggplot

library(ggplot2)
#theme_set(theme_bw() + theme(legend.key = element_blank()) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#                                                                   panel.background = element_blank()) )

# mean plots 
abs.mean.plot <- ggplot(abs.mean, aes(x=as.numeric(Frequency), y=absorbance, group=rs, colour=rs)) +
  #theme_bw() +
  scale_colour_manual(values = c("Normal, n = 253" = "#00BA38", "Mild, n = 69" = "#619CFF", "Severe, n = 85" = "#F8766D")) +
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
print(abs.mean.plot)

# mag.mean.plot <- ggplot(mag.mean, aes(x=as.numeric(Frequency), y=magnitude, group=rs, colour=rs)) +
#   geom_line(size=0.8)  +
#   xlab("Frequency, Hz") +
#   ylab(expression(paste(italic("Y"), ", mmho"))) +
#   scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
#   scale_y_continuous(expand=c(0, 0), breaks=c(0, 3, 6, 9, 12, 15), limits=c(0, 15)) +
#   theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1),
#         legend.position=c(0.01,0.99)) +
#   theme(axis.title.y = element_text(vjust = 0.6)) +
#   theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines"))
# print(mag.mean.plot)
# 
# pha.mean.plot <- ggplot(pha.mean, aes(x=as.numeric(Frequency), y=phase, group=rs, colour=rs)) +
#   geom_line(size=0.8)  +
#   xlab("Frequency, Hz") +
#   ylab(expression(paste(italic("Ypha"), ", mmho"))) +
#   scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
#   scale_y_continuous(expand=c(0, 0), breaks=c(-50, 0, 50, 100), limits=c(-50, 100)) +
#   theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1),
#         legend.position=c(0,1)) +
#   theme(axis.title.y = element_text(vjust = 0.6)) +
#   theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines")) +
#   theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1),
#         legend.position=c(0.06,0.2)) 
#   #theme(legend.position="none")
# print(pha.mean.plot)

# Multiplot 
#now put them together (0, 6, 12.24), align "v" is to align the axis of the plots, can alighn horizontal or vertical
# library(cowplot)
# mean.plot <- plot_grid(abs.mean.plot, mag.mean.plot, pha.mean.plot, nrow=3, ncol=1, align = "v", labels = c("A", "B", "C")) 

ggsave("abs.plot.tiff", abs.mean.plot, height=5, width=8, dpi=1200)

## RS test plots

# mean pass both, fail DP, type C/Pass DP, type C/fail DP, fail both-----
#frequencies vector (for use later to make the x axis numberic - geom_ribbon requires continuous x axis) 
FREQ.num <- c(226.00, 257.33, 280.62, 297.30, 324.21, 343.49, 363.91, 385.55, 408.48, 432.77, 458.50, 471.94, 500.00, 514.65, 545.25, 561.23, 577.68, 
              594.60, 629.96, 648.42, 667.42, 686.98, 707.11, 727.83, 749.15, 771.11, 793.70, 816.96, 840.90, 865.54, 890.90, 917.00, 943.87, 971.53, 
              1000.00, 1029.30, 1059.46, 1090.51, 1122.46, 1155.35, 1189.21, 1224.05, 1259.92, 1296.84, 1334.84, 1373.95, 1414.21, 1455.65, 1498.31, 
              1542.21, 1587.40, 1633.92, 1681.79, 1731.07, 1781.80, 1834.01, 1887.75, 1943.06, 2000.00, 2058.60, 2118.93, 2181.02, 2244.92, 2310.71, 
              2378.41, 2448.11, 2519.84, 2593.68, 2669.68, 2747.91, 2828.43, 2911.31, 2996.61, 3084.42, 3174.80, 3267.83, 3363.59, 3462.15, 3563.59, 
              3668.02, 3775.50, 3886.13, 4000.00, 4117.21, 4237.85, 4362.03, 4489.85, 4621.41, 4756.83, 4896.21, 5039.68, 5187.36, 5339.36, 5495.81, 
              5656.85, 5822.61, 5993.23, 6168.84, 6349.60, 6535.66, 6727.17, 6924.29, 7127.19, 7336.03, 7550.99, 7772.26, 8000.00)
FREQ.num <- rep(FREQ.num, each=12)
#make df for abs, mag and pha
library(plyr)
library(dplyr)

data = wai.24.df
summary(data$rs.all.possible)
data$rs.all.possible = factor(data$rs.all.possible, levels = c("Pass both", "Type A and fail DPOAEs", "Type C and pass DPOAEs", "Type C and fail DPOAEs",
                                                               "Type B and pass DPOAEs", "Fail both"))
data$rs.all.possible <- revalue(data$rs.all.possible, c("Pass both"="Pass both, n = 253", 
                                                        "Type A and fail DPOAEs" = "Type A and fail DPOAEs, n = 13",
                                                        "Type C and pass DPOAEs" = "Type C and pass DPOAEs, n = 13", 
                                                        "Type C and fail DPOAEs" = "Type C and fail DPOAEs, n = 9", 
                                                        "Type B and pass DPOAEs" = "Type B and pass DPOAEs, n = 34", 
                                                        "Fail both" = "Fail both, n = 85"))

abs2 <- data[,c(4, 8:114)]
colnames(abs2) <- c("rs", "226.00", "257.33", "280.62", "297.30", "324.21", "343.49", "363.91", "385.55", "408.48", "432.77", "458.50",
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

mag2 <- data[,c(4, 115:221)]
colnames(mag2) <- c("rs","226.00", "257.33", "280.62", "297.30", "324.21", "343.49", "363.91", "385.55", "408.48", "432.77", "458.50",
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

pha2 <- data[,c(4, 222:328)]
colnames(pha2) <- c("rs", "226.00", "257.33", "280.62", "297.30", "324.21", "343.49", "363.91", "385.55", "408.48", "432.77", "458.50",
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

# Group by ref standard category rs
library(dplyr)
abs2 <- group_by(abs2, rs)
mag2 <- group_by(mag2, rs)
pha2 <- group_by(pha2, rs)

# create median and IQR
abs.mean <- summarise_all(abs2, funs(mean))
mag.mean <- summarise_all(mag2, funs(mean))
pha.mean <- summarise_all(pha2, funs(mean))

library(tidyr)
abs.mean <- gather(abs.mean, Frequency, absorbance, 2:108)
mag.mean <- gather(mag.mean, Frequency, magnitude, 2:108)
pha.mean <- gather(pha.mean, Frequency, phase, 2:108)

# Plot 
# PLOT 1 - abs pass and refer median and IQR 
## I had an issue where line size and alpha were showing up in the legend because I had those inside the aes() - need to put outside aes: http://stackoverflow.com/questions/14794599/how-the-change-line-width-in-ggplot
library(ggplot2)

# label with the numbers in each group
abs.rs.plot <- ggplot(abs.mean) +
  #theme_bw() +
  scale_colour_manual(values = c("Pass both, n = 253" = "#00BA38", 
                                 "Type A and fail DPOAEs, n = 13" = "#B79F00", 
                                 "Type C and pass DPOAEs, n = 13" = "#00BFC4", 
                                 "Type C and fail DPOAEs, n = 9" = "#619CFF", 
                                 "Type B and pass DPOAEs, n = 34" = "#F564E3", 
                                 "Fail both, n = 85" = "#F8766D")) +
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
print(abs.rs.plot)

# mag.rs.plot <- ggplot(mag.mean) +
#   geom_line(aes(x = as.numeric(Frequency), y = magnitude, group=rs, linetype = rs, colour = rs))  +
#   xlab("Frequency, Hz") +
#   ylab(expression(paste(italic("mag"), " mmho"))) +
#   scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
#   scale_y_continuous(expand=c(0, 0), breaks=c(0, 5, 15, 20), limits=c(0, 20)) +
#   theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.position=c(0.18,0.76)) +
#   theme(axis.title.y = element_text(vjust = 0.6)) +
#   theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines"))
# print(mag.rs.plot)
# 
# pha.rs.plot <- ggplot(pha.mean) +
#   geom_line(aes(x = as.numeric(Frequency), y = phase, group=rs, linetype = rs, colour = rs))  +
#   xlab("Frequency, Hz") +
#   ylab(expression(paste(italic("pha"), " mmho"))) +
#   scale_x_log10(expand=c(0, 0), breaks=c(226, 500, 1000, 2000, 4000, 8000))  +
#   scale_y_continuous(expand=c(0, 0), breaks=c(-20, -15, -10, -5, 0, 5, 10, 50), limits=c(-20, 100)) +
#   theme(legend.title=element_blank(), legend.text=element_text(size=10), legend.justification=c(0,1), legend.position=c(1,0)) +
#   theme(axis.title.y = element_text(vjust = 0.6)) +
#   theme(plot.margin=unit(c(0.5, 0.8, 0.1, 0.5),"lines"))
#   #theme(legend.position="none")
# print(pha.rs.plot)

# Multiplot
# Now can put all together if want to - Just need to mask the x-axis labels (Frequency) - include "axis.title.x = element_blank() - see below
# with   theme(legend.title=element_blank(),axis.title.x = element_blank(), legend.text=element_text(size=10), legend.justification=c(1,0), legend.position=c(1,0), legend.background = element_rect(fill=alpha("white", 0))) +

#now put them together (0, 6, 12.24), align "v" is to align the axis of the plots, can alighn horizontal or vertical
#library(cowplot)
#RS.plot <- plot_grid(abs.rs.plot, mag.rs.plot, pha.rs.plot, nrow=3, ncol=1, align = "v", labels = c("A", "B", "C"))

ggsave("abs.rs.tiff", abs.rs.plot, height=5, width=8, dpi=1200)

library(cowplot)
abs.plots <- plot_grid(abs.mean.plot, abs.rs.plot, nrow=2, ncol=1, align = "v", labels = c("A", "B"))
ggsave("abs.plots.tiff", abs.plots, height=6, width=8, dpi=1200)

#######
# 95% range for eg plots
abs.24 <- group_by(abs.24, rs)

library(tidyr)
abs.median <- summarise_all(abs.24, funs(median))
abs.05 <- summarise_all(abs.24, funs(quantile(., probs = (0.05))))
abs.95 <- summarise_all(abs.24, funs(quantile(., probs = (0.95))))

##### abs long----
abs.90 <- rbind(abs.median, abs.05, abs.95)
abs.90 <- data.frame(abs.90)
names(abs.90) = c("rs", FREQ.num)
#add a column that labels what it is (otherwise didn't work)
stats.col <- c("median", "median", "median", "five", "five", "five", "ninety5", "ninety5", "ninety5")
abs.90 <- cbind.data.frame(abs.90, stats.col)

#Then reshape data in long form 
abs.90.long <- gather(abs.90, Frequency, absorbance, 2:108)
abs.90.long <- spread(abs.90.long, stats.col, absorbance)
abs.90.long$Frequency <- as.numeric(abs.90.long$Frequency)
abs.90.long$rs = as.character(abs.90.long$rs)
abs.90.long$rs[abs.90.long$rs=="Normal, n = 253"] = "Normal"
abs.90.long$rs[abs.90.long$rs=="Mild, n = 69"] = "Mild"
abs.90.long$rs[abs.90.long$rs=="Severe, n = 85"] = "Severe"
abs.90.long = filter(abs.90.long, rs=="Normal")

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

# removed the xlab and ylab defaults from plot.xmean.ordinaly - so I could set my own----
My.plot.xmean.ordinaly =
  function (x, data, subset, na.action, subn = TRUE, cr = FALSE, 
            topcats = 1, cex.points = 0.75, ...) 
  {
    X <- match.call(expand.dots = FALSE)
    X$subn <- X$cr <- X$topcats <- X$cex.points <- X$... <- NULL
    if (missing(na.action)) 
      X$na.action <- na.keep
    Terms <- if (missing(data)) 
      terms(x)
    else terms(x, data = data)
    X$formula <- Terms
    X[[1]] <- as.name("model.frame")
    X <- eval.parent(X)
    resp <- attr(Terms, "response")
    if (resp == 0) 
      stop("must have a response variable")
    nx <- ncol(X) - 1
    Y <- X[[resp]]
    nam <- as.character(attr(Terms, "variables"))
    nam <- nam[-1]
    dopl <- function(x, y, cr, xname, yname) {
      s <- !is.na(unclass(Y) + x)
      y <- y[s]
      x <- x[s]
      n <- length(x)
      f <- lrm.fit(x, y)
      fy <- f$freq/n
      ns <- length(fy) - 1
      k <- ns + 1
      intcept <- f$coef[1:ns]
      xb <- f$linear.predictors - intcept[1]
      xb <- sapply(intcept, "+", xb)
      P <- 1/(1 + exp(-xb))
      P <- matrix(P, ncol = ns)
      P <- cbind(1, P) - cbind(P, 0)
      xmean.y <- tapply(x, y, mean)
      xp <- x * P/n
      xmean.y.po <- apply(xp, 2, sum)/fy
      yy <- 1:length(fy)
      rr <- c(xmean.y, xmean.y.po)
      if (cr) {
        u <- cr.setup(y)
        s <- u$subs
        yc <- u$y
        xc <- x[s]
        cohort <- u$cohort
        xcohort <- matrix(0, nrow = length(xc), ncol = length(levels(cohort)) - 
                            1)
        xcohort[col(xcohort) == unclass(cohort) - 1] <- 1
        cof <- lrm.fit(cbind(xcohort, xc), yc)$coefficients
        cumprob <- rep(1, n)
        for (j in 1:k) {
          P[, j] <- cumprob * (if (j == k) 
            1
            else plogis(cof[1] + (if (j > 1) 
              cof[j]
              else 0) + cof[k] * x))
          cumprob <- cumprob - P[, j]
        }
        xp <- x * P/n
        xmean.y.cr <- apply(xp, 2, sum)/fy
        rr <- c(rr, xmean.y.cr)
      }
      plot(yy, xmean.y, type = "b", ylim = range(rr), axes = FALSE, 
           ...)
      mgp.axis(1, at = yy, labels = names(fy))
      mgp.axis(2)
      lines(yy, xmean.y.po, lty = 2, ...)
      if (cr) 
        points(yy, xmean.y.cr, pch = "C", cex = cex.points)
      if (subn) 
        title(sub = paste("n=", n, sep = ""), adj = 0)
    }
    for (i in 1:nx) {
      x <- X[[resp + i]]
      if (is.factor(x)) {
        f <- table(x)
        ncat <- length(f)
        if (ncat < 2) {
          warning(paste("predictor", nam[resp + i], "only has one level and is ignored"))
          next
        }
        nc <- min(ncat - 1, topcats)
        cats <- (names(f)[order(-f)])[1:nc]
        for (wcat in cats) {
          xx <- 1 * (x == wcat)
          xname <- paste(nam[resp + i], wcat, sep = "=")
          dopl(xx, Y, cr, xname, nam[resp])
        }
      }
      else dopl(x, Y, cr, nam[resp + i], nam[resp])
    }
    invisible()
  }


