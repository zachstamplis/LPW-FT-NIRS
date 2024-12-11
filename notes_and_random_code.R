

##### PCA functions for plotting and other stuff from old code ####

pca.var <- function(mydf, mypca) {
  PC1perc <<- round(mypca$eigenvals[1] / sum(mypca$eigenvals), digits = 4) * 100
  PC2perc <<- round(mypca$eigenvals[2] / sum(mypca$eigenvals), digits = 4) * 100
  varsc0 <- as.data.frame(mypca$loadings[, 1:3])
  objsc0 <- as.data.frame(mypca$calres$scores[, 1:3])
  Spectrascores_all <- as.data.frame(cbind(objsc0$`Comp 1`, objsc0$`Comp 2`, objsc0$`Comp 3`))
  colnames(Spectrascores_all) <- c("PCs1", "PCs2", "PCs3")
  SpecVec <- as.data.frame(cbind(varsc0$`Comp 1`, varsc0$`Comp 2`, varsc0$`Comp 3`))
  head(SpecVec)
  colnames(SpecVec) <- c("PC1", "PC2", "PC3")
  Spectrascores_all <- cbind(Spectrascores_all, mydf[, 1:8])
}


PCA.plot <- function(mydf, mypca, color.by) {
  temp <- pca.var(mydf = mydf, mypca = mypca)
  ggplot() +
    geom_hline(yintercept = 0, color = "gray") +
    geom_vline(xintercept = 0, color = "gray") +
    geom_point(data = temp, aes(x = PCs1, y = PCs2, color = {{ color.by }}), size = 3) +
    labs(x = paste("PC1 (", PC1perc, "%)", sep = ""), y = paste("PC2 (", PC2perc, "%)", sep = "")) +
    scale_color_viridis_c() +
    theme(
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(size = 16),
      axis.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 10.5),
      plot.title = element_text(size = 16, face = "bold"),
      strip.text.x = element_text(size = 14)
    )
}


PCA.plot.bigger <- function(mydf, mypca, color.by) {
  temp <- pca.var(mydf = mydf, mypca = mypca)
  ggplot() +
    geom_hline(yintercept = 0, color = "gray") +
    geom_vline(xintercept = 0, color = "gray") +
    geom_point(data = temp, aes(x = PCs1, y = PCs2, color = {{ color.by }}), size = 5) +
    labs(
      x = paste("PC1 (", PC1perc, "%)", sep = ""),
      y = paste("PC2 (", PC2perc, "%)", sep = "")
    ) +
    scale_color_viridis_c() +
    theme(
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(size = 16),
      axis.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 10.5),
      plot.title = element_text(size = 16, face = "bold"),
      strip.text.x = element_text(size = 14)
    )
}

mypca <- pca(age_only[,21:ncol(age_only)], scale = F, center = T)
plotVariance(mypca)
plot(mypca)
PC1perc <<- round(mypca$eigenvals[1] / sum(mypca$eigenvals), digits = 4) * 100
PC2perc <<- round(mypca$eigenvals[2] / sum(mypca$eigenvals), digits = 4) * 100
varsc0 <- as.data.frame(mypca$loadings[, 1:3])
objsc0 <- as.data.frame(mypca$calres$scores[, 1:3])
Spectrascores_all <- as.data.frame(cbind(objsc0$`Comp 1`, objsc0$`Comp 2`, objsc0$`Comp 3`))
colnames(Spectrascores_all) <- c("PCs1", "PCs2", "PCs3")
SpecVec <- as.data.frame(cbind(varsc0$`Comp 1`, varsc0$`Comp 2`, varsc0$`Comp 3`))
head(SpecVec)
colnames(SpecVec) <- c("PC1", "PC2", "PC3")
Spectrascores_all <- cbind(Spectrascores_all, read_age = age_only[,11])
fig <- ggplot() +
  geom_hline(yintercept = 0, color = "gray") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_point(data = Spectrascores_all, aes(x = PCs1, y = PCs2, color = read_age), size = 3) +
  labs(x = paste("PC1 (", PC1perc, "%)", sep = ""), y = paste("PC2 (", PC2perc, "%)", sep = "")) +
  scale_color_viridis_c() +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(size = 16),
    axis.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 10.5),
    plot.title = element_text(size = 16, face = "bold"),
    strip.text.x = element_text(size = 14)
  )
#library(plotly)
ggplotly(fig)




##### JUNK OUTLIERS AND PCA PLOTTING #####



# potential outliers: 53(age 175), 52(age 135)
# other junk ages maybe (148 (44 in spectrascores), 147)
age_only_original <- age_only
age_only <- age_only %>% dplyr::filter(read_age != 175 & read_age != 135 & read_age != 146 & 
                                         specimen != 65)


mypca <- pca(temp_ages[,21:ncol(temp_ages)], scale = F, center = T)
PC1perc <<- round(mypca$eigenvals[1] / sum(mypca$eigenvals), digits = 4) * 100
PC2perc <<- round(mypca$eigenvals[2] / sum(mypca$eigenvals), digits = 4) * 100
varsc0 <- as.data.frame(mypca$loadings[, 1:3])
objsc0 <- as.data.frame(mypca$calres$scores[, 1:3])
Spectrascores_all <- as.data.frame(cbind(objsc0$`Comp 1`, objsc0$`Comp 2`, objsc0$`Comp 3`))
colnames(Spectrascores_all) <- c("PCs1", "PCs2", "PCs3")
SpecVec <- as.data.frame(cbind(varsc0$`Comp 1`, varsc0$`Comp 2`, varsc0$`Comp 3`))
colnames(SpecVec) <- c("PC1", "PC2", "PC3")
Spectrascores_all <- cbind(Spectrascores_all, read_age = temp_ages[,11])
fig <- ggplot() +
  geom_hline(yintercept = 0, color = "gray") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_point(data = Spectrascores_all, aes(x = PCs1, y = PCs2, color = read_age), size = 3) +
  labs(x = paste("PC1 (", PC1perc, "%)", sep = ""), y = paste("PC2 (", PC2perc, "%)", sep = "")) +
  scale_color_viridis_c() +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(size = 16),
    axis.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 10.5),
    plot.title = element_text(size = 16, face = "bold"),
    strip.text.x = element_text(size = 14)
  )
ggplotly(fig)


AIC(fit.lm, fit.gam)

fit.lm <- lm(data = pctest[-splits[[1]],], read_age ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10)
fit.gam <- gam(data = pctest[-splits[[1]],], read_age ~ s(PC1) + s(PC2) + s(PC3) + s(PC4) + s(PC5) + s(PC6) + s(PC7) + s(PC8) + s(PC9) + s(PC10), method="ML", select = T)

fit.gam$aic
AIC(fit.gam)


bs="cs"

#### BRANCH FIT FROM LAB 10 MODEL SELECTION FOR GETTING AIC COMPARISONS: NOT USABLE WITH PLS!!!!! ####

# 
# Branch.fits <- vector("list")
# Branch.fits$A <- lm(data = pctest[-splits[[1]],], read_age ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10)
# step(Branch.fits$A)
# 
# 
# 
# 
# Branch.fits$B <- lm(logRS ~ S + SST.t2 + SST.t3 + PDO.t2 + PDO.t3, data=Branch, x=T)
# Branch.fits$C <- lm(logRS ~ S + SST.t2 + SST.t3 + regime, data=Branch, x=T)
# Branch.fits$D <- lm(logRS ~ S + SST.t2 + SST.t3, data=Branch, x=T)
# Branch.fits$E <- lm(logRS ~ S + PDO.t2 + PDO.t3, data=Branch, x=T)
# Branch.fits$F <- lm(logRS ~ S + regime, data=Branch, x=T)
# Branch.fits$G <- lm(logRS ~ S, data=Branch, x=T)
# 
# 
# aictab(Branch.fits, names(Branch.fits))          # small-sample AICc (default)
# aictab(Branch.fits, names(Branch.fits), sec=F) 
# 
# 
# out <- matrix(NA, 7, 7)
# dimnames(out) <- list(names(Branch.fits), c("k", "R2", "logL", "AIC", "delAIC", "AICc", "delAICc"))
# 
# for(i in names(Branch.fits)) {
#   mod <- Branch.fits[[i]]
#   logL <- logLik(mod)
#   out[i, "k"] <-  attr(logL, "df")
#   out[i, "R2"] <- summary(mod)$r.sq
#   out[i,"logL"] <- logL
#   out[i,"AIC"] <- AIC(mod)
#   out[i, "AICc"] <- AICc(mod)
# }
# out
# 
# # Subtract minimum AIC (best model) from all values:
# out[,"delAIC"] <- out[,"AIC"] - min(out[,"AIC"])
# out[,"delAICc"] <- out[,"AICc"] - min(out[,"AICc"])
# round(out,2)
# 
# 




####    TRY WITH LENGTH TO SEE HOW IT PERFORMS!~!!>?!?!!?!? ####


# 10 fold CV split - create folds
set.seed(6)
splits <- caret::createFolds(scan_avg_filter$length, k = 10, list = TRUE, returnTrain = FALSE)


# extract PC's for each calibration set, create test sets with ages and spectra
cal <- list()
test <- list()
for(i in 1:10){
  # calibration set and PC's
  pc.mod <- preProcess(scan_avg_filter[-splits[[i]],-c(1:20)], method = "pca", thresh = 0.95, pcaComp = 10)
  pc.cal <- predict(pc.mod, scan_avg_filter[-splits[[i]],-c(1:20)])
  pc.cal <- cbind(pc.cal, scan_avg_filter[-splits[[i]],])
  cal[[i]] <- pc.cal
  # test sets
  pc.test <- predict(pc.mod, scan_avg_filter[splits[[i]],-c(1:20)])
  pc.test <- cbind(pc.test, scan_avg_filter[splits[[i]],])
  test[[i]] <- pc.test
}


# store metrics from each fold and each model type
RMSE.length <- list() 
r2.length <- list() 
AIC.length <- list()
AICc.length <- list()

# determine which PC's to include via step & AIC selection
mod.sel <- list()
for(i in 1:10){
  pctest <- cal[[i]]
  temp <- step(lm(data = pctest[-splits[[i]],], length ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10))
  mod.sel[[i]] <- rownames(summary(temp)$coef)
}
table(unlist(mod.sel))

# PC 1, 2, 6, 9 are most informative???  Could include 3, 4, 7 & 8 possibly. Exclude 10 and 5 for now

lm.mods <- list()
for(i in 1:10){
  calibrate <- cal[[i]]
  testing <- test[[i]]
  mod <- lm(data = calibrate, length ~ PC1 + PC2 + PC6 + PC9)
  RMSE.length$lm.cal[i] <- caret::RMSE(pred = mod$fitted.values, obs = calibrate[,12])
  preds <- predict(mod, newdata = testing)
  RMSE.length$lm.test[i] <-  caret::RMSE(pred = preds, obs = testing[,12])
  r2.length$lm.cal[i] <- summary(mod)$r.squared
  RSS <- sum((testing$length - preds)^2)
  TSS <- sum((testing$length - mean(testing$length))^2)
  r2.length$lm.test[i] <- 1 - (RSS/TSS)
  AIC.length$lm[i] <- AIC(mod)
  AICc.length$lm[i] <- AICc(mod)
  lm.mods[[i]] <- mod
}

RMSE.length$lm.cal <- mean(RMSE.length$lm.cal)
RMSE.length$lm.test <- mean(RMSE.length$lm.test)
r2.length$lm.cal <- mean(r2.length$lm.cal)
r2.length$lm.test <- mean(r2.length$lm.test)



GAM.mods <- list()
# GAM with 10 fold split

for(i in 1:10){
  calibrate <- cal[[i]]
  testing <- test[[i]]
  mod <- gam(data = calibrate, length ~ s(PC1) + s(PC2) + s(PC3) + s(PC4) + s(PC5) + s(PC6) + s(PC7) + s(PC8) + s(PC9) + s(PC10), method="ML", select = T)
  # Extract AIC, AICc, RMSE (cal & test) & r2 (cal & test)
  RMSE.length$GAM.cal[i] <- caret::RMSE(pred = mod$fitted.values, obs = calibrate[,12])
  preds <- predict(mod, newdata = testing)
  RMSE.length$GAM.test[i] <- caret::RMSE(pred = preds, obs = testing[,12])
  r2.length$gam.cal[i] <- summary(mod)$r.sq
  RSS <- sum((testing$length - preds)^2)
  TSS <- sum((testing$length - mean(testing$length))^2) 
  r2.length$gam.test[i] <- 1 - (RSS/TSS)
  AIC.length$gam[i] <- AIC(mod)
  AICc.length$gam[i] <- AICc(mod)
  GAM.mods[[i]] <- mod
}
r2.length$gam.cal <- mean(r2.length$gam.cal)
r2.length$gam.test <- mean(r2.length$gam.test)
RMSE.length$GAM.cal <- mean(RMSE.length$GAM.cal)
RMSE.length$GAM.test <- mean(RMSE.length$GAM.test)

##### PLS COMPARISON ####

pls.mods <- list()
# test pls, no variable selection
for(i in 1:10) {
  calibrate <- cal[[i]]
  testing <- test[[i]]
  mod <- pls(calibrate[,25:ncol(pctest)], calibrate[,15],
             scale = F, center = T,
             info = "Length Prediction Model", cv = 1,
             x.test = testing[,25:ncol(pctest)], y.test = testing[,15])
  RMSE.length$pls.cal[i] <- mod$calres$rmse[[3]]
  RMSE.length$pls.test[i] <- mod$testres$rmse[[3]]
  r2.length$pls.cal[i] <- mod$calres$r2[[3]]
  r2.length$pls.test[i] <- mod$testres$r2[[3]]
  # RSS <- sum((mod$calres$y.ref - mod$calres$y.pred[,3,])^2)
  # n <- length(mod$calres$y.ref)
  # AIC.length$pls[i] <- n * log(RSS/n) + (2 * mod$ncomp.selected)
  pls.mods[[i]] <- mod
}
RMSE.length$pls.cal <- mean(RMSE.length$pls.cal)
RMSE.length$pls.test <- mean(RMSE.length$pls.test)
r2.length$pls.cal <- mean(r2.length$pls.cal)
r2.length$pls.test <- mean(r2.length$pls.test)


mod.summary <- data.frame(model = c("lm_cal", "lm_test", "gam_cal", "gam_test", "pls_cal", "pls_test"),
                          r2 = 1:6,
                          RMSE = 1:6)
mod.summary$r2 <- unlist(r2.length)
mod.summary$RMSE <- unlist(RMSE.length)
# AIC.length$lm <- mean(AIC.length$lm)
# AIC.length$gam <- mean(AIC.length$gam)
# AIC.length$pls <- mean(AIC.length$pls)
AIC.summary <- data.frame(model = c(rep("lm",10),rep("gam",10)),
                          AIC = 1:20,
                          AICc = 1:20)
# AIC.length <- AIC.length[-3]
AIC.summary$AIC <- unlist(AIC.length)
AIC.summary$AICc <- unlist(AICc.length)






pls.mods <- list()
# test pls, no variable selection
for(i in 1:10) {
  calibrate <- cal[[i]]
  testing <- test[[i]]
  mod <- pls(calibrate[,25:ncol(pctest)], calibrate[,12],
             scale = F, center = T,
             info = "Length Prediction Model", cv = 1,
             x.test = testing[,31:ncol(pctest)], y.test = testing[,12])
  RMSE.length$pls.cal[i] <- mod$calres$rmse[[3]]
  RMSE.length$pls.test[i] <- mod$testres$rmse[[3]]
  r2.length$pls.cal[i] <- mod$calres$r2[[3]]
  r2.length$pls.test[i] <- mod$testres$r2[[3]]
  # RSS <- sum((mod$calres$y.ref - mod$calres$y.pred[,3,])^2)
  # n <- length(mod$calres$y.ref)
  # AIC.length$pls[i] <- n * log(RSS/n) + (2 * mod$ncomp.selected)
  pls.mods[[i]] <- mod
}




#### OLD JUNK FROM RMD ####


# determine which PC's to include via step & AIC selection
mod.sel <- list()
for(i in 1:10){
  pctest <- pcs[[i]]
  temp <- step(lm(data = pctest[-splits[[i]],], read_age ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10))
  mod.sel[[i]] <- rownames(summary(temp)$coef)
}
table(unlist(mod.sel))

# 1, 3, 4, 5 & 7 are most common, will start with those

lm.mods <- list()
for(i in 1:10){
  pctest <- pcs[[i]]
  mod <- lm(data = pctest, read_age ~ PC1 + PC3 + PC4 + PC5 + PC7)
  RMSE.age$lm.cal[i] <- caret::RMSE(mod$fitted.values,pctest[-splits[[i]],21])
  RMSE.age$lm.test[i] <-  caret::RMSE(preds,pctest[splits[[i]],21])
  r2.age$lm.cal[i] <- summary(mod)$r.squared
  RSS <- sum((pctest[splits[[i]],]$read_age - preds)^2)
  TSS <- sum((pctest[splits[[i]],]$read_age - mean(pctest[splits[[i]],]$read_age))^2)
  r2.age$lm.test[i] <- 1 - (RSS/TSS)
  AIC.age$lm[i] <- AIC(mod)
  AICc.age$lm[i] <- AICc(mod)
  lm.mods[[i]] <- mod
}
RMSE.age$lm.cal <- mean(RMSE.age$lm.cal)
RMSE.age$lm.test <- mean(RMSE.age$lm.test)
r2.age$lm.cal <- mean(r2.age$lm.cal)
r2.age$lm.test <- mean(r2.age$lm.test)

GAM.mods <- list()
# GAM with 10 fold split
for(i in 1:10){
  mod <- gam(data = pctest[-splits[[i]],], read_age ~ s(PC1) + s(PC2) + s(PC3) + s(PC4) + s(PC5) + s(PC6) + s(PC7) + s(PC8) + s(PC9) + s(PC10), method="ML", select = T)
  # Extract AIC, AICc, RMSE (cal & test) & r2 (cal & test)
  RMSE.age$GAM.cal[i] <- caret::RMSE(mod$fitted.values,pctest[-splits[[i]],21])
  preds <- predict(mod, newdata = pctest[splits[[i]],])
  RMSE.age$GAM.test[i] <- caret::RMSE(pctest[splits[[i]],]$read_age, preds)
  r2.age$gam.cal[i] <- summary(mod)$r.sq
  RSS <- sum((pctest[splits[[i]],]$read_age - preds)^2)
  TSS <- sum((pctest[splits[[i]],]$read_age - mean(pctest[splits[[i]],]$read_age))^2) 
  r2.age$gam.test[i] <- 1 - (RSS/TSS)
  AIC.age$gam[i] <- AIC(mod)
  AICc.age$gam[i] <- MuMIn::AICc(mod)
  GAM.mods[[i]] <- mod
}
r2.age$gam.cal <- mean(r2.age$gam.cal)
r2.age$gam.test <- mean(r2.age$gam.test)
RMSE.age$GAM.cal <- mean(RMSE.age$GAM.cal)
RMSE.age$GAM.test <- mean(RMSE.age$GAM.test)

##### PLS COMPARISON

pls.mods <- list()
# test pls, no variable selection
for(i in 1:10) {
  mod <- pls(pctest[-splits[[i]],c(31:ncol(pctest))], pctest[-splits[[i]],21],
             scale = F, center = T,
             info = "Age Prediction Model", cv = 1,
             x.test = pctest[splits[[i]],c(31:ncol(pctest))], y.test = pctest[splits[[i]],21])
  RMSE.age$pls.cal[i] <- mod$calres$rmse[[3]]
  RMSE.age$pls.test[i] <- mod$testres$rmse[[3]]
  r2.age$pls.cal[i] <- mod$calres$r2[[3]]
  r2.age$pls.test[i] <- mod$testres$r2[[3]]
  # RSS <- sum((mod$calres$y.ref - mod$calres$y.pred[,3,])^2)
  # n <- length(mod$calres$y.ref)
  # AIC.age$pls[i] <- n * log(RSS/n) + (2 * mod$ncomp.selected)
  pls.mods[[i]] <- mod
}
RMSE.age$pls.cal <- mean(RMSE.age$pls.cal)
RMSE.age$pls.test <- mean(RMSE.age$pls.test)
r2.age$pls.cal <- mean(r2.age$pls.cal)
r2.age$pls.test <- mean(r2.age$pls.test)


mod.summary <- data.frame(model = c("lm_cal", "lm_test", "gam_cal", "gam_test", "pls_cal", "pls_test"),
                          r2 = 1:6,
                          RMSE = 1:6)
mod.summary$r2 <- unlist(r2.age)
mod.summary$RMSE <- unlist(RMSE.age)
# AIC.age$lm <- mean(AIC.age$lm)
# AIC.age$gam <- mean(AIC.age$gam)
# AIC.age$pls <- mean(AIC.age$pls)
AIC.summary <- data.frame(model = c(rep("lm",10),rep("gam",10)),
                          AIC = 1:20,
                          AICc = 1:20)
# AIC.age <- AIC.age[-3]
AIC.summary$AIC <- unlist(AIC.age)
AIC.summary$AICc <- unlist(AICc.age)



# library(mdatools)
# mod <- plsr(read_age ~ .,
#             ncomp = 3,
#             data = test.df,
#             subset = test.sub,
#             scale = F, center = T)
# summary(mod)        
#               
#               , pctest[-splits[[1]],21],
#   scale = F, center = T,
#   info = "Age Prediction Model", cv = 1,
#   x.test = pctest[splits[[1]],c(31:ncol(pctest))], y.test = pctest[splits[[1]],21])
# 



b <- getViz(GAM.mods[[1]], nsim = 50)

gridPrint(check1D(b, "PC1") + l_gridCheck1D(gridFun = sd, showReps = TRUE), 
          check1D(b, "PC2") + l_gridCheck1D(gridFun = sd, showReps = TRUE), 
          check1D(b, "PC3") + l_gridCheck1D(gridFun = sd, showReps = TRUE),
          check1D(b, "PC4") + l_gridCheck1D(gridFun = sd, showReps = TRUE),ncol = 2)


(p <- predict_response(GAM.mods[[1]], terms=c("PC1")))
p %>% plot()


par(mfrow=c(2,2))
gam.check(GAM.mods[[5]])
par(mfrow=c(1,1))








ggplot() +
  geom_point(data = test[[5]], aes(x = PC1, y = read_age), col = "red") + 
 # geom_smooth(method = "lm", data = test[[5]], aes(x = PC1, y = read_age), col = "red") + 
  geom_point(data = cal[[5]], aes(x = PC1, y = read_age), col = "black") +
  geom_smooth(data = cal[[5]], aes(x = PC1, y = read_age), method = "gam") +

ggplot() +
  geom_point(data = test[[5]], aes(x = PC2, y = read_age), col = "red") + 
  #geom_smooth(method = "lm", data = test[[5]], aes(x = PC2, y = read_age), col = "red") + 
  geom_point(data = cal[[5]], aes(x = PC2, y = read_age), col = "black") + 
  geom_smooth(data = cal[[5]], aes(x = PC2, y = read_age), method = "gam") +

ggplot() +
  geom_point(data = test[[5]], aes(x = PC3, y = read_age), col = "red") + 
  #geom_smooth(method = "lm", data = test[[5]], aes(x = PC3, y = read_age), col = "red") + 
  geom_point(data = cal[[5]], aes(x = PC3, y = read_age), col = "black") + 
  geom_smooth(data = cal[[5]], aes(x = PC3, y = read_age), method = "gam") +

ggplot() +
  geom_point(data = test[[5]], aes(x = PC4, y = read_age), col = "red") + 
#  geom_smooth(method = "lm", data = test[[5]], aes(x = PC4, y = read_age), col = "red") + 
  geom_point(data = cal[[5]], aes(x = PC4, y = read_age), col = "black")+ 
  geom_smooth(data = cal[[5]], aes(x = PC4, y = read_age), method = "gam")

