# Requires package 'rms'. To install:
# install.packages("rms")

PredictCORE <- function(data) {
  # Load necessary rms functions
  "%ia%" <- rms::`%ia%`
  rcs <- rms::rcs
  
  # Transformation functions
  h <- log
  risk <- plogis
  
  # 
  if (is.null(data$age)) {
    data$age <- data$start_age
  }
  
  # Knot locations for splines
  kn <- list(age = c(30.56, 39.16, 46.64, 56.18, 74.61), 
             AST = c(0.21, 0.28, 0.33, 0.41, 0.79), 
             ALT = c(0.15, 0.24, 0.35, 0.51, 1.33), 
             GGT = c(0.14, 0.22, 0.31, 0.48, 1.72))
  ikn <- list(age = c(32.90, 46.64, 64.38), 
              AST = c(0.23, 0.33, 0.52), 
              ALT = c(0.18, 0.35, 0.77), 
              GGT = c(0.17, 0.31, 0.81))
  
  # Model formula
  CORE_formula <- formula(" ~ sex + rcs(age, kn$age) +
                        rcs(age, ikn$age) %ia% sex + 
                        rcs(h(GGT), h(kn$GGT)) + 
                        rcs(h(AST), h(kn$AST)) + 
                        rcs(h(ALT), h(kn$ALT)) + 
                        rcs(h(GGT), h(ikn$GGT)) %ia% sex + 
                        rcs(h(AST), h(ikn$AST)) %ia% sex +
                        rcs(h(ALT), h(ikn$ALT)) %ia% sex +
                        rcs(h(GGT), h(ikn$GGT)) %ia% rcs(age, ikn$age) +
                        rcs(h(AST), h(ikn$AST)) %ia% rcs(age, ikn$age) +
                        rcs(h(ALT), h(ikn$ALT)) %ia% rcs(age, ikn$age) +
                        rcs(h(GGT), h(ikn$GGT)) %ia% rcs(h(AST), h(ikn$AST))+
                        rcs(h(GGT), h(ikn$GGT)) %ia% rcs(h(ALT), h(ikn$ALT))+
                        rcs(h(AST), h(ikn$AST)) %ia% rcs(h(ALT), h(ikn$ALT))
                        ") 
  # Model coefficients
  beta <- c(alpha    = -9.254482879, sex      =  3.667106987, 
            age      =  0.057032410, age2     = -0.204268545, 
            age3     =  0.406601401, age4     = -0.326945974, 
            age_sex  = -0.029801996, age2_sex =  0.013586036, 
            GGT      =  1.630793452, GGT2     =  16.71805261, 
            GGT3     = -54.37998654, GGT4     =  49.01438678,
            AST      =  5.511944025, AST2     = -8.585417058, 
            AST3     =  49.33950323, AST4     = -65.23172726, 
            ALT      = -2.317930678, ALT2     = -1.523886029, 
            ALT3     =  1.805714672, ALT4     =  4.053019631, 
            GGT_sex  =  0.619220765, GGT2_sex = -0.745079371,
            AST_sex  =  0.577803458, AST2_sex = -0.266060892, 
            ALT_sex  =  0.351862597, ALT2_sex = -0.546217603, 
            GGT_age  = -0.042883894, GGT_age2 =  0.016134286, 
            GGT2_age =  0.017862019, AST_age  = -0.008686066, 
            AST_age2 = -0.042139306, AST2_age =  0.032095785,
            ALT_age  =  0.019174037, ALT_age2 =  0.032095785, 
            ALT2_age = -0.016519747, GGT_AST  =  2.066139042, 
            GGT_AST2 =  0.571482852, GGT2_AST = -2.620549353,
            GGT_ALT  = -1.408748611, GGT_ALT2 = -0.671668287, 
            GGT2_ALT =  1.733937890, AST_ALT  =  0.943178392, 
            AST_ALT2 = -0.797298460, AST2_ALT = -0.028686747
  )
  # Linear predictor
  X <- model.matrix(CORE_formula, data)
  lp <- X %*% beta
  
  # Risk
  risk(lp)
}

test_data <- data.frame(Id = 1:5, 
                        age = c(33.33, 64.82, 48.65, 42.95, 49.15), 
                        sex = c(0, 0, 0, 1, 1), 
                        AST = c(0.29, 0.33, 0.22, 0.57, 50),
                        ALT = c(0.27, 0.22, 0.33, 10, 0.46),
                        GGT = c(0.19, 0.38, 50, 0.30, 0.18))

# read in your data here (using e.g. read.table, load, haven::read_dta, ...)
# data <-

# Generate predictions
p <- PredictCORE(data)
data$p <- p

# Calculate AUC #---------------------------------------------------------------

# Requires the timeROC package
# install.packages("timeROC")
auc <- with(data,
  timeROC::timeROC(time, event, p, cause = 1, times = 1:10, ROC = FALSE)$AUC_2
)
auc

# Observed:Expected ratio #-----------------------------------------------------
# Requires cmprsk package
# install.packages("cmprsk")
cinc <- with(data, as.data.frame(cmprsk::cuminc(time, event)[[1]]))
observed <- cinc[which(cinc$time > 10)[1] - 1, ]
expected <- mean(data$p)

oe <- c(observed, expected, 
        exp(log(observed$est) - log(expected) +
            c(0, -1, 1) * 1.96 * sqrt(var(p) / length(p) / expected ^ 2 +
                                        observed$var / observed$est ^ 2)))
names(oe) <- c("Observed", "Expected", "O:E ratio", "2.5%", "97.5%")
oe

# Calibration curves #----------------------------------------------------------
# Prepare data for calibration curves

# install.packages("tidyverse")
library(tidyverse)
{
  cal_df <- select(data, time, event, p) %>% 
    mutate(event = ifelse(time > 10, 0, event), 
           time = pmin(time, 10), 
           p = pmin(p, 1)
    ) %>% arrange(p)
  
  cal_df$cumev <- cumsum(cal_df$event==1)
  m <- pmin(pmax(floor(max(cal_df$cumev) / 15), 10), 50)
  cal_df$set <- cal_df$cumev %/% m
  
  cdf <- cal_df %>% filter(!is.na(p)) %>% group_by(set) %>% 
                summarise(n = length(p),
                          ev = sum(event == 1),
                          mean_pred = mean(p),
                          lower_pred = min(p), 
                          upper_pred = max(p))
  cinc <- with(cal_df, cmprsk::cuminc(time, event, set))
  cinc$Tests <- NULL
  
  cinc <- sapply(1:(length(cinc)/2), function(i) {
    d <- as.data.frame(cinc[[i]])
    d <- unlist(d[nrow(d), ])
  }) %>% t %>% as.data.frame
  
  cdf$obs <- cinc$est
  cdf$lower <- pmax(cinc$est - 1.96 * sqrt(cinc$var), 0)
  cdf$upper <- pmin(cinc$est + 1.96 * sqrt(cinc$var), 1)
}

# Plot
cal_plot <- ggplot(cdf, aes(mean_pred, obs)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = lower, ymax = upper, width = 0)) + 
  geom_segment(aes(x = lower_pred, xend = upper_pred, y = lower, yend = lower)) + 
  geom_segment(aes(x = lower_pred, xend = upper_pred, y = upper, yend = upper)) + 
  # geom_smooth() +
  xlim(0, 1) + ylim(0, 1) + 
  geom_abline(intercept = 0, slope = 1, linetype = 3)

# Zoom in on p = 0-0.2 
p_max <- 0.2 
cal_plot2 <- ggplot(cdf, aes(mean_pred, obs)) + geom_point() + 
  geom_errorbar(aes(ymin = lower, ymax = pmin(upper, p_max), width = 0)) + 
  geom_segment(aes(x = lower_pred, xend = pmin(upper_pred, p_max), y = lower, yend = pmin(lower, p_max))) + 
  geom_segment(aes(x = lower_pred, xend = pmin(upper_pred, p_max), y = upper, yend = pmin(upper, p_max))) + 
  # geom_smooth() +
  xlim(0, p_max) + ylim(0, p_max) + 
  geom_abline(intercept = 0, slope = 1, linetype = 3) 

# Net Benefit #-----------------------------------------------------------------
NetBenefit <- function(risk, time, status, event = 1, t_pred = 10, p_max = 1) {
  d <- data.frame(t = pmin(time, t_pred), 
                  s = ifelse(time > t_pred, 0, status), 
                  r = risk)
  N <- nrow(d)
  ev <- paste(1, event)
  
  compCI <- with(d, as.data.frame(cmprsk::cuminc(t, s)[[ev]]))
  compCI <- compCI[nrow(compCI), ]
  compCI$tp <- compCI$est * N
  compCI$fp <- (1 - compCI$est) * N
  
  p_max <- min(max(d$r[which(d$s == event)]), p_max)
  p_t <- c(seq(0, 0.01, by = 0.002), 
           seq(0.015, 0.1, by = 0.005), 
           seq(0.11, p_max, by = 0.01))

  w <- p_t / (1 - p_t)
  
  all_dc <- (compCI$tp - w * compCI$fp) / N
  all_dc
  model_dc <- sapply(1:length(p_t), function(i) {
    p <- p_t[i]
    ix <- which(d$r >= p)
    ci <- with(d[ix, ], as.data.frame(cmprsk::cuminc(t, s)[[ev]]))
    ci <- ci[nrow(ci), 2]
    m <- length(ix)
    tp <- ci * m
    fp <- (1 - ci) * m
    nb <- (tp - w[i] * fp) / N
    #c(p_t = p, ci = ci, tp = tp, fp = fp, nb = nb)
    nb
  }) 
  data.frame(p_t = p_t, all = all_dc, model = model_dc)
}

nb <- NetBenefit(data$p, data$time, data$event, p_max = 0.2)

nb_long <- pivot_longer(nb, c("all", "model"))
nb_long <- rbind(nb_long, data.frame(p_t = nb$all[1], name = "all", value = 0))

nb_plot <- ggplot(nb_long, aes(p_t * 100, value, color = name)) + 
  geom_line() + geom_hline(yintercept = 0, linetype = 3) + 
  scale_color_discrete("Strategy", labels = c("Treat All", "Use CORE")) + 
  theme(legend.position = c(0.7, 0.7)) + 
  scale_x_continuous("Risk Threshold (%)", breaks = c(2 * (0:10)), 
                     minor_breaks = 2 * (0:10) + 1) + 
  scale_y_continuous("Net Benefit", limits = c(-0.0001, 0.003))


# All results collected #-------------------------------------------------------

# Prints
auc
oe
cdf
nb

#Plots
cal_plot
cal_plot2
nb_plot



