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
  
  
  kn <- list(age = c(30.65, 39.27, 46.68, 56.16, 70.52), 
             AST = c(0.21, 0.30, 0.37, 0.63, 1.81), 
             ALT = c(0.15, 0.28, 0.43, 1.01, 2.66), 
             GGT = c(0.14, 0.25, 0.39, 1.18, 5.0))
  
  # Model formula
  CORE_formula <- formula(" ~ sex + rcs(age, kn$age) +
                        rcs(age, kn$age) %ia% sex + 
                        rcs(h(GGT), h(kn$GGT)) + 
                        rcs(h(AST), h(kn$AST)) + 
                        rcs(h(ALT), h(kn$ALT)) + 
                        rcs(h(GGT), h(kn$GGT)) %ia% sex + 
                        rcs(h(AST), h(kn$AST)) %ia% sex +
                        rcs(h(ALT), h(kn$ALT)) %ia% sex +
                        rcs(h(GGT), h(kn$GGT)) %ia% rcs(age, kn$age) +
                        rcs(h(GGT), h(kn$GGT)) %ia% rcs(h(AST), h(kn$AST))+
                        rcs(h(GGT), h(kn$GGT)) %ia% rcs(h(ALT), h(kn$ALT))+
                        rcs(h(AST), h(kn$AST)) %ia% rcs(h(ALT), h(kn$ALT))
                        ") 
  # Coefficients
  beta <- c(alpha    = -13.00009578, sex      =   0.60980873,
            age      =   0.11304760, age2     =  -0.26559739, 
            age3     =   0.53007135, age4     =  -0.23699264, 
            age_sex  =  -0.04002686, age2_sex =   0.23812831, 
            age3_sex =  -0.37176718, age4_sex =  -0.14079736, 
            GGT      =  -0.96652829, GGT2     =  26.11194089, 
            GGT3     = -79.41819012, GGT4     =  62.58544918,
            AST      =  -0.62542781, AST2     =   4.20000076, 
            AST3     =  16.03675909, AST4     = -36.41540273, 
            ALT      =  -0.52588932, ALT2     =   1.30767217, 
            ALT3     =  -4.81015945, ALT4     =   1.95395416, 
            GGT_sex  =  -0.26228717, GGT2_sex =   0.72371978,
            GGT3_sex =   1.33605173, GGT4_sex =  -3.76002473,
            AST_sex  =  -0.24704669, AST2_sex =  -5.62259586, 
            AST3_sex =  26.06519108, AST4_sex = -25.30405136, 
            ALT_sex  =  -0.03140536, ALT2_sex =  -1.03656480, 
            ALT3_sex =   5.35217631, ALT4_sex =  -5.27551838, 
            GGT_age  =   0.01526520, GGT_age2 =  -0.07484819, 
            GGT_age3 =   0.19591789, GGT_age4 =  -0.17511797, 
            GGT2_age =  -0.23356007, GGT3_age =   0.81073184, 
            GGT4_age =  -0.71781504, GGT_AST  =  -0.86358788, 
            GGT_AST2 =   2.61912495, GGT_AST3 =   4.39796651, 
            GGT_AST4 = -13.08487962, GGT2_AST =   7.73735030,
            GGT3_AST = -21.15764757, GGT4_AST =  14.02740518, 
            GGT_ALT  =   0.01375014, GGT_ALT2 =  -0.82961745, 
            GGT_ALT3 =  -3.16116607, GGT_ALT4 =   7.65893291, 
            GGT2_ALT =  -1.72353722, GGT3_ALT =   5.50934118, 
            GGT4_ALT =  -3.65492090, AST_ALT  =  -0.03154256, 
            AST_ALT2 =   0.34739590, AST_ALT3 =   0.26474348, 
            AST_ALT4 =  -4.60623685, AST2_ALT =  -6.32755773, 
            AST3_ALT =  40.63303767, AST4_ALT = -43.48380371 
  )
  # Linear predictor
  X <- model.matrix.lm(CORE_formula, data, na.action = "na.pass")
  lp <- X %*% beta
  
  # Risk
  c(risk(lp))
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

oe <- c(observed$est, expected, 
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
  scale_y_continuous("Net Benefit", limits = c(-0.0001, nb$all[1]+0.0001))


# All results collected #-------------------------------------------------------

# Prints
auc
oe
as.data.frame(cdf)
nb

#Plots
cal_plot
cal_plot2
nb_plot



