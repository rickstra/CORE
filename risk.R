# Requires package 'rms'. To install:
# install.packages("rms")

PredictCORE <- function(data) {
  
  # Load necessary rms functions
  "%ia%" <- rms::`%ia%`
  rcs <- rms::rcs
  
  # Transformation functions
  cr <- function(x) x ^ (1 / 3)
  risk <- function(lp) 1 - exp(-exp(lp))
  
  # Knot locations for splines
  kn <- list(start_age = c(20.04107, 39.35113, 46.83368, 56.37235, 79.99726), 
             AST = c(0.10, 0.28, 0.33, 0.41, 90.90), 
             ALT = c(0.10, 0.25, 0.35, 0.51, 96.90), 
             GGT = c(0.10, 0.22, 0.31, 0.49, 51.31))
  ikn <- list(start_age = c(20.04107, 46.83368, 79.99726), 
              AST = c(0.10, 0.33, 90.90), 
              ALT = c(0.10, 0.35, 96.90), 
              GGT = c(0.10, 0.31, 51.31))
  
  # Model formula
  CORE_formula <- formula(" ~ sex + rcs(start_age, kn$start_age) +
                        rcs(start_age, ikn$start_age) %ia% sex + 
                        rcs(cr(GGT), cr(kn$GGT)) + 
                        rcs(cr(GGT), cr(ikn$GGT)) %ia% sex + 
                        rcs(cr(GGT), cr(ikn$GGT)) %ia% rcs(start_age, ikn$start_age) +
                        rcs(cr(AST), cr(kn$AST)) + 
                        rcs(cr(AST), cr(ikn$AST)) %ia% sex +
                        rcs(cr(ALT), cr(kn$ALT)) + 
                        rcs(cr(ALT), cr(ikn$ALT)) %ia% sex +
                        rcs(cr(ALT), cr(ikn$ALT)) %ia% rcs(cr(AST), cr(ikn$AST))
                        ") 
  # Model coefficients
  beta <- c(alpha   = -6.516541e+0, sex      = -8.689450e-1, 
            age     =  6.952553e-2, age2     = -6.655955e-2, 
            age3    =  2.984620e-1, age4     = -3.315944e-1, 
            age_sex = -8.003664e-3, age2_sex = -5.951540e-3, 
            GGT     = -3.613434e+0, GGT2     =  1.330111e+3, 
            GGT3    = -5.190295e+3, GGT4     =  4.691223e+3, 
            GGT_sex =  4.054654e-1, GGT2_sex = -3.486140e+0, 
            GGT_age =  1.931010e-2, GGT_age2 = -3.250991e-2, GGT2_age = -4.515743e-2, 
            AST     = -2.159130e+0, AST2     = -3.295898e+1, 
            AST3    =  8.942042e+3, AST4     = -1.630740e+4, 
            AST_sex =  6.370176e-1, AST2_sex =  2.425573e+0, 
            ALT     = -4.434939e+0, ALT2     =  2.157671e+2, 
            ALT3    = -1.820423e+2, ALT4     = -2.838977e+2, 
            ALT_sex =  9.013652e-1, ALT2_sex = -5.125481e+1, 
            ALT_AST =  3.678255e+0, ALT_AST2 =  1.035145e+2, AST2_AST = -9.628953e+1
  )
  # Linear predictor
  X <- model.matrix(CORE_formula, data)
  lp <- X %*% beta
  
  # Risk
  risk(lp)
}

# Check that the prediction function works
test_data <- data.frame(Id = 1:5, 
                        start_age = c(33.33, 64.82, 48.65, 42.95, 49.15), 
                        sex = c(0, 0, 0, 1, 1), 
                        AST = c(0.29, 0.33, 0.22, 0.57, 0.34),
                        ALT = c(0.27, 0.22, 0.33, 0.76, 0.46),
                        GGT = c(0.19, 0.38, 0.67, 0.30, 0.18))
PredictCORE(test_data)

# Read in your data
# data <-

# Generate predictions
p <- PredictCORE(data)

# Calculate AUC
# Requires the timeROC package
# install.packages("timeROC")
auc <- with(data,
         timeROC::timeROC(time, event, p, cause = 1, times = 1:10, 
                          ROC = FALSE)$AUC_2
       )
auc
# Observed:Expected ratio
# Requires cmprsk package
# install.packages("cmprsk")
cinc <- with(data, as.data.frame(cmprsk::cuminc(time, event)[[1]]))
observed <- cinc[which(cinc$time > 10)[1] - 1, ]
expected <- mean(p)

oe <- exp(log(observed$est) - log(expected) + 
      c(-1, 0, 1) * 1.96 * sqrt(var(p) / length(p) / expected ^ 2 + 
                               observed$var / observed$est ^ 2))
oe










