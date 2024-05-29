PredictCORE_full <- function(data) {
  
  # Load necessary rms functions
  "%ia%" <- rms::`%ia%`
  rcs <- rms::rcs
  
  # Transformation functions
  cr <- function(x) x ^ (1 / 3)
  risk <- function(lp) 1 - exp(-exp(lp))
  
  # Knot locations for splines
  kn <- list(age = c(29.01, 39.19, 46.61, 56.13, 74.77), 
             AST = c(0.18, 0.28, 0.33, 0.41, 0.79), 
             ALT = c(0.12, 0.25, 0.35, 0.51, 1.33), 
             GGT = c(0.12, 0.22, 0.31, 0.48, 1.72))
  ikn <- list(age = c(32.94, 46.61, 64.45), 
              AST = c(0.23, 0.33, 0.52), 
              ALT = c(0.18, 0.35, 0.77), 
              GGT = c(0.17, 0.31, 0.82))
  
  # Model formula
  CORE_formula <- formula(" ~ sex + rcs(age, kn$age) +
                        rcs(age, ikn$age) %ia% sex + 
                        rcs(cr(GGT), cr(kn$GGT)) + 
                        rcs(cr(GGT), cr(ikn$GGT)) %ia% sex + 
                        rcs(cr(GGT), cr(ikn$GGT)) %ia% rcs(age, ikn$age) +
                        rcs(cr(AST), cr(kn$AST)) + 
                        rcs(cr(AST), cr(ikn$AST)) %ia% sex +
                        rcs(cr(ALT), cr(kn$ALT)) + 
                        rcs(cr(ALT), cr(ikn$ALT)) %ia% sex +
                        rcs(cr(ALT), cr(ikn$ALT)) %ia% rcs(cr(AST), cr(ikn$AST))
                        ") 
  # Model coefficients
  beta <- c(alpha   = -1.523234e+1, sex      = -3.185493e+0, 
            age     =  1.294303e-1, age2     = -1.206007e-1, 
            age3    =  3.624573e-1, age4     = -4.105956e-1, 
            age_sex = -1.218539e-2, age2_sex = -9.738338e-3, 
            GGT     =  1.807689e+0, GGT2     =  5.228144e+1, 
            GGT3    = -1.475804e+2, GGT4     =  9.069085e+1,
            GGT_sex =  1.745903e+0, GGT2_sex = -1.984829e+0,
            GGT_age = -6.285634e-2, GGT_age2 = -8.238631e-3, GGT2_age = 6.330310e-2, 
            AST     =  5.965814e+0, AST2     = -3.325477e+1, 
            AST3    =  1.212408e+2, AST4     = -1.351026e+2, 
            AST_sex =  2.837304e+0, AST2_sex = -2.204382e+0, 
            ALT     = -5.689420e-2, ALT2     =  5.582491e+0, 
            ALT3    =  1.400243e+2, ALT4     = -2.523082e+2, 
            ALT_sex =  1.541426e+0, ALT2_sex = -2.109573e+0, 
            ALT_AST = -2.493454e+0, ALT_AST2 =  2.217570e+1, AST2_AST = -2.269961e+1
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

# read in your data
# data <-
data <- test_data

# Generate predictions
p <- PredictCORE_full(data)
