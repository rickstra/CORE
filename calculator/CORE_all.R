PredictCORE_full <- function(data) {
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
