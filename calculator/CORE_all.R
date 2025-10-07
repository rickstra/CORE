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

load("PredictionIntervals.RData")
