test_perc_fat <- function() {
    cat("Informe o número de pregas.\n")
    NS <- scan(n = 1, what = numeric())
    cat("Informe o sexo (M/F).\n")
    S <- scan(n = 1, what = character())
    cat("Informe a idade.\n")
    Age <- scan(n = 1, what = numeric())
    cat("Informe o peso.\n")
    Weight <- scan(n = 1, what = numeric())
    cat("Informe o Tríceps (mm).\n")
    Triceps <- scan(n = 1, what = numeric())
    cat("Informe o Peito (mm).\n")
    Pectoral <- scan(n = 1, what = numeric())
    cat("Informe o Sub-Axilar (mm).\n")
    Midaxilla <- scan(n = 1, what = numeric())
    cat("Informe o Subescapular (mm).\n")
    Subscapula <- scan(n = 1, what = numeric())
    cat("Informe a Abdominal (mm).\n")
    Abdomen <- scan(n = 1, what = numeric())
    cat("Informe a Supra-ilíaca (mm).\n")
    Suprailiac <- scan(n = 1, what = numeric())
    cat("Informe a Coxa (mm).\n")
    Quadriceps <- scan(n = 1, what = numeric())
    
    SumSeven <- Triceps * 1 + Pectoral * 1 + Midaxilla * 1 + Subscapula * 1 + 
        Abdomen * 1 + Suprailiac * 1 + Quadriceps * 1
    SumThreeMale <- Pectoral * 1 + Abdomen * 1 + Quadriceps * 1
    SumThreeFemale <- Triceps * 1 + Suprailiac * 1 + Quadriceps * 1
    
    if (S == 'M' & NS == 7) {
        DC <- 1.112 - 0.00043499 * SumSeven + 0.00000055 * SumSeven^2 - 0.00028826 * Age
    } else if (S == 'F' & NS == 7) {
        DC <- 1.097 - 0.00046971 * SumSeven + 0.00000056 * SumSeven^2 - 0.00012828 * Age
    } else if (S == 'M' & NS == 3) {
        DC <- 1.10938 - 0.0008267 * SumThreeMale + 0.0000016 * SumThreeMale^2 - 0.0002574 * Age
    } else if (S == 'F' & NS == 3) {
        DC <- 1.0994921 - 0.0009929 * SumThreeFemale + 0.0000023 * SumThreeFemale^2 - 0.0001392 * Age
    }
    PercentFat <- (4.95 / DC - 4.5) * 100
    FatWeight <- Weight * PercentFat / 100
    LeanWeight <- Weight - FatWeight
    
    if (S == 'M') {
        PopulationAverage <- 13.815 + 0.13 * Age
    } else if (S == 'F') {
        PopulationAverage <- 21.55 + 0.1 * Age
    }
    
    if (S == 'M') {
        StandDev <- 6
    } else if (S == 'F' & PercentFat <= PopulationAverage) {
        StandDev <- 8
    } else if (S == 'F' & PercentFat > PopulationAverage) {
        StandDev <- 7
    }
    
    Zscore <-  (PopulationAverage - PercentFat) / StandDev
    PercRegress <- 0.49402 + 0.44379 * abs(Zscore) - 0.082675 * abs(Zscore)^2 - 
        0.021233 * abs(Zscore)^3 + 0.0060829 * abs(Zscore)^4

    if (Zscore > 0) {
        Score <- round(PercRegress * 100, 2)
    } else if (Zscore <= 0) {
        Score <- round((1 - PercRegress) * 100, 2)
    }
    
    if (Zscore >= 1) {
        Rating <- "Excellent"
    } else if (Zscore < 1 & Zscore >= 0.5) {
        Rating <- "Good"
    } else if (Zscore < 0.5 & Zscore >= -0.5) {
        Rating <- "Average"
    } else if (Zscore < -0.5 & Zscore >= -1) {
        Rating <- "Fair"
    } else if (Zscore < -1) {
        Rating <- "Poor"
    }
    cat("Densidade Corporal:", DC, "\n",
        "Massa Magra:", LeanWeight, "\n",
        "Massa Gorda:", FatWeight, "\n",
        "% Gordura:", PercentFat, "\n",
        "Rating:", PopulationAverage, "\n",
        "Score:", Score, "\n",
        "Rating:", Rating, "\n")
}
test_perc_fat()
