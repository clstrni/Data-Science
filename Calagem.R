calc_nc <- function(v1, v2, ctc, prnt) {
    nc <- (v2 - v1) * ctc / prnt
    return(nc)
}

#Utilizando scan----------------------------------------------------------------
calagem_read <- function() {
    cat("Informe a saturação por bases inical do solo.\n")
    v1 <- scan(n = 1, what = numeric())
    cat("Informe a saturação por bases desejada.\n")
    v2 <- scan(n = 1, what = numeric())
    cat("Informe a capacitade de troca de cátions.\n")
    ctc <- scan(n = 1, what = numeric())
    cat("Informe o poder relativo de neutralização total.\n")
    prnt <- scan(n = 1, what = numeric())
    return(calc_nc(v1, v2, ctc, prnt))
}

calagem_read()
#-------------------------------------------------------------------------------

#Utilizando arquivo dcf---------------------------------------------------------
calagem_dcf <- function(path) {
    x <- read.dcf(path)
    z <- as.numeric(x)
    names(z) <- colnames(x)
    return(calc_nc(z['v1'], z['v2'], z['ctc'], z['prnt']))
}

calagem_dcf(path = "calagem.dcf")
#-------------------------------------------------------------------------------
#setwd("C:/Users/rhsilva/PERFECT/Eduardo Silva - DOMPER/Base Conhecimento/Jornada Data Science/1.R Para Decolar/")
#getwd()
