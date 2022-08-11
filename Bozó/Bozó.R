full_house <- 0
sequencia <- 0
quadra <- 0
general <- 0
i <- 1
n <- 50000
repeat {
    s0 <- sample(1:6, size = 5, replace = TRUE)
    s1 <- sort(s0)
    s2 <- diff(s1)
    i <- i + 1
    if (i > n) break
    ifelse(all(s2 == c(1, 1, 1, 1)), sequencia <- sequencia + 1,
           ifelse(all(s2 == c(0, 0, 0, 0)), general <- general + 1,
                  ifelse(unique(s2)[1] == 0, full_house <- full_house + 1,
                         ifelse(unique(s2)[1] > 0, quadra <- quadra + 1,
                                NA_real_
                         )
                  )
           )
    )
}
cat("Probabilidade Full House:", full_house/n, "\n",
    "Probabilidade Sequência:", sequencia/n, "\n", 
    "Probabilidade Quadra:", quadra/n, "\n", 
    "Probabilidade General:", general/n, "\n")


# length(unique((sort(c(5, 5, 2, 2, 5)))))
# t<-diff(sort(c(5, 2, 5, 5, 5)))
# s<-unique(t)[1]
# s
