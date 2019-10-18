test_that("SVM works", {
  data_test = data.frame(mnt_25_l93= rnorm(100),
                         pente_degree = rnorm(100),
                         u = rnorm(100),
                         v = rnorm(100),
                         x = rnorm(100),
                         y = rnorm(100))
  colnames(data_test) = stringi::stri_rand_strings(6,5)
  
    test=SVM_parral(Yvar = rnorm(100),variable = data_test,
                    cross = 5,k = 5,cost = c(2^(-3:8)),kernel = "radial",epsilon = c(0.05,seq(0,1,0.1)))
    expect_that(test, is_a("list"))
    expect_that(length(test), equals(3))
    expect_that(nrow(test$DON), equals(100))
})
