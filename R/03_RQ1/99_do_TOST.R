do_TOST <- function(fit, covar, sesoi, type = c("gauss", "beta")) {
  # create formula
  form <- as.formula(paste0("~ ", covar))

  # get expected means for males and females
  means <- emmeans::emmeans(fit, specs = form)

  # compute contrast
  contr <- emmeans::contrast(means, method = "pairwise")

  # get p-value and se
  p <- emmeans::test(contr)$p.value
  se <- emmeans::test(contr)$SE

  # create upper and lower equivalence bound
  upper <- sesoi
  lower <- - sesoi



  # compute upper and lower thresholds
  if(type == "beta") {
    male <- summary(means)$emmean[1]
    female <- summary(means)$emmean[2]
    male_resp <- plogis(male)
    female_resp <- plogis(female)

    # upper and lower thresholds
    upper <- male_resp + sesoi / 2
    lower <- male_resp - sesoi / 2

    # transform to linear predictors in order to test
    upper <- qlogis(upper) - male
    lower <- qlogis(lower) - male

    # expected difference
    m_f <- male_resp - female_resp


    # get confidence interval for difference based on confidence interval for OR
    confint <- confint(contr)
    CI_lower <- confint$asymp.LCL
    CI_upper <- confint$asymp.UCL

    # multiply OR by odds female and compute limits for difference
    CI_lower_male <- plogis(log(exp(female + CI_lower))) - female_resp
    CI_upper_male <- plogis(log(exp(female + CI_upper))) - female_resp

    # divide odds male by OR and compute limits for difference
    CI_lower_female <- male_resp - (plogis(log(exp(male - CI_lower))))
    CI_upper_female <- male_resp - (plogis(log(exp(male - CI_upper))))

    # take average of upper and lower
    CI_lower <- mean(CI_lower_male, CI_lower_female)
    CI_upper <- mean(CI_upper_male, CI_upper_female)

  } else {
    m_f <- summary(contr)$estimate

    # get confidence interval
    confint <- confint(contr)
    CI_lower <- confint$lower.CL
    CI_upper <- confint$upper.CL
  }

  # tests
  t1 <- emmeans::test(contr, null = upper, side = "<")
  t2 <- emmeans::test(contr, null = lower, side = ">")

  # get p-values
  p1 <- summary(t1)$p.value
  p2 <- summary(t2)$p.value

  return(data.frame(male_female = m_f,
                    se = se,
                    CI_lower = CI_lower,
                    CI_upper = CI_upper,
                    p = p,
                    tost_sesoi = sesoi,
                    tost_p = max(p1, p2)))
}
