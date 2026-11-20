# Multivariable Logistic Regression / warns about standard errors displayed alongside odds ratios

    Code
      cat(qjeResult[["runWarn"]]$content)
    Output
      Standard errors (SE) correspond to the unexponentiated coefficients, not the displayed odds ratios.

# Multivariable Logistic Regression / rejects non-binary outcomes

    Code
      tblRegLogistic(data = data, dep = "Grade {group}", covs = "age", modelTerms = list(
        "age"))
    Condition
      Error:
      ! The dependent variable "Grade {group}" must have exactly two levels for binomial logistic regression

