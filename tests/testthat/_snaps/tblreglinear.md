# Multivariable Linear Regression / rejects polynomials on factors and preserves their names in the error

    Code
      tblRegLinear(data = data, dep = "marker", factors = "Grade {group}",
        modelTerms = list(c("Grade {group}", "Grade {group}")))
    Condition
      Error:
      ! Polynomial terms cannot be applied to Factors. Please remove the polynomial term for "Grade {group}".

