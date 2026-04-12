# Developer Notes

## Quadratic Terms & Categorical Variables

**Cleaning Terms:** Currently, quadratic terms and polynomial powers appear uncleaned in output tables with backticks (`` ` ``) and `I(...)` formatting. We need to decide whether to leave these uncleaned in the future or write a utility to format them properly.

**Categorical Variables Check:** We need to determine whether we should add an explicit check/warning when users try to apply quadratic or polynomial terms to categorical variables, since standard polynomial terms are not typically applicable to categorical factors.

## Word Export Options

**Autofit Options:** Consider adding specific Word export options to target column autofit behavior, allowing the user to select between fitting the table to the "window" versus fitting it to the "content".

**Repeat Headers:** Add an export option to automatically repeat table headers across page breaks for very long tables generated in Word.

## tblsurvfit Tests

**Tarone-Ware Test:** Need to add the Tarone-Ware test back to both `testDefault` and the specific estimator test options (immediately after Peto & Peto) in the `tblsurvfit.a.yaml` file after discussing the mathematical implementation issue with the `gtsummary` authors:

```yaml
      - title: Tarone-Ware
        name: tarone
```

## Non-syntactic names in survfit

The `survival` package version 3.8-7 now supports non-syntactic names. In the future, I need to deprecate the usage of `make.names` and instead use the actual column names. This might require a modified version of `validateVarNames`—for instance, I might only need to check for backslashes (`\\`), or perhaps no manual checks will be needed at all. I will implement and validate this once jamovi updates its bundled version of the `survival` package.