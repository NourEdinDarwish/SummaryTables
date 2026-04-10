# Developer Notes

## Quadratic Terms & Categorical Variables

### Cleaning Terms
Currently, quadratic terms and polynomial powers appear uncleaned in output tables with backticks (`` ` ``) and `I(...)` formatting. We need to decide whether to leave these uncleaned in the future or write a utility to format them properly.

### Categorical Variables Check
We need to determine whether we should add an explicit check/warning when users try to apply quadratic or polynomial terms to categorical variables, since standard polynomial terms are not typically applicable to categorical factors.

## Word Export Options

### Autofit Options
Consider adding specific Word export options to target column autofit behavior, allowing the user to select between fitting the table to the "window" versus fitting it to the "content".

### Repeat Headers
Add an export option to automatically repeat table headers across page breaks for very long tables generated in Word.
