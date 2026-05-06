# User Guide

This guide covers the core features, default settings, and options available when using SummaryTables.

## 1. Manual Run Mode

When adding multiple variables or making several option changes concurrently, calculating the tables can sometimes cause accumulating lag. To prevent this, you can enable **Manual Run Mode**.

![Run Manually UI](assets/run-manually.png)

* **Why use it?** It prevents the module from recalculating after every single click or variable addition. It is especially useful when building complex multivariable regression models or working with very large datasets.
* **How to use it:** Check the "Run Manually" option. A "Calculate" button will appear. Make all your desired variable assignments and option changes, then click "Calculate" once to generate the final table.

## 2. Rounding (Auto vs Fixed)

SummaryTables provides flexible rounding options for your data:

* **Auto (Default):** Uses intelligent relative rounding based on the data's scale. This leverages `gtsummary`'s styling functions to present publication-ready numbers (e.g., smaller percentages get more decimal places, while large ones get fewer).
* **Fixed Digits:** If you require strict consistency across all values (e.g., exactly 2 decimal places for all numbers, regardless of their magnitude), you can select the fixed digit option from the drop-down menu.

## 3. P-Value Rounding

To ensure standard scientific formatting, p-values are rounded using specific criteria:

* Extremely small p-values are formatted as `<0.001`.
* Large p-values are typically rounded to 1 or 2 decimal places (e.g., `>0.9` or `0.25`).
* For the exact rounding thresholds, see the [style_pvalue](https://www.danieldsjoberg.com/gtsummary/reference/style_pvalue.html) documentation in `gtsummary`.

## 4. Statistical Tests

The module automatically selects appropriate statistical tests based on your data types and group sizes:

* **Continuous Variables (Summary & Continuous Tables):**
    * **Parametric:** Uses the independent t-test (for 2 groups) or One-way ANOVA (for >2 groups). If a grouping variable is supplied in the continuous table, a Two-way ANOVA is used by default.
    * **Non-parametric:** Uses the Wilcoxon rank-sum test (for 2 groups) or Kruskal-Wallis rank-sum test (for >2 groups).
* **Categorical Variables (Summary & Cross Tables):**
    * Uses **Pearson's Chi-square test** if all expected cell counts are ≥ 5.
    * Automatically falls back to **Fisher's exact test** if any expected cell count is < 5.

## 5. Standardized Mean Difference (SMD)

In the Summary Table, you can calculate the Standardized Mean Difference (SMD) by checking the "Difference" option. 

* The SMD is calculated under the hood using the [`smd` R package](https://bsaul.github.io/smd/index.html).
* This provides a standardized effect size measure to assess the magnitude of difference between groups, regardless of the underlying units.

## 6. Exporting to Word

SummaryTables allows you to export any table directly as a `.docx` file for easy inclusion in manuscripts.

⚠️ **Warning: Overwriting Files**
When exporting a table, if you select a filename that already exists in the chosen folder, the module will **silently overwrite the existing file** without a warning prompt. Please double-check your filenames before saving.

![Word Export](assets/word-export.png)

## 7. Survival & Cox Regression Variables

When running Survival analyses or Cox regression models, the **Event variable** must be formatted correctly:

* **If continuous:** It must be recoded strictly as `0` for censored and `1` for the event. Any other numeric values (like 1 and 2) will cause an error.
* **If categorical/factor:** You can map custom text levels (e.g., "Alive" / "Dead") by choosing the specific level that represents the event in the UI.
