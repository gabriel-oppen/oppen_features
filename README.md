
# ITT and LATE Estimation Function

This repository contains an R functions designed to estimate the Average Treatment Effect on the Treated (ITT) and Local Average Treatment Effect (LATE) using various methods, including Difference in Means and Difference in Differences. The function allows for adjustment for clustering and provides options for methods such as Lee Bounds and Inverse Probability Weighting (IPW).

## Function Description

The function `f_oppen_estima_ITT_LATE` takes the following parameters:

- `dados`: The dataset containing the variables of interest.
- `vars_controle`: A list of control variables.
- `vars_resultado`: A list of outcome variables.
- `var_tratamento_sorteado`: The treatment assigned by randomization.
- `var_tratamento_recebido`: The actual treatment received.
- `tempo_final`: The final time point for impact analysis.
- `vars_cluster`: The clustering variable(s).
- `output_path`: The output path to save the results.

The function performs the following steps:
1. Filters the dataset based on the specified variables and time points.
2. Implements Lee Bounds method to handle non-random attrition if applicable.
3. Estimates ITT and LATE using specified methods (e.g., Difference in Means, Difference in Differences).
4. Computes standard errors using robust or clustered standard errors.
5. Saves the results to an Excel file.

## Usage

To use the function:
1. Load the function into your R environment.
2. Prepare your dataset with appropriate variables.
3. Call the function with the required parameters.

```R
f_oppen_estima_ITT_LATE(dados, vars_controle, vars_resultado,
                        var_tratamento_sorteado, var_tratamento_recebido,
                        tempo_final, vars_cluster, output_path)
```

## Note

Ensure that your dataset and variable names match the parameters provided to the function. Additionally, review the output for meaningful interpretation of treatment effects.

