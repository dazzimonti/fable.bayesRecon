# reconcile_fbl_list Function Notes

## Overview
This is the core reconciliation function used by `min_trace` and other reconciliation methods in fabletools. It takes base forecasts and applies a reconciliation transformation to ensure consistency across hierarchies while preserving uncertainty.

**Key inputs:**
- `fc`: Forecast objects (one per series)
- `S`: Summation matrix (hierarchy structure)
- `P`: Reconciliation weight matrix
- `W`: Weight matrix for variance scaling
- `point_forecast`: Function for computing point forecasts (mean, median, etc.)

---

## Phase 1: Setup & Initialization

First thing: check if we're dealing with sparse or dense matrices. If sparse (Matrix package available), we swap in sparse versions of matrix operations.

**Important**: Compute `SP = S × P` once at the start. This is the core reconciliation transformation that can be applied to all horizon steps uniformly. Smart design!

---

## Phase 2: Extract Distributions & Compute Base Statistics

Extract the forecast distributions from each forecast object, then compute:
- `fc_mean`: Matrix of forecast means [n_horizon × n_series]
- `fc_var`: Matrix of forecast variances [n_horizon × n_series]
- `dist_type`: What type of distributions we have (normal, sample, degenerate, etc.)

---

## Phase 3: Apply Reconciliation to Means

**Single matrix multiplication:**
```
fc_mean = SP × base_mean
```

This applies the reconciliation transformation to all horizons at once. No per-horizon loop needed here because the hierarchy structure (`SP`) doesn't change across horizons.

Then convert to list format (one element per series for later processing).

---

## Phase 4: Handle Different Distribution Types

### Case 1: Normal Distributions

This is where it gets interesting. **The variance DOES change per horizon**, so we handle it differently:

1. Extract correlation structure from weight matrix: `R1 = cov2cor(W)`
2. For **each horizon** `h`: Scale correlation by horizon-specific variance
   - `W_h = sqrt(var_h) × R1 × sqrt(var_h)`
3. For **each horizon** `h`: Transform through reconciliation
   - `var_reconciled_h = diag(SP × W_h × SP^T)`
4. Combine reconciled means with reconciled standard deviations to create new normal distributions

**Key insight**: Unlike means, variance scales per horizon through the `W_h` map, accounting for increasing uncertainty into the future.

### Case 2: Sample Distributions

Here, we have actual sample paths, not just mean/variance.

1. Extract all samples: array of shape [n_samples × n_horizon × n_series]
2. **For each sample path independently**: Apply reconciliation
   - `reconciled_sample = SP × base_sample`
3. Reshape back to [n_series × n_horizon × n_samples]
4. Convert array back to sample distributions

**Why this works**: By reconciling each sample independently, we preserve the joint structure of the samples while enforcing hierarchy constraints.

### Case 3: Other Distributions

Use degenerate (point) distributions from the reconciled means.

---

## Phase 5: Update Forecast Objects

Replace the distribution column in each forecast object with the reconciled distribution. Also compute point forecasts (mean, median, etc.) and update those columns. This keeps everything compatible with the fable pipeline.

---

## Key Design Insights

### Why Apply SP Once, Not Per-Horizon?

The reconciliation matrix `SP` is **hierarchy-specific**, not **time-specific**. The structure of which series aggregate to which is the same at every forecast horizon. So computing it once and applying to all horizons is more efficient.

However:
- **Means**: Apply SP once to entire mean matrix ✓
- **Variances**: Apply SP per horizon through W_h map (because variance changes with horizon)
- **Samples**: Apply SP per sample (each sample is independent)

### Variance Scaling Strategy

The clever bit: instead of storing separate variance matrices for each horizon, we:
1. Derive a normalized correlation structure `R1` from W
2. Scale it horizon-by-horizon by the forecast variances
3. This captures how uncertainty grows into the future while maintaining the covariance structure

This is way more efficient than computing full covariance matrices per horizon.

### Distribution-Agnostic Design

The function handles different distribution types gracefully:
- **Normal**: Assumes structure can be captured by mean + variance
- **Sample**: Preserves full distributional shape by reconciling samples
- **Degenerate**: Falls back to point forecasts

This flexibility is why it works for so many different models.

---

## Connection to MixCond

In my `bayesRecon_MixCond` function, I'm doing something similar but for importance-sampled distributions:
- Generate samples from bottom forecasts
- Weight them based on how well they aggregate to upper forecasts
- Resample to create reconciled distribution

Both approaches use the same core idea: apply hierarchy constraints while preserving distributional properties.

---

## Questions for Next Investigation

1. How does variance grow at different horizons in practice? Does W scaling capture this well?
2. For sample distributions, are there numerical stability issues when reconciling many samples?
3. Could we extend this to handle forecast combinations or other uncertainty quantification methods?
