# Tutorial on Conformal Prediction

This repository contains code examples illustrating different **Conformal Prediction (CP)** methods, created for a workshop organized by the **Statistics Section at the University of Oslo**.  
Each R script is self-contained and demonstrates one specific aspect or extension of conformal prediction.

## Contents

| Script | Description |
|---------|--------------|
| `part1_splitcp.R` | Basic example of the **split conformal prediction** algorithm on simulated data. |
| `part1_splitcp_normalized.R` | Split CP on simulated data, using **normalized nonconformity scores**. |
| `part1_splitcp_mondrian.R` | Demonstrates **Mondrian (group-wise)** calibration of prediction intervals. |
| `part1_splitcp_manysims.R` | Wraps the above methods in a loop to run multiple (e.g. 1000) simulations for empirical coverage analysis. |
| `part2_electricity.R` | Applies CP to the **ELEC2 dataset** [1], comparing three variants: a naïve split CP, a **weighted CP** inspired by [2], and an **adaptive CP** approach as in [3]. |
| `part3_oslohousing.R` | Uses a **real-estate transactions dataset** from the Oslo housing market to demonstrate **spatial CP**, following the approach in [4]. *(Data unavailable for confidentiality reasons.)* |

## References

1. [ELEC2 dataset on Kaggle](https://www.kaggle.com/datasets/yashsharan/the-elec2-dataset)  
2. Barber, Candès, Ramdas, Tibshirani (2023). *Conformal Prediction Beyond Exchangeability.* **Annals of Statistics**, 51(2): 816–845.  
3. Gibbs, Candès (2021). *Adaptive Conformal Inference Under Distribution Shift.* *NeurIPS 2021*, 35: 1660–1672.  
4. Hjort, Horn Hermansen, Pensar, Williams (2025). *Uncertainty Quantification in Automated Valuation Models through Spatially Weighted Conformal Prediction.* **International Journal of Data Science and Analytics**.  
