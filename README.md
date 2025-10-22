# Tutorial on conformal prediction
This repository contains some code examples of conformal prediction (CP), made for a workshop for the Statistics section at the University of Oslo. 
Each R script is self-contained and demonstrates one idea of conformal prediction. 


`part1_splitcp`: A basic example of the split CP algorithm on simulated data. 
`part1_splitcp_normalized`: The split CP algorithm on simulated data, with normalized non-conformity scores. 
`part1_splitcp_mondrian`: Shows you how to calibrate the prediction intervals per groups. 
`part1_splitcp_manysims`: Wraps the methods studied so far in a loop to run e.g. 1000 simulations. 
`part2_electricity`: Studies the `ELEC2` data set [1] and shows three variants of CP: a naïve split CP, a weighted approach inspired by [2], and an adaptive CP method as described in [3]. 
`part3_oslohousing`: Studies a data set of transactions from the Oslo housing market, and applies the spatial CP approach studied in [4]. The data is unavailable for confidentiality reasons. 


[1] https://www.kaggle.com/datasets/yashsharan/the-elec2-dataset
[2] Barber, Candés, Ramdas, Tibshirani (2023). _Conformal Prediction Beyond Exchangeability_. Annals of Statistics, Vol. 51, No. 2, 816-845.  
[3] Gibbs, Candés (2021). _Adaptive Conformal Inference Under Distribution Shift_. Proceedings of the 35th International Conference on Neural Information Processing Systems, No 128, 1660-1672.
[4] Hjort, Horn Hermansen, Pensar, Williams (2025). _Uncertainty Quantification in Automated Valuation Models through Spatially Weighted Conformal Prediction_ International Journal of Data Science and Analytics

