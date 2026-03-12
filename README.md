
# Stat 405 project
The goal of this project is to infer the true underlying movement trajectory of an animal from noisy GPS observations using Bayesian state-space models. GPS tracking devices record an animal’s location over time, but these measurements contain sensor noise and may not reflect the exact position of the animal. We model the true location as a latent trajectory that evolves over time and treat the observed GPS coordinates as noisy measurements of this trajectory.

We compare three approaches for reconstructing the trajectory: a naive model, a Kalman filtering method and a Bayesian model estimated using Markov Chain Monte Carlo (MCMC). The Kalman filter provides a baseline trajectory estimate under Gaussian assumptions, while the Bayesian approach allows us to place priors on the movement and observation noise parameters and compute the full posterior distribution of the latent trajectory.

Using real animal tracking data from the Movebank database, we evaluate how well each approach reconstructs the animal’s path and quantify uncertainty in the estimated movement trajectory.


## Proposed Methodology

The project explores three progressively complex Bayesian models for estimating animal movement from GPS data.

**Model 1 — Naive Baseline:** Treats all GPS observations as exchangeable, assuming no temporal structure. A conjugate prior setup yields a closed-form posterior, making this a clean and interpretable baseline.

**Model 2 — Kalman Filter:** Introduces a state-space structure where the animal's true position evolves as a latent random walk, observed through noisy GPS readings. Variance parameters are estimated via maximum likelihood, and the Kalman smoother produces exact analytic posteriors over the hidden states.

**Model 3 — Full Bayesian (MCMC via Stan):** Shares the same state-space structure as Model 2 but treats all parameters as unknown. Priors are placed on both hidden states and variance terms, and a Metropolis-Hastings sampler (via Stan) is used to draw from the full joint posterior.

> ⚠️ *Specific distributional choices for likelihoods and priors will be finalized after further research.*
