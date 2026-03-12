
# Stat 405 project

Proposed Methodology
The project explores three progressively complex Bayesian models for estimating animal movement from GPS data.
Model 1 — Naive Baseline: Treats all GPS observations as exchangeable, assuming no temporal structure. A conjugate prior setup yields a closed-form posterior, making this a clean and interpretable baseline.
Model 2 — Kalman Filter: Introduces a state-space structure where the animal's true position evolves as a latent random walk, observed through noisy GPS readings. Variance parameters are estimated via maximum likelihood, and the Kalman smoother produces exact analytic posteriors over the hidden states.
Model 3 — Full Bayesian (MCMC via Stan): Shares the same state-space structure as Model 2 but treats all parameters as unknown. Priors are placed on both hidden states and variance terms, and a Metropolis-Hastings sampler (via Stan) is used to draw from the full joint posterior.

⚠️ Specific distributional choices for likelihoods and priors will be finalized after further research.

