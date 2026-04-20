
# Stat 405 project: Bayesian Wolf Trajectory Inference from GPS Telemetry

## Overview

This project applies Bayesian state-space models to GPS telemetry data from wolf J112 (Sunwapta pack) in the Banff-Jasper ecosystem to reconstruct the wolf's true movement trajectory from noisy GPS observations. We compare two models of increasing complexity and two MCMC inference methods.

**Data:** 12,850 GPS observations from the Movebank Data Repository (Hebblewhite 2025), subset to a 20-day window of 899 observations.

## Models

**Simple — Random Walk:** The wolf's true position evolves as a random walk. Each step is independent noise with no directional memory.

**Complex — Constant Velocity:** The hidden state includes both position and velocity. Position updates depend on the previous velocity, allowing the model to capture directed travel versus stationary behavior.

## Inference Methods

**Metropolis-Hastings:** Hand-coded in R. Updates each hidden state one at a time via Normal proposals with manual tuning.

**HMC/NUTS:** Implemented in Stan via `cmdstanr`. Uses gradient information to propose moves in all dimensions simultaneously with automatic tuning.

## Key Findings

- The velocity model successfully identifies traveling vs. stationary behavioral states
- HMC achieves orders of magnitude greater sampling efficiency than MH for this high-dimensional problem
- GPS measurement error was fixed at a known value (0.001°) based on collar specifications, resolving mixing issues observed when estimating it freely
