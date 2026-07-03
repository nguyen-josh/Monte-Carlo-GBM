# Regime-Switching Monte Carlo Portfolio Simulator
https://monte-carlo-gbm.streamlit.app/

A Markov regime-switching Monte Carlo engine for retirement-portfolio projection built in Python
with an interactive Streamlit front-end. The model simulates thousands of
lifecycle paths through Bull / Bear / Crisis market regimes to estimate the
probability that a portfolio survives through a target age,
along with nominal and inflation-adjusted percentile bands. The original, simpler version I built in R/Shiny was based on a for loop, and I converted this to a Markov model in Python, vectorized to increase model speed as recommended to me by a coworker. Both are included in the GitHub repository for reference.

> **Disclaimer:** This project makes simplifying assumptions and should not be used to make real investment or retirement decisions.

https://github.com/user-attachments/assets/fe82af98-7acd-4928-b02c-3a6508a78fe0

## What it does

- **Three-regime Markov chain** (Bull, Bear, Crisis) with user-supplied monthly
  transition probabilities. The chain is validated for irreducibility and its
  stationary distribution is computed analytically.
- **Regime-dependent multivariate geometric Brownian motion (GBM)** for each asset.
  Arithmetic return/volatility inputs are moment-matched to log-normal drift and
  diffusion parameters, net of expense ratios and tax drag.
- **Correlated asset shocks** via a Cholesky factor per regime, with 
  cross-asset correlations converging to 1.0 during
  stressed regimes (diversification breakdown). Non-positive-definite correlation
  matrices are repaired to the nearest valid correlation matrix using Higham's
  alternating-projections algorithm.
- **Lifecycle cash flows:** an accumulation phase (contributions routed to
  underweight assets) and a drawdown phase (inflation-indexed withdrawals taken
  from overweight assets first), modeling sequence-of-returns risk.
- **Outputs:** survival probability over time, median / 10th / 90th percentile
  paths in both nominal and real terms, a statistics table, and a CSV export.

## Quick start

```bash
# 1. Clone and enter the repo
git clone https://github.com/nguyen-josh/Monte-Carlo-GBM.git
cd Monte-Carlo-GBM

# 2. Create a virtual environment
python -m venv .venv
source .venv/bin/activate        # Windows: .venv\Scripts\activate

# 3. Install dependencies
pip install -r requirements.txt

# 4. Launch the app
streamlit run app.py
```

The app opens in your browser. Configure assets, correlations, and regime
transition probabilities on the **Portfolio Setup** page, click **Run
Simulation**, then view charts and download results on **Simulation Results**.

## Using the engine directly

The engine has no UI dependencies (NumPy + SciPy only) and can be scripted:

```python
import numpy as np
import engine as E

etf = E.ETFData(
    initial_balance=[100_000, 50_000, 25_000],
    target_weight=[0.5, 0.3, 0.2],
    tax_drag=[0.005, 0.005, 0.005],
    expense_ratio=[0.001, 0.001, 0.0005],
    bull_ret=[0.10, 0.09, 0.04],   bull_vol=[0.15, 0.17, 0.05],
    bear_ret=[-0.03, -0.04, 0.02], bear_vol=[0.22, 0.24, 0.07],
    crisis_ret=[-0.35, -0.40, -0.05], crisis_vol=[0.35, 0.40, 0.10],
)
chol_list, _ = E.build_chol_list(
    corr_base=np.array([[1, .7, .1], [.7, 1, .1], [.1, .1, 1]]),
    bear_surge=0.15, crisis_surge=0.50,
)
rp = E.RegimeParams(pBB=.015, pBC=.001, pRB=.05, pRC=.01, pCB=.08, pCR=.12)

res = E.run_monte_carlo(
    etf, etf.target_weight, chol_list,
    current_age=30, retirement_age=65, annual_expenses=60_000,
    inflation_rate=0.03, total_contribution=1_500, contrib_freq=12,
    inflate_contrib=True, num_simulations=10_000, max_years=60,
    regime_params=rp, init_regime=0, seed=42,
)
print(res.survival_prob[-1])   # probability funds last to the final age
```

## Methodology

Asset prices follow regime-specific GBM, `dS = μ·S·dt + σ·S·dW`, discretized at
monthly steps. For each asset and regime, the arithmetic mean simple return
(net of fees and tax drag) and volatility are converted to log-normal parameters:

```
σ²_log = ln(1 + σ²_simple / m²)
μ_log  = ln(m) − ½·σ²_log
```

Regime transitions are sampled each month by inverse-CDF from the transition
matrix. The stationary distribution (used for the default starting regime) is
found by solving `(Pᵀ − I)π = 0` under the normalization `Σπ = 1`, with an
eigenvector fallback. See the **Help & Documentation** page in the app for more.

## Limitations (don't use to make real investment decisions)

- Within-regime returns are log-normal, which understates tail risk relative to real markets
- The user provides the transition probabilities for market states, and the model assumes transitions probabilities are unchanging; realistically, transition probabilities are unknown and fluctuate.
- Transition probabilities, returns, and volatilities are fixed point estimates, meaning there is no parameter uncertainty
- Taxes are modeled only as a flat drag from dividends; cost basis and tax lots are not tracked, meaning taxes during the drawdown period are not modeled. Income and tax brackets are not modeled either.
- Tax reduction opportunities, such as tax loss harvesting or state-tax-exempt treasuries, are not modeled.
- Tax-advantaged accounts such as IRAs, HSAs, 401(k)s, and 529 plans are not modeled. Prioritizing these accounts may reduce taxable income and tax drag, thereby increasing gains and net worth.
- Leverage, margin, options, individual stocks, managed futures, real estate, precious metals, crypto, and other asset classes contributing to net worth are not modeled.
- The simulation assumes no selling occurs until the drawdown phase. This ignores any selling during the accumulation phase that often occurs, such as funding the downpayment for a mortgage or similar major expenditure.
- Inflation is assumed to be constant annually, with the default at 3%. Hyper-inflation and deflation are not unheard of—Argentina and Japan. The model is limited to inflation in one market; inflation in other markets will affect equities held in these markets.
- This model assumes markets are predictable and there are no drastic events causing disruption, like wars or global market crises.
- Reported survival probabilities and percentiles are Monte Carlo point estimates and carry sampling error.

## Project structure

```
Monte-Carlo-GBM/
├── .gitignore
├── LICENSE
├── README.md
├── R/
│   └── monte_carlo.R
└── Python/
    ├── requirements.txt
    ├── engine.py 
    └── app.py  
```

## License

Released under the MIT License — see [LICENSE](LICENSE).
