# Portfolio simulator working instructions

## Project
Python/Streamlit portfolio scenario explorer. app.py owns the interface;
engine.py owns numerical simulation and must remain independent of Streamlit.
Files may be in the repo root or Python/; inspect the actual layout before running commands.
This model illustrates assumptions; it is not validated investment advice.

## Numerical contracts
- UI percentages convert to fractions at the engine boundary. Returns and volatility
  are annual; transition probabilities and simulation steps are monthly.
- Preserve the simple-return-to-log-parameter conversion. Changes to cash-flow timing,
  fees, tax drag, or survival semantics require an explicit explanation.
- run_monte_carlo init_regime uses 0 for stationary draw and 1/2/3 for fixed regimes;
  internal regime indices use 0/1/2.
- Contributions precede monthly growth and stop at retirement. Withdrawals start at
  retirement. Preserve nonnegative balances and inflation-adjusted reporting.
- Cholesky factors are upper triangular: U.T @ U equals the target covariance;
  row-vector random shocks use Z @ U.
- survival_prob measures positive balances at each time, not necessarily never-depleted paths.

## Verification
For numerical changes use a small case with a known answer before a full simulation.
Examples: zero-volatility 10% annual growth turns $1,000 into $1,100 in one year;
zero-return $100 contributions add $1,200 monthly or $2,600 biweekly per year.
Run `python verify_demo.py` from the directory containing it and engine.py.
These local checks use synthetic fixtures with no production access. Run relevant
checks and fix regressions from your change without asking at each step.
For UI changes also exercise the affected Streamlit flow; engine checks alone do not validate UI behavior.

## Delivery
Make focused changes. Preserve unrelated user edits. Inspect diffs before finishing.
Report changed behavior, actual checks run, and remaining limitations. Distinguish
executed checks from suggested ones. Never invent benchmark improvements or usage.
Keep secrets and personal financial data out of fixtures and logs. Work locally;
publishing or deploying needs a separate user request. Explain new dependencies.
