"""
Markov regime-switching Monte Carlo retirement-portfolio engine.

Regime convention (0-based internally): 0 = Bull, 1 = Bear, 2 = Crisis.

"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Callable, Optional, Sequence

import numpy as np

# Portfolio is considered surviving while its value exceeds this dollar floor
SURVIVAL_THRESHOLD = 1e-8
# Values below this are snapped to exactly zero
ZERO_FLOOR = 1e-10

# Small helpers
def coalesce(x, fallback):
    """Mirror of R's `%||%`: fall back for None / empty / scalar-NaN."""
    if x is None:
        return fallback
    if isinstance(x, float) and np.isnan(x):
        return fallback
    if isinstance(x, (list, tuple, np.ndarray)) and len(x) == 0:
        return fallback
    return x


def robust_chol(mat: np.ndarray, max_tries: int = 6) -> np.ndarray:
    """
    Return an UPPER-triangular Cholesky factor U with U.T @ U == mat

    numpy returns the lower factor L (L @ L.T == mat); the upper factor is L.T.
    On failure, escalating diagonal jitter is added to absorb floating-point
    edge cases
    """
    mat = np.asarray(mat, dtype=float)
    mat = (mat + mat.T) / 2.0  # enforce symmetry
    n = mat.shape[0]
    try:
        return np.linalg.cholesky(mat).T
    except np.linalg.LinAlgError:
        pass
    # Scale jitter relative to the matrix magnitude for numerical stability
    base = max(np.trace(mat) / max(n, 1), 1.0)
    jitter = 1e-10 * base
    for _ in range(max_tries):
        try:
            return np.linalg.cholesky(mat + np.eye(n) * jitter).T
        except np.linalg.LinAlgError:
            jitter *= 10.0
    raise np.linalg.LinAlgError(
        "Cholesky factorization failed even after diagonal regularization; "
        "the input matrix is ill-conditioned."
    )


def is_irreducible(P: np.ndarray, tol: float = 1e-12) -> bool:
    """Boolean transitive-closure reachability test"""
    P = np.asarray(P, dtype=float)
    reach = P > tol
    np.fill_diagonal(reach, True)
    while True:
        updated = reach | ((reach.astype(int) @ reach.astype(int)) > 0)
        if np.array_equal(updated, reach):
            break
        reach = updated
    return bool(reach.all())


@dataclass
class Check:
    ok: bool
    message: Optional[str] = None


def validate_transition_matrix(P: np.ndarray, tol: float = 1e-10) -> Check:
    P = np.asarray(P, dtype=float)
    if P.ndim != 2 or P.shape[0] != P.shape[1]:
        return Check(False, "Transition matrix must be square.")
    if not np.all(np.isfinite(P)):
        return Check(False, "Transition matrix contains non-finite values.")
    if np.any(P < -tol) or np.any(P > 1 + tol):
        return Check(False, "Transition probabilities must lie in [0, 1].")
    row_sums = P.sum(axis=1)
    if np.any(np.abs(row_sums - 1) > 1e-8):
        return Check(False, "Each row of the transition matrix must sum to 1.")
    if not is_irreducible(P, tol=tol):
        return Check(
            False,
            "The transition matrix is reducible; at least one regime is "
            "not reachable from another.",
        )
    return Check(True, None)


def compute_stationary(P: np.ndarray) -> np.ndarray:
    """Stationary distribution = left eigenvector of P for eigenvalue 1

    Always returns a finite, non-negative vector that sums to exactly 1
    """
    P = np.asarray(P, dtype=float)
    n = P.shape[0]
    if not validate_transition_matrix(P).ok:
        return np.full(n, 1.0 / n)

    vals, vecs = np.linalg.eig(P.T)
    idx = int(np.argmin(np.abs(vals - 1.0)))
    vec = np.real(vecs[:, idx])

    # Perron-Frobenius guarantees a single-signed eigenvector; flip if needed
    if vec.sum() < 0:
        vec = -vec
    vec = np.maximum(vec, 0.0)
    total = vec.sum()
    if not np.isfinite(total) or total <= 0:
        return np.full(n, 1.0 / n)
    return vec / total


def nearest_correlation(A: np.ndarray, max_iter: int = 200,
                        tol: float = 1e-9, eps: float = 1e-8) -> np.ndarray:
    """
    Nearest positive-definite correlation matrix via Higham's alternating
    projections
    """
    A = np.asarray(A, dtype=float)
    Y = (A + A.T) / 2.0
    dS = np.zeros_like(Y)
    prev = Y.copy()
    for _ in range(max_iter):
        R = Y - dS
        # Project onto the PSD cone (clip eigenvalues to a small positive eps)
        w, V = np.linalg.eigh((R + R.T) / 2.0)
        w = np.maximum(w, eps)
        X = (V * w) @ V.T
        dS = X - R
        # Project onto the set of unit-diagonal matrices
        Y = X.copy()
        np.fill_diagonal(Y, 1.0)
        if np.linalg.norm(Y - prev, ord="fro") < tol * max(1.0, np.linalg.norm(Y, ord="fro")):
            break
        prev = Y.copy()
    Y = (Y + Y.T) / 2.0
    np.fill_diagonal(Y, 1.0)
    return Y


def make_pd(mat: np.ndarray, eps: float = 1e-8):
    """Return (matrix, was_adjusted). 1x1 collapses to [[1]]

    Never mutates the caller's array
    """
    mat = np.array(mat, dtype=float, copy=True)
    if mat.shape[0] == 1:
        return np.array([[1.0]]), False
    w = np.linalg.eigvalsh((mat + mat.T) / 2.0)
    if w.min() < eps:
        fixed = nearest_correlation(mat, eps=eps)
        np.fill_diagonal(fixed, 1.0)
        return fixed, True
    mat = (mat + mat.T) / 2.0
    np.fill_diagonal(mat, 1.0)
    return mat, False


# Parameter containers
@dataclass
class RegimeParams:
    """Monthly transition probabilities (fractions, not percentages)."""
    pBB: float  # Bull -> Bear
    pBC: float  # Bull -> Crisis
    pRB: float  # Bear -> Bull
    pRC: float  # Bear -> Crisis
    pCB: float  # Crisis -> Bull
    pCR: float  # Crisis -> Bear

    def matrix(self) -> np.ndarray:
        return np.array([
            [1 - self.pBB - self.pBC, self.pBB, self.pBC],
            [self.pRB, 1 - self.pRB - self.pRC, self.pRC],
            [self.pCB, self.pCR, 1 - self.pCB - self.pCR],
        ], dtype=float)


@dataclass
class ETFData:
    """
    Per-asset parameters. All sequences share length n_etfs.
    Returns/vols/ratios are FRACTIONS (e.g. 0.10 == 10%).
    """
    initial_balance: np.ndarray
    target_weight: np.ndarray
    tax_drag: np.ndarray
    expense_ratio: np.ndarray
    bull_ret: np.ndarray
    bull_vol: np.ndarray
    bear_ret: np.ndarray
    bear_vol: np.ndarray
    crisis_ret: np.ndarray
    crisis_vol: np.ndarray

    _FIELDS = ("initial_balance", "target_weight", "tax_drag", "expense_ratio",
               "bull_ret", "bull_vol", "bear_ret", "bear_vol",
               "crisis_ret", "crisis_vol")

    def __post_init__(self):
        for f in self._FIELDS:
            setattr(self, f, np.asarray(getattr(self, f), dtype=float).ravel())
        lengths = {f: getattr(self, f).size for f in self._FIELDS}
        if len(set(lengths.values())) != 1:
            raise ValueError(f"All ETFData fields must share one length; got {lengths}.")
        if self.n == 0:
            raise ValueError("ETFData requires at least one asset.")
        if np.any(self.initial_balance < 0):
            raise ValueError("Initial balances must be non-negative.")

    @property
    def n(self) -> int:
        return len(self.initial_balance)


@dataclass
class SimulationResult:
    ages: np.ndarray               # (n_periods,)
    years: np.ndarray              # (n_periods,)
    median: np.ndarray             # nominal
    median_real: np.ndarray        # inflation-adjusted to today's dollars
    p10: np.ndarray                # nominal 10th percentile
    p90: np.ndarray                # nominal 90th percentile
    p10_real: np.ndarray           # real 10th percentile
    p90_real: np.ndarray           # real 90th percentile
    survival_prob: np.ndarray      # P(value > 0) per period, in [0, 1]
    num_simulations: int
    # Full path matrices are optional and only retained when keep_paths=True
    # (each is num_sim x n_periods and can be hundreds of MB)
    simulations: Optional[np.ndarray] = None       # (num_sim, n_periods)
    survival_matrix: Optional[np.ndarray] = None    # bool (num_sim, n_periods)


# Core simulation
def run_monte_carlo(
    etf: ETFData,
    target_weights: Sequence[float],
    chol_list: Sequence[np.ndarray],   # [Bull, Bear, Crisis] upper factors
    current_age: float,
    retirement_age: float,
    annual_expenses: float,
    inflation_rate: float,             # fraction
    total_contribution: float,
    contrib_freq: int,                 # 1, 12, or 26
    inflate_contrib: bool,
    num_simulations: int,
    max_years: float,
    regime_params: RegimeParams,
    init_regime: int,                  # 0 => stationary draw; 1/2/3 fixed
    progress_fun: Optional[Callable[[float], None]] = None,
    seed: Optional[int] = None,
    keep_paths: bool = False,
) -> SimulationResult:
    rng = np.random.default_rng(seed)

    n_etfs = etf.n

    # Input validation (fail fast with actionable messages)
    num_simulations = int(num_simulations)
    if num_simulations < 1:
        raise ValueError("num_simulations must be a positive integer.")
    if not np.isfinite(max_years) or max_years <= 0:
        raise ValueError("max_years must be a positive, finite number.")
    if not np.isfinite(inflation_rate) or inflation_rate <= -1:
        raise ValueError("inflation_rate must be greater than -100%.")
    if contrib_freq not in (1, 12, 26):
        raise ValueError("Unsupported contribution frequency (use 1, 12, or 26).")
    if int(init_regime) not in (0, 1, 2, 3):
        raise ValueError("init_regime must be 0 (stationary) or 1/2/3 (fixed).")

    target_weights = np.asarray(target_weights, dtype=float).ravel()
    if target_weights.size != n_etfs:
        raise ValueError(
            f"target_weights has length {target_weights.size}, expected {n_etfs}.")
    if np.any(target_weights < 0) or not np.all(np.isfinite(target_weights)):
        raise ValueError("target_weights must be finite and non-negative.")
    w_sum = target_weights.sum()
    if w_sum <= 0:
        raise ValueError("target_weights must sum to a positive value.")
    target_weights = target_weights / w_sum  # normalize

    if len(chol_list) != 3:
        raise ValueError("chol_list must contain exactly 3 factors [Bull, Bear, Crisis].")
    chol_list = [np.asarray(c, dtype=float) for c in chol_list]
    for r, c in enumerate(chol_list):
        if c.shape != (n_etfs, n_etfs):
            raise ValueError(
                f"chol_list[{r}] has shape {c.shape}, expected {(n_etfs, n_etfs)}.")
        if not np.all(np.isfinite(c)):
            raise ValueError(f"chol_list[{r}] contains non-finite values.")

    n_months = max(1, int(np.ceil(max_years * 12)))
    years = np.arange(0, n_months + 1) / 12.0
    n_periods = len(years)
    dt = 1.0 / 12.0
    sqrt_dt = np.sqrt(dt)

    # Convert annual simple-return moments to GBM log parameters
    regime_log_drifts = np.zeros((3, n_etfs))
    regime_sigmas = np.zeros((3, n_etfs))
    arith = np.vstack([etf.bull_ret, etf.bear_ret, etf.crisis_ret])   # (3, n)
    vols = np.vstack([etf.bull_vol, etf.bear_vol, etf.crisis_vol])    # (3, n)
    for i in range(n_etfs):
        for r in range(3):
            net_mean_simple = arith[r, i] - etf.expense_ratio[i]
            tax_deduction = min(net_mean_simple, etf.tax_drag[i]) if net_mean_simple > 0 else 0.0
            gross_mean = 1.0 + net_mean_simple - tax_deduction
            if not np.isfinite(gross_mean) or gross_mean <= 0:
                raise ValueError(
                    f"Asset {i+1} in regime {r+1} implies a non-positive gross "
                    "return expectation after fees/tax drag."
                )
            if not np.isfinite(vols[r, i]) or vols[r, i] < 0:
                raise ValueError(f"Asset {i+1} in regime {r+1} has invalid volatility.")
            sigma2_log = np.log1p((vols[r, i] ** 2) / (gross_mean ** 2))
            regime_sigmas[r, i] = np.sqrt(sigma2_log)
            regime_log_drifts[r, i] = np.log(gross_mean) - 0.5 * sigma2_log

    # Per-month deterministic drift (precomputed once).
    regime_drift_dt = regime_log_drifts * dt
    regime_sigma_sqrt_dt = regime_sigmas * sqrt_dt

    # Transition matrix
    trans_matrix = regime_params.matrix()
    check = validate_transition_matrix(trans_matrix)
    if not check.ok:
        raise ValueError(check.message)
    cum_trans = np.cumsum(trans_matrix, axis=1)

    # Initial regime per path
    if int(init_regime) == 0:
        stat = compute_stationary(trans_matrix)
        stat = stat / stat.sum()  # guarantee exact unit sum for rng.choice
        regime_vec = rng.choice(3, size=num_simulations, replace=True, p=stat)
    else:
        regime_vec = np.full(num_simulations, int(init_regime) - 1, dtype=int)

    # State
    current_vals = np.tile(etf.initial_balance, (num_simulations, 1)).astype(float)
    portfolio_values = np.zeros((num_simulations, n_periods))
    portfolio_values[:, 0] = current_vals.sum(axis=1)

    months = np.arange(0, n_periods - 1)
    inflation_factors = (1.0 + inflation_rate) ** (months / 12.0)  # index by elapsed month
    prog_interval = max(1, (n_periods - 1) // 20)

    # Keep values comfortably inside float64 range to avoid inf propagation
    big_cap = np.finfo(float).max / 10.0

    for c in range(1, n_periods):
        if progress_fun is not None and (c % prog_interval == 0 or c == n_periods - 1):
            progress_fun(c / (n_periods - 1))

        month_idx = c - 1  # elapsed whole months at the start of this step
        age_start = current_age + month_idx / 12.0
        port_totals = current_vals.sum(axis=1)

        zero_mask = port_totals < ZERO_FLOOR
        if zero_mask.any():
            current_vals[zero_mask, :] = 0.0
            port_totals[zero_mask] = 0.0

        if age_start >= retirement_age:
            # Drawdown: withdraw from overweight assets first
            monthly_withdrawal = annual_expenses * inflation_factors[month_idx] / 12.0
            monthly_withdrawal = min(monthly_withdrawal, big_cap)

            target_dollars = port_totals[:, None] * target_weights[None, :]
            overweight_amts = np.maximum(current_vals - target_dollars, 0.0)
            sum_overweight = overweight_amts.sum(axis=1)

            take_from_overweight = np.minimum(monthly_withdrawal, sum_overweight)
            take_from_all = np.maximum(monthly_withdrawal - sum_overweight, 0.0)

            over_ratio = np.zeros(num_simulations)
            idx_over = sum_overweight > 1e-12
            over_ratio[idx_over] = take_from_overweight[idx_over] / sum_overweight[idx_over]

            overweight_withdrawal = overweight_amts * over_ratio[:, None]
            remaining_vals = np.maximum(current_vals - overweight_withdrawal, 0.0)

            sum_remaining = remaining_vals.sum(axis=1)
            all_ratio = np.zeros(num_simulations)
            idx_rem = sum_remaining > 1e-12
            all_ratio[idx_rem] = np.minimum(take_from_all[idx_rem] / sum_remaining[idx_rem], 1.0)

            all_withdrawal = remaining_vals * all_ratio[:, None]
            current_vals = np.maximum(remaining_vals - all_withdrawal, 0.0)
        else:
            # Accumulation: route contributions to underweight assets
            contrib_amount = 0.0
            if contrib_freq == 1:
                if month_idx % 12 == 0:
                    contrib_amount = total_contribution
            elif contrib_freq == 12:
                contrib_amount = total_contribution
            elif contrib_freq == 26:
                contrib_amount = (
                    np.floor((month_idx + 1) * 26 / 12) - np.floor(month_idx * 26 / 12)
                ) * total_contribution

            if contrib_amount > 0 and inflate_contrib:
                contrib_amount = contrib_amount * inflation_factors[month_idx]

            if contrib_amount > 0:
                target_dollars = (port_totals + contrib_amount)[:, None] * target_weights[None, :]
                gaps = np.maximum(target_dollars - current_vals, 0.0)
                sum_gaps = gaps.sum(axis=1)

                alloc_weights = np.zeros((num_simulations, n_etfs))
                needs_fix = sum_gaps > 1e-12
                if needs_fix.any():
                    alloc_weights[needs_fix, :] = gaps[needs_fix, :] / sum_gaps[needs_fix, None]
                if (~needs_fix).any():
                    alloc_weights[~needs_fix, :] = target_weights[None, :]

                current_vals = current_vals + contrib_amount * alloc_weights

        # Correlated GBM shock
        Z = np.zeros((num_simulations, n_etfs))
        for r in range(3):
            mask = regime_vec == r
            n_r = int(mask.sum())
            if n_r > 0:
                Z[mask, :] = rng.standard_normal((n_r, n_etfs)) @ chol_list[r]

        log_ret = regime_drift_dt[regime_vec, :] + regime_sigma_sqrt_dt[regime_vec, :] * Z
        growth_factors = np.exp(np.clip(log_ret, -50, 50))
        current_vals = np.minimum(np.maximum(current_vals * growth_factors, 0.0), big_cap)

        portfolio_values[:, c] = current_vals.sum(axis=1)

        # Regime transition (inverse-CDF)
        u_t = rng.random(num_simulations)
        next_regime = np.full(num_simulations, 2, dtype=int)
        for r in range(3):
            mask = regime_vec == r
            if not mask.any():
                continue
            u_r = u_t[mask]
            nr = np.full(u_r.shape, 2, dtype=int)
            nr[u_r <= cum_trans[r, 1]] = 1
            nr[u_r <= cum_trans[r, 0]] = 0
            next_regime[mask] = nr
        regime_vec = next_regime

    # Summary statistics
    median = np.median(portfolio_values, axis=0)
    p10 = np.quantile(portfolio_values, 0.10, axis=0)
    p90 = np.quantile(portfolio_values, 0.90, axis=0)
    deflator = (1.0 + inflation_rate) ** years
    median_real = median / deflator
    p10_real = p10 / deflator
    p90_real = p90 / deflator
    survival_bool = portfolio_values > SURVIVAL_THRESHOLD
    survival_prob = survival_bool.mean(axis=0)

    return SimulationResult(
        ages=current_age + years,
        years=years,
        median=median,
        median_real=median_real,
        p10=p10,
        p90=p90,
        p10_real=p10_real,
        p90_real=p90_real,
        survival_prob=survival_prob,
        num_simulations=num_simulations,
        simulations=portfolio_values if keep_paths else None,
        survival_matrix=survival_bool if keep_paths else None,
    )


# Convenience: build correlation matrices + Cholesky factors from a base corr
def build_chol_list(corr_base: np.ndarray, bear_surge: float, crisis_surge: float):
    """
    Returns (chol_list, notes) where chol_list = [Bull, Bear, Crisis] upper
    Cholesky factors and notes lists any matrices adjusted to nearest-PD
    """
    corr_base = np.array(corr_base, dtype=float, copy=True)
    n = corr_base.shape[0]
    if corr_base.shape != (n, n):
        raise ValueError("corr_base must be a square matrix.")

    def surge(c, s):
        out = c + (1 - c) * s
        np.fill_diagonal(out, 1.0)
        return out

    corr_bear = surge(corr_base, bear_surge)
    corr_crisis = surge(corr_base, crisis_surge)

    notes = []
    cb, adj = make_pd(corr_base);   notes += (["Base"] if adj else [])
    cr, adj = make_pd(corr_bear);   notes += (["Bear"] if adj else [])
    cc, adj = make_pd(corr_crisis); notes += (["Crisis"] if adj else [])

    return [robust_chol(cb), robust_chol(cr), robust_chol(cc)], notes
