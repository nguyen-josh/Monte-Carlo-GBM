"""
Portfolio Simulator — Streamlit front-end

Run with:
    streamlit run app.py
"""
from __future__ import annotations

import numpy as np
import pandas as pd
import plotly.graph_objects as go
import streamlit as st

import engine as E

WEIGHT_TOL_PCT = 1.0

st.set_page_config(page_title="Portfolio Simulator", layout="wide",
                   page_icon="📈")

# Asset-row defaults mirroring the original UI.
DEFAULTS = [
    dict(name="US Equity",   bal=100000, w=None, drag=0.5, exp=0.10,
         br=10, bv=15, ber=-3, bev=22, cr=-35, cv=35),
    dict(name="Intl Equity", bal=50000,  w=None, drag=0.5, exp=0.10,
         br=9,  bv=17, ber=-4, bev=24, cr=-40, cv=40),
    dict(name="Bonds",       bal=25000,  w=None, drag=0.5, exp=0.05,
         br=4,  bv=5,  ber=2,  bev=7,  cr=-5,  cv=10),
]


def asset_default(i, key):
    if i < len(DEFAULTS):
        return DEFAULTS[i][key]
    fallback = dict(name=f"Asset {i+1}", bal=10000, drag=0.5, exp=0.10,
                    br=5, bv=10, ber=-5, bev=15, cr=-20, cv=30)
    return fallback[key]


# Sidebar navigation
page = st.sidebar.radio("Navigate",
                        ["Portfolio Setup", "Simulation Results", "Help & Documentation"])

if "results" not in st.session_state:
    st.session_state.results = None
    st.session_state.run_meta = {}


# PAGE 1 — Portfolio Setup
if page == "Portfolio Setup":
    st.title("📈 Portfolio Simulator — Setup")

    st.subheader("Life & Expenses")
    c = st.columns(6)
    current_age = c[0].number_input("Current Age", 18, 100, 30)
    retirement_age = c[1].number_input("Target Retire Age", 18, 100, 65)
    target_survival_age = c[2].number_input("Target Survival Age", 50, 120, 90)
    annual_expenses = c[3].number_input("Retire Expenses (Today's $)", 0, value=60000, step=1000)
    inflation_rate = c[4].number_input("Expected Inflation (%)", 0.0, 100.0, 3.0, 0.1)
    num_simulations = c[5].number_input("Simulations", 10, 20000, 1000, 100)

    c = st.columns(4)
    total_contribution = c[0].number_input("Amount per Contribution ($)", 0, value=1500)
    contrib_label = c[1].selectbox("Contribution Frequency",
                                   ["Biweekly", "Monthly", "Annual"], index=1)
    contrib_freq = {"Biweekly": 26, "Monthly": 12, "Annual": 1}[contrib_label]
    inflate_contrib = c[2].checkbox("Index Contributions to Inflation", True)
    c[3].caption("Contributions are actively directed toward underweight assets "
                 "to control drift without forced liquidation.")
    seed_on = st.checkbox("Use fixed random seed (reproducible runs)", False)
    seed_val = st.number_input("Seed", 0, value=42, disabled=not seed_on)

    st.divider()
    st.subheader("ETF Holdings, Allocations & Regime Profiles")
    n_etfs = st.number_input("Number of Portfolio Assets", 1, 10, 3)

    assets = []
    for i in range(int(n_etfs)):
        with st.container(border=True):
            r1 = st.columns([3, 3, 2, 2, 2])
            name = r1[0].text_input(f"Asset {i+1} Name", asset_default(i, "name"),
                                    key=f"name_{i}")
            bal = r1[1].number_input("Principal ($)", 0, value=int(asset_default(i, "bal")),
                                     key=f"bal_{i}")
            # Default weights sum to exactly 100%: the last asset absorbs the
            # rounding remainder (avoids 33.3*3 = 99.9 self-rejection bug)
            _base_w = round(100.0 / int(n_etfs), 1)
            _default_w = (round(100.0 - _base_w * (int(n_etfs) - 1), 1)
                          if i == int(n_etfs) - 1 else _base_w)
            w = r1[2].number_input("Target Weight (%)", 0.0, 100.0,
                                   _default_w, key=f"w_{i}")
            drag = r1[3].number_input("Tax Drag (%)", 0.0, 5.0, float(asset_default(i, "drag")),
                                      0.1, key=f"drag_{i}")
            exp = r1[4].number_input("Exp Ratio (%)", 0.0, 5.0, float(asset_default(i, "exp")),
                                     0.01, key=f"exp_{i}")
            r2 = st.columns(6)
            br = r2[0].number_input("Bull Ret (%)", value=float(asset_default(i, "br")),
                                    step=0.5, key=f"br_{i}")
            bv = r2[1].number_input("Bull Vol (%)", 0.0, value=float(asset_default(i, "bv")),
                                    step=0.5, key=f"bv_{i}")
            ber = r2[2].number_input("Bear Ret (%)", value=float(asset_default(i, "ber")),
                                     step=0.5, key=f"ber_{i}")
            bev = r2[3].number_input("Bear Vol (%)", 0.0, value=float(asset_default(i, "bev")),
                                     step=0.5, key=f"bev_{i}")
            crr = r2[4].number_input("Crisis Ret (%)", value=float(asset_default(i, "cr")),
                                     step=1.0, key=f"cr_{i}")
            cv = r2[5].number_input("Crisis Vol (%)", 0.0, value=float(asset_default(i, "cv")),
                                    step=0.5, key=f"cv_{i}")
            assets.append(dict(name=name, bal=bal, w=w, drag=drag, exp=exp,
                               br=br, bv=bv, ber=ber, bev=bev, cr=crr, cv=cv))

    wt_sum_live = sum(a["w"] for a in assets)
    if abs(wt_sum_live - 100) <= WEIGHT_TOL_PCT:
        st.success(f"Target weights sum to {wt_sum_live:.1f}%.")
    else:
        st.warning(f"Target weights currently sum to {wt_sum_live:.1f}% "
                   f"(they must total 100% ± {WEIGHT_TOL_PCT:.0f}% before running).")

    st.divider()
    st.subheader("Correlation Matrix & Crisis Dynamics")
    cc = st.columns([3, 1, 1])
    corr = np.eye(int(n_etfs))
    with cc[0]:
        st.markdown("**Pairwise correlations**")
        for i in range(int(n_etfs)):
            for j in range(i + 1, int(n_etfs)):
                default = {(0, 1): 0.7, (0, 2): 0.1, (1, 2): 0.1}.get((i, j), 0.5)
                val = st.number_input(
                    f"corr({assets[i]['name']}, {assets[j]['name']})",
                    -1.0, 1.0, float(default), 0.01, key=f"corr_{i}_{j}")
                corr[i, j] = corr[j, i] = val
    bear_surge = cc[1].number_input("Bear Correlation Surge", 0.0, 1.0, 0.15, 0.05)
    crisis_surge = cc[2].number_input("Crisis Correlation Surge", 0.0, 1.0, 0.50, 0.05)
    st.caption("Surge controls how much cross-asset correlations spike toward 1.0 "
               "during Bear and Crisis regimes. 0 = no change, 1 = perfect correlation.")

    st.divider()
    st.subheader("MONTHLY Regime Transition Probabilities")
    st.caption("These probabilities govern month-to-month state changes.")
    t = st.columns(4)
    with t[0]:
        st.markdown("**From Bull**")
        bull_bear = st.number_input("Bull → Bear (%)", 0.0, 50.0, 1.5, 0.1)
        bull_crisis = st.number_input("Bull → Crisis (%)", 0.0, 20.0, 0.1, 0.1)
    with t[1]:
        st.markdown("**From Bear**")
        bear_bull = st.number_input("Bear → Bull (%)", 0.0, 80.0, 5.0, 0.5)
        bear_crisis = st.number_input("Bear → Crisis (%)", 0.0, 30.0, 1.0, 0.5)
    with t[2]:
        st.markdown("**From Crisis**")
        crisis_bull = st.number_input("Crisis → Bull (%)", 0.0, 80.0, 8.0, 1.0)
        crisis_bear = st.number_input("Crisis → Bear (%)", 0.0, 80.0, 12.0, 1.0)
    with t[3]:
        st.markdown("**Initialization & Stats**")
        regime_label = st.selectbox(
            "Starting Market Regime",
            ["Draw from Stationary Dist.", "Bull Market", "Bear Market", "Crisis"])
        init_regime = ["Draw from Stationary Dist.", "Bull Market",
                       "Bear Market", "Crisis"].index(regime_label)

    rp = E.RegimeParams(
        pBB=bull_bear / 100, pBC=bull_crisis / 100,
        pRB=bear_bull / 100, pRC=bear_crisis / 100,
        pCB=crisis_bull / 100, pCR=crisis_bear / 100,
    )
    # Live regime statistics panel
    if (rp.pBB + rp.pBC) > 1 or (rp.pRB + rp.pRC) > 1 or (rp.pCB + rp.pCR) > 1:
        t[3].error("Probabilities exceed 100%.")
    else:
        def dur(s):
            return f"{round(1/s)} mos" if s > 0 else "Inf"
        msg = (f"Avg Duration:\n  Bull: {dur(rp.pBB+rp.pBC)}\n"
               f"  Bear: {dur(rp.pRB+rp.pRC)}\n  Crisis: {dur(rp.pCB+rp.pCR)}")
        chk = E.validate_transition_matrix(rp.matrix())
        if chk.ok:
            pi = E.compute_stationary(rp.matrix())
            msg += (f"\nStationary Dist:\n  Bull: {round(pi[0]*100,1)}%\n"
                    f"  Bear: {round(pi[1]*100,1)}%\n  Crisis: {round(pi[2]*100,1)}%")
        else:
            msg += "\nReducible chain: No unique stationary dist."
        t[3].text(msg)

    st.divider()
    if st.button("▶ Run Simulation", type="primary"):
        errors = []
        if retirement_age < current_age:
            errors.append("Target Retirement Age cannot be less than Current Age.")
        if target_survival_age < retirement_age:
            errors.append("Target Survival Age must be ≥ Retirement Age.")

        for i, a in enumerate(assets):
            def net(r, exp=a["exp"]):
                return (r / 100) - (exp / 100)
            if net(a["br"]) <= -0.99 or net(a["ber"]) <= -0.99 or net(a["cr"]) <= -0.99:
                errors.append(f"Arithmetic return minus expense ratio for Asset {i+1} "
                              "must be strictly greater than -99%.")

        wt_sum = sum(a["w"] for a in assets)
        if wt_sum <= 0:
            errors.append("Target weights must sum to more than 0%.")
        elif abs(wt_sum - 100) > WEIGHT_TOL_PCT:
            errors.append(f"Target weights must sum to 100% (±{WEIGHT_TOL_PCT:.0f}%); "
                          f"they currently total {wt_sum:.1f}%.")

        if (rp.pBB + rp.pBC) > 1 or (rp.pRB + rp.pRC) > 1 or (rp.pCB + rp.pCR) > 1:
            errors.append("Transition probabilities from a single regime cannot exceed 100%.")
        tm_check = E.validate_transition_matrix(rp.matrix())
        if not tm_check.ok:
            errors.append(tm_check.message)

        if errors:
            for e in errors:
                st.error(f"Error: {e}")
        else:
            bar = st.progress(0.0, "Simulating Lifecycles...")
            try:
                etf = E.ETFData(
                    initial_balance=[a["bal"] for a in assets],
                    target_weight=[a["w"] for a in assets],
                    tax_drag=[a["drag"] / 100 for a in assets],
                    expense_ratio=[a["exp"] / 100 for a in assets],
                    bull_ret=[a["br"] / 100 for a in assets],
                    bull_vol=[a["bv"] / 100 for a in assets],
                    bear_ret=[a["ber"] / 100 for a in assets],
                    bear_vol=[a["bev"] / 100 for a in assets],
                    crisis_ret=[a["cr"] / 100 for a in assets],
                    crisis_vol=[a["cv"] / 100 for a in assets],
                )
                tw = etf.target_weight / etf.target_weight.sum()
                chol_list, notes = E.build_chol_list(corr, bear_surge, crisis_surge)
                for nm in notes:
                    st.warning(f"Adjusted the {nm} correlation matrix to the nearest "
                               "positive-definite correlation matrix via Higham's algorithm.")

                horizon = max(0.1, target_survival_age - current_age)
                res = E.run_monte_carlo(
                    etf, tw, chol_list, current_age, retirement_age,
                    annual_expenses, inflation_rate / 100, total_contribution,
                    contrib_freq, inflate_contrib,
                    int(max(10, min(20000, num_simulations))), horizon, rp,
                    init_regime, progress_fun=lambda v: bar.progress(min(1.0, v)),
                    seed=int(seed_val) if seed_on else None,
                )
            except (ValueError, np.linalg.LinAlgError) as exc:
                bar.empty()
                st.error(f"Error: {exc}")
            except Exception as exc:  # pragma: no cover - last-resort guard
                bar.empty()
                st.error(f"Unexpected error while running the simulation: {exc}")
            else:
                bar.progress(1.0)
                st.session_state.results = res
                st.session_state.run_meta = dict(
                    target_survival_age=target_survival_age, current_age=current_age,
                    retirement_age=retirement_age,
                    start_value=sum(a["bal"] for a in assets),
                )
                st.success("Simulation complete! Open **Simulation Results**.")


# PAGE 2 — Results
elif page == "Simulation Results":
    st.title("Simulation Results")
    res = st.session_state.results
    if res is None:
        st.info("Run a simulation from the **Portfolio Setup** tab to see results.")
    else:
        m = st.session_state.run_meta
        idx = int(np.argmin(np.abs(res.ages - m["target_survival_age"])))
        actual_age = round(res.ages[idx])

        k = st.columns(3)
        rate = round(float(res.survival_prob[idx]) * 100, 1)
        k[0].metric(f"Survival Rate (Funds > $0) at Age {actual_age}", f"{rate}%")
        med_real = res.median_real[idx]
        k[1].metric(f"Median Real Balance at Age {actual_age}",
                    f"${med_real:,.0f}")
        k[2].metric("Starting Portfolio", f"${m['start_value']:,.0f}")
        st.caption(f"Based on {res.num_simulations:,} simulated lifecycles.")

        # Lifecycle projection
        keep = sorted(set(list(range(0, len(res.ages), 12)) + [len(res.ages) - 1]))
        band = st.radio("Percentile band (10th–90th)", ["Nominal", "Real (today's $)"],
                        index=0, horizontal=True)
        p10_series = res.p10_real if band.startswith("Real") else res.p10
        p90_series = res.p90_real if band.startswith("Real") else res.p90
        fig = go.Figure()
        fig.add_scatter(x=res.ages[keep], y=res.median[keep], name="Median (Nominal)",
                        line=dict(color="blue", width=2))
        fig.add_scatter(x=res.ages[keep], y=res.median_real[keep], name="Median (Real)",
                        line=dict(color="green", width=2))
        fig.add_scatter(x=res.ages[keep], y=p10_series[keep],
                        name=f"10th Percentile ({band})",
                        line=dict(color="red", width=1, dash="dot"))
        fig.add_scatter(x=res.ages[keep], y=p90_series[keep],
                        name=f"90th Percentile ({band})",
                        line=dict(color="orange", width=1, dash="dot"))
        ymax = max(float(np.nanmax(p90_series[keep])), float(m["start_value"]), 1.0)
        fig.add_shape(type="line", x0=m["retirement_age"], x1=m["retirement_age"],
                      y0=0, y1=ymax, line=dict(color="purple", dash="dash"))
        fig.add_annotation(x=m["retirement_age"], y=ymax, text="Retirement Starts",
                           showarrow=False, yshift=10, font=dict(color="purple"))
        fig.update_layout(title="Lifecycle Value Projection", xaxis_title="Age",
                          yaxis_title="Portfolio Value ($)", hovermode="x unified",
                          yaxis_tickformat=",.0f")
        st.plotly_chart(fig, width='stretch')

        # Survival probability
        probs = res.survival_prob * 100
        fig2 = go.Figure()
        fig2.add_scatter(x=res.ages[keep], y=probs[keep], mode="lines+markers",
                         line=dict(color="teal", width=2))
        fig2.update_layout(title="Probability Portfolio is Greater Than $0",
                           xaxis_title="Age", yaxis_title="Survival Probability (%)",
                           yaxis_range=[0, 100], showlegend=False)
        st.plotly_chart(fig2, width='stretch')

        # Statistics table
        st.subheader("Detailed Lifecycle Statistics")
        rows = []
        for ag in range(int(m["current_age"]), int(m["target_survival_age"]) + 1, 5):
            j = int(np.argmin(np.abs(res.ages - ag)))
            if abs(res.ages[j] - ag) > (1 / 24):
                continue
            phase = "Accumulation" if ag < m["retirement_age"] else "Drawdown"
            rows.append({
                "Age": ag, "Phase": phase,
                "Median Nominal": f"${res.median[j]:,.0f}",
                "Median Real (Today's $)": f"${res.median_real[j]:,.0f}",
                "Survival Rate (> $0)": f"{round(float(res.survival_prob[j])*100,1)}%",
                "10th Percentile": f"${res.p10[j]:,.0f}",
                "90th Percentile": f"${res.p90[j]:,.0f}",
            })
        stats_df = pd.DataFrame(rows)
        st.dataframe(stats_df, hide_index=True, width='stretch')

        # Full per-period export (one row per simulated month).
        export_df = pd.DataFrame({
            "Age": res.ages,
            "Year": res.years,
            "Median_Nominal": res.median,
            "Median_Real": res.median_real,
            "P10_Nominal": res.p10,
            "P90_Nominal": res.p90,
            "P10_Real": res.p10_real,
            "P90_Real": res.p90_real,
            "Survival_Probability": res.survival_prob,
        })
        st.download_button(
            "⬇ Download full results (CSV)",
            data=export_df.to_csv(index=False).encode("utf-8"),
            file_name="portfolio_simulation_results.csv",
            mime="text/csv",
        )


# PAGE 3 — Help
else:
    st.title("Methodology Documentation")
    st.write("This simulator runs a natively simulated Markov Regime-Switching environment.")
    st.markdown(
        "- **Inflation-Indexed Accumulation:** If enabled, contributions scale with "
        "expected inflation so their real allocation power is preserved over decades.\n"
        "- **Zero-Liquidation Rebalancing:** During accumulation, new contributions buy "
        "underweighted assets; overweighted assets are never liquidated, avoiding "
        "unnecessary capital gains. If contributions are insufficient to hit target "
        "weights, the portfolio is allowed to drift. During drawdown, withdrawals target "
        "overweighted assets first. Cost-basis is not modeled.\n"
        "- **Stochastic Calculus (GBM):** Asset returns follow a regime-specific geometric "
        "Brownian motion. Arithmetic return and volatility inputs are transformed into "
        "continuous log-normal drift and diffusion parameters via "
        "dSₜ = μ Sₜ dt + σ Sₜ dWₜ.\n"
        "- **Liquidity Crunch Profiles:** During Bear and Crisis periods the correlation "
        "matrix shifts toward 1.0 (diversification breakdown). Non-positive-definite "
        "states are projected to the nearest valid correlation matrix via Higham's algorithm."
    )
