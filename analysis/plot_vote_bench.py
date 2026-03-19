#!/usr/bin/env python3
"""
Visualise criterion benchmark results for NPV (non-persistent vote) operations.

Usage:
    python analysis/plot_bench.py bench-report.csv
    python analysis/plot_bench.py bench-report.csv --outdir results/
"""

import argparse
import os
import re
import sys

import matplotlib.pyplot as plt
import pandas as pd

# ---------------------------------------------------------------------------
# Parsing
# ---------------------------------------------------------------------------

def parse_name(name: str) -> dict | None:
    """Return a dict of fields for NPV rows, None for anything else."""
    parts = name.split("/")
    if parts[0] != "npv" or len(parts) < 5:
        return None
    m_n2 = re.match(r"n2=(\d+)", parts[2])
    m_sigma = re.match(r"sigma=(.+)", parts[3])
    if not m_n2 or not m_sigma:
        return None
    return {
        "n2": int(m_n2.group(1)),
        "sigma": float(m_sigma.group(1)),
        "operation": parts[4],
    }


def load(csv_path: str) -> pd.DataFrame:
    raw = pd.read_csv(csv_path)
    records = []
    for _, row in raw.iterrows():
        parsed = parse_name(row["Name"])
        if parsed is None:
            continue
        parsed["mean_us"] = row["Mean"] * 1e6
        parsed["lb_us"] = row["MeanLB"] * 1e6
        parsed["ub_us"] = row["MeanUB"] * 1e6
        parsed["lambda_"] = parsed["sigma"] * parsed["n2"]
        records.append(parsed)
    return pd.DataFrame(records)


# ---------------------------------------------------------------------------
# Shared helpers
# ---------------------------------------------------------------------------

OPERATIONS = ["create-win", "create-lose", "verify", "sortition-check"]
TITLES = {
    "create-win": "create-win",
    "create-lose": "create-lose",
    "verify": "verify",
    "sortition-check": "sortition-check",
}


def _make_fig():
    return plt.subplots(2, 2, figsize=(12, 9))


def _ax_for(axes, i: int):
    return axes[i // 2][i % 2]


def _save(fig, path: str):
    fig.tight_layout()
    fig.savefig(path, dpi=150)
    plt.close(fig)
    print(f"  wrote {path}")


def _add_legend(fig, handles, labels, title: str):
    # deduplicate while preserving order
    seen = {}
    for h, l in zip(handles, labels):
        if l not in seen:
            seen[l] = h
    fig.legend(
        seen.values(),
        seen.keys(),
        title=title,
        loc="upper right",
        bbox_to_anchor=(1.0, 1.0),
        framealpha=0.8,
    )


# ---------------------------------------------------------------------------
# Figure 1 — mean time vs n2, one line per σ
# ---------------------------------------------------------------------------

def plot_vs_n2(df: pd.DataFrame, outdir: str):
    sigmas = sorted(df["sigma"].unique())
    cmap = plt.get_cmap("tab10")
    colors = {s: cmap(i) for i, s in enumerate(sigmas)}

    fig, axes = _make_fig()
    fig.suptitle("NPV timing vs n2 (one line per σ)", fontsize=13)
    all_handles, all_labels = [], []

    for idx, op in enumerate(OPERATIONS):
        ax = _ax_for(axes, idx)
        sub = df[df["operation"] == op].sort_values("n2")
        for sigma, grp in sub.groupby("sigma"):
            grp = grp.sort_values("n2")
            color = colors[sigma]
            label = f"σ={sigma:.3g}"
            (line,) = ax.plot(grp["n2"], grp["mean_us"], marker="o", color=color, label=label)
            ax.fill_between(grp["n2"], grp["lb_us"], grp["ub_us"], alpha=0.15, color=color)
            all_handles.append(line)
            all_labels.append(label)
        ax.set_title(TITLES[op])
        ax.set_xlabel("n2")
        ax.set_ylabel("time (μs)")

    _add_legend(fig, all_handles, all_labels, "σ")
    _save(fig, os.path.join(outdir, "npv_vs_n2.png"))


# ---------------------------------------------------------------------------
# Figure 2 — mean time vs σ (log scale), one line per n2
# ---------------------------------------------------------------------------

def plot_vs_sigma(df: pd.DataFrame, outdir: str):
    n2_vals = sorted(df["n2"].unique())
    cmap = plt.get_cmap("tab10")
    colors = {n: cmap(i) for i, n in enumerate(n2_vals)}

    fig, axes = _make_fig()
    fig.suptitle("NPV timing vs σ (one line per n2)", fontsize=13)
    all_handles, all_labels = [], []

    for idx, op in enumerate(OPERATIONS):
        ax = _ax_for(axes, idx)
        sub = df[df["operation"] == op].sort_values("sigma")
        for n2, grp in sub.groupby("n2"):
            grp = grp.sort_values("sigma")
            color = colors[n2]
            label = f"n2={n2}"
            (line,) = ax.plot(grp["sigma"], grp["mean_us"], marker="o", color=color, label=label)
            ax.fill_between(grp["sigma"], grp["lb_us"], grp["ub_us"], alpha=0.15, color=color)
            all_handles.append(line)
            all_labels.append(label)
        ax.set_xscale("log")
        ax.set_title(TITLES[op])
        ax.set_xlabel("σ")
        ax.set_ylabel("time (μs)")

    _add_legend(fig, all_handles, all_labels, "n2")
    _save(fig, os.path.join(outdir, "npv_vs_sigma.png"))


# ---------------------------------------------------------------------------
# Figure 3 — mean time vs λ = σ×n2, coloured by σ
# If timing collapses onto one curve, λ is the sole predictor.
# ---------------------------------------------------------------------------

def plot_vs_lambda(df: pd.DataFrame, outdir: str):
    sigmas = sorted(df["sigma"].unique())
    cmap = plt.get_cmap("tab10")
    colors = {s: cmap(i) for i, s in enumerate(sigmas)}

    fig, axes = _make_fig()
    fig.suptitle("NPV timing vs λ = σ×n2 (collapse check)", fontsize=13)
    all_handles, all_labels = [], []

    for idx, op in enumerate(OPERATIONS):
        ax = _ax_for(axes, idx)
        sub = df[df["operation"] == op].sort_values("lambda_")
        for sigma, grp in sub.groupby("sigma"):
            grp = grp.sort_values("lambda_")
            color = colors[sigma]
            label = f"σ={sigma:.3g}"
            (line,) = ax.plot(grp["lambda_"], grp["mean_us"], marker="o", color=color, label=label)
            ax.fill_between(grp["lambda_"], grp["lb_us"], grp["ub_us"], alpha=0.15, color=color)
            all_handles.append(line)
            all_labels.append(label)
        ax.set_xscale("log")
        ax.set_title(TITLES[op])
        ax.set_xlabel("λ = σ × n2")
        ax.set_ylabel("time (μs)")

    _add_legend(fig, all_handles, all_labels, "σ")
    _save(fig, os.path.join(outdir, "npv_vs_lambda.png"))


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("csv", help="criterion --csv output file")
    parser.add_argument("--outdir", default=None, help="output directory (default: same as csv)")
    args = parser.parse_args()

    outdir = args.outdir or os.path.dirname(os.path.abspath(args.csv))
    os.makedirs(outdir, exist_ok=True)

    df = load(args.csv)
    if df.empty:
        print("ERROR: no NPV rows found in CSV", file=sys.stderr)
        sys.exit(1)

    print(f"Loaded {len(df)} NPV rows — operations: {sorted(df['operation'].unique())}")
    print(f"n2 values : {sorted(df['n2'].unique())}")
    print(f"σ  values : {sorted(df['sigma'].unique())}")
    print(f"Output dir: {outdir}")

    plot_vs_n2(df, outdir)
    plot_vs_sigma(df, outdir)
    plot_vs_lambda(df, outdir)


if __name__ == "__main__":
    main()
