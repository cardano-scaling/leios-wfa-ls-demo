#!/usr/bin/env python3
"""
Visualise criterion benchmark results for certificate (create / verify) operations.

Usage:
    python analysis/plot_cert_bench.py bench-report-cert.csv
    python analysis/plot_cert_bench.py bench-report-cert.csv --outdir results/
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
    """Return a dict of fields for cert rows, None for anything else."""
    parts = name.split("/")
    if parts[0] != "cert" or len(parts) < 4:
        return None
    group = parts[1]  # pv-sweep | npv-sweep | diagonal
    m = re.match(r"pv=(\d+),npv=(\d+)", parts[2])
    if not m:
        return None
    return {
        "group": group,
        "pv": int(m.group(1)),
        "npv": int(m.group(2)),
        "operation": parts[3],  # create | verify
    }


def load(csv_path: str) -> pd.DataFrame:
    raw = pd.read_csv(csv_path)
    records = []
    for _, row in raw.iterrows():
        parsed = parse_name(row["Name"])
        if parsed is None:
            continue
        parsed["mean_ms"] = row["Mean"] * 1e3
        parsed["lb_ms"] = row["MeanLB"] * 1e3
        parsed["ub_ms"] = row["MeanUB"] * 1e3
        records.append(parsed)
    return pd.DataFrame(records)


# ---------------------------------------------------------------------------
# Shared helpers
# ---------------------------------------------------------------------------

OPERATIONS = ["create", "verify"]
OP_COLORS = {"create": "steelblue", "verify": "darkorange"}


def _save(fig, path: str):
    fig.tight_layout()
    fig.savefig(path, dpi=150)
    plt.close(fig)
    print(f"  wrote {path}")


# ---------------------------------------------------------------------------
# Figure 1 — pv-sweep: time vs pv count (npv fixed at 0)
# ---------------------------------------------------------------------------


def plot_pv_sweep(df: pd.DataFrame, outdir: str):
    sub = df[df["group"] == "pv-sweep"]
    fig, axes = plt.subplots(1, 2, figsize=(11, 5))
    fig.suptitle("Certificate timing — pv-sweep  (npv = 0)", fontsize=13)

    for ax, op in zip(axes, OPERATIONS):
        data = sub[sub["operation"] == op].sort_values("pv")
        color = OP_COLORS[op]
        ax.plot(data["pv"], data["mean_ms"], marker="o", color=color)
        ax.fill_between(data["pv"], data["lb_ms"], data["ub_ms"], alpha=0.2, color=color)
        ax.set_title(op)
        ax.set_xlabel("pv count  (n₁)")
        ax.set_ylabel("time (ms)")

    _save(fig, os.path.join(outdir, "cert_pv_sweep.png"))


# ---------------------------------------------------------------------------
# Figure 2 — npv-sweep: time vs npv winner count (pv fixed at 1)
# ---------------------------------------------------------------------------


def plot_npv_sweep(df: pd.DataFrame, outdir: str):
    sub = df[df["group"] == "npv-sweep"]
    fig, axes = plt.subplots(1, 2, figsize=(11, 5))
    fig.suptitle("Certificate timing — npv-sweep  (pv = 1)", fontsize=13)

    for ax, op in zip(axes, OPERATIONS):
        data = sub[sub["operation"] == op].sort_values("npv")
        color = OP_COLORS[op]
        ax.plot(data["npv"], data["mean_ms"], marker="o", color=color)
        ax.fill_between(data["npv"], data["lb_ms"], data["ub_ms"], alpha=0.2, color=color)
        ax.set_title(op)
        ax.set_xlabel("npv winner count  (n₂ winners)")
        ax.set_ylabel("time (ms)")

    _save(fig, os.path.join(outdir, "cert_npv_sweep.png"))


# ---------------------------------------------------------------------------
# Figure 3 — diagonal: pv↑ as npv↓, both operations on one axis each
# ---------------------------------------------------------------------------


def plot_diagonal(df: pd.DataFrame, outdir: str):
    sub = df[df["group"] == "diagonal"].sort_values("pv")
    fig, axes = plt.subplots(1, 2, figsize=(11, 5))
    fig.suptitle("Certificate timing — diagonal  (pv ↑  as  npv ↓)", fontsize=13)

    for ax, op in zip(axes, OPERATIONS):
        data = sub[sub["operation"] == op].sort_values("pv")
        color = OP_COLORS[op]
        ax.plot(data["pv"], data["mean_ms"], marker="o", color=color)
        ax.fill_between(data["pv"], data["lb_ms"], data["ub_ms"], alpha=0.2, color=color)

        # Annotate the corresponding npv count on each data point
        for _, row in data.iterrows():
            ax.annotate(
                f"npv={int(row['npv'])}",
                xy=(row["pv"], row["mean_ms"]),
                xytext=(5, 4),
                textcoords="offset points",
                fontsize=7,
                color="dimgray",
            )

        ax.set_title(op)
        ax.set_xlabel("pv count  (n₁)")
        ax.set_ylabel("time (ms)")

    _save(fig, os.path.join(outdir, "cert_diagonal.png"))


# ---------------------------------------------------------------------------
# Figure 4 — combined: create cost per vote type (pv-sweep vs npv-sweep)
# Puts both sweeps on one axes to show the relative cost of aggregating
# a PV vote vs an NPV vote.
# ---------------------------------------------------------------------------


def plot_cost_comparison(df: pd.DataFrame, outdir: str):
    fig, axes = plt.subplots(1, 2, figsize=(11, 5))
    fig.suptitle(
        "Certificate create / verify cost:  PV-sweep vs NPV-sweep", fontsize=13
    )

    pv_data = df[(df["group"] == "pv-sweep")].copy()
    npv_data = df[(df["group"] == "npv-sweep")].copy()

    for ax, op in zip(axes, OPERATIONS):
        pv = pv_data[pv_data["operation"] == op].sort_values("pv")
        npv = npv_data[npv_data["operation"] == op].sort_values("npv")

        (l1,) = ax.plot(pv["pv"], pv["mean_ms"], marker="o", color="steelblue", label="pv-sweep (npv=0)")
        ax.fill_between(pv["pv"], pv["lb_ms"], pv["ub_ms"], alpha=0.15, color="steelblue")

        (l2,) = ax.plot(npv["npv"], npv["mean_ms"], marker="s", color="darkorange", label="npv-sweep (pv=1)")
        ax.fill_between(npv["npv"], npv["lb_ms"], npv["ub_ms"], alpha=0.15, color="darkorange")

        ax.set_title(op)
        ax.set_xlabel("vote count  (pv or npv)")
        ax.set_ylabel("time (ms)")
        ax.legend(handles=[l1, l2], fontsize=8)

    _save(fig, os.path.join(outdir, "cert_cost_comparison.png"))


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
        print("ERROR: no cert rows found in CSV", file=sys.stderr)
        sys.exit(1)

    groups = sorted(df["group"].unique())
    print(f"Loaded {len(df)} cert rows — groups: {groups}")
    print(f"Output dir: {outdir}")
    for g in groups:
        sub = df[df["group"] == g]
        pv_vals = sorted(sub["pv"].unique())
        npv_vals = sorted(sub["npv"].unique())
        print(f"  {g}: pv={pv_vals}  npv={npv_vals}")

    plot_pv_sweep(df, outdir)
    plot_npv_sweep(df, outdir)
    plot_diagonal(df, outdir)
    plot_cost_comparison(df, outdir)


if __name__ == "__main__":
    main()
