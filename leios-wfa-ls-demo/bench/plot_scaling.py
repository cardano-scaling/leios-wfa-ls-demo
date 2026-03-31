#!/usr/bin/env python3
"""Plot cert/synthetic/scaling benchmark results from criterion JSON output.

Usage:
    nix run .#leios-wfa-ls-demo-bench -- \\
        --match pattern "cert/synthetic/scaling" \\
        --json scaling.json
    python3 bench/plot_scaling.py scaling.json [output.png]

Outputs a PNG with two side-by-side plots (create / verify time vs n)
for Pareto(α=0.5) and linear stake distributions.
"""

import json
import re
import sys

import matplotlib.pyplot as plt


def load_results(path):
    with open(path) as f:
        data = json.load(f)

    # Criterion JSON is [tag, version, [reports...]]
    if isinstance(data, list) and len(data) == 3 and isinstance(data[0], str):
        data = data[2]

    results = {}
    for entry in data:
        # criterion JSON uses either 'reportName'+'reportAnalysis' or 'name'+'mean'
        name = entry.get("reportName") or entry.get("name", "")
        if not name:
            continue
        if "reportAnalysis" in entry:
            mean_s = entry["reportAnalysis"]["anMean"]["estPoint"]
        else:
            m = entry.get("mean", {})
            mean_s = m["estPoint"] if isinstance(m, dict) else m
        results[name] = mean_s
    return results


def extract_series(results, group):
    """Return (ns, create_ms, verify_ms) for a given group substring."""
    points = {}
    for name, mean_s in results.items():
        if group not in name:
            continue
        m = re.search(r"/n=(\d+)/(create|verify)$", name)
        if not m:
            continue
        n = int(m.group(1))
        op = m.group(2)
        points.setdefault(n, {})[op] = mean_s * 1_000  # s → ms

    ns = sorted(points)
    creates = [points[n].get("create", float("nan")) for n in ns]
    verifys = [points[n].get("verify", float("nan")) for n in ns]
    return ns, creates, verifys


def main():
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <criterion-json> [output.png]")
        sys.exit(1)

    results = load_results(sys.argv[1])
    out_path = sys.argv[2] if len(sys.argv) > 2 else "scaling.png"

    # Support both the 'scaling' and 'synthetic' benchmark groups
    group = "scaling" if any("scaling/pareto" in k for k in results) else "synthetic"
    pareto_ns, pareto_create, pareto_verify = extract_series(
        results, f"{group}/pareto"
    )
    linear_ns, linear_create, linear_verify = extract_series(
        results, f"{group}/linear"
    )

    fig, (ax_c, ax_v) = plt.subplots(1, 2, figsize=(12, 5), sharey=False)

    pareto_alpha = next((re.search(r"alpha=([\d.]+)", k).group(1) for k in results if "pareto" in k and re.search(r"alpha=", k)), "?")
    ax_c.plot(pareto_ns, pareto_create, "o-", label=f"Pareto (α={pareto_alpha})")
    ax_c.plot(linear_ns, linear_create, "s-", label="Linear")
    ax_c.set_xlabel("Number of pools (n)")
    ax_c.set_ylabel("Time (ms)")
    ax_c.set_title("Certificate creation")
    ax_c.legend()
    ax_c.grid(True, alpha=0.3)

    ax_v.plot(pareto_ns, pareto_verify, "o-", label=f"Pareto (α={pareto_alpha})")
    ax_v.plot(linear_ns, linear_verify, "s-", label="Linear")
    ax_v.set_xlabel("Number of pools (n)")
    ax_v.set_ylabel("Time (ms)")
    ax_v.set_title("Certificate verification")
    ax_v.legend()
    ax_v.grid(True, alpha=0.3)

    all_ns = sorted(set(pareto_ns + linear_ns))
    n_range = f"{min(all_ns)}–{max(all_ns)}" if all_ns else "?"
    fig.suptitle(
        f"Worst-case certificate benchmarks (all votes), {n_range} pools"
    )
    fig.tight_layout()
    fig.savefig(out_path, dpi=150)
    print(f"Saved {out_path}")


if __name__ == "__main__":
    main()
