import csv
import matplotlib.pyplot as plt

path = "stake.csv"

persistent = None
committee = None
ranks = []
stakes = []

with open(path, "r") as f:
    # Read metadata lines
    header = None
    while True:
        line = f.readline()
        if not line:
            break
        if line.startswith("#persistent="):
            persistent = int(line.strip().split("=", 1)[1])
        elif line.startswith("#committee="):
            committee = int(line.strip().split("=", 1)[1])
        elif line.startswith("#"):
            pass
        else:
            header = line
            break

    if header is None:
        raise RuntimeError("CSV missing header line")

    reader = csv.DictReader([header] + f.readlines())
    for row in reader:
        ranks.append(int(row["rank"]))
        stakes.append(100.0 * float(row["stake"]))  # stake as % of total

fig, ax = plt.subplots()

ax.plot(ranks, stakes)
ax.set_title("Stake distribution (ordered)")
ax.set_xlabel("Pool rank (1 = largest)")
ax.set_ylabel("Stake (% of total)")

# Persistent boundary (line)
if persistent is not None:
    ax.axvline(persistent, linestyle="--")
    # label near top-left of the line
    ymax = max(stakes) if stakes else 1.0
    ax.text(persistent, ymax * 0.95, f"persistent = {persistent}",
            rotation=90, va="top", ha="right")

# Committee + non-persistent as horizontal text (no line)
text_lines = []
if committee is not None:
    text_lines.append(f"committee size: {committee}")
if persistent is not None and committee is not None:
    non_persistent = committee - persistent
    text_lines.append(f"non-persistent voters: {non_persistent}")

    # Optional sanity: if negative, flag it
    if non_persistent < 0:
        text_lines.append("(warning: committee < persistent)")

if text_lines:
    ax.text(
        0.98, 0.98,
        "\n".join(text_lines),
        transform=ax.transAxes,
        ha="right", va="top",
        bbox=dict(boxstyle="round,pad=0.3", alpha=0.2)
    )

fig.tight_layout()
plt.show()
