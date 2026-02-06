# app.py
# WCI Dashboard (Streamlit + Plotly)
#
# Key design choices:
# - NO Plotly "selection" state (select_event=False) because iOS Safari + rerun + selection = pain.
# - Use streamlit-plotly-events for click capture.
# - Force Choropleth locationmode="ISO-3" to prevent "grey map" mismatches.
#
# Requirements (requirements.txt):
# streamlit
# pandas
# numpy
# plotly
# streamlit-plotly-events

from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, List, Optional, Tuple

import numpy as np
import pandas as pd
import plotly.graph_objs as go
import streamlit as st
from streamlit_plotly_events import plotly_events


# =========================================================
# Styling
# =========================================================

GBR_COLORSCALE = [
    [0.00, "#e5f5e0"],
    [0.30, "#a1d99b"],
    [0.55, "#2b8cbe"],
    [0.80, "#253494"],
    [1.00, "#d73027"],
]


# =========================================================
# Text cleanup + canonicalization
# =========================================================

_INVIS = ["\u200b", "\u200c", "\u200d", "\ufeff", "\u2060", "\u00a0"]
_PUNCT_TO_SPACE = [".", ",", "'", '"', "’", "(", ")", "-", "–", "—"]


def clean_country_string(x):
    if not isinstance(x, str):
        return x
    s = x
    for ch in _INVIS:
        s = s.replace(ch, "")
    return s.strip()


def canon(x: str) -> str:
    if x is None:
        return ""
    s = clean_country_string(str(x)).lower()
    for ch in _PUNCT_TO_SPACE:
        s = s.replace(ch, " ")
    return " ".join(s.split())


# =========================================================
# Data helpers
# =========================================================

def require_columns(df: pd.DataFrame, cols: List[str], name: str) -> None:
    missing = [c for c in cols if c not in df.columns]
    if missing:
        raise ValueError(f"Missing required columns in {name}: {missing}")


def to_numeric_frame(df: pd.DataFrame, exclude: str = "Country") -> pd.DataFrame:
    out = df.copy()
    for c in out.columns:
        if c == exclude:
            continue
        out[c] = pd.to_numeric(out[c], errors="coerce").fillna(0.0)
    return out


def column_totals(df_num: pd.DataFrame, exclude: str = "Country") -> Dict[str, float]:
    cols = [c for c in df_num.columns if c != exclude]
    return {c: float(df_num[c].sum()) for c in cols}


def row_totals(df_num: pd.DataFrame, exclude: str = "Country") -> List[float]:
    cols = [c for c in df_num.columns if c != exclude]
    return df_num[cols].sum(axis=1).astype(float).tolist()


def hover_format(metric_label: str) -> str:
    if metric_label in ("WCI per capita", "WCI per GDP"):
        return ".2e"
    if metric_label.startswith("Respondents"):
        return ".0f"
    return ".4f"


# =========================================================
# Data model
# =========================================================

@dataclass(frozen=True)
class AccusationStore:
    # mode -> numeric df
    acc_num: Dict[str, pd.DataFrame]
    # mode -> canon(country) -> row index (rows=accused case)
    row_lookup: Dict[str, Dict[str, int]]
    # mode -> totals over columns (cols=attributors case)
    col_totals: Dict[str, Dict[str, float]]
    # mode -> row totals (rows=attributors case)
    row_totals: Dict[str, List[float]]


# =========================================================
# Data loading
# =========================================================

@st.cache_data
def load_data() -> Tuple[pd.DataFrame, AccusationStore]:
    wci_path = "data/df_wci_with_respondents.csv"
    acc_nat_path = "data/accusations_nationality.csv"
    acc_res_path = "data/accusations_residence.csv"

    df_wci = pd.read_csv(wci_path)
    df_wci["Country"] = df_wci["Country"].apply(clean_country_string)

    required_cols = [
        "Country",
        "ISO3",
        "WCI",
        "WCI_per_capita",
        "WCI_per_GDP",
        "respondents_nat",
        "respondents_res",
    ]
    require_columns(df_wci, required_cols, "df_wci")

    # Ensure ISO3 is clean
    df_wci["ISO3"] = df_wci["ISO3"].astype(str).str.strip()

    acc_raw = {
        "By nationality": pd.read_csv(acc_nat_path),
        "By residence": pd.read_csv(acc_res_path),
    }

    for _, df in acc_raw.items():
        df["Country"] = df["Country"].apply(clean_country_string)
        df.columns = [clean_country_string(c) for c in df.columns]

    acc_num = {k: to_numeric_frame(df) for k, df in acc_raw.items()}
    row_lookup = {k: {canon(c): i for i, c in enumerate(df["Country"].tolist())} for k, df in acc_raw.items()}
    col_totals = {k: column_totals(df) for k, df in acc_num.items()}
    row_totals_ = {k: row_totals(df) for k, df in acc_num.items()}

    store = AccusationStore(
        acc_num=acc_num,
        row_lookup=row_lookup,
        col_totals=col_totals,
        row_totals=row_totals_,
    )
    return df_wci, store


# =========================================================
# Plot builders
# =========================================================

def build_map(
    df_wci: pd.DataFrame,
    metric_label: str,
    metric_col: str,
    *,
    is_mobile: bool,
) -> go.Figure:
    vals = pd.to_numeric(df_wci[metric_col], errors="coerce").fillna(0.0).astype(float).to_numpy()
    vmin, vmax = float(np.min(vals)), float(np.max(vals))
    if vmin == vmax:
        vmax = vmin + 1e-9

    fmt = hover_format(metric_label)

    fig = go.Figure()
    fig.add_trace(
        go.Choropleth(
            locationmode="ISO-3",  # critical: prevents grey map mismatches
            locations=df_wci["ISO3"].astype(str).tolist(),
            z=vals.tolist(),
            text=df_wci["Country"].astype(str).tolist(),
            customdata=df_wci["Country"].astype(str).tolist(),
            colorscale=GBR_COLORSCALE,
            zmin=vmin,
            zmax=vmax,
            marker_line_color="black",
            marker_line_width=0.3,
            hovertemplate="<b>%{text}</b><br>" + metric_label + f": %{{z:{fmt}}}<extra></extra>",
            showscale=not is_mobile,
        )
    )

    fig.update_geos(
        showcountries=True,
        countrycolor="black",
        projection_type="natural earth",
        bgcolor="#e6e8ff",
    )

    fig.update_layout(
        title="World Cybercrime Index - A collection of surveyed responses by cybercrime specialists",
        height=520 if is_mobile else 600,
        margin=dict(t=40, b=0, l=0, r=0) if is_mobile else dict(t=50, b=10, l=10, r=10),
        clickmode="event",  # NOT select
        dragmode=False,
        uirevision="keep",
    )
    return fig


def get_top_attributors(
    store: AccusationStore,
    accused_country: str,
    mode: str,
    *,
    top_n: int = 10,
) -> List[Tuple[str, float, float, float]]:
    """
    Orientation-aware:

    - If accused_country exists as a column (rows=attributors, cols=accused):
        take that column and normalize by row totals (attributor totals).

    - Else (rows=accused, cols=attributors):
        take that row and normalize by column totals (attributor totals).
    """
    df_num = store.acc_num[mode]
    cols = [c for c in df_num.columns if c != "Country"]

    key = canon(accused_country)

    # Preferred: accused appears as a column name
    accused_col = next((c for c in cols if canon(c) == key), None)
    out: List[Tuple[str, float, float, float]] = []

    if accused_col is not None:
        series = df_num[accused_col].astype(float).tolist()
        denoms = store.row_totals[mode]
        for i, count in enumerate(series):
            denom = float(denoms[i])
            if count > 0 and denom > 0:
                attributor = str(df_num.iloc[i]["Country"])
                out.append((attributor, count / denom, count, denom))
    else:
        row_i = store.row_lookup[mode].get(key)
        if row_i is None:
            return []

        row = df_num.iloc[row_i]
        totals = store.col_totals[mode]
        for attributor in cols:
            count = float(row[attributor])
            denom = float(totals.get(attributor, 0.0))
            if count > 0 and denom > 0:
                out.append((attributor, count / denom, count, denom))

    out.sort(key=lambda t: t[1], reverse=True)
    return out[:top_n]


def build_bar(
    items: List[Tuple[str, float, float, float]],
    accused_country: str,
    mode: str,
    *,
    is_mobile: bool,
) -> go.Figure:
    fig = go.Figure()

    if not items:
        fig.add_trace(
            go.Bar(
                orientation="h",
                x=[1.0],
                y=["No data"],
                customdata=[(0.0, 0.0)],
                hovertemplate="No attribution data<extra></extra>",
                marker=dict(color="#cccccc"),
            )
        )
        title = f"Who attributes {accused_country}? ({mode})"
    else:
        names = [t[0] for t in items][::-1]
        shares = [t[1] * 100.0 for t in items][::-1]
        counts = [t[2] for t in items][::-1]
        denoms = [t[3] for t in items][::-1]

        fig.add_trace(
            go.Bar(
                orientation="h",
                x=shares,
                y=names,
                customdata=list(zip(counts, denoms)),
                marker=dict(
                    color=shares,
                    colorscale=GBR_COLORSCALE,
                    reversescale=False,
                    colorbar=dict(title="%", tickformat=".0f") if not is_mobile else None,
                ),
                hovertemplate=(
                    "%{y}<br>"
                    "Share: %{x:.2f}%<br>"
                    "Count: %{customdata[0]:.0f} of %{customdata[1]:.0f} attributor attributions"
                    "<extra></extra>"
                ),
            )
        )
        title = f"Who attributes {accused_country}? ({mode})"

    fig.update_layout(
        title=title,
        height=520 if is_mobile else 420,
        margin=dict(l=60, r=10, t=60, b=40) if is_mobile else dict(l=140, r=20, t=60, b=50),
        xaxis_title="Share of attributor's total attributions (%)",
    )
    return fig


# =========================================================
# App state
# =========================================================

def init_state() -> None:
    st.session_state.setdefault("selected_country", None)
    st.session_state.setdefault("map_key", 0)
    st.session_state.setdefault("mobile_layout", False)


def reset_selection() -> None:
    st.session_state["selected_country"] = None
    st.session_state["map_key"] += 1
    st.rerun()


# =========================================================
# App
# =========================================================

def main() -> None:
    st.set_page_config(layout="wide", page_title="World Cybercrime Index")
    init_state()

    df_wci, store = load_data()

    metric_to_col = {
        "WCI": "WCI",
        "WCI per capita": "WCI_per_capita",
        "WCI per GDP": "WCI_per_GDP",
        "Respondents (by nationality)": "respondents_nat",
        "Respondents (by residence)": "respondents_res",
    }

    st.sidebar.title("WCI Dashboard")
    metric_label = st.sidebar.selectbox("Metric:", list(metric_to_col.keys()), index=0)
    mode = st.sidebar.selectbox("Attributions:", ["By nationality", "By residence"], index=0)

    # Manual toggle is more reliable than UA sniffing
    st.session_state["mobile_layout"] = st.sidebar.toggle("Mobile layout", value=st.session_state["mobile_layout"])
    is_mobile = bool(st.session_state["mobile_layout"])

    if st.sidebar.button("Reset Selection"):
        reset_selection()

    # Layout: stacked on mobile
    if is_mobile:
        left = st.container()
        right = st.container()
    else:
        left, right = st.columns([2, 1])

    with left:
        map_fig = build_map(df_wci, metric_label, metric_to_col[metric_label], is_mobile=is_mobile)

        # Click capture via streamlit-plotly-events
        # This avoids Plotly selection state (which breaks on iOS).
        clicked = plotly_events(
            map_fig,
            click_event=True,
            select_event=False,
            hover_event=False,
            override_height=520 if is_mobile else 600,
            key=f"map_events_{st.session_state['map_key']}",
        )

        if clicked:
            # clicked[0] keys vary, but customdata is the most stable for this trace
            st.session_state["selected_country"] = clicked[0].get("customdata") or clicked[0].get("text")

        # Backup selector (always works, especially on mobile when tapping is awkward)
        with st.expander("Trouble tapping on mobile? Use selector", expanded=False):
            options = df_wci["Country"].astype(str).tolist()
            prev = st.session_state.get("selected_country")
            idx = options.index(prev) if prev in options else 0
            chosen = st.selectbox("Selected country", options, index=idx)
            st.session_state["selected_country"] = chosen

    with right:
        accused = st.session_state.get("selected_country")
        if accused:
            st.subheader(f"=== {accused} ===")

            col = metric_to_col[metric_label]
            s = df_wci.loc[df_wci["Country"] == accused, col]
            val = float(pd.to_numeric(s, errors="coerce").iloc[0]) if not s.empty else float("nan")
            fmt = hover_format(metric_label)

            if fmt == ".2e":
                st.write(f"**{metric_label}:** {val:.2e}")
            elif fmt == ".0f":
                st.write(f"**{metric_label}:** {val:.0f}")
            else:
                st.write(f"**{metric_label}:** {val:.4f}")

            items = get_top_attributors(store, accused, mode, top_n=10)
            bar_fig = build_bar(items, accused, mode, is_mobile=is_mobile)
            st.plotly_chart(bar_fig, use_container_width=True, config={"displayModeBar": False})
        else:
            st.info("Tap a country on the map to see attribution details (or open the selector).")


if __name__ == "__main__":
    main()