from __future__ import annotations

import pandas as pd
import plotly.graph_objs as go
import streamlit as st


# =========================================================
# Constants
# =========================================================

DATA_WCI = "data/df_wci_with_respondents.csv"
DATA_ACC_NAT = "data/accusations_nationality.csv"
DATA_ACC_RES = "data/accusations_residence.csv"

GBR_COLORSCALE = [
    [0.00, "#e5f5e0"],
    [0.30, "#a1d99b"],
    [0.55, "#2b8cbe"],
    [0.80, "#253494"],
    [1.00, "#d73027"],
]

_INVIS = ["\u200b", "\u200c", "\u200d", "\ufeff", "\u2060", "\u00a0"]
_PUNCT_TO_SPACE = [".", ",", "'", '"', "’", "(", ")", "-", "–", "—"]

METRIC_TO_COL = {
    "WCI": "WCI",
    "WCI per capita": "WCI_per_capita",
    "WCI per GDP": "WCI_per_GDP",
    "Respondents (by nationality)": "respondents_nat",
    "Respondents (by residence)": "respondents_res",
}

ACCUSER_MODES = ["By nationality", "By residence"]

REQUIRED_WCI_COLS = [
    "Country",
    "ISO3",
    "WCI",
    "WCI_per_capita",
    "WCI_per_GDP",
    "respondents_nat",
    "respondents_res",
]


# =========================================================
# String normalization
# =========================================================

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

def require_columns(df: pd.DataFrame, cols: list[str], name: str) -> None:
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


def column_totals(df_num: pd.DataFrame, exclude: str = "Country") -> dict[str, float]:
    cols = [c for c in df_num.columns if c != exclude]
    return {c: float(df_num[c].sum()) for c in cols}


def hover_format(metric_label: str) -> str:
    if metric_label in ("WCI per capita", "WCI per GDP"):
        return ".2e"
    if metric_label.startswith("Respondents"):
        return ".0f"
    return ".4f"


# =========================================================
# Data loading
# =========================================================

@st.cache_data(show_spinner=False)
def load_data():
    df_wci = pd.read_csv(DATA_WCI)
    df_wci["Country"] = df_wci["Country"].apply(clean_country_string)
    require_columns(df_wci, REQUIRED_WCI_COLS, "df_wci")

    acc_raw = {
        "By nationality": pd.read_csv(DATA_ACC_NAT),
        "By residence": pd.read_csv(DATA_ACC_RES),
    }

    for _, df in acc_raw.items():
        df["Country"] = df["Country"].apply(clean_country_string)
        df.columns = [clean_country_string(c) for c in df.columns]

    acc_num = {k: to_numeric_frame(df) for k, df in acc_raw.items()}
    acc_row_lookup = {
        k: {canon(c): i for i, c in enumerate(df["Country"].tolist())}
        for k, df in acc_raw.items()
    }
    acc_totals = {k: column_totals(df) for k, df in acc_num.items()}

    return df_wci, acc_num, acc_row_lookup, acc_totals


# =========================================================
# Plot builders
# =========================================================

def build_map(df_wci: pd.DataFrame, metric_label: str, metric_col: str) -> go.Figure:
    vals = pd.to_numeric(df_wci[metric_col], errors="coerce").fillna(0.0)
    vmin, vmax = float(vals.min()), float(vals.max())
    if vmin == vmax:
        vmax = vmin + 1e-9

    fmt = hover_format(metric_label)

    fig = go.Figure()
    fig.add_trace(
        go.Choropleth(
            locations=df_wci["ISO3"],
            z=vals,
            text=df_wci["Country"],
            customdata=df_wci["Country"],
            colorscale=GBR_COLORSCALE,
            zmin=vmin,
            zmax=vmax,
            marker_line_color="black",
            marker_line_width=0.3,
            hovertemplate="<b>%{text}</b><br>"
            + metric_label
            + f": %{{z:{fmt}}}<extra></extra>",
        )
    )

    # Prevent selection dimming (keeps colors consistent after click)
    fig.update_traces(
        selected=dict(marker=dict(opacity=1.0)),
        unselected=dict(marker=dict(opacity=1.0)),
        selector=dict(type="choropleth"),
    )

    fig.update_geos(
        showcountries=True,
        countrycolor="black",
        projection_type="natural earth",
        bgcolor="#e6e8ff",
    )
    fig.update_layout(
        title="World Cybercrime Index - A collection of surveyed responses by cybercrime specialists",
        height=600,
        margin=dict(t=50, b=10, l=10, r=10),
        clickmode="event+select",
    )
    return fig


def get_top_attributors(
    acc_num,
    acc_row_lookup,
    acc_totals,
    accused_country: str,
    mode: str,
    top_n: int = 10,
):
    df_num = acc_num[mode]
    row_lookup = acc_row_lookup[mode]
    totals = acc_totals[mode]

    key = canon(accused_country)
    i = row_lookup.get(key)
    if i is None:
        return []

    row = df_num.iloc[i]
    attributors = [c for c in df_num.columns if c != "Country"]

    out = []
    for attributor in attributors:
        count = float(row[attributor])
        denom = float(totals.get(attributor, 0.0))
        if count > 0 and denom > 0:
            out.append((attributor, count / denom, count, denom))

    out.sort(key=lambda t: t[1], reverse=True)
    return out[:top_n]


def build_bar(items, accused_country: str, mode: str) -> go.Figure:
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
                    colorbar=dict(title="%", tickformat=".0f"),
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
        height=420,
        margin=dict(l=140, r=20, t=60, b=50),
        xaxis_title="Share of attributor's total attributions (%)",
    )
    return fig


# =========================================================
# Session state
# =========================================================

def init_state() -> None:
    st.session_state.setdefault("selected_country", None)
    st.session_state.setdefault("map_key", 0)


def reset_selection() -> None:
    st.session_state["selected_country"] = None
    st.session_state["map_key"] += 1  # force Plotly remount to clear selection UI
    st.rerun()


# =========================================================
# App
# =========================================================

def main() -> None:
    st.set_page_config(layout="wide", page_title="World Cybercrime Index")
    init_state()

    df_wci, acc_num, acc_row_lookup, acc_totals = load_data()

    # Sidebar
    st.sidebar.title("WCI Dashboard")
    metric_label = st.sidebar.selectbox("Metric:", list(METRIC_TO_COL.keys()), index=0)
    accuser_mode = st.sidebar.selectbox("Attributions:", ACCUSER_MODES, index=0)

    if st.sidebar.button("Reset Selection"):
        reset_selection()

    # Layout
    col1, col2 = st.columns([2, 1])

    # Map
    with col1:
        metric_col = METRIC_TO_COL[metric_label]
        map_fig = build_map(df_wci, metric_label, metric_col)

        selected = st.plotly_chart(
            map_fig,
            use_container_width=True,
            on_select="rerun",
            key=f"map_{st.session_state['map_key']}",
        )

        if (
            selected
            and "selection" in selected
            and "points" in selected["selection"]
            and len(selected["selection"]["points"]) > 0
        ):
            st.session_state["selected_country"] = selected["selection"]["points"][0]["customdata"]

    # Detail panel
    with col2:
        accused = st.session_state.get("selected_country")
        if not accused:
            st.info("Click on a country in the map to see attribution details.")
            return

        st.subheader(f"=== {accused} ===")

        # Metric value
        metric_col = METRIC_TO_COL[metric_label]
        s = df_wci.loc[df_wci["Country"] == accused, metric_col]
        val = float(pd.to_numeric(s, errors="coerce").iloc[0]) if not s.empty else float("nan")
        fmt = hover_format(metric_label)

        if fmt == ".2e":
            st.write(f"**{metric_label}:** {val:.2e}")
        elif fmt == ".0f":
            st.write(f"**{metric_label}:** {val:.0f}")
        else:
            st.write(f"**{metric_label}:** {val:.4f}")

        # Bar chart
        items = get_top_attributors(acc_num, acc_row_lookup, acc_totals, accused, accuser_mode)
        bar_fig = build_bar(items, accused, accuser_mode)
        st.plotly_chart(bar_fig, use_container_width=True)


if __name__ == "__main__":
    main()