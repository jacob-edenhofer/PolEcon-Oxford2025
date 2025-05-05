import numpy as np
import panel as pn
import plotly.graph_objs as go
from scipy.stats import beta
from plotly.subplots import make_subplots

# Enable Panel with Plotly
pn.extension('plotly')

# --- Voter distribution logic ---
def get_voter_distribution(dist_type, n=5000, noise=1e-6):
    if dist_type == 'Normal':
        voters = np.random.normal(0.5, 0.15, n)
    elif dist_type == 'Bimodal':
        left = np.random.normal(0.35, 0.08, n // 2)
        right = np.random.normal(0.65, 0.08, n // 2)
        voters = np.concatenate([left, right])
    elif dist_type == 'Skewed':
        voters = beta.rvs(a=2, b=5, size=n)
    else:
        raise ValueError("Unsupported distribution.")
    voters = np.clip(voters, 0, 1) + np.random.uniform(0, noise, n)
    voters.sort()
    return voters

# --- Plotting logic ---
def make_plot(dist, pos_A, pos_B):
    voters = get_voter_distribution(dist)
    median = np.median(voters)

    dists_A = np.abs(voters - pos_A)
    dists_B = np.abs(voters - pos_B)
    votes_A = voters[dists_A < dists_B]
    votes_B = voters[dists_B < dists_A]
    ties = voters[dists_A == dists_B]

    total_votes = len(voters)
    vote_A_pct = (len(votes_A) + 0.5 * len(ties)) / total_votes
    vote_B_pct = (len(votes_B) + 0.5 * len(ties)) / total_votes

    fig = make_subplots(rows=1, cols=1, subplot_titles=["Downsian competition in 1D policy space"])

    fig.add_trace(go.Histogram(x=voters, nbinsx=60, name="Voter ideal points",
                               opacity=0.5, marker_color='lightgray'))

    fig.add_trace(go.Scatter(x=[pos_A], y=[0], mode='markers+text', name='Party A',
                             marker=dict(size=14, color='#1f77b4'),
                             text=["A"], textposition="top center"))

    fig.add_trace(go.Scatter(x=[pos_B], y=[0], mode='markers+text', name='Party B',
                             marker=dict(size=14, color='#d62728'),
                             text=["B"], textposition="top center"))

    fig.add_trace(go.Scatter(x=[median], y=[0], mode='markers+text', name='Median voter',
                             marker=dict(size=12, color='green'),
                             text=["Median"], textposition="bottom center"))

    fig.add_annotation(text=f"Vote share A: {vote_A_pct:.3%}", x=0.2, y=250, showarrow=False,
                       font=dict(color='#1f77b4'))
    fig.add_annotation(text=f"Vote share B: {vote_B_pct:.3%}", x=0.8, y=250, showarrow=False,
                       font=dict(color='#d62728'))

    fig.update_layout(
        xaxis=dict(title="Policy space [0,1]"),
        yaxis=dict(title="Number of voters"),
        barmode='overlay',
        showlegend=True,
        height=500,
        width=800,
        paper_bgcolor='white',
        plot_bgcolor='white'
    )

    return fig

# --- Interactive widgets ---
slider_A = pn.widgets.DiscreteSlider(name='Party A', options=[round(x, 2) for x in np.linspace(0.0, 1.0, 101)], value=0.2)
slider_B = pn.widgets.DiscreteSlider(name='Party B', options=[round(x, 2) for x in np.linspace(0.0, 1.0, 101)], value=0.8)
dropdown = pn.widgets.Select(name='Distribution', options=['Normal', 'Bimodal', 'Skewed'], value='Normal')

# --- Bind widgets to Plotly pane ---
plot_callback = pn.bind(lambda dist, pos_A, pos_B:
                        pn.pane.Plotly(make_plot(dist, pos_A, pos_B), config={'responsive': True}),
                        dist=dropdown, pos_A=slider_A, pos_B=slider_B)

# --- Layout ---
app = pn.Column(
    pn.pane.Markdown("## Downsian model of party competition"),
    pn.Row(dropdown),
    pn.Row(slider_A, slider_B),
    plot_callback
)

# --- Serve app (required for Render.com) ---
app.servable()
