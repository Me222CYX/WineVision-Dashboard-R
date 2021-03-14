import numpy as np
import pandas as pd
import altair as alt

import dash
import dash_html_components as html
import dash_core_components as dcc
from dash.dependencies import Input, Output
import dash_bootstrap_components as dbc

from utils import Header, make_dash_table,Header2


# Allow large data set
alt.data_transformers.disable_max_rows()

# Get data
wine = pd.read_csv("data/processed/wine_quality.csv")
corr_df = pd.read_csv("data/processed/correlation.csv")


# Get a list of unique column names
variables = corr_df["level_0"].unique()
variables = np.delete(variables, np.argwhere(variables == "Quality Factor"))

# Don't want this as an option in scatterplot
variables = np.delete(variables, np.argwhere(
    variables == "Quality Factor Numeric"))

# Setup app
def create_layout(app):
    # Page layouts
    return html.Div(
        [Header(app),
         dbc.Container([
    dbc.Row([
        dbc.Col([
            html.Iframe(
                id = "matrix",
                style={'border-width': '0', 'width': '500px', 'height': '500px'}),

            html.H5("Wine Type"),

            dcc.Checklist(
                id = "winetype",
                options = [
                    {"label": "White Wines", "value": "white"},
                    {"label": "Red Wines", "value": "red"}
                ],
                value = ["red", "white"],
                labelStyle={"display": "inline-block"}
            ),

            html.H5("Quality"),

            dcc.Slider(
                id = "quality",
                min=0,
                max=3,
                step=1,
                value = 1,
                marks={
                    0: "below average",
                    1: "average",
                    2: "above average",
                    3: "any"
                }
            )

        ]),
        dbc.Col([
            html.Iframe(
                id = "scatter",
                style={'border-width': '0', 'width': '500px', 'height': '500px'}),
                
            html.H5("x-axis:"),

            dcc.Dropdown(
                id = "x-axis",
                options=[{"label": i, "value": i} for i in variables],
                value = "Alcohol (%)",
                clearable = False
                ),

            html.H5("y-axis"),

            dcc.Dropdown(
                id = "y-axis",
                options=[{"label": i, "value": i} for i in variables],
                value = "Chlorides (g/dm^3)",
                clearable = False),
     
        ])
    ]),
    dbc.Row([
    html.Iframe(
        id = "densityplot",
        style={'border-width': '0', 'width': '1200px', 'height': '400px'}
    ),
    ]),

    dbc.Row([html.H5("\t Density Plot Variable")]),

    dcc.Dropdown(
            id = "densvalue",
            options=[{"label": i, "value": i} for i in variables],
            value = "Chlorides (g/dm^3)",
            clearable = False)

    
])


         ])
    
    
def create_layout_fullview(app):
    # Page layouts
    return html.Div(
        [html.Div(
            html.H1("Graphics")
        ),
         dbc.Container([
    dbc.Row([
        dbc.Col([
            html.Iframe(
                id = "matrix",
                style={'border-width': '0', 'width': '500px', 'height': '500px'}),

            html.H5("Wine Type"),

            dcc.Checklist(
                id = "winetype",
                options = [
                    {"label": "White Wines", "value": "white"},
                    {"label": "Red Wines", "value": "red"}
                ],
                value = ["red", "white"],
                labelStyle={"display": "inline-block"}
            ),

            html.H5("Quality"),

            dcc.Slider(
                id = "quality",
                min=0,
                max=3,
                step=1,
                value = 1,
                marks={
                    0: "below average",
                    1: "average",
                    2: "above average",
                    3: "any"
                }
            )

        ]),
        dbc.Col([
            html.Iframe(
                id = "scatter",
                style={'border-width': '0', 'width': '500px', 'height': '500px'}),
                
            html.H5("x-axis:"),

            dcc.Dropdown(
                id = "x-axis",
                options=[{"label": i, "value": i} for i in variables],
                value = "Alcohol (%)",
                clearable = False
                ),

            html.H5("y-axis"),

            dcc.Dropdown(
                id = "y-axis",
                options=[{"label": i, "value": i} for i in variables],
                value = "Chlorides (g/dm^3)",
                clearable = False),
     
        ])
    ]),
    dbc.Row([
    html.Iframe(
        id = "densityplot",
        style={'border-width': '0', 'width': '1200px', 'height': '400px'}
    ),
    ]),

    dbc.Row([html.H5("\t Density Plot Variable")]),

    dcc.Dropdown(
            id = "densvalue",
            options=[{"label": i, "value": i} for i in variables],
            value = "Chlorides (g/dm^3)",
            clearable = False)

    
])


         ])