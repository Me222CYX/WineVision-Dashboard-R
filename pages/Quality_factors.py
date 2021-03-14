import numpy as np
import pandas as pd
import altair as alt
import pathlib

import dash_core_components as dcc
import dash_html_components as html
import dash_bootstrap_components as dbc

from utils import Header, make_dash_table, Header2


alt.data_transformers.disable_max_rows()

wine = pd.read_csv("data/processed/wine_quality.csv")


def create_layout(app):
    return html.Div([
        Header(app),
        dbc.Container([
            #  html.H1('Various Features in Different Quality Factors'),
             dbc.Row([
                 dbc.Col([
                     dbc.Card(
                         dbc.CardBody(html.H4('Wine Type')),
                         color='warning', inverse=True),

                     dcc.Checklist(
                         id="winetype",
                         options=[
                             {"label": "White Wine", "value": "white"},
                             {"label": "Red Wine", "value": "red"}
                         ],
                         value=["red", "white"],
                         labelStyle={"display": "block"}
                     ),

                     html.H4('Interactive Plots'),
                     html.H6('Drag your mouse to choose the range you want to explore!')
                 ]),

                 dbc.Col([
                     html.H4('Select your variables:'),

                     html.H5('X-axis'),

                     # dcc.Dropdown(
                     #     id = "type-widget",
                     #     options=[
                     #         {'label': 'White', 'value': 'white'},
                     #         {'label': 'Red', 'value': 'red'}],
                     #     value='white', placeholder = "Select Wine Type"),
                     dcc.Dropdown(
                         id='xcol-widget',
                         value='pH',
                         options=[{'label': col, 'value': col}
                                  for col in wine.columns],
                         clearable=False
                     ),

                     html.H5("Y-axis"),

                     dcc.Dropdown(
                         id='ycol-widget',
                         value='pH',
                         options=[{'label': col, 'value': col}
                                  for col in wine.columns],
                         clearable=False
                     ),
                 ]),

                 html.Iframe(
                     id="scatter_1",
                     # srcDoc = plot_scatter(),
                     style={'border-width': '0', 'width': '120%', 'height': '700px'})

             ])
         ])

         ]
    )
   

def create_layout_fullview(app):
     return html.Div([
 
        dbc.Container([
            #  html.H1('Various Features in Different Quality Factors'),
             dbc.Row([
                 dbc.Col([
                     dbc.Card(
                         dbc.CardBody(html.H4('Wine Type')),
                         color='warning', inverse=True),

                     dcc.Checklist(
                         id="winetype",
                         options=[
                             {"label": "White Wine", "value": "white"},
                             {"label": "Red Wine", "value": "red"}
                         ],
                         value=["red", "white"],
                         labelStyle={"display": "block"}
                     ),

                     html.H4('Interactive Plots'),
                     html.H6('Drag your mouse to choose the range you want to explore!')
                 ]),

                 dbc.Col([
                     html.H4('Select your variables:'),

                     html.H5('X-axis'),

                     # dcc.Dropdown(
                     #     id = "type-widget",
                     #     options=[
                     #         {'label': 'White', 'value': 'white'},
                     #         {'label': 'Red', 'value': 'red'}],
                     #     value='white', placeholder = "Select Wine Type"),
                     dcc.Dropdown(
                         id='xcol-widget',
                         value='pH',
                         options=[{'label': col, 'value': col}
                                  for col in wine.columns],
                         clearable=False
                     ),

                     html.H5("Y-axis"),

                     dcc.Dropdown(
                         id='ycol-widget',
                         value='pH',
                         options=[{'label': col, 'value': col}
                                  for col in wine.columns],
                         clearable=False
                     ),
                 ]),

                 html.Iframe(
                     id="scatter_1",
                     # srcDoc = plot_scatter(),
                     style={'border-width': '0', 'width': '120%', 'height': '700px'})

             ])
         ])

         ]
    )
   