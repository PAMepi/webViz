import dash
import dash_core_components as dcc
import dash_html_components as html
import dash_leaflet as dl
from dash.dependencies import Output, Input
from dash_leaflet import express as dlx
import plotly.graph_objects as go
import json
import numpy as np
import pandas as pd

################################################################################################################
#Load data
with open("data/mapaBr/map.geojson", 'r') as f:
    data = json.load(f)

#################################################################################################################
#Load data files for model
stateDF = pd.read_csv("data/dataState.csv")
capitalDF = pd.read_csv("data/dataCapital.csv")
inlandDF = pd.read_csv("data/dataInland.csv")

#################################################################################################################

#################################################################################################################


#################################################################################################################
#Define function to work with map
def get_style(feature):
    color = []
    if feature["properties"]["coefVar"] <= -70:
        color.append("#2166ac")
    elif feature["properties"]["coefVar"] > -70 and feature["properties"]["coefVar"] <= -50:
        color.append("#67a9cf")
    elif feature["properties"]["coefVar"] > -50 and feature["properties"]["coefVar"] <= -25:
        color.append("#d1e5f0")
    elif feature["properties"]["coefVar"] > -25 and feature["properties"]["coefVar"] <= 0:
        color.append("#f7f7f7")
    elif feature["properties"]["coefVar"] > 0 and feature["properties"]["coefVar"] <= 25:
        color.append("#fddbc7")
    elif feature["properties"]["coefVar"] > 25 and feature["properties"]["coefVar"] <= 50:
        color.append("#ef8a62")
    else:
        color.append("#b2182b")
    return dict(fillColor = color, weight = 2, opacity = 1, color = 'white', dashArray = '3', fillOpacity = .6)


def get_info(feature = None):
    header = [html.H4("Variation on transmission Rate")]
    if not feature:
        return header + ["Click on a state"]
    return header + [html.B(feature["properties"]["name"]), html.Br(),
                     "{:.2f}".format(feature["properties"]["coefVar"])]

def get_uf(feature = None):
    if not feature:
        return ["Select a state"]
    return feature["properties"]["sigla"]

def plotTs(df):
    fitted_trace = go.Scatter(
        x  = df["date"],
        y =  df["Infec"],
        mode ='lines',
        name = "Fitted",
        line = {"color": "#d73027"}
    )

    observed_trace = go.Scatter(
        x  = df["date"],
        y =  df["cases"],
        mode ='markers',
        name = "Observed",
        marker = {"color": "#253494","size":4}
    )

   
    data = [fitted_trace, observed_trace]
    layout = go.Layout(yaxis = {"title":"Cummulative cases"})

    return {"data": data, "layout": layout}
    
def plotRt(df):
    rt_trace = go.Scatter(
        x  = df["date"],
        y =  df["Rtdata"],
        mode ='lines',
        name = "Observed",
        line = {"color": "#252525"}
    )

    mod_trace = go.Scatter(
        x  = df["date"],
        y =  df["Rtmod"],
        mode ='lines',
        name = "Smoothed",
        line = {"color": "#225ea8", "dash":"dash"}
    )
    hline = go.Scatter(
        x = df["date"],
        y = [1] * len(df),
        mode = "lines",
        line = {"color": "#b2182b", "dash":"dash"},
        showlegend = False
    )

   
    data = [rt_trace, mod_trace, hline]
    layout = go.Layout(yaxis = {"title":"Reproduction effective number",
                                "range":[0,10]})

    return {"data": data, "layout": layout}



marks = [-70, -50, -25, 0, 25, 50, 70]
colorScale = ["#2166ac", "#67a9cf", "#d1e5f0", "#f7f7f7", "#fddbc7", "#ef8a62", "#b2182b"]
options = dict(hoverStyle = dict(weight = 5, color = '#666', dashArray = ''), zoomToBoundsOnClick = False)
#################################################################################################################


#################################################################################################################
#Convert data to work with dash
#Convert data to geojson
geojson = dlx.geojson(data, id = "geojson", defaultOptions = options, style = get_style)

# Create colorbar.
ctg = ["{}".format(mark, marks[i + 1]) for i, mark in enumerate(marks[:-1])] + ["{}".format(marks[-1])]
colorbar = dlx.categorical_colorbar(categories = ctg, 
                                    colorscale = colorScale,
                                    opacity = .6,
                                    width = 300, 
                                    height = 30,
                                    position = "bottomright")
#define option to show text                                 

#################################################################################################################


#################################################################################################################
#specify css
external_stylesheets = ['assets/style.css']


#start dash application
app = dash.Dash(__name__,prevent_initial_callbacks = True, external_stylesheets = external_stylesheets)

server = app.server

#Create specfic layout for map info
info = html.Div(children = get_info(), id = "info", className = "info",
                style = {"position": "absolute", "top": "10px", "right": "10px", "z-index": "1000"})

#printSpace = html.Div(children = get_uf(), id = "my_print")


#Create a layout for graph
graphlay = html.Div(children = [
    dcc.Graph(id = "graph")
],className = "twelve columns")

#Create a core component for dropdow menu
dropDowSate = html.Div(children = [
    dcc.Dropdown(id = "state_selector",
                options = [
                     {'label': 'State', 'value': 'state'},
                     {'label': 'Capital', 'value': 'capital'},
                     {'label': "Inland cities", "value":"inland"}
                ],
                value = "state"
    )


], className = "six columns")


#Create a temporary drpdow for rt
dropDowModel = html.Div(children = [
    dcc.Dropdown(id = "graph_selector",
                options = [
                     {'label': 'R(t)', 'value': 'rt'},
                     {'label': 'Model', 'value': 'model'},
                ],
                value = "model")
], className = "six columns")

#Create app layout
app.layout = html.Div(children = [

    #First colum to put title
    html.Div(children = [
        #Insert title
        html.H1("Assessing the nation wide impact of COVID-19 mitigation policies on the transmission rate of SARS-CoV-2 in Brazil"),
    ], className = "twelve columns"),

    #Given space btetwen title and components
    html.Div(children = [html.Br()], className = "twelve columns"),

    #Create a main body
    html.Div(children = [

        #Create left panel
        html.Div(children = [
            dl.Map(children = [dl.TileLayer(), geojson, colorbar, info],
                   center = [-20, -54],
                   zoom = 4,
                   style = {'width': '100%', 'height': '70vh', 'margin': "auto", "display": "block"}, id = "map")
        ], className = "six columns"),

        #Create right panel
        html.Div(children = [
            dropDowSate,
            dropDowModel,
            #printSpace
            graphlay,
        ], className = "six columns")
    ],className = "twelve columns")

])
#################################################################################################################

#################################################################################################################
#add app functionality
@app.callback(Output("info", "children"), [Input("geojson", "featureClick")])
def info_hover(feature):
    return get_info(feature)

# @app.callback(Output(component_id = "my_print", component_property = "children"),
#              [Input(component_id = "geojson", component_property = "featureHover")])
# def uf_hover(feature = None):
#     state = get_uf(feature)
#     return get_uf(feature)

@app.callback(Output(component_id = "graph", component_property = "figure"),
              [Input(component_id = "geojson", component_property = "featureClick"),
               Input(component_id = "state_selector", component_property = "value"),
               Input(component_id = "graph_selector", component_property = "value")])
def update_Graph(feature, selected_state, selected_model):
    if selected_state == "inland":
        df  = inlandDF
    elif selected_state == "capital":
        df = capitalDF
    else:
        df = stateDF
    filtered_df = df[df["state"] == get_uf(feature)]
    if selected_model == "model":
        return plotTs(df = filtered_df)
    if selected_model == "rt":
        return plotRt(df = filtered_df)

#################################################################################################################



#run app
if __name__ == '__main__':
    app.run_server(debug=True)
