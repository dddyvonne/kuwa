import dash
import dash_core_components as dcc
import dash_html_components as html

import plotly.plotly as py
import plotly.graph_objs as go
import plotly
from dash.dependencies import Input, Output
from random import randint

import pandas as pd
import numpy as np


plotly.tools.set_credentials_file(username='y.z.elshater', api_key='Bgmu9BwfZx3lVTjG7puZ')


app = dash.Dash()

mapbox_access_token = 'pk.eyJ1IjoieWVsc2hhdGVyIiwiYSI6ImNqbGR0a2E3cjBleHkzcXBtaTVlb3hoZGcifQ.i3B3Z9ABPJno6_kQ5SiRXQ'

tp_shape_values_df = pd.read_csv("../Hospitals_Features/tp_shape_values_df_with_lat_long.csv")
#tp_shape_values_df = tp_shape_values_df[tp_shape_values_df.Is_Customer >= 0.8]
tp_shape_values_df["Provider_ID"] = tp_shape_values_df["Provider_ID"].astype(str) 

general_info_df = pd.read_csv("../../web_data/Hospital_General_Information.csv")
general_info_df["Provider_ID"] = general_info_df["Provider ID"].astype(str)
general_info_df["Provider_ID"] = general_info_df["Provider_ID"].apply(lambda x : x.zfill(6))
tp_shape_values_df = pd.merge(tp_shape_values_df,general_info_df,on="Provider_ID")

data = [
    go.Scattermapbox(
        lat=tp_shape_values_df["lat"],
        lon=tp_shape_values_df["longit"],
        mode='markers',
        marker=dict(
            size= 5 * np.exp(tp_shape_values_df["Is_Customer"])
        ),
        text=tp_shape_values_df["Hospital Name"],
        ids = tp_shape_values_df["Provider_ID"]
    )
]

layout = go.Layout(
    #autosize=True,
    hovermode='closest',
    mapbox=dict(
        accesstoken=mapbox_access_token,
        bearing=0,
        center=dict(
            lat=38.92,
            lon=-77.07
        ),
        pitch=1,
        zoom=4
    ),
)


fig = dict(data=data, layout=layout)




app.layout = html.Div(children=[
    html.H1(children='Hello Dash'),

    html.Div([
        html.P("QuVa Customer Probability")
    ]),

    

    dcc.Slider(
    id = 'customer-prop-slider',
    marks={i: '{}'.format(i) for i in range(0, 110,10)},
    min=0,
    max=100,
    step = 50,
    value=70
    ),

    html.Div([
        html.P("")
    ]),

    
    dcc.Graph(
        id='map-graph',
        figure=fig,
        style={"height": "800"}
    ),
    dcc.Graph(
        id='feature-values'
        )
])
@app.callback(
    Output(component_id='feature-values', component_property='figure'),
    [Input(component_id='map-graph', component_property='clickData')]
)
def update_feature_values_bar_div(input_value):
    print(input_value)
    N = 10
    x = np.linspace(-2, 2, N)
    y = np.random.randn(N)
    df = pd.DataFrame({'x': x, 'y': y})
    data = [
        go.Bar(
            x=df['x'], # assign x as the dataframe column 'x'
            y=df['y'],
            orientation='h'
        )
    ]
    return dict(data=data)


@app.callback(
    Output(component_id='map-graph', component_property='selectedData'),
    [Input(component_id='customer-prop-slider', component_property='value')]
)
def update_map_customer_prop(prob_value):
    tp_shape_values_df = tp_shape_values_df[tp_shape_values_df.Is_Customer >= (prob_value/100)]
    return 
    go.Scattermapbox(
        lat=tp_shape_values_df["lat"],
        lon=tp_shape_values_df["longit"],
        mode='markers',
        marker=dict(
            size= 5 * np.exp(tp_shape_values_df["Is_Customer"])
        ),
        text=tp_shape_values_df["Hospital Name"],
        ids = tp_shape_values_df["Provider_ID"]
    )



if __name__ == '__main__':
    app.run_server(debug=True,port=8777)
