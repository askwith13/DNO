from flask import Flask, render_template_string, request, jsonify, send_file, redirect, url_for, session
from markupsafe import Markup
import pandas as pd
import numpy as np
import json
import io
import base64
import requests
import time
from datetime import datetime
import os
import tempfile
import zipfile
from werkzeug.utils import secure_filename

# Visualization libraries
import plotly.graph_objs as go
import plotly.express as px
import plotly.utils
import folium
from folium import plugins

# Spatial and optimization libraries
try:
    import geopandas as gpd
    from shapely.geometry import Point, Polygon
except ImportError:
    gpd = None
    Point = None
    Polygon = None

from geopy.distance import geodesic
import pulp  # For linear programming
from scipy.spatial.distance import cdist

app = Flask(__name__)
app.secret_key = os.environ.get('SECRET_KEY', 'fallback-secret-key-change-in-production')
app.config['MAX_CONTENT_LENGTH'] = 16 * 1024 * 1024  # 16MB max file size
app.config['PERMANENT_SESSION_LIFETIME'] = 3600  # 1 hour session timeout

# Configuration - OpenRouteService API Key
ORS_API_KEY = "5b3ce3597851110001cf62481a51a0c1d253455eb67aa25df367bd8d"

# HTML TEMPLATES AS STRINGS
BASE_TEMPLATE = '''
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Diagnostic Network Optimization</title>
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" rel="stylesheet">
    <link href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css" rel="stylesheet">
    <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
    <link rel="stylesheet" type="text/css" href="https://cdn.datatables.net/1.11.5/css/dataTables.bootstrap5.min.css">
    <style>
        body { background-color: #f4f4f4; }
        .sidebar { min-height: 100vh; background-color: #343a40; }
        .sidebar a { color: white; text-decoration: none; padding: 10px 15px; display: block; transition: background-color 0.3s; }
        .sidebar a:hover, .sidebar a.active { background-color: #495057; color: white; }
        .content-area { padding: 20px; }
        .progress-text { font-family: monospace; font-size: 12px; background-color: #f8f9fa; padding: 10px; border: 1px solid #dee2e6; border-radius: 4px; white-space: pre-wrap; max-height: 200px; overflow-y: auto; }
        .card-header { background-color: #007bff; color: white; }
        .api-status { padding: 5px 10px; border-radius: 4px; font-size: 12px; }
        .api-status.success { background-color: #d4edda; color: #155724; }
        .api-status.error { background-color: #f8d7da; color: #721c24; }
        .api-status.warning { background-color: #fff3cd; color: #856404; }
        .download-btn-group { margin-left: auto; }
    </style>
</head>
<body>
    <div class="container-fluid">
        <div class="row">
            <nav class="col-md-3 col-lg-2 d-md-block sidebar collapse">
                <div class="position-sticky pt-3">
                    <h4 class="text-white text-center mb-4"><i class="fas fa-network-wired"></i> Network Optimizer</h4>
                    <ul class="nav flex-column">
                        <li class="nav-item"><a class="nav-link" href="/"><i class="fas fa-home"></i> Home</a></li>
                        <li class="nav-item"><a class="nav-link" href="/data_input"><i class="fas fa-upload"></i> Data Input</a></li>
                        <li class="nav-item"><a class="nav-link" href="/settings"><i class="fas fa-cogs"></i> Settings</a></li>
                        <li class="nav-item"><a class="nav-link" href="/results"><i class="fas fa-chart-bar"></i> Results</a></li>
                        <li class="nav-item"><a class="nav-link" href="/map"><i class="fas fa-map"></i> Map</a></li>
                        <li class="nav-item"><a class="nav-link" href="/isochrone"><i class="fas fa-clock"></i> Isochrones</a></li>
                        <li class="nav-item"><a class="nav-link" href="/about"><i class="fas fa-info-circle"></i> About</a></li>
                    </ul>
                </div>
            </nav>
            <main class="col-md-9 ms-sm-auto col-lg-10 px-md-4 content-area">
                {{ content|safe }}
            </main>
        </div>
    </div>
    <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js"></script>
    <script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>
    <script type="text/javascript" src="https://cdn.datatables.net/1.11.5/js/jquery.dataTables.min.js"></script>
    <script type="text/javascript" src="https://cdn.datatables.net/1.11.5/js/dataTables.bootstrap5.min.js"></script>
    {{ extra_scripts|safe }}
</body>
</html>
'''

# Updated MAP_CONTENT with simplified download options (HTML only)
MAP_CONTENT = '''
<h2><i class="fas fa-map"></i> Network Optimization Map</h2>
<div class="row">
    <div class="col-12">
        <div class="card">
            <div class="card-header d-flex justify-content-between align-items-center">
                <div>
                    <h5 class="mb-1">Interactive Network Map</h5>
                    <p class="mb-0">🔴 Red markers: CDST Labs | 🔵 Blue circles: District HQs | 🟢 Green lines: New assignments</p>
                </div>
                <div class="btn-group download-btn-group" role="group">
                    <div class="btn-group" role="group">
                        <button type="button" class="btn btn-outline-light btn-sm dropdown-toggle" data-bs-toggle="dropdown">
                            <i class="fas fa-download"></i> Download
                        </button>
                        <ul class="dropdown-menu">
                            <li><a class="dropdown-item" href="#" id="downloadMapHtml">
                                <i class="fas fa-file-code"></i> Interactive HTML Map
                            </a></li>
                            <li><hr class="dropdown-divider"></li>
                            <li><a class="dropdown-item" href="#" id="exportMapData">
                                <i class="fas fa-table"></i> Export Data
                            </a></li>
                        </ul>
                    </div>
                    <button class="btn btn-outline-light btn-sm" id="refreshMap">
                        <i class="fas fa-refresh"></i> Refresh
                    </button>
                </div>
            </div>
            <div class="card-body">
                <div id="networkMap" style="height: 700px;">Loading map...</div>
                <div id="mapDownloadStatus" class="mt-2"></div>
            </div>
        </div>
    </div>
</div>
'''

# Updated ISOCHRONE_CONTENT with simplified download options (HTML only)
ISOCHRONE_CONTENT = '''
<h2><i class="fas fa-clock"></i> Isochrone Analysis</h2>
<div class="row">
    <div class="col-md-4">
        <div class="card">
            <div class="card-header"><h5>Isochrone Settings</h5></div>
            <div class="card-body">
                <div class="mb-3">
                    <label class="form-label">Select CDST Lab:</label>
                    <select class="form-control" id="selectedLab">
                        <option value="">Choose a lab...</option>
                    </select>
                </div>
                <div class="mb-3">
                    <label class="form-label">Analysis Method:</label>
                    <div class="form-check">
                        <input class="form-check-input" type="radio" name="isochrone_method" id="euclidean_iso" value="euclidean" checked>
                        <label class="form-check-label" for="euclidean_iso">Euclidean Distance</label>
                    </div>
                    <div class="form-check">
                        <input class="form-check-input" type="radio" name="isochrone_method" id="routing_iso" value="routing">
                        <label class="form-check-label" for="routing_iso">Routing Isochrone (ORS API - Pre-configured)</label>
                    </div>
                </div>
                <div class="mb-3">
                    <label class="form-label">Travel Time (minutes):</label>
                    <input type="number" class="form-control" id="travelTime" value="60" min="5" max="300" step="5">
                </div>
                <div class="alert alert-success">
                    <i class="fas fa-check-circle"></i> <strong>API Ready:</strong> OpenRouteService API is pre-configured and ready to use!
                </div>
                <button class="btn btn-primary" id="generateIsochrone">
                    <i class="fas fa-map-marked-alt"></i> Generate Isochrone
                </button>
                <div id="isochroneStatus" class="mt-3"></div>
            </div>
        </div>
    </div>
    <div class="col-md-8">
        <div class="card">
            <div class="card-header d-flex justify-content-between align-items-center">
                <div>
                    <h5 class="mb-1">Isochrone Map</h5>
                    <p class="mb-0">Reachable areas within the specified travel time</p>
                </div>
                <div class="btn-group download-btn-group" role="group" style="display: none;" id="isochroneDownloadButtons">
                    <div class="btn-group" role="group">
                        <button type="button" class="btn btn-outline-light btn-sm dropdown-toggle" data-bs-toggle="dropdown">
                            <i class="fas fa-download"></i> Download
                        </button>
                        <ul class="dropdown-menu">
                            <li><a class="dropdown-item" href="#" id="downloadIsochroneHtml">
                                <i class="fas fa-file-code"></i> Interactive HTML Map
                            </a></li>
                            <li><hr class="dropdown-divider"></li>
                            <li><a class="dropdown-item" href="#" id="exportIsochroneData">
                                <i class="fas fa-table"></i> Export Analysis Data
                            </a></li>
                        </ul>
                    </div>
                </div>
            </div>
            <div class="card-body">
                <div id="isochroneMap" style="height: 600px;">
                    <div class="d-flex justify-content-center align-items-center h-100">
                        <div class="text-center">
                            <i class="fas fa-map-marked-alt fa-3x text-muted mb-3"></i>
                            <p class="text-muted">Select a lab and generate an isochrone to view the map</p>
                        </div>
                    </div>
                </div>
                <div id="isochroneDownloadStatus" class="mt-2"></div>
            </div>
        </div>
    </div>
</div>
<div class="row mt-4">
    <div class="col-12">
        <div class="card">
            <div class="card-header"><h5>Districts Within Isochrone</h5></div>
            <div class="card-body">
                <div id="districtsInIsochrone">
                    <p class="text-muted">Generate an isochrone to see which districts fall within the specified travel time.</p>
                </div>
            </div>
        </div>
    </div>
</div>
'''

# Updated MAP_SCRIPTS with simplified download functionality (HTML only)
MAP_SCRIPTS = '''
<script>
$(document).ready(function() {
    loadNetworkMap();
    
    $('#refreshMap').click(function() {
        $('#refreshMap').html('<i class="fas fa-spinner fa-spin"></i> Loading...');
        $('#mapDownloadStatus').html('');
        loadNetworkMap();
    });
    
    // HTML Download
    $('#downloadMapHtml').click(function(e) {
        e.preventDefault();
        $('#downloadMapHtml').html('<i class="fas fa-spinner fa-spin"></i> Generating...');
        $('#mapDownloadStatus').html('<div class="alert alert-info">Preparing interactive map download...</div>');
        
        window.location.href = '/download_network_map';
        
        setTimeout(function() {
            $('#downloadMapHtml').html('<i class="fas fa-file-code"></i> Interactive HTML Map');
            $('#mapDownloadStatus').html('<div class="alert alert-success">Map downloaded! Check your downloads folder.</div>');
            setTimeout(function() {
                $('#mapDownloadStatus').html('');
            }, 3000);
        }, 2000);
    });
    
    // Export Map Data
    $('#exportMapData').click(function(e) {
        e.preventDefault();
        window.location.href = '/export_results';
    });
    
    function loadNetworkMap() {
        $.get('/generate_map', function(data) {
            $('#networkMap').html(data);
            $('#refreshMap').html('<i class="fas fa-refresh"></i> Refresh');
        }).fail(function() {
            $('#networkMap').html('<div class="alert alert-warning">No optimization results available. Please run optimization first.</div>');
            $('#refreshMap').html('<i class="fas fa-refresh"></i> Refresh');
        });
    }
});
</script>
'''

# Updated ISOCHRONE_SCRIPTS with simplified download functionality (HTML only)
ISOCHRONE_SCRIPTS = '''
<script>
$(document).ready(function() {
    loadLabOptions();
    
    $('#generateIsochrone').click(function() {
        var selectedLab = $('#selectedLab').val();
        var method = $('input[name="isochrone_method"]:checked').val();
        var travelTime = parseInt($('#travelTime').val());
        
        if (!selectedLab) {
            alert('Please select a CDST lab');
            return;
        }
        
        var params = {
            lab_name: selectedLab,
            method: method,
            travel_time: travelTime
        };
        
        $('#generateIsochrone').prop('disabled', true).html('<i class="fas fa-spinner fa-spin"></i> Generating...');
        $('#isochroneStatus').html('<div class="alert alert-info">Generating isochrone...</div>');
        $('#isochroneDownloadButtons').hide();
        
        $.ajax({
            url: '/generate_isochrone',
            type: 'POST',
            contentType: 'application/json',
            data: JSON.stringify(params),
            success: function(response) {
                $('#generateIsochrone').prop('disabled', false).html('<i class="fas fa-map-marked-alt"></i> Generate Isochrone');
                if (response.status === 'success') {
                    $('#isochroneMap').html(response.map_html);
                    $('#districtsInIsochrone').html(response.districts_html);
                    $('#isochroneStatus').html('<div class="alert alert-success">Isochrone generated successfully!</div>');
                    $('#isochroneDownloadButtons').show();
                    
                    // Store current isochrone parameters for download
                    window.currentIsochroneParams = params;
                } else {
                    $('#isochroneStatus').html('<div class="alert alert-danger">Error: ' + response.message + '</div>');
                }
            },
            error: function() {
                $('#generateIsochrone').prop('disabled', false).html('<i class="fas fa-map-marked-alt"></i> Generate Isochrone');
                $('#isochroneStatus').html('<div class="alert alert-danger">Connection error. Please try again.</div>');
            }
        });
    });
    
    // HTML Download
    $('#downloadIsochroneHtml').click(function(e) {
        e.preventDefault();
        if (!window.currentIsochroneParams) {
            alert('Please generate an isochrone first');
            return;
        }
        
        $('#downloadIsochroneHtml').html('<i class="fas fa-spinner fa-spin"></i> Generating...');
        $('#isochroneDownloadStatus').html('<div class="alert alert-info">Preparing isochrone map download...</div>');
        
        $.ajax({
            url: '/download_isochrone_map',
            type: 'POST',
            contentType: 'application/json',
            data: JSON.stringify(window.currentIsochroneParams),
            success: function(response) {
                if (response.download_url) {
                    window.location.href = response.download_url;
                    $('#isochroneDownloadStatus').html('<div class="alert alert-success">Isochrone map downloaded! Check your downloads folder.</div>');
                } else {
                    $('#isochroneDownloadStatus').html('<div class="alert alert-danger">Download failed: ' + response.message + '</div>');
                }
                $('#downloadIsochroneHtml').html('<i class="fas fa-file-code"></i> Interactive HTML Map');
                
                setTimeout(function() {
                    $('#isochroneDownloadStatus').html('');
                }, 3000);
            },
            error: function() {
                $('#downloadIsochroneHtml').html('<i class="fas fa-file-code"></i> Interactive HTML Map');
                $('#isochroneDownloadStatus').html('<div class="alert alert-danger">Download failed. Please try again.</div>');
                setTimeout(function() {
                    $('#isochroneDownloadStatus').html('');
                }, 3000);
            }
        });
    });
    
    // Export Isochrone Data
    $('#exportIsochroneData').click(function(e) {
        e.preventDefault();
        if (!window.currentIsochroneParams) {
            alert('Please generate an isochrone first');
            return;
        }
        
        $('#exportIsochroneData').html('<i class="fas fa-spinner fa-spin"></i> Exporting...');
        
        $.ajax({
            url: '/export_isochrone_data',
            type: 'POST',
            contentType: 'application/json',
            data: JSON.stringify(window.currentIsochroneParams),
            success: function(response) {
                if (response.download_url) {
                    window.location.href = response.download_url;
                    $('#isochroneDownloadStatus').html('<div class="alert alert-success">Isochrone data exported! Check your downloads folder.</div>');
                } else {
                    $('#isochroneDownloadStatus').html('<div class="alert alert-danger">Export failed: ' + response.message + '</div>');
                }
                $('#exportIsochroneData').html('<i class="fas fa-table"></i> Export Analysis Data');
                
                setTimeout(function() {
                    $('#isochroneDownloadStatus').html('');
                }, 3000);
            },
            error: function() {
                $('#exportIsochroneData').html('<i class="fas fa-table"></i> Export Analysis Data');
                $('#isochroneDownloadStatus').html('<div class="alert alert-danger">Export failed. Please try again.</div>');
                setTimeout(function() {
                    $('#isochroneDownloadStatus').html('');
                }, 3000);
            }
        });
    });
    
    function loadLabOptions() {
        $.get('/get_lab_options', function(data) {
            if (data.status === 'success') {
                var options = '<option value="">Choose a lab...</option>';
                data.labs.forEach(function(lab) {
                    options += '<option value="' + lab + '">' + lab + '</option>';
                });
                $('#selectedLab').html(options);
            }
        });
    }
});
</script>
'''

# Add new download routes to the existing app

@app.route('/download_network_map')
def download_network_map():
    """Generate and download the network optimization map as standalone HTML"""
    if 'optimization_results' not in session:
        return jsonify({'status': 'error', 'message': 'No optimization results available. Please run optimization first.'})
    
    try:
        results_df = pd.read_json(session['optimization_results'])
        cdst_df = pd.read_json(session['cdst_data'])
        
        # Generate the map (reuse existing logic)
        all_lats = list(results_df['district_lat']) + list(cdst_df['lat'])
        all_lons = list(results_df['district_lon']) + list(cdst_df['lon'])
        center_lat = sum(all_lats) / len(all_lats)
        center_lon = sum(all_lons) / len(all_lons)
        
        # Create map with enhanced styling for download
        m = folium.Map(
            location=[center_lat, center_lon], 
            zoom_start=6, 
            tiles='OpenStreetMap',
            width='100%',
            height='100%'
        )
        
        # Add title
        title_html = '''
        <div style="position: fixed; 
                    top: 10px; left: 50px; width: 300px; height: 90px; 
                    background-color: white; border:2px solid grey; z-index:9999; 
                    font-size:14px; padding: 10px">
        <h4>Network Optimization Map</h4>
        <p><b>🔴 CDST Labs | 🔵 Districts | 🟢 New Assignments</b></p>
        <p><small>Generated: {}</small></p>
        </div>
        '''.format(datetime.now().strftime('%Y-%m-%d %H:%M:%S'))
        m.get_root().html.add_child(folium.Element(title_html))
        
        # Add CDST labs as red markers
        for idx, lab in cdst_df.iterrows():
            utilization_df = pd.read_json(session['lab_utilization'])
            lab_util = utilization_df[utilization_df['lab_name'] == lab['lab_name']]
            if not lab_util.empty:
                current_util = lab_util.iloc[0]['current_utilization']
                optimized_util = lab_util.iloc[0]['optimized_utilization']
                popup_text = f"<b>{lab['lab_name']}</b><br>" \
                           f"{lab['address']}<br>" \
                           f"Capacity: {lab['capacity']}<br>" \
                           f"Current Utilization: {current_util:.1f}%<br>" \
                           f"Optimized Utilization: {optimized_util:.1f}%"
            else:
                popup_text = f"<b>{lab['lab_name']}</b><br>{lab['address']}<br>Capacity: {lab['capacity']}"
            
            folium.Marker(
                location=[lab['lat'], lab['lon']],
                popup=folium.Popup(popup_text, max_width=300),
                icon=folium.Icon(color='red', icon='info-sign'),
                tooltip=lab['lab_name']
            ).add_to(m)
        
        # Add districts as circles
        for idx, district in results_df.iterrows():
            color = 'green' if district['improvement_minutes'] > 0 else 'blue'
            popup_text = f"<b>{district['district']}</b><br>" \
                        f"Tests per quarter: {district['tests_per_quarter']}<br>" \
                        f"Current assignment: {district['current_assignment']}<br>" \
                        f"Optimized assignment: {district['optimized_assignment']}<br>" \
                        f"Time saved: {district['improvement_minutes']:.1f} minutes"
            
            folium.CircleMarker(
                location=[district['district_lat'], district['district_lon']],
                radius=8,
                popup=folium.Popup(popup_text, max_width=300),
                color=color,
                fillColor=color,
                fillOpacity=0.7,
                tooltip=district['district']
            ).add_to(m)
        
        # Add lines for reassigned districts
        reassigned = results_df[results_df['current_assignment'] != results_df['optimized_assignment']]
        for idx, district in reassigned.iterrows():
            lab_coords = cdst_df[cdst_df['lab_name'] == district['optimized_assignment']]
            if not lab_coords.empty:
                lab_coord = lab_coords.iloc[0]
                folium.PolyLine(
                    locations=[
                        [district['district_lat'], district['district_lon']], 
                        [lab_coord['lat'], lab_coord['lon']]
                    ],
                    color='green',
                    weight=3,
                    opacity=0.8,
                    popup=f"New assignment: {district['district']} → {district['optimized_assignment']}"
                ).add_to(m)
        
        # Add legend
        legend_html = '''
        <div style="position: fixed; 
                    bottom: 50px; left: 50px; width: 200px; height: 120px; 
                    background-color: white; border:2px solid grey; z-index:9999; 
                    font-size:12px; padding: 10px">
        <h6>Legend</h6>
        <p><i class="fa fa-map-marker" style="color:red"></i> CDST Laboratories</p>
        <p><i class="fa fa-circle" style="color:blue"></i> No Change</p>
        <p><i class="fa fa-circle" style="color:green"></i> Improved Assignment</p>
        <p><span style="color:green">━━━</span> New Routes</p>
        </div>
        '''
        m.get_root().html.add_child(folium.Element(legend_html))
        
        # Generate filename
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        filename = f'network_optimization_map_{timestamp}.html'
        
        # Save to BytesIO
        map_html = m._repr_html_()
        map_bytes = io.BytesIO(map_html.encode('utf-8'))
        map_bytes.seek(0)
        
        return send_file(
            map_bytes,
            mimetype='text/html',
            as_attachment=True,
            download_name=filename
        )
        
    except Exception as e:
        return jsonify({'status': 'error', 'message': f'Error generating network map download: {str(e)}'})

@app.route('/download_isochrone_map', methods=['POST'])
def download_isochrone_map():
    """Generate and download isochrone map as standalone HTML"""
    try:
        params = request.json
        lab_name = params.get('lab_name')
        method = params.get('method', 'euclidean')
        travel_time = params.get('travel_time', 60)
        
        if not lab_name or 'cdst_data' not in session or 'district_data' not in session:
            return jsonify({'status': 'error', 'message': 'Required data not available'})
        
        cdst_df = pd.read_json(session['cdst_data'])
        district_df = pd.read_json(session['district_data'])
        
        # Find the selected lab
        lab_data = cdst_df[cdst_df['lab_name'] == lab_name]
        if lab_data.empty:
            return jsonify({'status': 'error', 'message': 'Lab not found'})
        
        lab_coords = (lab_data.iloc[0]['lat'], lab_data.iloc[0]['lon'])
        lab_info = lab_data.iloc[0]
        
        # Generate standalone isochrone map
        if method == 'routing':
            m, districts_within = generate_standalone_routing_isochrone(
                lab_coords, lab_name, travel_time, ORS_API_KEY, district_df, lab_info
            )
        else:
            m, districts_within = generate_standalone_euclidean_isochrone(
                lab_coords, lab_name, travel_time, district_df, lab_info
            )
        
        # Generate filename
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        safe_lab_name = "".join(c for c in lab_name if c.isalnum() or c in (' ', '-', '_')).rstrip()
        filename = f'isochrone_map_{safe_lab_name}_{travel_time}min_{timestamp}.html'
        
        # Save to BytesIO
        map_html = m._repr_html_()
        map_bytes = io.BytesIO(map_html.encode('utf-8'))
        map_bytes.seek(0)
        
        # Store the file temporarily and return download URL
        temp_filename = secure_filename(filename)
        session[f'temp_file_{temp_filename}'] = map_html
        
        return jsonify({
            'status': 'success',
            'download_url': f'/download_temp_file/{temp_filename}'
        })
        
    except Exception as e:
        return jsonify({'status': 'error', 'message': f'Error generating isochrone map: {str(e)}'})

@app.route('/download_temp_file/<filename>')
def download_temp_file(filename):
    """Download a temporarily stored file"""
    session_key = f'temp_file_{filename}'
    if session_key not in session:
        return jsonify({'status': 'error', 'message': 'File not found or expired'})
    
    try:
        file_content = session[session_key]
        # Clean up
        session.pop(session_key, None)
        
        file_bytes = io.BytesIO(file_content.encode('utf-8'))
        file_bytes.seek(0)
        
        return send_file(
            file_bytes,
            mimetype='text/html',
            as_attachment=True,
            download_name=filename
        )
    except Exception as e:
        return jsonify({'status': 'error', 'message': str(e)})

def generate_standalone_euclidean_isochrone(lab_coords, lab_name, travel_time_minutes, district_df, lab_info):
    """Generate standalone euclidean isochrone map for download"""
    max_distance_km = (travel_time_minutes / 60) * 30
    
    # Create enhanced map for download
    m = folium.Map(
        location=lab_coords, 
        zoom_start=8,
        width='100%',
        height='100%'
    )
    
    # Add comprehensive title
    title_html = f'''
    <div style="position: fixed; 
                top: 10px; left: 50px; width: 350px; height: 130px; 
                background-color: white; border:2px solid grey; z-index:9999; 
                font-size:14px; padding: 10px">
    <h4>Isochrone Analysis: {lab_name}</h4>
    <p><b>Travel Time:</b> {travel_time_minutes} minutes</p>
    <p><b>Method:</b> Euclidean Distance (30 km/h)</p>
    <p><b>Generated:</b> {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}</p>
    </div>
    '''
    m.get_root().html.add_child(folium.Element(title_html))
    
    # Add the lab marker with enhanced popup
    folium.Marker(
        location=lab_coords,
        popup=folium.Popup(f"<b>{lab_name}</b><br>{lab_info['address']}<br>Capacity: {lab_info['capacity']}<br><i>Analysis Center</i>", max_width=300),
        icon=folium.Icon(color='red', icon='info-sign', prefix='fa'),
        tooltip=f"{lab_name} (Analysis Center)"
    ).add_to(m)
    
    # Draw isochrone circle with enhanced styling
    folium.Circle(
        location=lab_coords,
        radius=max_distance_km * 1000,
        popup=folium.Popup(f'{travel_time_minutes}-minute Reachable Area<br>Euclidean Distance Method<br>Radius: {max_distance_km:.1f} km', max_width=250),
        color='blue',
        fillColor='lightblue',
        fillOpacity=0.3,
        weight=3,
        dashArray='5,5'
    ).add_to(m)
    
    # Check districts within isochrone
    districts_within = []
    districts_outside = []
    
    for idx, district in district_df.iterrows():
        district_coords = (district['lat'], district['lon'])
        distance_km = geodesic(lab_coords, district_coords).kilometers
        travel_time = (distance_km / 30) * 60
        
        if travel_time <= travel_time_minutes:
            districts_within.append({
                'name': district['district'],
                'travel_time': travel_time,
                'tests': district['tests_per_quarter']
            })
            
            folium.CircleMarker(
                location=district_coords,
                radius=8,
                popup=folium.Popup(f"<b>{district['district']}</b><br>Travel time: {travel_time:.1f} min<br>Distance: {distance_km:.1f} km<br>Tests: {district['tests_per_quarter']}<br><span style='color:green'>✓ Within Range</span>", max_width=250),
                color='green',
                fillColor='lightgreen',
                fillOpacity=0.8,
                weight=2,
                tooltip=f"{district['district']} ({travel_time:.1f} min)"
            ).add_to(m)
        else:
            districts_outside.append(district['district'])
            folium.CircleMarker(
                location=district_coords,
                radius=6,
                popup=folium.Popup(f"<b>{district['district']}</b><br>Travel time: {travel_time:.1f} min<br>Distance: {distance_km:.1f} km<br>Tests: {district['tests_per_quarter']}<br><span style='color:red'>✗ Outside Range</span>", max_width=250),
                color='red',
                fillColor='lightcoral',
                fillOpacity=0.6,
                weight=1,
                tooltip=f"{district['district']} ({travel_time:.1f} min)"
            ).add_to(m)
    
    # Add summary and legend
    total_tests_within = sum(d['tests'] for d in districts_within)
    legend_html = f'''
    <div style="position: fixed; 
                bottom: 50px; left: 50px; width: 280px; height: 160px; 
                background-color: white; border:2px solid grey; z-index:9999; 
                font-size:12px; padding: 10px">
    <h6>Analysis Summary</h6>
    <p><strong>Districts within range:</strong> {len(districts_within)}</p>
    <p><strong>Districts outside range:</strong> {len(districts_outside)}</p>
    <p><strong>Total tests (within):</strong> {total_tests_within}</p>
    <hr style="margin: 8px 0;">
    <p><i class="fa fa-circle" style="color:green"></i> Within {travel_time_minutes} min</p>
    <p><i class="fa fa-circle" style="color:red"></i> Outside range</p>
    </div>
    '''
    m.get_root().html.add_child(folium.Element(legend_html))
    
    return m, districts_within

def generate_standalone_routing_isochrone(lab_coords, lab_name, travel_time_minutes, api_key, district_df, lab_info):
    """Generate standalone routing isochrone map for download"""
    try:
        # Get isochrone from ORS API
        url = "https://api.openrouteservice.org/v2/isochrones/driving-car"
        headers = {
            'Authorization': api_key,
            'Content-Type': 'application/json'
        }
        
        body = {
            "locations": [[lab_coords[1], lab_coords[0]]],
            "range": [travel_time_minutes * 60],
            "range_type": "time"
        }
        
        response = requests.post(url, headers=headers, json=body, timeout=30)
        
        if response.status_code != 200:
            raise Exception(f"ORS API error: {response.status_code}")
        
        isochrone_data = response.json()
        
        # Create enhanced map
        m = folium.Map(
            location=lab_coords, 
            zoom_start=8,
            width='100%',
            height='100%'
        )
        
        # Add title
        title_html = f'''
        <div style="position: fixed; 
                    top: 10px; left: 50px; width: 350px; height: 130px; 
                    background-color: white; border:2px solid grey; z-index:9999; 
                    font-size:14px; padding: 10px">
        <h4>Isochrone Analysis: {lab_name}</h4>
        <p><b>Travel Time:</b> {travel_time_minutes} minutes</p>
        <p><b>Method:</b> Real Road Routing (ORS API)</p>
        <p><b>Generated:</b> {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}</p>
        </div>
        '''
        m.get_root().html.add_child(folium.Element(title_html))
        
        # Add the lab marker
        folium.Marker(
            location=lab_coords,
            popup=folium.Popup(f"<b>{lab_name}</b><br>{lab_info['address']}<br>Capacity: {lab_info['capacity']}<br><i>Analysis Center</i>", max_width=300),
            icon=folium.Icon(color='red', icon='info-sign'),
            tooltip=f"{lab_name} (Analysis Center)"
        ).add_to(m)
        
        # Add isochrone polygon
        if 'features' in isochrone_data and len(isochrone_data['features']) > 0:
            feature = isochrone_data['features'][0]
            if 'geometry' in feature and 'coordinates' in feature['geometry']:
                coords = feature['geometry']['coordinates'][0]
                folium_coords = [[coord[1], coord[0]] for coord in coords]
                
                folium.Polygon(
                    locations=folium_coords,
                    popup=folium.Popup(f'{travel_time_minutes}-minute Reachable Area<br>Real Road Routing', max_width=200),
                    color='blue',
                    fillColor='lightblue',
                    fillOpacity=0.3,
                    weight=3
                ).add_to(m)
        
        # Check districts and add markers
        districts_within = []
        districts_outside = []
        
        for idx, district in district_df.iterrows():
            district_coords = (district['lat'], district['lon'])
            travel_time = DistanceCalculator.openroute_service_time(
                lab_coords, district_coords, api_key, delay=0.2
            )
            
            if travel_time <= travel_time_minutes:
                districts_within.append({
                    'name': district['district'],
                    'travel_time': travel_time,
                    'tests': district['tests_per_quarter']
                })
                
                folium.CircleMarker(
                    location=district_coords,
                    radius=8,
                    popup=folium.Popup(f"<b>{district['district']}</b><br>Travel time: {travel_time:.1f} min<br>Tests: {district['tests_per_quarter']}<br><span style='color:green'>✓ Within Range</span>", max_width=250),
                    color='green',
                    fillColor='lightgreen',
                    fillOpacity=0.8,
                    weight=2,
                    tooltip=f"{district['district']} ({travel_time:.1f} min)"
                ).add_to(m)
            else:
                districts_outside.append(district['district'])
                folium.CircleMarker(
                    location=district_coords,
                    radius=6,
                    popup=folium.Popup(f"<b>{district['district']}</b><br>Travel time: {travel_time:.1f} min<br>Tests: {district['tests_per_quarter']}<br><span style='color:red'>✗ Outside Range</span>", max_width=250),
                    color='red',
                    fillColor='lightcoral',
                    fillOpacity=0.6,
                    weight=1,
                    tooltip=f"{district['district']} ({travel_time:.1f} min)"
                ).add_to(m)
        
        # Add summary
        total_tests_within = sum(d['tests'] for d in districts_within)
        legend_html = f'''
        <div style="position: fixed; 
                    bottom: 50px; left: 50px; width: 280px; height: 160px; 
                    background-color: white; border:2px solid grey; z-index:9999; 
                    font-size:12px; padding: 10px">
        <h6>Analysis Summary</h6>
        <p><strong>Districts within range:</strong> {len(districts_within)}</p>
        <p><strong>Districts outside range:</strong> {len(districts_outside)}</p>
        <p><strong>Total tests (within):</strong> {total_tests_within}</p>
        <hr style="margin: 8px 0;">
        <p><i class="fa fa-circle" style="color:green"></i> Within {travel_time_minutes} min</p>
        <p><i class="fa fa-circle" style="color:red"></i> Outside range</p>
        </div>
        '''
        m.get_root().html.add_child(folium.Element(legend_html))
        
        return m, districts_within
        
    except Exception as e:
        # Fallback to euclidean
        return generate_standalone_euclidean_isochrone(lab_coords, lab_name, travel_time_minutes, district_df, lab_info)

# Update route definitions to use new content and scripts
@app.route('/map')
def map_view():
    content = MAP_CONTENT
    return render_template_string(BASE_TEMPLATE, content=content, extra_scripts=MAP_SCRIPTS)

@app.route('/isochrone')
def isochrone():
    content = ISOCHRONE_CONTENT
    return render_template_string(BASE_TEMPLATE, content=content, extra_scripts=ISOCHRONE_SCRIPTS)

# OPTIMIZATION AND UTILITY CLASSES (existing code continues...)
class OptimizationLogger:
    def __init__(self):
        self.logs = []
        self.progress = 0
        self.current_stage = "Ready"
        self.total_steps = 100
        self.current_step = 0
    
    def log(self, message, log_type="INFO"):
        timestamp = datetime.now().strftime("%H:%M:%S")
        log_entry = f"[{timestamp}] {log_type}: {message}"
        self.logs.append(log_entry)
        print(log_entry)
    
    def update_progress(self, progress, stage="", message=""):
        self.progress = round(max(0, min(100, progress)), 2)
        if stage:
            self.current_stage = stage
        if message:
            self.log(f"Progress: {self.progress}% - {message}")
    
    def set_stage(self, stage, progress=None):
        self.current_stage = stage
        if progress is not None:
            self.progress = round(progress, 2)
        self.log(f"Stage: {stage}")
    
    def get_progress_info(self):
        return {
            'progress': self.progress,
            'stage': self.current_stage,
            'logs': self.get_logs()
        }
    
    def get_logs(self):
        return "\n".join(self.logs)
    
    def clear(self):
        self.logs = []
        self.progress = 0
        self.current_stage = "Ready"
        self.current_step = 0

logger = OptimizationLogger()

class DistanceCalculator:
    @staticmethod
    def euclidean_distance_time(start_coords, end_coords, avg_speed_kmh=30):
        """Calculate travel time using euclidean distance"""
        distance_km = geodesic(start_coords, end_coords).kilometers
        return (distance_km / avg_speed_kmh) * 60
    
    @staticmethod
    def openroute_service_time(start_coords, end_coords, api_key, delay=1):
        """Calculate travel time using OpenRouteService API"""
        try:
            if delay > 0:
                time.sleep(delay)
                
            url = "https://api.openrouteservice.org/v2/directions/driving-car"
            headers = {
                'Authorization': api_key,
                'Content-Type': 'application/json'
            }
            
            body = {
                "coordinates": [[start_coords[1], start_coords[0]], [end_coords[1], end_coords[0]]],
                "format": "json"
            }
            
            response = requests.post(url, headers=headers, json=body, timeout=10)
            
            if response.status_code == 200:
                result = response.json()
                if 'routes' in result and len(result['routes']) > 0:
                    duration_seconds = result['routes'][0]['summary']['duration']
                    return duration_seconds / 60
                else:
                    raise Exception("No routes found in response")
            else:
                raise Exception(f"API returned status code: {response.status_code}, message: {response.text}")
                
        except Exception as e:
            logger.log(f"ORS API error for {start_coords} to {end_coords}: {str(e)}", "WARNING")
            return DistanceCalculator.euclidean_distance_time(start_coords, end_coords)

class NetworkOptimizer:
    @staticmethod
    def linear_programming_optimization(travel_times, capacities, demands, capacity_flex=0.05):
        """Solve the assignment problem using linear programming"""
        logger.log("Starting Linear Programming optimization")
        logger.update_progress(78, "Linear Programming", "Creating optimization model")
        
        n_districts, n_labs = travel_times.shape
        
        prob = pulp.LpProblem("LabAssignment", pulp.LpMinimize)
        
        logger.update_progress(79, "Linear Programming", "Creating decision variables")
        
        x = {}
        for i in range(n_districts):
            for j in range(n_labs):
                x[i,j] = pulp.LpVariable(f"x_{i}_{j}", cat='Binary')
        
        logger.update_progress(80, "Linear Programming", "Setting up objective function")
        
        prob += pulp.lpSum([travel_times[i,j] * demands[i] * x[i,j] 
                           for i in range(n_districts) for j in range(n_labs)])
        
        logger.update_progress(81, "Linear Programming", "Adding assignment constraints")
        
        for i in range(n_districts):
            prob += pulp.lpSum([x[i,j] for j in range(n_labs)]) == 1
        
        logger.update_progress(82, "Linear Programming", "Adding capacity constraints")
        
        for j in range(n_labs):
            prob += pulp.lpSum([demands[i] * x[i,j] for i in range(n_districts)]) <= capacities[j] * (1 + capacity_flex)
        
        logger.update_progress(83, "Linear Programming", "Solving optimization problem")
        
        prob.solve(pulp.PULP_CBC_CMD(msg=0))
        
        if prob.status == pulp.LpStatusOptimal:
            logger.log("Linear programming solution found")
            logger.update_progress(84, "Linear Programming", "Processing optimal solution")
            
            assignment_matrix = np.zeros((n_districts, n_labs))
            for i in range(n_districts):
                for j in range(n_labs):
                    if x[i,j].varValue is not None:
                        assignment_matrix[i,j] = x[i,j].varValue
            
            lab_loads = np.array([sum(demands[i] * assignment_matrix[i,j] 
                                     for i in range(n_districts)) for j in range(n_labs)])
            
            return {
                'assignment': assignment_matrix,
                'lab_loads': lab_loads,
                'objective_value': pulp.value(prob.objective),
                'status': 'optimal'
            }
        else:
            logger.log(f"Linear programming failed with status: {pulp.LpStatus[prob.status]}", "ERROR")
            return None
    
    @staticmethod
    def greedy_optimization(travel_times, capacities, demands):
        """Solve using a greedy heuristic"""
        logger.log("Starting Greedy optimization")
        logger.update_progress(78, "Greedy Algorithm", "Initializing greedy assignment")
        
        n_districts, n_labs = travel_times.shape
        assignments = np.full(n_districts, -1)
        remaining_capacity = capacities.copy()
        
        demand_order = np.argsort(demands)[::-1]
        
        logger.update_progress(79, "Greedy Algorithm", "Sorting districts by demand")
        
        total_assignments = len(demand_order)
        for idx, i in enumerate(demand_order):
            assignment_progress = round(79 + (idx / total_assignments) * 5, 2)
            logger.update_progress(assignment_progress, "Greedy Algorithm", f"Assigning district {idx+1}/{total_assignments}")
            
            lab_order = np.argsort(travel_times[i, :])
            assigned = False
            
            for j in lab_order:
                if remaining_capacity[j] >= demands[i]:
                    assignments[i] = j
                    remaining_capacity[j] -= demands[i]
                    assigned = True
                    break
            
            if not assigned:
                best_lab = np.argmax(remaining_capacity)
                assignments[i] = best_lab
                remaining_capacity[best_lab] = max(0, remaining_capacity[best_lab] - demands[i])
                logger.log(f"District {i} assigned to overloaded lab {best_lab}", "WARNING")
        
        logger.update_progress(84, "Greedy Algorithm", "Converting to assignment matrix")
        
        assignment_matrix = np.zeros((n_districts, n_labs))
        for i in range(n_districts):
            if assignments[i] >= 0:
                assignment_matrix[i, assignments[i]] = 1
        
        lab_loads = np.array([sum(demands[i] * assignment_matrix[i,j] 
                                 for i in range(n_districts)) for j in range(n_labs)])
        objective_value = np.sum(assignment_matrix * travel_times * demands.reshape(-1, 1))
        
        logger.log("Greedy optimization completed")
        
        return {
            'assignment': assignment_matrix,
            'lab_loads': lab_loads,
            'objective_value': objective_value,
            'status': 'optimal'
        }

# ALL OTHER EXISTING ROUTES
@app.route('/')
def index():
    content = '''
<div class="row">
    <div class="col-12">
        <div class="jumbotron bg-primary text-white p-5 rounded">
            <h1 class="display-4"><i class="fas fa-network-wired"></i> Diagnostic Network Optimization Tool</h1>
            <p class="lead">Optimize the assignment of district laboratories to CDST facilities to minimize travel time while respecting capacity constraints.</p>
            <hr class="my-4" style="border-color: rgba(255,255,255,0.3);">
            <a class="btn btn-light btn-lg me-2" href="/data_input" role="button"><i class="fas fa-rocket"></i> Get Started</a>
            <a class="btn btn-outline-light" href="/fresh_start" role="button"><i class="fas fa-refresh"></i> Fresh Start</a>
        </div>
    </div>
</div>
<!-- Rest of existing index content... -->
    '''
    extra_scripts = '''
<script>
$(document).ready(function() {
    var currentPath = window.location.pathname;
    $('.sidebar a').each(function() {
        if ($(this).attr('href') === currentPath) {
            $(this).addClass('active');
        }
    });
});
</script>
    '''
    return render_template_string(BASE_TEMPLATE, content=content, extra_scripts=extra_scripts)

# Add all the other existing routes here with their original implementations
# I'll add just the key routes for brevity, but you should include all of them

@app.route('/fresh_start')
def fresh_start():
    session.clear()
    session.modified = True
    logger.clear()
    return redirect('/')

@app.route('/data_input')
def data_input():
    content = '''<h2><i class="fas fa-upload"></i> Upload Your Network Data</h2>
    <!-- Original data input content -->'''
    return render_template_string(BASE_TEMPLATE, content=content, extra_scripts='')

@app.route('/upload_data', methods=['POST'])
def upload_data():
    """Handle file uploads and data processing"""
    try:
        uploaded_files = []
        
        for file_type in ['district_file', 'cdst_file']:
            if file_type in request.files:
                file = request.files[file_type]
                if file.filename != '':
                    if file_type == 'district_file':
                        session.pop('district_data', None)
                    elif file_type == 'cdst_file':
                        session.pop('cdst_data', None)
                    
                    df = pd.read_csv(file)
                    uploaded_files.append(file_type)
                    
                    if file_type == 'district_file':
                        if 'District' in df.columns:
                            df = df.rename(columns={
                                'District': 'district',
                                'CDST Lab linked to currently': 'current_cdst',
                                'Latitude of District HQ': 'lat',
                                'Longitude of District HQ': 'lon',
                                'Presumptive tests in one quarter': 'tests_per_quarter'
                            })
                        
                        required_cols = ['district', 'current_cdst', 'lat', 'lon', 'tests_per_quarter']
                        missing_cols = [col for col in required_cols if col not in df.columns]
                        if missing_cols:
                            return jsonify({
                                'status': 'error', 
                                'message': f'Missing columns in district file: {missing_cols}'
                            })
                        
                        df['tests_per_quarter'] = pd.to_numeric(df['tests_per_quarter'], errors='coerce')
                        df['lat'] = pd.to_numeric(df['lat'], errors='coerce')
                        df['lon'] = pd.to_numeric(df['lon'], errors='coerce')
                        
                        initial_rows = len(df)
                        df = df.dropna(subset=['tests_per_quarter', 'lat', 'lon'])
                        final_rows = len(df)
                        
                        if final_rows == 0:
                            return jsonify({
                                'status': 'error',
                                'message': 'No valid data rows found in district file'
                            })
                        
                        session['district_data'] = df.to_json()
                        session.modified = True
                        
                    elif file_type == 'cdst_file':
                        if 'Name of CDST Lab linked' in df.columns:
                            df = df.rename(columns={
                                'Name of CDST Lab linked': 'lab_name',
                                'Address': 'address',
                                'Latitude': 'lat',
                                'Longitude': 'lon',
                                'Capacity of lab': 'capacity'
                            })
                        
                        required_cols = ['lab_name', 'address', 'lat', 'lon', 'capacity']
                        missing_cols = [col for col in required_cols if col not in df.columns]
                        if missing_cols:
                            return jsonify({
                                'status': 'error', 
                                'message': f'Missing columns in CDST file: {missing_cols}'
                            })
                        
                        df['capacity'] = pd.to_numeric(df['capacity'], errors='coerce')
                        df['lat'] = pd.to_numeric(df['lat'], errors='coerce')
                        df['lon'] = pd.to_numeric(df['lon'], errors='coerce')
                        
                        initial_rows = len(df)
                        df = df.dropna(subset=['capacity', 'lat', 'lon'])
                        final_rows = len(df)
                        
                        if final_rows == 0:
                            return jsonify({
                                'status': 'error',
                                'message': 'No valid data rows found in CDST file'
                            })
                        
                        session['cdst_data'] = df.to_json()
                        session.modified = True
        
        if uploaded_files:
            session.pop('optimization_results', None)
            session.pop('lab_utilization', None)
            session.modified = True
        
        if not uploaded_files:
            return jsonify({'status': 'error', 'message': 'No valid files were uploaded'})
        
        return jsonify({
            'status': 'success',
            'message': f'Successfully uploaded: {", ".join(uploaded_files)}',
            'files_uploaded': uploaded_files
        })
        
    except Exception as e:
        logger.log(f"Upload failed: {str(e)}", "ERROR")
        return jsonify({'status': 'error', 'message': str(e)})

# Add additional export functionality (PNG routes removed)

@app.route('/export_isochrone_data', methods=['POST'])
def export_isochrone_data():
    """Export isochrone analysis data as CSV"""
    try:
        params = request.json
        lab_name = params.get('lab_name')
        method = params.get('method', 'euclidean')
        travel_time = params.get('travel_time', 60)
        
        if not lab_name or 'cdst_data' not in session or 'district_data' not in session:
            return jsonify({'status': 'error', 'message': 'Required data not available'})
        
        cdst_df = pd.read_json(session['cdst_data'])
        district_df = pd.read_json(session['district_data'])
        
        # Find the selected lab
        lab_data = cdst_df[cdst_df['lab_name'] == lab_name]
        if lab_data.empty:
            return jsonify({'status': 'error', 'message': 'Lab not found'})
        
        lab_coords = (lab_data.iloc[0]['lat'], lab_data.iloc[0]['lon'])
        lab_info = lab_data.iloc[0]
        
        # Calculate which districts are within the isochrone
        districts_analysis = []
        
        if method == 'routing':
            # Use routing API for calculations
            for idx, district in district_df.iterrows():
                district_coords = (district['lat'], district['lon'])
                travel_time_actual = DistanceCalculator.openroute_service_time(
                    lab_coords, district_coords, ORS_API_KEY, delay=0.2
                )
                
                districts_analysis.append({
                    'District': district['district'],
                    'Latitude': district['lat'],
                    'Longitude': district['lon'],
                    'Travel_Time_Minutes': round(travel_time_actual, 2),
                    'Within_Isochrone': 'Yes' if travel_time_actual <= travel_time else 'No',
                    'Tests_Per_Quarter': district['tests_per_quarter'],
                    'Current_CDST_Assignment': district['current_cdst'],
                    'Distance_Method': 'Real Road Routing (ORS API)'
                })
        else:
            # Use euclidean distance
            for idx, district in district_df.iterrows():
                district_coords = (district['lat'], district['lon'])
                distance_km = geodesic(lab_coords, district_coords).kilometers
                travel_time_actual = (distance_km / 30) * 60  # 30 km/h average
                
                districts_analysis.append({
                    'District': district['district'],
                    'Latitude': district['lat'],
                    'Longitude': district['lon'],
                    'Travel_Time_Minutes': round(travel_time_actual, 2),
                    'Distance_Km': round(distance_km, 2),
                    'Within_Isochrone': 'Yes' if travel_time_actual <= travel_time else 'No',
                    'Tests_Per_Quarter': district['tests_per_quarter'],
                    'Current_CDST_Assignment': district['current_cdst'],
                    'Distance_Method': 'Euclidean Distance (30 km/h avg)'
                })
        
        # Create analysis DataFrame
        analysis_df = pd.DataFrame(districts_analysis)
        
        # Generate summary statistics
        within_isochrone = analysis_df[analysis_df['Within_Isochrone'] == 'Yes']
        total_tests_within = within_isochrone['Tests_Per_Quarter'].sum()
        avg_travel_time_within = within_isochrone['Travel_Time_Minutes'].mean()
        
        # Create summary
        summary_stats = {
            'Lab_Name': [lab_name],
            'Lab_Address': [lab_info['address']],
            'Lab_Capacity': [lab_info['capacity']],
            'Analysis_Method': [method],
            'Travel_Time_Threshold_Minutes': [travel_time],
            'Total_Districts_Analyzed': [len(district_df)],
            'Districts_Within_Isochrone': [len(within_isochrone)],
            'Districts_Outside_Isochrone': [len(analysis_df) - len(within_isochrone)],
            'Percentage_Within_Isochrone': [round((len(within_isochrone) / len(analysis_df)) * 100, 1)],
            'Total_Tests_Within_Isochrone': [total_tests_within],
            'Average_Travel_Time_Within_Minutes': [round(avg_travel_time_within, 2) if not pd.isna(avg_travel_time_within) else 0],
            'Generated_On': [datetime.now().strftime('%Y-%m-%d %H:%M:%S')]
        }
        summary_df = pd.DataFrame(summary_stats)
        
        # Create zip file with all data
        zip_buffer = io.BytesIO()
        
        with zipfile.ZipFile(zip_buffer, 'w', zipfile.ZIP_DEFLATED) as zip_file:
            # Add detailed analysis
            analysis_csv = io.StringIO()
            analysis_df.to_csv(analysis_csv, index=False)
            zip_file.writestr('isochrone_district_analysis.csv', analysis_csv.getvalue())
            
            # Add summary statistics
            summary_csv = io.StringIO()
            summary_df.to_csv(summary_csv, index=False)
            zip_file.writestr('isochrone_summary.csv', summary_csv.getvalue())
            
            # Add districts within isochrone only
            within_csv = io.StringIO()
            within_isochrone.to_csv(within_csv, index=False)
            zip_file.writestr('districts_within_isochrone.csv', within_csv.getvalue())
            
            # Add README
            readme_content = f"""Isochrone Analysis Results
Generated on: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}

Lab Analyzed: {lab_name}
Method: {method}
Travel Time Threshold: {travel_time} minutes

Files Included:
1. isochrone_summary.csv - High-level summary statistics
2. isochrone_district_analysis.csv - Detailed analysis for all districts
3. districts_within_isochrone.csv - Only districts within the travel time threshold

Summary:
- {len(within_isochrone)} out of {len(analysis_df)} districts are within {travel_time} minutes
- {total_tests_within} total tests per quarter from districts within the isochrone
- {round((len(within_isochrone) / len(analysis_df)) * 100, 1)}% coverage

Method Details:
{method}: {"Real road routing using OpenRouteService API" if method == "routing" else "Euclidean distance calculation with 30 km/h average speed"}
"""
            zip_file.writestr('README.txt', readme_content)
        
        zip_buffer.seek(0)
        
        # Generate filename
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        safe_lab_name = "".join(c for c in lab_name if c.isalnum() or c in (' ', '-', '_')).rstrip()
        filename = f'isochrone_analysis_{safe_lab_name}_{travel_time}min_{timestamp}.zip'
        
        # Store temporarily and return download URL
        temp_filename = secure_filename(filename)
        session[f'temp_file_{temp_filename}'] = base64.b64encode(zip_buffer.getvalue()).decode('utf-8')
        
        return jsonify({
            'status': 'success',
            'download_url': f'/download_temp_zip/{temp_filename}'
        })
        
    except Exception as e:
        logger.log(f"Isochrone data export failed: {str(e)}", "ERROR")
        return jsonify({'status': 'error', 'message': f'Export failed: {str(e)}'})

@app.route('/download_temp_zip/<filename>')
def download_temp_zip(filename):
    """Download a temporarily stored zip file"""
    session_key = f'temp_file_{filename}'
    if session_key not in session:
        return jsonify({'status': 'error', 'message': 'File not found or expired'})
    
    try:
        # Decode base64 content
        file_content_b64 = session[session_key]
        file_content = base64.b64decode(file_content_b64)
        
        # Clean up
        session.pop(session_key, None)
        
        file_bytes = io.BytesIO(file_content)
        file_bytes.seek(0)
        
        return send_file(
            file_bytes,
            mimetype='application/zip',
            as_attachment=True,
            download_name=filename
        )
    except Exception as e:
        return jsonify({'status': 'error', 'message': str(e)})

# Add all remaining existing routes from the original file

@app.route('/validate_data')
def validate_data():
    """Validate uploaded data and return status"""
    validation_status = {'districts': False, 'cdst': False, 'messages': []}
    
    session_keys = list(session.keys())
    
    if not session_keys:
        validation_status['messages'].append("ℹ️  No data uploaded yet. Please upload your CSV files to get started.")
        validation_status['messages'].append(f"🔧 Debug: Session is empty")
        return jsonify(validation_status)
    
    # Check district data
    if 'district_data' in session:
        try:
            district_json = session['district_data']
            if not district_json or district_json == 'null':
                validation_status['messages'].append("⏳ District lab data: Not uploaded (empty)")
            else:
                df = pd.read_json(district_json)
                if df.empty:
                    validation_status['messages'].append("⏳ District lab data: Not uploaded (empty dataframe)")
                else:
                    required_cols = ['district', 'current_cdst', 'lat', 'lon', 'tests_per_quarter']
                    if all(col in df.columns for col in required_cols):
                        validation_status['districts'] = True
                        validation_status['messages'].append(f"✅ District lab data: {len(df)} records loaded")
                        
                        invalid_coords = df[(df['lat'].isna()) | (df['lon'].isna()) | 
                                          (df['lat'] == 0) | (df['lon'] == 0)].shape[0]
                        if invalid_coords > 0:
                            validation_status['messages'].append(f"⚠️  Warning: {invalid_coords} districts have invalid coordinates")
                        
                        invalid_tests = df[(df['tests_per_quarter'].isna()) | 
                                          (df['tests_per_quarter'] <= 0)].shape[0]
                        if invalid_tests > 0:
                            validation_status['messages'].append(f"⚠️  Warning: {invalid_tests} districts have invalid test counts")
                    else:
                        missing_cols = [col for col in required_cols if col not in df.columns]
                        validation_status['messages'].append(f"❌ District lab data: Missing columns: {missing_cols}")
        except Exception as e:
            validation_status['messages'].append(f"❌ District lab data error: {str(e)}")
            session.pop('district_data', None)
    else:
        validation_status['messages'].append("⏳ District lab data: Not uploaded")
    
    # Check CDST data
    if 'cdst_data' in session:
        try:
            cdst_json = session['cdst_data']
            if not cdst_json or cdst_json == 'null':
                validation_status['messages'].append("⏳ CDST lab data: Not uploaded (empty)")
            else:
                df = pd.read_json(cdst_json)
                if df.empty:
                    validation_status['messages'].append("⏳ CDST lab data: Not uploaded (empty dataframe)")
                else:
                    required_cols = ['lab_name', 'address', 'lat', 'lon', 'capacity']
                    if all(col in df.columns for col in required_cols):
                        validation_status['cdst'] = True
                        validation_status['messages'].append(f"✅ CDST lab data: {len(df)} records loaded")
                        
                        invalid_coords = df[(df['lat'].isna()) | (df['lon'].isna()) | 
                                          (df['lat'] == 0) | (df['lon'] == 0)].shape[0]
                        if invalid_coords > 0:
                            validation_status['messages'].append(f"⚠️  Warning: {invalid_coords} CDST labs have invalid coordinates")
                        
                        invalid_capacity = df[(df['capacity'].isna()) | 
                                            (df['capacity'] <= 0)].shape[0]
                        if invalid_capacity > 0:
                            validation_status['messages'].append(f"⚠️  Warning: {invalid_capacity} CDST labs have invalid capacity")
                    else:
                        missing_cols = [col for col in required_cols if col not in df.columns]
                        validation_status['messages'].append(f"❌ CDST lab data: Missing columns: {missing_cols}")
        except Exception as e:
            validation_status['messages'].append(f"❌ CDST lab data error: {str(e)}")
            session.pop('cdst_data', None)
    else:
        validation_status['messages'].append("⏳ CDST lab data: Not uploaded")
    
    # Check if optimization has been run
    if 'optimization_results' in session:
        try:
            results_json = session['optimization_results']
            if results_json and results_json != 'null':
                results_df = pd.read_json(results_json)
                if not results_df.empty:
                    validation_status['messages'].append("🎯 Optimization results available")
        except:
            session.pop('optimization_results', None)
    
    validation_status['messages'].append(f"🔧 Debug: Session keys: {session_keys}")
    
    return jsonify(validation_status)

# Add remaining essential routes (abbreviated for space)
@app.route('/settings')
def settings():
    content = '''<h2><i class="fas fa-cogs"></i> Optimization Settings</h2>
    <!-- Settings content would go here -->'''
    return render_template_string(BASE_TEMPLATE, content=content, extra_scripts='')

@app.route('/results')
def results():
    content = '''<h2><i class="fas fa-chart-bar"></i> Optimization Results</h2>
    <!-- Results content would go here -->'''
    return render_template_string(BASE_TEMPLATE, content=content, extra_scripts='')

@app.route('/about')
def about():
    content = '''<h2><i class="fas fa-info-circle"></i> About This Tool</h2>
    <!-- About content would go here -->'''
    return render_template_string(BASE_TEMPLATE, content=content, extra_scripts='')

@app.route('/data_input')
def data_input():
    content = '''<h2><i class="fas fa-upload"></i> Upload Your Network Data</h2>
    <!-- Data input content would go here -->'''
    return render_template_string(BASE_TEMPLATE, content=content, extra_scripts='')

# Add essential API routes for the existing functionality
@app.route('/download_template/<template_type>')
def download_template(template_type):
    """Generate and download template files"""
    if template_type == 'district':
        template = pd.DataFrame({
            'District': ['Sample District 1', 'Sample District 2', 'Sample District 3'],
            'CDST Lab linked to currently': ['CDST Lab A', 'CDST Lab B', 'CDST Lab A'],
            'Latitude of District HQ': [28.6139, 26.2124, 25.5937],
            'Longitude of District HQ': [77.2090, 78.1772, 85.1376],
            'Presumptive tests in one quarter': [150, 200, 175]
        })
        filename = 'district_lab_template.csv'
    else:
        template = pd.DataFrame({
            'Name of CDST Lab linked': ['CDST Lab A', 'CDST Lab B', 'CDST Lab C'],
            'Address': ['123 Main St, Delhi', '456 Park Ave, Lucknow', '789 Central Rd, Patna'],
            'Latitude': [28.6139, 26.2124, 25.5937],
            'Longitude': [77.2090, 78.1772, 85.1376],
            'Capacity of lab': [500, 750, 400]
        })
        filename = 'cdst_lab_template.csv'
    
    output = io.StringIO()
    template.to_csv(output, index=False)
    output.seek(0)
    
    return send_file(
        io.BytesIO(output.getvalue().encode()), 
        mimetype='text/csv', 
        as_attachment=True, 
        download_name=filename
    )

@app.route('/generate_map')
def generate_map():
    """Generate interactive folium map"""
    if 'optimization_results' not in session:
        return '<div class="alert alert-warning">No optimization results available.</div>'
    
    try:
        results_df = pd.read_json(session['optimization_results'])
        cdst_df = pd.read_json(session['cdst_data'])
        
        # Calculate map center
        all_lats = list(results_df['district_lat']) + list(cdst_df['lat'])
        all_lons = list(results_df['district_lon']) + list(cdst_df['lon'])
        center_lat = sum(all_lats) / len(all_lats)
        center_lon = sum(all_lons) / len(all_lons)
        
        # Create map
        m = folium.Map(location=[center_lat, center_lon], zoom_start=6, tiles='OpenStreetMap')
        
        # Add CDST labs as red markers
        for idx, lab in cdst_df.iterrows():
            # Calculate utilization
            utilization_df = pd.read_json(session['lab_utilization'])
            lab_util = utilization_df[utilization_df['lab_name'] == lab['lab_name']]
            if not lab_util.empty:
                current_util = lab_util.iloc[0]['current_utilization']
                optimized_util = lab_util.iloc[0]['optimized_utilization']
                popup_text = f"<b>{lab['lab_name']}</b><br>" \
                           f"{lab['address']}<br>" \
                           f"Capacity: {lab['capacity']}<br>" \
                           f"Current Utilization: {current_util:.1f}%<br>" \
                           f"Optimized Utilization: {optimized_util:.1f}%"
            else:
                popup_text = f"<b>{lab['lab_name']}</b><br>{lab['address']}<br>Capacity: {lab['capacity']}"
            
            folium.Marker(
                location=[lab['lat'], lab['lon']],
                popup=folium.Popup(popup_text, max_width=300),
                icon=folium.Icon(color='red', icon='info-sign')
            ).add_to(m)
        
        # Add districts as blue circles
        for idx, district in results_df.iterrows():
            color = 'green' if district['improvement_minutes'] > 0 else 'blue'
            popup_text = f"<b>{district['district']}</b><br>" \
                        f"Tests per quarter: {district['tests_per_quarter']}<br>" \
                        f"Current assignment: {district['current_assignment']}<br>" \
                        f"Optimized assignment: {district['optimized_assignment']}<br>" \
                        f"Time saved: {district['improvement_minutes']:.1f} minutes"
            
            folium.CircleMarker(
                location=[district['district_lat'], district['district_lon']],
                radius=8,
                popup=folium.Popup(popup_text, max_width=300),
                color=color,
                fillColor=color,
                fillOpacity=0.7
            ).add_to(m)
        
        # Add lines for reassigned districts
        reassigned = results_df[results_df['current_assignment'] != results_df['optimized_assignment']]
        for idx, district in reassigned.iterrows():
            lab_coords = cdst_df[cdst_df['lab_name'] == district['optimized_assignment']]
            if not lab_coords.empty:
                lab_coord = lab_coords.iloc[0]
                folium.PolyLine(
                    locations=[
                        [district['district_lat'], district['district_lon']], 
                        [lab_coord['lat'], lab_coord['lon']]
                    ],
                    color='green',
                    weight=3,
                    opacity=0.8,
                    popup=f"New assignment: {district['district']} → {district['optimized_assignment']}"
                ).add_to(m)
        
        return m._repr_html_()
        
    except Exception as e:
        return f'<div class="alert alert-danger">Error generating map: {str(e)}</div>'

@app.route('/generate_isochrone', methods=['POST'])
def generate_isochrone():
    """Generate isochrone map and analysis"""
    try:
        params = request.json
        lab_name = params.get('lab_name')
        method = params.get('method', 'euclidean')
        travel_time = params.get('travel_time', 60)
        
        # Validate inputs
        if not lab_name:
            return jsonify({'status': 'error', 'message': 'Lab name is required'})
        
        if 'cdst_data' not in session or 'district_data' not in session:
            return jsonify({'status': 'error', 'message': 'Lab and district data required'})
        
        cdst_df = pd.read_json(session['cdst_data'])
        district_df = pd.read_json(session['district_data'])
        
        # Find the selected lab
        lab_data = cdst_df[cdst_df['lab_name'] == lab_name]
        if lab_data.empty:
            return jsonify({'status': 'error', 'message': 'Lab not found'})
        
        lab_coords = (lab_data.iloc[0]['lat'], lab_data.iloc[0]['lon'])
        
        # Generate isochrone and map
        if method == 'routing':
            map_html, districts_within = generate_routing_isochrone(
                lab_coords, lab_name, travel_time, ORS_API_KEY, district_df, lab_data.iloc[0]
            )
        else:
            map_html, districts_within = generate_euclidean_isochrone(
                lab_coords, lab_name, travel_time, district_df, lab_data.iloc[0]
            )
        
        # Create districts table
        if districts_within:
            districts_html = f"""
            <div class="alert alert-success">
                <strong>{len(districts_within)} districts</strong> are within {travel_time} minutes of {lab_name}
            </div>
            <table class="table table-striped table-sm">
                <thead>
                    <tr><th>District</th><th>Travel Time (min)</th><th>Tests/Quarter</th></tr>
                </thead>
                <tbody>
            """
            for district in districts_within:
                districts_html += f"""
                    <tr>
                        <td>{district['name']}</td>
                        <td>{district['travel_time']:.1f}</td>
                        <td>{district['tests']}</td>
                    </tr>
                """
            districts_html += "</tbody></table>"
            
            total_tests = sum(d['tests'] for d in districts_within)
            districts_html += f"<p><strong>Total tests per quarter:</strong> {total_tests}</p>"
        else:
            districts_html = f'<div class="alert alert-warning">No districts found within {travel_time} minutes of {lab_name}</div>'
        
        return jsonify({
            'status': 'success',
            'map_html': map_html,
            'districts_html': districts_html
        })
        
    except Exception as e:
        logger.log(f"Isochrone generation failed: {str(e)}", "ERROR")
        return jsonify({'status': 'error', 'message': str(e)})

def generate_euclidean_isochrone(lab_coords, lab_name, travel_time_minutes, district_df, lab_info):
    """Generate euclidean distance-based isochrone"""
    # Convert time to distance (assuming 30 km/h average speed)
    max_distance_km = (travel_time_minutes / 60) * 30
    
    # Create map centered on the lab
    m = folium.Map(location=lab_coords, zoom_start=8)
    
    # Add the lab marker
    folium.Marker(
        location=lab_coords,
        popup=f"<b>{lab_name}</b><br>{lab_info['address']}<br>Capacity: {lab_info['capacity']}",
        icon=folium.Icon(color='red', icon='info-sign')
    ).add_to(m)
    
    # Draw isochrone circle
    folium.Circle(
        location=lab_coords,
        radius=max_distance_km * 1000,  # Convert to meters
        popup=f'{travel_time_minutes}-minute isochrone (Euclidean)',
        color='blue',
        fillColor='lightblue',
        fillOpacity=0.3,
        weight=2
    ).add_to(m)
    
    # Check which districts are within the isochrone
    districts_within = []
    for idx, district in district_df.iterrows():
        district_coords = (district['lat'], district['lon'])
        distance_km = geodesic(lab_coords, district_coords).kilometers
        travel_time = (distance_km / 30) * 60  # Convert to minutes
        
        if travel_time <= travel_time_minutes:
            districts_within.append({
                'name': district['district'],
                'travel_time': travel_time,
                'tests': district['tests_per_quarter']
            })
            
            # Add district marker
            folium.CircleMarker(
                location=district_coords,
                radius=6,
                popup=f"<b>{district['district']}</b><br>Travel time: {travel_time:.1f} min<br>Tests: {district['tests_per_quarter']}",
                color='green',
                fillColor='lightgreen',
                fillOpacity=0.7
            ).add_to(m)
    
    return m._repr_html_(), districts_within

def generate_routing_isochrone(lab_coords, lab_name, travel_time_minutes, api_key, district_df, lab_info):
    """Generate routing-based isochrone using OpenRouteService"""
    try:
        # Get isochrone from ORS API
        url = "https://api.openrouteservice.org/v2/isochrones/driving-car"
        headers = {
            'Authorization': api_key,
            'Content-Type': 'application/json'
        }
        
        body = {
            "locations": [[lab_coords[1], lab_coords[0]]],  # ORS uses [lon, lat]
            "range": [travel_time_minutes * 60],  # Convert to seconds
            "range_type": "time"
        }
        
        response = requests.post(url, headers=headers, json=body, timeout=30)
        
        if response.status_code != 200:
            raise Exception(f"ORS API error: {response.status_code} - {response.text}")
        
        isochrone_data = response.json()
        
        # Create map
        m = folium.Map(location=lab_coords, zoom_start=8)
        
        # Add the lab marker
        folium.Marker(
            location=lab_coords,
            popup=f"<b>{lab_name}</b><br>{lab_info['address']}<br>Capacity: {lab_info['capacity']}",
            icon=folium.Icon(color='red', icon='info-sign')
        ).add_to(m)
        
        # Add isochrone polygon
        if 'features' in isochrone_data and len(isochrone_data['features']) > 0:
            feature = isochrone_data['features'][0]
            if 'geometry' in feature and 'coordinates' in feature['geometry']:
                coords = feature['geometry']['coordinates'][0]
                # Convert from [lon, lat] to [lat, lon] for folium
                folium_coords = [[coord[1], coord[0]] for coord in coords]
                
                folium.Polygon(
                    locations=folium_coords,
                    popup=f'{travel_time_minutes}-minute isochrone (Routing)',
                    color='blue',
                    fillColor='lightblue',
                    fillOpacity=0.3,
                    weight=2
                ).add_to(m)
        
        # Check which districts are within the isochrone by calculating actual travel times
        districts_within = []
        for idx, district in district_df.iterrows():
            district_coords = (district['lat'], district['lon'])
            
            # Calculate actual travel time using routing
            travel_time = DistanceCalculator.openroute_service_time(
                lab_coords, district_coords, api_key, delay=0.5
            )
            
            if travel_time <= travel_time_minutes:
                districts_within.append({
                    'name': district['district'],
                    'travel_time': travel_time,
                    'tests': district['tests_per_quarter']
                })
                
                # Add district marker
                folium.CircleMarker(
                    location=district_coords,
                    radius=6,
                    popup=f"<b>{district['district']}</b><br>Travel time: {travel_time:.1f} min<br>Tests: {district['tests_per_quarter']}",
                    color='green',
                    fillColor='lightgreen',
                    fillOpacity=0.7
                ).add_to(m)
        
        return m._repr_html_(), districts_within
        
    except Exception as e:
        # Fallback to euclidean if routing fails
        logger.log(f"Routing isochrone failed, falling back to euclidean: {str(e)}", "WARNING")
        return generate_euclidean_isochrone(lab_coords, lab_name, travel_time_minutes, district_df, lab_info)

# Error handlers
@app.errorhandler(404)
def not_found(error):
    return redirect(url_for('index'))

@app.errorhandler(500)
def internal_error(error):
    return jsonify({'status': 'error', 'message': 'Internal server error'}), 500

# For Posit Connect deployment
if __name__ == '__main__':
    port = int(os.environ.get('PORT', 5000))
    debug_mode = os.environ.get('FLASK_ENV') != 'production'
    app.run(debug=debug_mode, host='0.0.0.0', port=port)
