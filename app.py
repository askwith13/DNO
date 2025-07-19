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

INDEX_CONTENT = '''
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
<div class="row mt-3">
    <div class="col-12">
        <div class="alert alert-info">
            <strong>Having issues with cached data?</strong> If you see old data that won't clear, click "Fresh Start" above or use the debugging tools in the Data Input section.
        </div>
    </div>
</div>
<div class="row mt-4">
    <div class="col-md-4">
        <div class="card h-100">
            <div class="card-header"><h5><i class="fas fa-upload"></i> Step 1: Upload Data</h5></div>
            <div class="card-body">
                <p>Upload your district laboratory and CDST facility data using our templates.</p>
                <a href="/data_input" class="btn btn-primary">Start Here</a>
            </div>
        </div>
    </div>
    <div class="col-md-4">
        <div class="card h-100">
            <div class="card-header"><h5><i class="fas fa-cogs"></i> Step 2: Configure Settings</h5></div>
            <div class="card-body">
                <p>Choose optimization algorithms and distance calculation methods.</p>
                <a href="/settings" class="btn btn-primary">Configure</a>
            </div>
        </div>
    </div>
    <div class="col-md-4">
        <div class="card h-100">
            <div class="card-header"><h5><i class="fas fa-chart-bar"></i> Step 3: View Results</h5></div>
            <div class="card-body">
                <p>Analyze optimization results with interactive visualizations and maps.</p>
                <a href="/results" class="btn btn-primary">View Results</a>
            </div>
        </div>
    </div>
</div>
'''

DATA_INPUT_CONTENT = '''
<h2><i class="fas fa-upload"></i> Upload Your Network Data</h2>
<div class="row">
    <div class="col-12">
        <div class="card">
            <div class="card-header">
                <h5>Data Templates and Upload</h5>
                <div class="btn-group" role="group">
                    <button class="btn btn-sm btn-outline-light" id="clearSession">
                        <i class="fas fa-trash"></i> Clear Session
                    </button>
                    <button class="btn btn-sm btn-warning" id="hardReset">
                        <i class="fas fa-nuclear"></i> Hard Reset
                    </button>
                    <a href="/debug_session" target="_blank" class="btn btn-sm btn-info">
                        <i class="fas fa-bug"></i> Debug
                    </a>
                </div>
            </div>
            <div class="card-body">
                <div class="alert alert-info">
                    <strong>Troubleshooting:</strong> If you see old data that won't clear, try the "Hard Reset" button above.
                </div>
                
                <p><strong>Step 1:</strong> Download template files</p>
                <div class="mb-3">
                    <a href="/download_template/district" class="btn btn-info me-2"><i class="fas fa-download"></i> District Template</a>
                    <a href="/download_template/cdst" class="btn btn-info"><i class="fas fa-download"></i> CDST Template</a>
                </div>
                <hr>
                <p><strong>Step 2:</strong> Upload your data files</p>
                <form id="uploadForm" enctype="multipart/form-data">
                    <div class="mb-3">
                        <label class="form-label">District Lab Data (CSV)</label>
                        <input type="file" class="form-control" id="district_file" name="district_file" accept=".csv">
                    </div>
                    <div class="mb-3">
                        <label class="form-label">CDST Lab Data (CSV)</label>
                        <input type="file" class="form-control" id="cdst_file" name="cdst_file" accept=".csv">
                    </div>
                    <button type="submit" class="btn btn-primary"><i class="fas fa-upload"></i> Upload Files</button>
                </form>
                <div id="uploadStatus" class="mt-3"></div>
                <hr>
                <h5>Data Validation Status:</h5>
                <div id="validationStatus" class="progress-text">Loading validation status...</div>
            </div>
        </div>
    </div>
</div>
'''

SETTINGS_CONTENT = '''
<h2><i class="fas fa-cogs"></i> Optimization Settings</h2>
<div class="row">
    <div class="col-md-6">
        <div class="card">
            <div class="card-header"><h5>Distance Calculation</h5></div>
            <div class="card-body">
                <div class="form-check">
                    <input class="form-check-input" type="radio" name="distance_method" id="euclidean" value="euclidean" checked>
                    <label class="form-check-label" for="euclidean"><strong>Euclidean Distance</strong> (Fastest)</label>
                </div>
                <div class="form-check">
                    <input class="form-check-input" type="radio" name="distance_method" id="ors_api" value="ors_api">
                    <label class="form-check-label" for="ors_api"><strong>OpenRouteService API</strong> (Most accurate, API key included)</label>
                </div>
                <div class="form-check">
                    <input class="form-check-input" type="radio" name="distance_method" id="osrm_api" value="osrm_api">
                    <label class="form-check-label" for="osrm_api"><strong>OSRM API</strong> (Free, no key required)</label>
                </div>
                <div class="mt-3">
                    <div class="alert alert-info">
                        <i class="fas fa-info-circle"></i> <strong>OpenRouteService API:</strong> Pre-configured with API key for your convenience. No setup required!
                    </div>
                </div>
            </div>
        </div>
    </div>
    <div class="col-md-6">
        <div class="card">
            <div class="card-header"><h5>Optimization Algorithm</h5></div>
            <div class="card-body">
                <div class="form-check">
                    <input class="form-check-input" type="radio" name="optimization_method" id="linear_programming" value="linear_programming" checked>
                    <label class="form-check-label" for="linear_programming"><strong>Linear Programming</strong> (Optimal)</label>
                </div>
                <div class="form-check">
                    <input class="form-check-input" type="radio" name="optimization_method" id="greedy" value="greedy">
                    <label class="form-check-label" for="greedy"><strong>Greedy Heuristic</strong> (Fast)</label>
                </div>
                <div class="mt-3">
                    <label class="form-label">Capacity Flexibility (%):</label>
                    <input type="number" class="form-control" id="capacity_flexibility" value="5" min="0" max="50">
                    <small class="form-text text-muted">Allow labs to exceed capacity by this percentage</small>
                </div>
            </div>
        </div>
    </div>
</div>
<div class="row mt-4">
    <div class="col-12">
        <div class="card">
            <div class="card-header"><h5>Run Optimization</h5></div>
            <div class="card-body">
                <button id="runOptimization" class="btn btn-primary btn-lg"><i class="fas fa-play"></i> Run Optimization</button>
                
                <!-- Progress Bar Section -->
                <div class="mt-3">
                    <div id="progressSection" style="display:none;">
                        <div class="d-flex justify-content-between align-items-center mb-2">
                            <span><strong>Optimization Progress</strong></span>
                            <span id="progressPercentage">0%</span>
                        </div>
                        <div class="progress mb-2" style="height: 25px;">
                            <div id="progressBar" class="progress-bar progress-bar-striped progress-bar-animated bg-success" 
                                 role="progressbar" style="width: 0%" aria-valuenow="0" aria-valuemin="0" aria-valuemax="100">
                                <span id="progressText">Starting...</span>
                            </div>
                        </div>
                        <div id="currentStage" class="text-muted small">Ready to start optimization</div>
                    </div>
                </div>
                
                <h5 class="mt-3">Optimization Logs:</h5>
                <div id="optimizationLogs" class="progress-text" style="min-height: 150px;">Ready to run optimization.</div>
            </div>
        </div>
    </div>
</div>
'''

RESULTS_CONTENT = '''
<h2><i class="fas fa-chart-bar"></i> Optimization Results</h2>
<div class="row">
    <div class="col-12">
        <div class="card">
            <div class="card-header">
                <h5>Summary Statistics</h5>
                <div class="btn-group" role="group">
                    <button class="btn btn-sm btn-outline-light" id="clearResults">
                        <i class="fas fa-trash"></i> Clear Results
                    </button>
                    <a href="/debug_plots" target="_blank" class="btn btn-sm btn-info">
                        <i class="fas fa-bug"></i> Debug Plots
                    </a>
                </div>
            </div>
            <div class="card-body">
                <div id="summaryTable">No optimization results available. Please run optimization first.</div>
            </div>
        </div>
    </div>
</div>
<div class="row mt-4">
    <div class="col-md-6">
        <div class="card">
            <div class="card-header"><h5>Lab Utilization Comparison</h5></div>
            <div class="card-body"><div id="utilizationPlot" style="height: 400px;"></div></div>
        </div>
    </div>
    <div class="col-md-6">
        <div class="card">
            <div class="card-header"><h5>Travel Time Improvements</h5></div>
            <div class="card-body"><div id="improvementPlot" style="height: 400px;"></div></div>
        </div>
    </div>
</div>
<div class="row mt-4">
    <div class="col-md-6">
        <div class="card">
            <div class="card-header"><h5>Current Travel Times Distribution</h5></div>
            <div class="card-body"><div id="currentTravelHistogram" style="height: 400px;"></div></div>
        </div>
    </div>
    <div class="col-md-6">
        <div class="card">
            <div class="card-header"><h5>Optimized Travel Times Distribution</h5></div>
            <div class="card-body"><div id="optimizedTravelHistogram" style="height: 400px;"></div></div>
        </div>
    </div>
</div>
<div class="row mt-4">
    <div class="col-12">
        <div class="card">
            <div class="card-header">
                <h5>Detailed Assignments</h5>
                <button class="btn btn-sm btn-outline-light" id="exportResults">
                    <i class="fas fa-download"></i> Export Results
                </button>
            </div>
            <div class="card-body">
                <table id="resultsTable" class="table table-striped">
                    <thead>
                        <tr><th>District</th><th>Current Assignment</th><th>Optimized Assignment</th><th>Current Time</th><th>Optimized Time</th><th>Time Saved</th><th>Tests/Quarter</th></tr>
                    </thead>
                    <tbody></tbody>
                </table>
            </div>
        </div>
    </div>
</div>
'''

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

ABOUT_CONTENT = '''
<h2><i class="fas fa-info-circle"></i> About This Tool</h2>
<div class="row">
    <div class="col-12">
        <div class="card">
            <div class="card-body">
                <div class="row">
                    <div class="col-md-8">
                        <h3>Diagnostic Network Optimization Tool</h3>
                        <p class="lead">An advanced spatial optimization platform for healthcare diagnostic networks, designed to minimize travel time while respecting laboratory capacity constraints.</p>
                        
                        <h4>Overview</h4>
                        <p>This tool addresses the critical challenge of optimizing diagnostic laboratory networks in healthcare systems. By analyzing the geographical distribution of district laboratories and CDST (Culture & Drug Sensitivity Testing) facilities, the platform provides data-driven recommendations for efficient sample routing and network planning.</p>
                        
                        <h4>Key Features</h4>
                        <div class="row">
                            <div class="col-md-6">
                                <ul>
                                    <li><strong>📊 Multiple Distance Calculation Methods</strong>
                                        <ul>
                                            <li>Euclidean Distance (Fast computation)</li>
                                            <li>OpenRouteService API (Real road routing - Pre-configured)</li>
                                            <li>OSRM API (Free routing service)</li>
                                        </ul>
                                    </li>
                                    <li><strong>🧮 Advanced Optimization Algorithms</strong>
                                        <ul>
                                            <li>Linear Programming (Optimal solutions)</li>
                                            <li>Greedy Heuristic (Fast approximation)</li>
                                        </ul>
                                    </li>
                                    <li><strong>🗺️ Interactive Mapping and Visualization</strong>
                                        <ul>
                                            <li>Network optimization maps</li>
                                            <li>Isochrone analysis</li>
                                            <li>Catchment area visualization</li>
                                            <li>Downloadable HTML maps</li>
                                        </ul>
                                    </li>
                                </ul>
                            </div>
                            <div class="col-md-6">
                                <ul>
                                    <li><strong>📈 Comprehensive Results Analysis</strong>
                                        <ul>
                                            <li>Travel time distribution histograms</li>
                                            <li>Lab utilization comparisons</li>
                                            <li>Performance improvement metrics</li>
                                        </ul>
                                    </li>
                                    <li><strong>💾 Data Management</strong>
                                        <ul>
                                            <li>CSV template downloads</li>
                                            <li>Data validation and cleaning</li>
                                            <li>Results export functionality</li>
                                        </ul>
                                    </li>
                                    <li><strong>🔧 Flexible Configuration</strong>
                                        <ul>
                                            <li>Capacity flexibility settings</li>
                                            <li>API key management</li>
                                            <li>Customizable time parameters</li>
                                        </ul>
                                    </li>
                                </ul>
                            </div>
                        </div>
                        
                        <h4>Analysis Capabilities</h4>
                        <div class="row">
                            <div class="col-md-4">
                                <div class="card bg-light">
                                    <div class="card-body">
                                        <h6><i class="fas fa-route"></i> Distance Calculation</h6>
                                        <p class="small">Multiple methods for calculating travel times including straight-line distance and real road routing using external APIs.</p>
                                    </div>
                                </div>
                            </div>
                            <div class="col-md-4">
                                <div class="card bg-light">
                                    <div class="card-body">
                                        <h6><i class="fas fa-clock"></i> Isochrone Analysis</h6>
                                        <p class="small">Visualize catchment areas and determine which districts can reach each lab within specified time limits.</p>
                                    </div>
                                </div>
                            </div>
                            <div class="col-md-4">
                                <div class="card bg-light">
                                    <div class="card-body">
                                        <h6><i class="fas fa-balance-scale"></i> Capacity Planning</h6>
                                        <p class="small">Optimize lab assignments while respecting capacity constraints and flexibility parameters.</p>
                                    </div>
                                </div>
                            </div>
                        </div>
                        
                        <h4>Technical Specifications</h4>
                        <ul>
                            <li><strong>Platform:</strong> Flask Web Application with Python backend</li>
                            <li><strong>Optimization Engine:</strong> PuLP Linear Programming solver</li>
                            <li><strong>Mapping:</strong> Folium with OpenStreetMap integration</li>
                            <li><strong>Visualization:</strong> Plotly interactive charts</li>
                            <li><strong>Geospatial Analysis:</strong> GeoPy and spatial distance calculations</li>
                            <li><strong>Data Processing:</strong> Pandas and NumPy for efficient data handling</li>
                        </ul>
                    </div>
                    <div class="col-md-4">
                        <div class="card bg-primary text-white">
                            <div class="card-header">
                                <h5><i class="fas fa-building"></i> About PATH</h5>
                            </div>
                            <div class="card-body">
                                <p><strong>PATH</strong> (Program for Appropriate Technology in Health) is a global nonprofit organization dedicated to achieving health equity through innovation and partnerships.</p>
                                
                                <p>For over 40 years, PATH has worked with partners around the world to accelerate health innovations that address the most pressing health challenges in low- and middle-income countries.</p>
                                
                                <h6><i class="fas fa-bullseye"></i> Mission</h6>
                                <p class="small">Advancing health equity for all through innovation.</p>
                                
                                <h6><i class="fas fa-eye"></i> Vision</h6>
                                <p class="small">A world where health equity is within reach for everyone.</p>
                                
                                <h6><i class="fas fa-globe"></i> Global Impact</h6>
                                <ul class="small">
                                    <li>70+ countries served</li>
                                    <li>1,600+ employees worldwide</li>
                                    <li>500+ partners globally</li>
                                    <li>Focus on innovation and scale</li>
                                </ul>
                                
                                <h6><i class="fas fa-laptop-code"></i> Digital Health</h6>
                                <p class="small">PATH develops digital tools and platforms to strengthen health systems, improve service delivery, and advance health equity through technology innovation.</p>
                                
                                <div class="mt-3">
                                    <a href="https://www.path.org" target="_blank" class="btn btn-light btn-sm">
                                        <i class="fas fa-external-link-alt"></i> Visit PATH.org
                                    </a>
                                </div>
                            </div>
                        </div>
                        
                        <div class="card mt-3 border-success">
                            <div class="card-header bg-success text-white">
                                <h6><i class="fas fa-balance-scale"></i> License & Usage</h6>
                            </div>
                            <div class="card-body">
                                <div class="alert alert-success">
                                    <h6><i class="fas fa-heart"></i> Free for Non-Commercial Use</h6>
                                    <p class="small mb-0">This tool is provided free of charge for non-commercial, research, and educational purposes.</p>
                                </div>
                                
                                <h6>Permitted Uses:</h6>
                                <ul class="small">
                                    <li>Academic research</li>
                                    <li>Educational purposes</li>
                                    <li>Public health planning</li>
                                    <li>Non-profit healthcare initiatives</li>
                                    <li>Government health programs</li>
                                </ul>
                                
                                <h6>Commercial Use:</h6>
                                <p class="small">For commercial licensing and enterprise support, please contact PATH's Digital Health team.</p>
                                
                                <h6>Support:</h6>
                                <p class="small">Community support is available through PATH's digital health resources and documentation.</p>
                            </div>
                        </div>
                    </div>
                </div>
                
                <hr class="my-4">
                
                <div class="row">
                    <div class="col-md-6">
                        <h4><i class="fas fa-question-circle"></i> Getting Started</h4>
                        <ol>
                            <li><strong>Upload Data:</strong> Use the provided templates to upload your district and CDST lab data</li>
                            <li><strong>Configure Settings:</strong> Choose your preferred distance calculation method and optimization algorithm</li>
                            <li><strong>Run Optimization:</strong> Execute the analysis to get optimal lab assignments</li>
                            <li><strong>Analyze Results:</strong> Review maps, charts, and detailed assignment recommendations</li>
                            <li><strong>Explore Isochrones:</strong> Analyze catchment areas and accessibility for each lab</li>
                            <li><strong>Download Maps:</strong> Export interactive HTML maps for presentations and reports</li>
                        </ol>
                    </div>
                    <div class="col-md-6">
                        <h4><i class="fas fa-life-ring"></i> Support & Resources</h4>
                        <ul>
                            <li><strong>Data Templates:</strong> CSV templates available in the Data Input section</li>
                            <li><strong>API Integration:</strong> OpenRouteService API pre-configured for immediate use</li>
                            <li><strong>Documentation:</strong> Detailed help available throughout the application</li>
                            <li><strong>Best Practices:</strong> Refer to PATH's digital health implementation guides</li>
                        </ul>
                        
                        <div class="alert alert-info">
                            <h6><i class="fas fa-lightbulb"></i> Pro Tip</h6>
                            <p class="small mb-0">Start with Euclidean distance for quick analysis, then use OpenRouteService (pre-configured) or OSRM for detailed road routing when you need precise travel times.</p>
                        </div>
                    </div>
                </div>
                
                <hr>
                <footer class="text-center">
                    <p><strong>Version:</strong> 2.1 Enhanced Edition with Map Downloads | <strong>Platform:</strong> Flask Web Application | <strong>Created by:</strong> PATH</p>
                    <p class="text-muted small">© 2024 PATH. This tool is provided free for non-commercial use. For commercial licensing, contact PATH's Digital Health team.</p>
                </footer>
            </div>
        </div>
    </div>
</div>
'''

# JavaScript for interactivity
COMMON_SCRIPTS = '''
<script>
$(document).ready(function() {
    // Highlight active navigation
    var currentPath = window.location.pathname;
    $('.sidebar a').each(function() {
        if ($(this).attr('href') === currentPath) {
            $(this).addClass('active');
        }
    });
});
</script>
'''

UPLOAD_SCRIPTS = '''
<script>
$(document).ready(function() {
    loadValidationStatus();
    
    $('#clearSession').click(function() {
        if (confirm('Are you sure you want to clear all uploaded data and results? This action cannot be undone.')) {
            $.ajax({
                url: '/clear_session',
                type: 'POST',
                success: function(response) {
                    if (response.status === 'success') {
                        $('#uploadStatus').html('<div class="alert alert-success">Session cleared! Debug info: ' + JSON.stringify(response.debug) + '</div>');
                        $('#district_file').val('');
                        $('#cdst_file').val('');
                        setTimeout(function() {
                            loadValidationStatus();
                        }, 500);
                    } else {
                        $('#uploadStatus').html('<div class="alert alert-danger">Error clearing session: ' + response.message + '</div>');
                    }
                },
                error: function() {
                    $('#uploadStatus').html('<div class="alert alert-danger">Error clearing session. Please try the Hard Reset.</div>');
                }
            });
        }
    });
    
    $('#hardReset').click(function() {
        if (confirm('HARD RESET: This will completely reset everything. Are you absolutely sure?')) {
            $.ajax({
                url: '/hard_reset',
                type: 'POST',
                success: function(response) {
                    if (response.status === 'success') {
                        $('#uploadStatus').html('<div class="alert alert-success">Complete reset performed! Remaining keys: ' + JSON.stringify(response.remaining_keys) + '</div>');
                        $('#district_file').val('');
                        $('#cdst_file').val('');
                        setTimeout(function() {
                            window.location.reload(); // Force page reload
                        }, 1000);
                    } else {
                        $('#uploadStatus').html('<div class="alert alert-danger">Hard reset failed: ' + response.message + '</div>');
                    }
                },
                error: function() {
                    $('#uploadStatus').html('<div class="alert alert-danger">Hard reset failed. Please refresh the page manually.</div>');
                }
            });
        }
    });
    
    $('#uploadForm').on('submit', function(e) {
        e.preventDefault();
        var formData = new FormData();
        var districtFile = $('#district_file')[0].files[0];
        var cdstFile = $('#cdst_file')[0].files[0];
        
        if (!districtFile && !cdstFile) {
            $('#uploadStatus').html('<div class="alert alert-warning">Please select at least one file to upload.</div>');
            return;
        }
        
        if (districtFile) formData.append('district_file', districtFile);
        if (cdstFile) formData.append('cdst_file', cdstFile);
        
        $('#uploadStatus').html('<div class="alert alert-info">Uploading files...</div>');
        
        $.ajax({
            url: '/upload_data',
            type: 'POST',
            data: formData,
            processData: false,
            contentType: false,
            success: function(response) {
                if (response.status === 'success') {
                    $('#uploadStatus').html('<div class="alert alert-success">Files uploaded successfully!</div>');
                    setTimeout(function() {
                        loadValidationStatus();
                    }, 500);
                } else {
                    $('#uploadStatus').html('<div class="alert alert-danger">Upload failed: ' + response.message + '</div>');
                }
            },
            error: function() {
                $('#uploadStatus').html('<div class="alert alert-danger">Upload failed. Please try again.</div>');
            }
        });
    });
    
    function loadValidationStatus() {
        $('#validationStatus').html('Loading validation status...');
        $.get('/validate_data', function(data) {
            $('#validationStatus').text(data.messages.join('\\n'));
        }).fail(function() {
            $('#validationStatus').text('Error loading validation status. Check debug info.');
        });
    }
    
    // Auto-refresh validation status every 5 seconds for debugging
    setInterval(function() {
        if ($('#validationStatus').is(':visible')) {
            loadValidationStatus();
        }
    }, 5000);
});
</script>
'''

SETTINGS_SCRIPTS = '''
<script>
$(document).ready(function() {
    var progressInterval = null;
    var optimizationRunning = false;
    
    $('#runOptimization').click(function() {
        if (optimizationRunning) {
            alert('Optimization is already running. Please wait for it to complete.');
            return;
        }
        
        var params = {
            distance_method: $('input[name="distance_method"]:checked').val(),
            optimization_method: $('input[name="optimization_method"]:checked').val(),
            capacity_flexibility: parseInt($('#capacity_flexibility').val())
        };
        
        // Start optimization
        startOptimization(params);
    });
    
    function startOptimization(params) {
        optimizationRunning = true;
        
        // Update UI
        $('#runOptimization').prop('disabled', true).html('<i class="fas fa-spinner fa-spin"></i> Running Optimization...');
        $('#progressSection').show();
        $('#optimizationLogs').text('Starting optimization...');
        
        // Reset progress bar
        updateProgressBar(0, 'Initializing', 'Starting optimization process...');
        
        // Start progress polling
        startProgressPolling();
        
        // Start the optimization
        $.ajax({
            url: '/run_optimization',
            type: 'POST',
            contentType: 'application/json',
            data: JSON.stringify(params),
            timeout: 300000, // 5 minute timeout
            success: function(response) {
                optimizationRunning = false;
                stopProgressPolling();
                
                $('#runOptimization').prop('disabled', false).html('<i class="fas fa-play"></i> Run Optimization');
                
                if (response.status === 'success') {
                    updateProgressBar(100, 'Completed', 'Optimization completed successfully!');
                    $('#optimizationLogs').text(response.logs);
                    
                    // Show summary
                    if (response.summary) {
                        var summary = response.summary;
                        var reassignedPercentage = ((summary.districts_reassigned / summary.total_districts) * 100).toFixed(1);
                        var summaryMsg = `Optimization Complete!\\n` +
                                       `• Total time saved: ${summary.total_time_saved.toFixed(1)} minutes\\n` +
                                       `• Districts reassigned: ${summary.districts_reassigned}/${summary.total_districts} (${reassignedPercentage}%)\\n` +
                                       `\\nCheck the Results tab for detailed analysis.`;
                        alert(summaryMsg);
                    } else {
                        alert('Optimization completed! Check the Results tab for detailed analysis.');
                    }
                    
                    // Hide progress after 3 seconds
                    setTimeout(function() {
                        $('#progressSection').fadeOut();
                    }, 3000);
                } else {
                    updateProgressBar(100, 'Failed', response.message);
                    $('#optimizationLogs').text(response.logs || response.message);
                    alert('Optimization failed: ' + response.message);
                }
            },
            error: function(xhr, status, error) {
                optimizationRunning = false;
                stopProgressPolling();
                
                $('#runOptimization').prop('disabled', false).html('<i class="fas fa-play"></i> Run Optimization');
                updateProgressBar(100, 'Error', 'Connection error occurred');
                
                var errorMsg = 'Optimization failed due to connection error.';
                if (status === 'timeout') {
                    errorMsg = 'Optimization timed out. Please try again or use a faster distance calculation method.';
                }
                alert(errorMsg);
            }
        });
    }
    
    function startProgressPolling() {
        // Poll every 500ms for smooth progress updates
        progressInterval = setInterval(function() {
            if (!optimizationRunning) {
                stopProgressPolling();
                return;
            }
            
            $.ajax({
                url: '/get_optimization_progress',
                type: 'GET',
                timeout: 5000,
                success: function(data) {
                    updateProgressBar(data.progress, data.stage, '');
                    
                    // Update logs if available (but don't overwrite if empty)
                    if (data.logs && data.logs.trim()) {
                        $('#optimizationLogs').text(data.logs);
                        
                        // Auto-scroll to bottom
                        var logsDiv = $('#optimizationLogs')[0];
                        logsDiv.scrollTop = logsDiv.scrollHeight;
                    }
                },
                error: function() {
                    // Silently handle progress polling errors
                    console.log('Progress polling error (this is normal during optimization)');
                }
            });
        }, 500);
    }
    
    function stopProgressPolling() {
        if (progressInterval) {
            clearInterval(progressInterval);
            progressInterval = null;
        }
    }
    
    function updateProgressBar(progress, stage, message) {
        // Ensure progress is within bounds and rounded to 2 decimal places
        progress = Math.round(Math.max(0, Math.min(100, progress)) * 100) / 100;
        
        // For display, show whole numbers for cleaner UI
        var displayProgress = Math.round(progress);
        
        // Update progress bar
        $('#progressBar').css('width', progress + '%').attr('aria-valuenow', progress);
        $('#progressPercentage').text(displayProgress + '%');
        $('#progressText').text(displayProgress + '%');
        
        // Update stage information
        if (stage) {
            $('#currentStage').text(stage + (message ? ' - ' + message : ''));
        }
        
        // Change color based on progress/status
        $('#progressBar').removeClass('bg-success bg-warning bg-danger bg-info');
        if (progress >= 100) {
            if (stage === 'Completed') {
                $('#progressBar').addClass('bg-success');
            } else if (stage === 'Failed' || stage === 'Error') {
                $('#progressBar').addClass('bg-danger');
            } else {
                $('#progressBar').addClass('bg-info');
            }
        } else if (progress >= 75) {
            $('#progressBar').addClass('bg-info');
        } else if (progress >= 50) {
            $('#progressBar').addClass('bg-warning');
        } else {
            $('#progressBar').addClass('bg-info');
        }
    }
    
    // Clean up polling when page is unloaded
    $(window).on('beforeunload', function() {
        stopProgressPolling();
    });
});
</script>
'''

RESULTS_SCRIPTS = '''
<script>
$(document).ready(function() {
    loadOptimizationResults();
    
    $('#exportResults').click(function() {
        window.location.href = '/export_results';
    });
    
    $('#clearResults').click(function() {
        if (confirm('Clear optimization results? You will need to re-run optimization.')) {
            $.ajax({
                url: '/force_clear_results',
                type: 'POST',
                success: function(response) {
                    if (response.status === 'success') {
                        alert('Results cleared! Please run optimization again.');
                        location.reload();
                    } else {
                        alert('Error: ' + response.message);
                    }
                },
                error: function() {
                    alert('Error clearing results');
                }
            });
        }
    });
    
    // Add refresh button functionality
    $('<button class="btn btn-sm btn-outline-primary ms-2" id="refreshPlots"><i class="fas fa-refresh"></i> Refresh Charts</button>')
        .insertAfter('#exportResults');
    
    $('#refreshPlots').click(function() {
        $('#refreshPlots').html('<i class="fas fa-spinner fa-spin"></i> Refreshing...');
        loadPlots(true);
        setTimeout(function() {
            $('#refreshPlots').html('<i class="fas fa-refresh"></i> Refresh Charts');
        }, 3000);
    });
    
    function loadOptimizationResults() {
        $.get('/get_optimization_data', function(data) {
            if (data.status === 'success') {
                createSummaryTable(data);
                loadPlots();
                createAssignmentsTable(data.results);
            } else {
                $('#summaryTable').html('<div class="alert alert-warning">No optimization results available. Please run optimization first.</div>');
            }
        }).fail(function() {
            $('#summaryTable').html('<div class="alert alert-danger">Error loading optimization data.</div>');
        });
    }
    
    function createSummaryTable(data) {
        var reassignedCount = data.results.filter(r => r.current_assignment !== r.optimized_assignment).length;
        var totalTimeSaved = data.results.reduce((sum, r) => sum + r.improvement_minutes, 0);
        var avgTimeSaved = totalTimeSaved / data.results.length;
        var reassignedPercentage = ((reassignedCount/data.results.length)*100).toFixed(1);
        
        var summaryHtml = `
            <table class="table table-striped">
                <tr><td><strong>Algorithm</strong></td><td>${data.method}</td></tr>
                <tr><td><strong>Distance Method</strong></td><td>${data.distance_method}</td></tr>
                <tr><td><strong>Total Districts</strong></td><td>${data.results.length}</td></tr>
                <tr><td><strong>Districts Reassigned</strong></td><td>${reassignedCount} (${reassignedPercentage}%)</td></tr>
                <tr><td><strong>Total Time Saved</strong></td><td>${totalTimeSaved.toFixed(2)} minutes</td></tr>
                <tr><td><strong>Average Time Saved per District</strong></td><td>${avgTimeSaved.toFixed(2)} minutes</td></tr>
            </table>
        `;
        $('#summaryTable').html(summaryHtml);
    }
    
    function createAssignmentsTable(results) {
        if ($.fn.DataTable.isDataTable('#resultsTable')) {
            $('#resultsTable').DataTable().destroy();
        }
        
        var tableData = results.map(r => [
            r.district, r.current_assignment, r.optimized_assignment,
            r.current_travel_time.toFixed(1), r.optimized_travel_time.toFixed(1),
            r.improvement_minutes.toFixed(1), r.tests_per_quarter
        ]);
        
        $('#resultsTable').DataTable({
            data: tableData,
            pageLength: 15,
            order: [[5, 'desc']]
        });
    }
    
    function loadPlots(forceRefresh = false) {
        // Add cache busting parameter
        var url = '/generate_plots';
        if (forceRefresh) {
            url += '?refresh=' + new Date().getTime();
        }
        
        console.log('Starting to load plots from:', url);
        
        // Clear existing plots first
        $('#utilizationPlot').html('<div class="text-center p-4"><i class="fas fa-spinner fa-spin fa-2x"></i><br>Loading utilization chart...</div>');
        $('#improvementPlot').html('<div class="text-center p-4"><i class="fas fa-spinner fa-spin fa-2x"></i><br>Loading improvements chart...</div>');
        $('#currentTravelHistogram').html('<div class="text-center p-4"><i class="fas fa-spinner fa-spin fa-2x"></i><br>Loading current histogram...</div>');
        $('#optimizedTravelHistogram').html('<div class="text-center p-4"><i class="fas fa-spinner fa-spin fa-2x"></i><br>Loading optimized histogram...</div>');
        
        $.ajax({
            url: url,
            method: 'GET',
            timeout: 30000,  // 30 second timeout
            success: function(data) {
                console.log('Plot data received. Status:', data.status);
                
                if (data.status === 'error') {
                    console.error('Server error:', data.message);
                    showPlotError('Server error: ' + data.message);
                    return;
                }
                
                // Try to render each plot individually with error handling
                renderPlot('utilizationPlot', data.utilization_plot, 'Utilization Chart');
                renderPlot('improvementPlot', data.improvement_plot, 'Improvement Chart');
                renderPlot('currentTravelHistogram', data.current_travel_histogram, 'Current Travel Histogram');
                renderPlot('optimizedTravelHistogram', data.optimized_travel_histogram, 'Optimized Travel Histogram');
            },
            error: function(xhr, status, error) {
                console.error('AJAX error:', status, error);
                console.error('Response:', xhr.responseText);
                showPlotError('Connection error: ' + error + '. Status: ' + status);
            }
        });
    }
    
    function renderPlot(containerId, plotData, plotName) {
        try {
            if (!plotData) {
                $('#' + containerId).html('<div class="alert alert-warning">No data for ' + plotName + '</div>');
                return;
            }
            
            console.log('Rendering', plotName, 'in container', containerId);
            var parsedData = JSON.parse(plotData);
            
            if (!parsedData.data || !parsedData.layout) {
                $('#' + containerId).html('<div class="alert alert-warning">Invalid plot data for ' + plotName + '</div>');
                return;
            }
            
            Plotly.newPlot(containerId, parsedData.data, parsedData.layout, {
                responsive: true,
                displayModeBar: false
            });
            
            console.log(plotName + ' rendered successfully');
            
        } catch (e) {
            console.error('Error rendering ' + plotName + ':', e);
            $('#' + containerId).html('<div class="alert alert-danger">Error rendering ' + plotName + ': ' + e.message + '</div>');
        }
    }
    
    function showPlotError(message) {
        var errorHtml = '<div class="alert alert-danger"><strong>Plot Error:</strong> ' + message + '</div>';
        $('#utilizationPlot').html(errorHtml);
        $('#improvementPlot').html(errorHtml);
        $('#currentTravelHistogram').html(errorHtml);
        $('#optimizedTravelHistogram').html(errorHtml);
    }
});
</script>
'''

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

# OPTIMIZATION AND UTILITY CLASSES
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
        print(log_entry)  # Also print to console for debugging
    
    def update_progress(self, progress, stage="", message=""):
        # Round progress to 2 decimal places
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
        return (distance_km / avg_speed_kmh) * 60  # Convert to minutes
    
    @staticmethod
    def openroute_service_time(start_coords, end_coords, api_key, delay=1):
        """Calculate travel time using OpenRouteService API"""
        try:
            if delay > 0:
                time.sleep(delay)  # Rate limiting
                
            url = "https://api.openrouteservice.org/v2/directions/driving-car"
            headers = {
                'Authorization': api_key,
                'Content-Type': 'application/json'
            }
            
            # Coordinates should be [longitude, latitude] for ORS
            body = {
                "coordinates": [[start_coords[1], start_coords[0]], [end_coords[1], end_coords[0]]],
                "format": "json"
            }
            
            response = requests.post(url, headers=headers, json=body, timeout=10)
            
            if response.status_code == 200:
                result = response.json()
                if 'routes' in result and len(result['routes']) > 0:
                    duration_seconds = result['routes'][0]['summary']['duration']
                    return duration_seconds / 60  # Convert to minutes
                else:
                    raise Exception("No routes found in response")
            else:
                raise Exception(f"API returned status code: {response.status_code}, message: {response.text}")
                
        except Exception as e:
            logger.log(f"ORS API error for {start_coords} to {end_coords}: {str(e)}", "WARNING")
            return DistanceCalculator.euclidean_distance_time(start_coords, end_coords)
    
    @staticmethod
    def osrm_api_time(start_coords, end_coords, delay=0.5):
        """Calculate travel time using OSRM API (free, no key required)"""
        try:
            if delay > 0:
                time.sleep(delay)  # Rate limiting
                
            # OSRM uses longitude,latitude format
            url = f"http://router.project-osrm.org/route/v1/driving/{start_coords[1]},{start_coords[0]};{end_coords[1]},{end_coords[0]}"
            params = {
                'overview': 'false',
                'geometries': 'geojson'
            }
            
            response = requests.get(url, params=params, timeout=10)
            
            if response.status_code == 200:
                result = response.json()
                if 'routes' in result and len(result['routes']) > 0:
                    duration_seconds = result['routes'][0]['duration']
                    return duration_seconds / 60  # Convert to minutes
                else:
                    raise Exception("No routes found in response")
            else:
                raise Exception(f"OSRM API returned status code: {response.status_code}")
                
        except Exception as e:
            logger.log(f"OSRM API error for {start_coords} to {end_coords}: {str(e)}", "WARNING")
            return DistanceCalculator.euclidean_distance_time(start_coords, end_coords)

class NetworkOptimizer:
    @staticmethod
    def linear_programming_optimization(travel_times, capacities, demands, capacity_flex=0.05):
        """Solve the assignment problem using linear programming"""
        logger.log("Starting Linear Programming optimization")
        logger.update_progress(78, "Linear Programming", "Creating optimization model")
        
        n_districts, n_labs = travel_times.shape
        
        # Create the optimization problem
        prob = pulp.LpProblem("LabAssignment", pulp.LpMinimize)
        
        logger.update_progress(79, "Linear Programming", "Creating decision variables")
        
        # Decision variables: x[i,j] = 1 if district i is assigned to lab j
        x = {}
        for i in range(n_districts):
            for j in range(n_labs):
                x[i,j] = pulp.LpVariable(f"x_{i}_{j}", cat='Binary')
        
        logger.update_progress(80, "Linear Programming", "Setting up objective function")
        
        # Objective function: minimize total weighted travel time
        prob += pulp.lpSum([travel_times[i,j] * demands[i] * x[i,j] 
                           for i in range(n_districts) for j in range(n_labs)])
        
        logger.update_progress(81, "Linear Programming", "Adding assignment constraints")
        
        # Constraint 1: Each district must be assigned to exactly one lab
        for i in range(n_districts):
            prob += pulp.lpSum([x[i,j] for j in range(n_labs)]) == 1
        
        logger.update_progress(82, "Linear Programming", "Adding capacity constraints")
        
        # Constraint 2: Lab capacity constraints (with flexibility)
        for j in range(n_labs):
            prob += pulp.lpSum([demands[i] * x[i,j] for i in range(n_districts)]) <= capacities[j] * (1 + capacity_flex)
        
        logger.update_progress(83, "Linear Programming", "Solving optimization problem")
        
        # Solve the problem
        prob.solve(pulp.PULP_CBC_CMD(msg=0))
        
        if prob.status == pulp.LpStatusOptimal:
            logger.log("Linear programming solution found")
            logger.update_progress(84, "Linear Programming", "Processing optimal solution")
            
            # Extract the solution
            assignment_matrix = np.zeros((n_districts, n_labs))
            for i in range(n_districts):
                for j in range(n_labs):
                    if x[i,j].varValue is not None:
                        assignment_matrix[i,j] = x[i,j].varValue
            
            # Calculate lab loads
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
        
        # Sort districts by demand (largest first)
        demand_order = np.argsort(demands)[::-1]
        
        logger.update_progress(79, "Greedy Algorithm", "Sorting districts by demand")
        
        total_assignments = len(demand_order)
        for idx, i in enumerate(demand_order):
            # Update progress for assignments (79% to 84%) - rounded to 2 decimal places
            assignment_progress = round(79 + (idx / total_assignments) * 5, 2)
            logger.update_progress(assignment_progress, "Greedy Algorithm", f"Assigning district {idx+1}/{total_assignments}")
            
            # Sort labs by travel time for this district
            lab_order = np.argsort(travel_times[i, :])
            assigned = False
            
            for j in lab_order:
                if remaining_capacity[j] >= demands[i]:
                    assignments[i] = j
                    remaining_capacity[j] -= demands[i]
                    assigned = True
                    break
            
            # If no lab has enough capacity, assign to the lab with most remaining capacity
            if not assigned:
                best_lab = np.argmax(remaining_capacity)
                assignments[i] = best_lab
                remaining_capacity[best_lab] = max(0, remaining_capacity[best_lab] - demands[i])
                logger.log(f"District {i} assigned to overloaded lab {best_lab}", "WARNING")
        
        logger.update_progress(84, "Greedy Algorithm", "Converting to assignment matrix")
        
        # Convert to assignment matrix
        assignment_matrix = np.zeros((n_districts, n_labs))
        for i in range(n_districts):
            if assignments[i] >= 0:
                assignment_matrix[i, assignments[i]] = 1
        
        # Calculate lab loads and objective value
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

# FLASK ROUTES
@app.route('/')
def index():
    content = INDEX_CONTENT
    return render_template_string(BASE_TEMPLATE, content=content, extra_scripts=COMMON_SCRIPTS)

@app.route('/fresh_start')
def fresh_start():
    """Force a completely fresh start - clears session and redirects to home"""
    session.clear()
    session.modified = True
    logger.clear()
    return redirect('/')

@app.route('/data_input')
def data_input():
    content = DATA_INPUT_CONTENT
    return render_template_string(BASE_TEMPLATE, content=content, extra_scripts=UPLOAD_SCRIPTS)

@app.route('/settings')
def settings():
    content = SETTINGS_CONTENT
    return render_template_string(BASE_TEMPLATE, content=content, extra_scripts=SETTINGS_SCRIPTS)

@app.route('/results')
def results():
    content = RESULTS_CONTENT
    return render_template_string(BASE_TEMPLATE, content=content, extra_scripts=RESULTS_SCRIPTS)

@app.route('/map')
def map_view():
    content = MAP_CONTENT
    return render_template_string(BASE_TEMPLATE, content=content, extra_scripts=MAP_SCRIPTS)

@app.route('/isochrone')
def isochrone():
    content = ISOCHRONE_CONTENT
    return render_template_string(BASE_TEMPLATE, content=content, extra_scripts=ISOCHRONE_SCRIPTS)

@app.route('/about')
def about():
    content = ABOUT_CONTENT
    return render_template_string(BASE_TEMPLATE, content=content, extra_scripts=COMMON_SCRIPTS)

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

@app.route('/upload_data', methods=['POST'])
def upload_data():
    """Handle file uploads and data processing"""
    try:
        uploaded_files = []
        
        for file_type in ['district_file', 'cdst_file']:
            if file_type in request.files:
                file = request.files[file_type]
                if file.filename != '':
                    # Clear any existing data of this type first
                    if file_type == 'district_file':
                        session.pop('district_data', None)
                    elif file_type == 'cdst_file':
                        session.pop('cdst_data', None)
                    
                    df = pd.read_csv(file)
                    uploaded_files.append(file_type)
                    
                    if file_type == 'district_file':
                        # Handle your specific CSV format
                        if 'District' in df.columns:
                            df = df.rename(columns={
                                'District': 'district',
                                'CDST Lab linked to currently': 'current_cdst',
                                'Latitude of District HQ': 'lat',
                                'Longitude of District HQ': 'lon',
                                'Presumptive tests in one quarter': 'tests_per_quarter'
                            })
                        
                        # Validate required columns
                        required_cols = ['district', 'current_cdst', 'lat', 'lon', 'tests_per_quarter']
                        missing_cols = [col for col in required_cols if col not in df.columns]
                        if missing_cols:
                            return jsonify({
                                'status': 'error', 
                                'message': f'Missing columns in district file: {missing_cols}'
                            })
                        
                        # Clean and validate data
                        df['tests_per_quarter'] = pd.to_numeric(df['tests_per_quarter'], errors='coerce')
                        df['lat'] = pd.to_numeric(df['lat'], errors='coerce')
                        df['lon'] = pd.to_numeric(df['lon'], errors='coerce')
                        
                        # Remove rows with invalid data
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
                        
                        if initial_rows != final_rows:
                            logger.log(f"Removed {initial_rows - final_rows} invalid rows from district data")
                        
                    elif file_type == 'cdst_file':
                        # Handle your specific CSV format
                        if 'Name of CDST Lab linked' in df.columns:
                            df = df.rename(columns={
                                'Name of CDST Lab linked': 'lab_name',
                                'Address': 'address',
                                'Latitude': 'lat',
                                'Longitude': 'lon',
                                'Capacity of lab': 'capacity'
                            })
                        
                        # Validate required columns
                        required_cols = ['lab_name', 'address', 'lat', 'lon', 'capacity']
                        missing_cols = [col for col in required_cols if col not in df.columns]
                        if missing_cols:
                            return jsonify({
                                'status': 'error', 
                                'message': f'Missing columns in CDST file: {missing_cols}'
                            })
                        
                        # Clean and validate data
                        df['capacity'] = pd.to_numeric(df['capacity'], errors='coerce')
                        df['lat'] = pd.to_numeric(df['lat'], errors='coerce')
                        df['lon'] = pd.to_numeric(df['lon'], errors='coerce')
                        
                        # Remove rows with invalid data
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
                        
                        if initial_rows != final_rows:
                            logger.log(f"Removed {initial_rows - final_rows} invalid rows from CDST data")
        
        # Clear any old optimization results when new data is uploaded
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

@app.route('/clear_session', methods=['POST'])
def clear_session():
    """Clear all session data"""
    try:
        # Get keys before clearing for debugging
        old_keys = list(session.keys())
        
        # Clear the entire session
        session.clear()
        
        # Force session modification
        session.modified = True
        
        # Verify session is empty
        remaining_keys = list(session.keys())
        
        return jsonify({
            'status': 'success', 
            'message': 'All session data cleared',
            'debug': {
                'old_keys': old_keys,
                'remaining_keys': remaining_keys
            }
        })
    except Exception as e:
        return jsonify({'status': 'error', 'message': str(e)})

@app.route('/debug_session')
def debug_session():
    """Debug route to see what's in the session"""
    session_info = {}
    for key in session.keys():
        try:
            value = session[key]
            if isinstance(value, str):
                if len(value) > 100:
                    session_info[key] = f"String data ({len(value)} characters)"
                else:
                    session_info[key] = value
            else:
                session_info[key] = str(type(value))
        except:
            session_info[key] = "Error reading value"
    
    return jsonify({
        'session_keys': list(session.keys()),
        'session_info': session_info,
        'session_id': request.cookies.get('session', 'No session cookie')
    })

@app.route('/hard_reset', methods=['POST'])
def hard_reset():
    """Nuclear option - completely reset everything"""
    try:
        # Clear session
        session.clear()
        session.modified = True
        
        # Clear any global variables if they exist
        global logger
        logger.clear()
        
        return jsonify({
            'status': 'success', 
            'message': 'Complete reset performed',
            'remaining_keys': list(session.keys())
        })
    except Exception as e:
        return jsonify({'status': 'error', 'message': str(e)})

@app.route('/debug_plots')
def debug_plots():
    """Debug route to check plot data"""
    if 'optimization_results' not in session:
        return jsonify({'error': 'No optimization results in session'})
    
    try:
        results_df = pd.read_json(session['optimization_results'])
        utilization_df = pd.read_json(session['lab_utilization'])
        
        debug_info = {
            'results_shape': results_df.shape,
            'results_columns': list(results_df.columns),
            'utilization_shape': utilization_df.shape,
            'utilization_columns': list(utilization_df.columns),
            'sample_results': results_df.head(3).to_dict('records'),
            'sample_utilization': utilization_df.head(3).to_dict('records'),
            'current_travel_time_stats': {
                'count': len(results_df['current_travel_time'].dropna()),
                'min': float(results_df['current_travel_time'].min()),
                'max': float(results_df['current_travel_time'].max()),
                'mean': float(results_df['current_travel_time'].mean())
            },
            'optimized_travel_time_stats': {
                'count': len(results_df['optimized_travel_time'].dropna()),
                'min': float(results_df['optimized_travel_time'].min()),
                'max': float(results_df['optimized_travel_time'].max()),
                'mean': float(results_df['optimized_travel_time'].mean())
            }
        }
        
        return jsonify(debug_info)
    except Exception as e:
        return jsonify({'error': str(e)})

@app.route('/force_clear_results', methods=['POST'])
def force_clear_results():
    """Force clear optimization results to start fresh"""
    try:
        # Clear only optimization results, keep the input data
        session.pop('optimization_results', None)
        session.pop('lab_utilization', None)
        session.pop('optimization_method', None)
        session.pop('distance_method', None)
        session.modified = True
        
        return jsonify({'status': 'success', 'message': 'Optimization results cleared. Please re-run optimization.'})
    except Exception as e:
        return jsonify({'status': 'error', 'message': str(e)})

@app.route('/get_optimization_progress')
def get_optimization_progress():
    """Get current optimization progress"""
    return jsonify(logger.get_progress_info())

@app.route('/run_optimization', methods=['POST'])
def run_optimization():
    """Run the optimization algorithm with real-time progress tracking"""
    try:
        logger.clear()
        logger.set_stage("Initializing", 0)
        params = request.json
        
        # Check if data is uploaded
        if 'district_data' not in session or 'cdst_data' not in session:
            return jsonify({'status': 'error', 'message': 'Please upload data files first'})
        
        district_df = pd.read_json(session['district_data'])
        cdst_df = pd.read_json(session['cdst_data'])
        
        logger.log("=== OPTIMIZATION STARTED ===")
        logger.log(f"Districts: {len(district_df)}, CDST Labs: {len(cdst_df)}")
        logger.update_progress(5, "Data Validation", "Data loaded successfully")
        
        # Calculate travel time matrix
        n_districts, n_labs = len(district_df), len(cdst_df)
        travel_times = np.zeros((n_districts, n_labs))
        
        distance_method = params.get('distance_method', 'euclidean')
        logger.log(f"Using distance method: {distance_method}")
        logger.update_progress(10, "Distance Calculation Setup", f"Calculating {n_districts * n_labs} distance pairs")
        
        # Calculate distances based on selected method
        total_calculations = n_districts * n_labs
        completed = 0
        
        # Progress tracking for distance calculations (10% to 70% of total progress)
        distance_progress_start = 10
        distance_progress_end = 70
        
        for i in range(n_districts):
            for j in range(n_labs):
                start_coords = (district_df.iloc[i]['lat'], district_df.iloc[i]['lon'])
                end_coords = (cdst_df.iloc[j]['lat'], cdst_df.iloc[j]['lon'])
                
                if distance_method == 'ors_api':
                    # Use hardcoded API key
                    travel_time = DistanceCalculator.openroute_service_time(start_coords, end_coords, ORS_API_KEY)
                elif distance_method == 'osrm_api':
                    travel_time = DistanceCalculator.osrm_api_time(start_coords, end_coords)
                else:
                    travel_time = DistanceCalculator.euclidean_distance_time(start_coords, end_coords)
                
                travel_times[i, j] = travel_time
                completed += 1
                
                # Update progress every 5% of calculations or every 20 calculations, whichever is less frequent
                update_frequency = max(1, min(total_calculations // 20, total_calculations // 20))
                if completed % update_frequency == 0 or completed == total_calculations:
                    calc_progress = (completed / total_calculations)
                    overall_progress = distance_progress_start + (calc_progress * (distance_progress_end - distance_progress_start))
                    
                    stage_msg = f"Distance Calculation ({completed}/{total_calculations})"
                    detail_msg = f"Calculated distances for {completed} pairs using {distance_method}"
                    logger.update_progress(round(overall_progress, 2), stage_msg, detail_msg)
        
        logger.update_progress(75, "Optimization Setup", "Distance calculations completed, setting up optimization")
        logger.log("Distance calculations completed")
        
        # Run optimization
        capacities = cdst_df['capacity'].values
        demands = district_df['tests_per_quarter'].values
        capacity_flex = params.get('capacity_flexibility', 5) / 100
        
        logger.log(f"Total demand: {demands.sum()}, Total capacity: {capacities.sum()}")
        logger.log(f"Capacity flexibility: {capacity_flex*100}%")
        
        if params.get('optimization_method') == 'linear_programming':
            logger.update_progress(80, "Linear Programming", "Running linear programming optimization")
            result = NetworkOptimizer.linear_programming_optimization(
                travel_times, capacities, demands, capacity_flex
            )
        else:
            logger.update_progress(80, "Greedy Algorithm", "Running greedy heuristic optimization")
            result = NetworkOptimizer.greedy_optimization(travel_times, capacities, demands)
        
        if result is None:
            logger.update_progress(100, "Failed", "Optimization failed - no feasible solution found")
            return jsonify({
                'status': 'error', 
                'message': 'Optimization failed - no feasible solution found',
                'logs': logger.get_logs()
            })
        
        logger.update_progress(85, "Processing Results", "Optimization completed, processing results")
        
        # Process results
        assignment_matrix = result['assignment']
        results_data = []
        
        for i in range(n_districts):
            assigned_lab_idx = np.argmax(assignment_matrix[i, :])
            optimized_assignment = cdst_df.iloc[assigned_lab_idx]['lab_name']
            optimized_travel_time = travel_times[i, assigned_lab_idx]
            
            # Find current assignment travel time
            current_cdst = district_df.iloc[i]['current_cdst']
            current_lab_mask = cdst_df['lab_name'] == current_cdst
            if current_lab_mask.any():
                current_lab_idx = cdst_df[current_lab_mask].index[0]
                current_travel_time = travel_times[i, current_lab_idx]
            else:
                current_travel_time = optimized_travel_time  # If current assignment not found
                logger.log(f"Warning: Current assignment '{current_cdst}' not found for district {district_df.iloc[i]['district']}", "WARNING")
            
            results_data.append({
                'district': district_df.iloc[i]['district'],
                'current_assignment': current_cdst,
                'optimized_assignment': optimized_assignment,
                'current_travel_time': current_travel_time,
                'optimized_travel_time': optimized_travel_time,
                'improvement_minutes': current_travel_time - optimized_travel_time,
                'tests_per_quarter': district_df.iloc[i]['tests_per_quarter'],
                'district_lat': district_df.iloc[i]['lat'],
                'district_lon': district_df.iloc[i]['lon']
            })
        
        results_df = pd.DataFrame(results_data)
        logger.update_progress(90, "Calculating Utilization", "Computing lab utilization statistics")
        
        # Calculate lab utilization
        utilization_data = []
        for j in range(n_labs):
            lab_name = cdst_df.iloc[j]['lab_name']
            capacity = cdst_df.iloc[j]['capacity']
            optimized_load = result['lab_loads'][j]
            
            # Calculate current load
            current_assignments = district_df[district_df['current_cdst'] == lab_name]
            current_load = current_assignments['tests_per_quarter'].sum() if not current_assignments.empty else 0
            
            utilization_data.append({
                'lab_name': lab_name,
                'capacity': capacity,
                'current_load': current_load,
                'optimized_load': optimized_load,
                'current_utilization': (current_load / capacity) * 100 if capacity > 0 else 0,
                'optimized_utilization': (optimized_load / capacity) * 100 if capacity > 0 else 0
            })
        
        utilization_df = pd.DataFrame(utilization_data)
        
        logger.update_progress(95, "Saving Results", "Storing optimization results")
        
        # Store results in session
        session['optimization_results'] = results_df.to_json()
        session['lab_utilization'] = utilization_df.to_json()
        session['optimization_method'] = params.get('optimization_method', 'linear_programming')
        session['distance_method'] = distance_method
        
        # Log summary
        total_time_saved = results_df['improvement_minutes'].sum()
        reassigned_count = len(results_df[results_df['current_assignment'] != results_df['optimized_assignment']])
        
        logger.update_progress(100, "Completed", "Optimization completed successfully")
        logger.log("=== OPTIMIZATION COMPLETED ===")
        logger.log(f"Total time saved: {total_time_saved:.2f} minutes")
        logger.log(f"Districts reassigned: {reassigned_count}/{len(results_df)}")
        logger.log(f"Objective value: {result['objective_value']:.2f}")
        
        return jsonify({
            'status': 'success',
            'message': 'Optimization completed successfully!',
            'logs': logger.get_logs(),
            'summary': {
                'total_time_saved': total_time_saved,
                'districts_reassigned': reassigned_count,
                'total_districts': len(results_df)
            }
        })
        
    except Exception as e:
        logger.update_progress(100, "Error", f"Optimization failed: {str(e)}")
        logger.log(f"Optimization failed: {str(e)}", "ERROR")
        return jsonify({
            'status': 'error', 
            'message': str(e), 
            'logs': logger.get_logs()
        })

@app.route('/get_optimization_data')
def get_optimization_data():
    """Return optimization results for display"""
    if 'optimization_results' not in session:
        return jsonify({'status': 'error', 'message': 'No results available'})
    
    try:
        results_df = pd.read_json(session['optimization_results'])
        utilization_df = pd.read_json(session['lab_utilization'])
        
        return jsonify({
            'status': 'success',
            'results': results_df.to_dict('records'),
            'utilization': utilization_df.to_dict('records'),
            'method': session.get('optimization_method', 'Unknown'),
            'distance_method': session.get('distance_method', 'Unknown')
        })
    except Exception as e:
        return jsonify({'status': 'error', 'message': str(e)})

@app.route('/generate_plots')
def generate_plots():
    """Generate plotly charts for results visualization"""
    if 'optimization_results' not in session:
        return jsonify({'status': 'error', 'message': 'No data available'})
    
    try:
        results_df = pd.read_json(session['optimization_results'])
        utilization_df = pd.read_json(session['lab_utilization'])
        
        logger.log(f"Generating plots - Results: {results_df.shape}, Utilization: {utilization_df.shape}")
        
        # Validate data
        if results_df.empty or utilization_df.empty:
            return jsonify({'status': 'error', 'message': 'Empty dataframes'})
        
        # Lab utilization comparison plot - Simplified for debugging
        utilization_plot = go.Figure()
        
        # Simple bar chart without complex features first
        utilization_plot.add_trace(go.Bar(
            name='Current', 
            x=list(range(len(utilization_df))),  # Use indices instead of names
            y=utilization_df['current_utilization'].tolist(), 
            marker_color='lightcoral',
            text=[f'{val:.0f}%' for val in utilization_df['current_utilization']],
            textposition='outside'
        ))
        utilization_plot.add_trace(go.Bar(
            name='Optimized', 
            x=list(range(len(utilization_df))),  # Use indices instead of names
            y=utilization_df['optimized_utilization'].tolist(), 
            marker_color='lightblue',
            text=[f'{val:.0f}%' for val in utilization_df['optimized_utilization']],
            textposition='outside'
        ))
        
        # Set x-axis to show lab names
        utilization_plot.update_layout(
            title='Lab Utilization Comparison (%)',
            xaxis_title='CDST Labs',
            yaxis_title='Utilization (%)',
            barmode='group',
            template='plotly_white',
            height=400,
            xaxis=dict(
                tickmode='array',
                tickvals=list(range(len(utilization_df))),
                ticktext=[name[:15] + '...' if len(name) > 15 else name for name in utilization_df['lab_name']]
            )
        )
        
        # Travel time improvements plot - Check for improvements
        improvements = results_df[results_df['improvement_minutes'] > 0.1]  # Filter for meaningful improvements
        logger.log(f"Found {len(improvements)} districts with improvements > 0.1 minutes")
        
        if not improvements.empty:
            improvements = improvements.nlargest(15, 'improvement_minutes')
            improvement_plot = go.Figure([go.Bar(
                x=improvements['improvement_minutes'].tolist(),
                y=improvements['district'].tolist(),
                orientation='h',
                marker_color='steelblue',
                text=[f'{val:.1f}' for val in improvements['improvement_minutes']],
                textposition='outside'
            )])
            improvement_plot.update_layout(
                title=f'Top {len(improvements)} Travel Time Improvements',
                xaxis_title='Minutes Saved',
                yaxis_title='Districts',
                template='plotly_white',
                height=400
            )
        else:
            improvement_plot = go.Figure()
            improvement_plot.add_annotation(
                text="Current network is already well-optimized!<br>No significant improvements found.",
                xref="paper", yref="paper",
                x=0.5, y=0.5, showarrow=False,
                font=dict(size=16, color="green")
            )
            improvement_plot.update_layout(
                title='Travel Time Improvements Analysis',
                template='plotly_white',
                height=400
            )
        
        # Current travel times histogram
        current_times = results_df['current_travel_time'].dropna().tolist()
        logger.log(f"Current travel times: count={len(current_times)}, range={min(current_times):.1f}-{max(current_times):.1f}")
        
        current_travel_histogram = go.Figure()
        current_travel_histogram.add_trace(go.Histogram(
            x=current_times,
            nbinsx=12,
            name='Current',
            marker_color='lightcoral',
            opacity=0.8
        ))
        current_travel_histogram.update_layout(
            title=f'Current Travel Times<br><sub>Mean: {np.mean(current_times):.1f} min | Count: {len(current_times)} districts</sub>',
            xaxis_title='Travel Time (minutes)',
            yaxis_title='Number of Districts',
            template='plotly_white',
            height=400,
            showlegend=False
        )
        
        # Optimized travel times histogram
        optimized_times = results_df['optimized_travel_time'].dropna().tolist()
        logger.log(f"Optimized travel times: count={len(optimized_times)}, range={min(optimized_times):.1f}-{max(optimized_times):.1f}")
        
        optimized_travel_histogram = go.Figure()
        optimized_travel_histogram.add_trace(go.Histogram(
            x=optimized_times,
            nbinsx=12,
            name='Optimized',
            marker_color='lightblue',
            opacity=0.8
        ))
        optimized_travel_histogram.update_layout(
            title=f'Optimized Travel Times<br><sub>Mean: {np.mean(optimized_times):.1f} min | Count: {len(optimized_times)} districts</sub>',
            xaxis_title='Travel Time (minutes)',
            yaxis_title='Number of Districts',
            template='plotly_white',
            height=400,
            showlegend=False
        )
        
        logger.log("Plot generation complete - preparing JSON response")
        
        response_data = {
            'status': 'success',
            'utilization_plot': json.dumps(utilization_plot, cls=plotly.utils.PlotlyJSONEncoder),
            'improvement_plot': json.dumps(improvement_plot, cls=plotly.utils.PlotlyJSONEncoder),
            'current_travel_histogram': json.dumps(current_travel_histogram, cls=plotly.utils.PlotlyJSONEncoder),
            'optimized_travel_histogram': json.dumps(optimized_travel_histogram, cls=plotly.utils.PlotlyJSONEncoder)
        }
        
        logger.log("JSON response prepared successfully")
        return jsonify(response_data)
        
    except Exception as e:
        error_msg = f"Plot generation error: {str(e)}"
        logger.log(error_msg, "ERROR")
        import traceback
        logger.log(f"Traceback: {traceback.format_exc()}", "ERROR")
        return jsonify({'status': 'error', 'message': error_msg})

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

@app.route('/export_results')
def export_results():
    """Export optimization results to CSV"""
    if 'optimization_results' not in session:
        return jsonify({'status': 'error', 'message': 'No results available'})
    
    try:
        results_df = pd.read_json(session['optimization_results'])
        utilization_df = pd.read_json(session['lab_utilization'])
        
        # Create a BytesIO object to store the zip file
        zip_buffer = io.BytesIO()
        
        with zipfile.ZipFile(zip_buffer, 'w', zipfile.ZIP_DEFLATED) as zip_file:
            # Add optimization results
            results_csv = io.StringIO()
            results_df.to_csv(results_csv, index=False)
            zip_file.writestr('optimization_results.csv', results_csv.getvalue())
            
            # Add lab utilization
            utilization_csv = io.StringIO()
            utilization_df.to_csv(utilization_csv, index=False)
            zip_file.writestr('lab_utilization.csv', utilization_csv.getvalue())
            
            # Add summary report
            total_time_saved = results_df['improvement_minutes'].sum()
            reassigned_count = len(results_df[results_df['current_assignment'] != results_df['optimized_assignment']])
            avg_improvement = results_df['improvement_minutes'].mean()
            
            summary = f"""Diagnostic Network Optimization Results Summary
Generated on: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}

Optimization Method: {session.get('optimization_method', 'Unknown')}
Distance Calculation: {session.get('distance_method', 'Unknown')}

Total Districts: {len(results_df)}
Districts Reassigned: {reassigned_count} ({(reassigned_count/len(results_df)*100):.1f}%)
Total Time Saved: {total_time_saved:.2f} minutes
Average Time Saved per District: {avg_improvement:.2f} minutes

Lab Utilization Summary:
{utilization_df[['lab_name', 'current_utilization', 'optimized_utilization']].to_string(index=False)}
"""
            zip_file.writestr('summary_report.txt', summary)
        
        zip_buffer.seek(0)
        
        return send_file(
            zip_buffer,
            mimetype='application/zip',
            as_attachment=True,
            download_name=f'optimization_results_{datetime.now().strftime("%Y%m%d_%H%M%S")}.zip'
        )
        
    except Exception as e:
        return jsonify({'status': 'error', 'message': str(e)})

@app.route('/get_lab_options')
def get_lab_options():
    """Get available CDST labs for isochrone dropdown"""
    if 'cdst_data' not in session:
        return jsonify({'status': 'error', 'message': 'No CDST data available. Please upload CDST lab data first.'})
    
    try:
        cdst_df = pd.read_json(session['cdst_data'])
        if cdst_df.empty:
            return jsonify({'status': 'error', 'message': 'CDST data is empty'})
        
        labs = cdst_df['lab_name'].tolist()
        return jsonify({'status': 'success', 'labs': labs})
    except Exception as e:
        return jsonify({'status': 'error', 'message': str(e)})

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

# MAP DOWNLOAD ROUTES

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