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

# Distance and optimization libraries
from geopy.distance import geodesic
import pulp  # For linear programming
from scipy.spatial.distance import cdist

app = Flask(__name__)
app.secret_key = 'your-secret-key-change-this-for-production'
app.config['MAX_CONTENT_LENGTH'] = 16 * 1024 * 1024  # 16MB max file size

# Create uploads directory if it doesn't exist
UPLOAD_FOLDER = tempfile.mkdtemp()
app.config['UPLOAD_FOLDER'] = UPLOAD_FOLDER

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
                        <li class="nav-item"><a class="nav-link" href="/about"><i class="fas fa-info-circle"></i> About</a></li>
                    </ul>
                </div>
            </nav>
            <main class="col-md-9 ms-sm-auto col-lg-10 px-md-4 content-area">
                {{ content }}
            </main>
        </div>
    </div>
    <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js"></script>
    <script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>
    <script type="text/javascript" src="https://cdn.datatables.net/1.11.5/js/jquery.dataTables.min.js"></script>
    <script type="text/javascript" src="https://cdn.datatables.net/1.11.5/js/dataTables.bootstrap5.min.js"></script>
    {{ extra_scripts }}
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
            <a class="btn btn-light btn-lg" href="/data_input" role="button"><i class="fas fa-rocket"></i> Get Started</a>
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
            <div class="card-header"><h5>Data Templates and Upload</h5></div>
            <div class="card-body">
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
                <div id="validationStatus" class="progress-text">Waiting for data upload...</div>
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
                    <label class="form-check-label" for="ors_api"><strong>OpenRouteService API</strong> (Most accurate)</label>
                </div>
                <div id="ors_settings" style="display:none;" class="mt-3">
                    <input type="text" class="form-control" id="ors_api_key" placeholder="API Key" value="5b3ce3597851110001cf62481a51a0c1d253455eb67aa25df367bd8d">
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
                <div class="mt-3">
                    <div class="progress" style="display:none;">
                        <div id="progressBar" class="progress-bar progress-bar-striped progress-bar-animated" style="width: 0%"></div>
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
            <div class="card-header"><h5>Summary Statistics</h5></div>
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
    <div class="col-12">
        <div class="card">
            <div class="card-header"><h5>Detailed Assignments</h5></div>
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
            <div class="card-header">
                <h5>Interactive Network Map</h5>
                <p class="mb-0">🔴 Red markers: CDST Labs | 🔵 Blue circles: District HQs | 🟢 Green lines: New assignments</p>
            </div>
            <div class="card-body">
                <div id="networkMap" style="height: 700px;">Loading map...</div>
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
                <h3>Diagnostic Network Optimization Tool</h3>
                <p>This tool helps optimize the assignment of district laboratories to CDST facilities to minimize travel time while respecting capacity constraints.</p>
                <h4>Key Features:</h4>
                <ul>
                    <li>📊 Multiple distance calculation methods</li>
                    <li>🧮 Linear Programming & Greedy optimization algorithms</li>
                    <li>🗺️ Interactive mapping and visualization</li>
                    <li>📈 Comprehensive results analysis</li>
                </ul>
                <hr>
                <p><strong>Version:</strong> 2.1 Flask Debugged Edition | <strong>Platform:</strong> Posit Connect Ready</p>
                <p><strong>Dependencies:</strong> Removed geopandas/shapely (not used in code)</p>
            </div>
        </div>
    </div>
</div>
'''

# JavaScript for interactivity
COMMON_SCRIPTS = '''
<script>
$(document).ready(function() {
    // Common functionality
    $('input[name="distance_method"]').change(function() {
        if ($(this).val() === 'ors_api') {
            $('#ors_settings').show();
        } else {
            $('#ors_settings').hide();
        }
    });
});
</script>
'''

UPLOAD_SCRIPTS = '''
<script>
$(document).ready(function() {
    loadValidationStatus();
    
    $('#uploadForm').on('submit', function(e) {
        e.preventDefault();
        var formData = new FormData();
        var districtFile = $('#district_file')[0].files[0];
        var cdstFile = $('#cdst_file')[0].files[0];
        
        if (districtFile) formData.append('district_file', districtFile);
        if (cdstFile) formData.append('cdst_file', cdstFile);
        
        $.ajax({
            url: '/upload_data',
            type: 'POST',
            data: formData,
            processData: false,
            contentType: false,
            success: function(response) {
                if (response.status === 'success') {
                    $('#uploadStatus').html('<div class="alert alert-success">Files uploaded successfully!</div>');
                    loadValidationStatus();
                } else {
                    $('#uploadStatus').html('<div class="alert alert-danger">Upload failed: ' + response.message + '</div>');
                }
            },
            error: function() {
                $('#uploadStatus').html('<div class="alert alert-danger">Upload failed: Network error</div>');
            }
        });
    });
    
    function loadValidationStatus() {
        $.get('/validate_data', function(data) {
            $('#validationStatus').text(data.messages.join('\\n'));
        });
    }
});
</script>
'''

SETTINGS_SCRIPTS = '''
<script>
$(document).ready(function() {
    $('#runOptimization').click(function() {
        var params = {
            distance_method: $('input[name="distance_method"]:checked').val(),
            optimization_method: $('input[name="optimization_method"]:checked').val(),
            capacity_flexibility: parseInt($('#capacity_flexibility').val()),
            ors_api_key: $('#ors_api_key').val()
        };
        
        $('#runOptimization').prop('disabled', true).html('<i class="fas fa-spinner fa-spin"></i> Running...');
        $('.progress').show();
        
        $.ajax({
            url: '/run_optimization',
            type: 'POST',
            contentType: 'application/json',
            data: JSON.stringify(params),
            success: function(response) {
                $('#runOptimization').prop('disabled', false).html('<i class="fas fa-play"></i> Run Optimization');
                $('.progress').hide();
                if (response.status === 'success') {
                    $('#optimizationLogs').text(response.logs);
                    alert('Optimization completed! Check Results tab.');
                } else {
                    $('#optimizationLogs').text('Error: ' + response.message + '\\n\\n' + (response.logs || ''));
                    alert('Optimization failed: ' + response.message);
                }
            },
            error: function() {
                $('#runOptimization').prop('disabled', false).html('<i class="fas fa-play"></i> Run Optimization');
                $('.progress').hide();
                alert('Network error occurred');
            }
        });
    });
});
</script>
'''

RESULTS_SCRIPTS = '''
<script>
$(document).ready(function() {
    loadOptimizationResults();
    
    function loadOptimizationResults() {
        $.get('/get_optimization_data', function(data) {
            if (data.status === 'success') {
                createSummaryTable(data);
                loadPlots();
                createAssignmentsTable(data.results);
            } else {
                $('#summaryTable').html('<div class="alert alert-warning">No optimization results available. Please run optimization first.</div>');
            }
        });
    }
    
    function createSummaryTable(data) {
        var reassignedCount = data.results.filter(r => r.current_assignment !== r.optimized_assignment).length;
        var totalTimeSaved = data.results.reduce((sum, r) => sum + r.improvement_minutes, 0);
        
        var summaryHtml = `
            <table class="table table-striped">
                <tr><td><strong>Algorithm</strong></td><td>${data.method}</td></tr>
                <tr><td><strong>Total Districts</strong></td><td>${data.results.length}</td></tr>
                <tr><td><strong>Districts Reassigned</strong></td><td>${reassignedCount}</td></tr>
                <tr><td><strong>Total Time Saved (minutes)</strong></td><td>${totalTimeSaved.toFixed(2)}</td></tr>
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
    
    function loadPlots() {
        $.get('/generate_plots', function(data) {
            if (data.utilization_plot && data.improvement_plot) {
                Plotly.newPlot('utilizationPlot', JSON.parse(data.utilization_plot).data, JSON.parse(data.utilization_plot).layout);
                Plotly.newPlot('improvementPlot', JSON.parse(data.improvement_plot).data, JSON.parse(data.improvement_plot).layout);
            }
        });
    }
});
</script>
'''

MAP_SCRIPTS = '''
<script>
$(document).ready(function() {
    $.get('/generate_map', function(data) {
        $('#networkMap').html(data);
    }).fail(function() {
        $('#networkMap').html('<div class="alert alert-warning">No optimization results available. Please upload data and run optimization first.</div>');
    });
});
</script>
'''

# OPTIMIZATION AND UTILITY CLASSES
class OptimizationLogger:
    def __init__(self):
        self.logs = []
        self.progress = 0
    
    def log(self, message, log_type="INFO"):
        timestamp = datetime.now().strftime("%H:%M:%S")
        log_entry = f"[{timestamp}] {log_type}: {message}"
        self.logs.append(log_entry)
        print(log_entry)  # Also print to console for debugging
    
    def update_progress(self, progress, message=""):
        self.progress = progress
        if message:
            self.log(f"Progress: {progress}% - {message}")
    
    def get_logs(self):
        return "\n".join(self.logs)
    
    def clear(self):
        self.logs = []
        self.progress = 0

logger = OptimizationLogger()

class DistanceCalculator:
    @staticmethod
    def euclidean_distance_time(start_coords, end_coords, avg_speed_kmh=30):
        """Calculate travel time using geodesic distance and average speed"""
        try:
            distance_km = geodesic(start_coords, end_coords).kilometers
            return (distance_km / avg_speed_kmh) * 60  # Return minutes
        except Exception as e:
            logger.log(f"Error calculating euclidean distance: {e}", "ERROR")
            return 999  # Return large number if calculation fails
    
    @staticmethod
    def openroute_service_time(start_coords, end_coords, api_key, delay=1):
        """Calculate travel time using OpenRouteService API"""
        try:
            time.sleep(delay)  # Rate limiting
            url = "https://api.openrouteservice.org/v2/directions/driving-car"
            headers = {'Authorization': f'Bearer {api_key}', 'Content-Type': 'application/json'}
            body = {"coordinates": [[start_coords[1], start_coords[0]], [end_coords[1], end_coords[0]]]}
            
            response = requests.post(url, headers=headers, json=body, timeout=30)
            if response.status_code == 200:
                result = response.json()
                return result['routes'][0]['summary']['duration'] / 60  # Convert to minutes
            else:
                logger.log(f"API error {response.status_code}, falling back to euclidean", "WARNING")
                return DistanceCalculator.euclidean_distance_time(start_coords, end_coords)
        except Exception as e:
            logger.log(f"API request failed: {e}, falling back to euclidean", "WARNING")
            return DistanceCalculator.euclidean_distance_time(start_coords, end_coords)

class NetworkOptimizer:
    @staticmethod
    def linear_programming_optimization(travel_times, capacities, demands, capacity_flex=0.05):
        """Solve using linear programming for optimal solution"""
        logger.log("Starting Linear Programming optimization")
        try:
            n_districts, n_labs = travel_times.shape
            
            prob = pulp.LpProblem("LabAssignment", pulp.LpMinimize)
            x = {}
            for i in range(n_districts):
                for j in range(n_labs):
                    x[i,j] = pulp.LpVariable(f"x_{i}_{j}", cat='Binary')
            
            # Objective function: minimize total weighted travel time
            prob += pulp.lpSum([travel_times[i,j] * demands[i] * x[i,j] for i in range(n_districts) for j in range(n_labs)])
            
            # Constraints
            # Each district must be assigned to exactly one lab
            for i in range(n_districts):
                prob += pulp.lpSum([x[i,j] for j in range(n_labs)]) == 1
            
            # Lab capacity constraints
            for j in range(n_labs):
                prob += pulp.lpSum([demands[i] * x[i,j] for i in range(n_districts)]) <= capacities[j] * (1 + capacity_flex)
            
            prob.solve(pulp.PULP_CBC_CMD(msg=0))
            
            if prob.status == pulp.LpStatusOptimal:
                assignment_matrix = np.zeros((n_districts, n_labs))
                for i in range(n_districts):
                    for j in range(n_labs):
                        if x[i,j].varValue:
                            assignment_matrix[i,j] = x[i,j].varValue
                
                lab_loads = np.array([sum(demands[i] * assignment_matrix[i,j] for i in range(n_districts)) for j in range(n_labs)])
                
                logger.log(f"Optimization successful! Objective value: {pulp.value(prob.objective):.2f}")
                return {
                    'assignment': assignment_matrix,
                    'lab_loads': lab_loads,
                    'objective_value': pulp.value(prob.objective),
                    'status': 'optimal'
                }
            else:
                logger.log(f"Optimization failed with status: {pulp.LpStatus[prob.status]}", "ERROR")
                return None
        except Exception as e:
            logger.log(f"Linear programming failed: {e}", "ERROR")
            return None
    
    @staticmethod
    def greedy_optimization(travel_times, capacities, demands):
        """Solve using greedy heuristic for fast approximate solution"""
        logger.log("Starting Greedy optimization")
        try:
            n_districts, n_labs = travel_times.shape
            assignments = np.full(n_districts, -1)
            remaining_capacity = capacities.copy()
            
            # Sort districts by demand (largest first) for better results
            demand_order = np.argsort(demands)[::-1]
            
            for i in demand_order:
                # Sort labs by travel time for this district
                lab_order = np.argsort(travel_times[i, :])
                assigned = False
                
                for j in lab_order:
                    if remaining_capacity[j] >= demands[i]:
                        assignments[i] = j
                        remaining_capacity[j] -= demands[i]
                        assigned = True
                        break
                
                # If no lab has enough capacity, assign to lab with most remaining capacity
                if not assigned:
                    best_lab = np.argmax(remaining_capacity)
                    assignments[i] = best_lab
                    remaining_capacity[best_lab] = max(0, remaining_capacity[best_lab] - demands[i])
                    logger.log(f"District {i} assigned to overloaded lab {best_lab}", "WARNING")
            
            # Convert to assignment matrix
            assignment_matrix = np.zeros((n_districts, n_labs))
            for i in range(n_districts):
                if assignments[i] >= 0:
                    assignment_matrix[i, assignments[i]] = 1
            
            lab_loads = np.array([sum(demands[i] * assignment_matrix[i,j] for i in range(n_districts)) for j in range(n_labs)])
            objective_value = np.sum(assignment_matrix * travel_times * demands.reshape(-1, 1))
            
            logger.log(f"Greedy optimization completed! Objective value: {objective_value:.2f}")
            return {
                'assignment': assignment_matrix,
                'lab_loads': lab_loads,
                'objective_value': objective_value,
                'status': 'optimal'
            }
        except Exception as e:
            logger.log(f"Greedy optimization failed: {e}", "ERROR")
            return None

# FLASK ROUTES
@app.route('/')
def index():
    from markupsafe import Markup
    content = Markup(INDEX_CONTENT)
    extra_scripts = Markup(COMMON_SCRIPTS)
    return render_template_string(BASE_TEMPLATE, content=content, extra_scripts=extra_scripts)

@app.route('/data_input')
def data_input():
    from markupsafe import Markup
    content = Markup(DATA_INPUT_CONTENT)
    extra_scripts = Markup(UPLOAD_SCRIPTS)
    return render_template_string(BASE_TEMPLATE, content=content, extra_scripts=extra_scripts)

@app.route('/settings')
def settings():
    from markupsafe import Markup
    content = Markup(SETTINGS_CONTENT)
    extra_scripts = Markup(SETTINGS_SCRIPTS)
    return render_template_string(BASE_TEMPLATE, content=content, extra_scripts=extra_scripts)

@app.route('/results')
def results():
    from markupsafe import Markup
    content = Markup(RESULTS_CONTENT)
    extra_scripts = Markup(RESULTS_SCRIPTS)
    return render_template_string(BASE_TEMPLATE, content=content, extra_scripts=extra_scripts)

@app.route('/map')
def map_view():
    from markupsafe import Markup
    content = Markup(MAP_CONTENT)
    extra_scripts = Markup(MAP_SCRIPTS)
    return render_template_string(BASE_TEMPLATE, content=content, extra_scripts=extra_scripts)

@app.route('/about')
def about():
    from markupsafe import Markup
    content = Markup(ABOUT_CONTENT)
    extra_scripts = Markup(COMMON_SCRIPTS)
    return render_template_string(BASE_TEMPLATE, content=content, extra_scripts=extra_scripts)

@app.route('/download_template/<template_type>')
def download_template(template_type):
    if template_type == 'district':
        template = pd.DataFrame({
            'district': ['Sample District 1', 'Sample District 2'],
            'current_cdst': ['CDST Lab A', 'CDST Lab B'],
            'lat': [25.5937, 26.2124],
            'lon': [85.1376, 78.1772],
            'tests_per_quarter': [150, 200]
        })
        filename = 'district_lab_template.csv'
    else:
        template = pd.DataFrame({
            'lab_name': ['CDST Lab A', 'CDST Lab B'],
            'address': ['123 Main St', '456 Park Ave'],
            'lat': [25.5937, 26.2124],
            'lon': [85.1376, 78.1772],
            'capacity': [500, 750]
        })
        filename = 'cdst_lab_template.csv'
    
    output = io.StringIO()
    template.to_csv(output, index=False)
    output.seek(0)
    
    return send_file(io.BytesIO(output.getvalue().encode()), mimetype='text/csv', as_attachment=True, download_name=filename)

@app.route('/upload_data', methods=['POST'])
def upload_data():
    try:
        for file_type in ['district_file', 'cdst_file']:
            if file_type in request.files:
                file = request.files[file_type]
                if file.filename != '':
                    try:
                        df = pd.read_csv(file)
                        
                        if file_type == 'district_file':
                            # Handle different column naming conventions
                            column_mapping = {
                                'District': 'district',
                                'CDST Lab linked to currently': 'current_cdst',
                                'Latitude of District HQ': 'lat',
                                'Longitude of District HQ': 'lon',
                                'Presumptive tests in one quarter': 'tests_per_quarter'
                            }
                            
                            # Apply mapping if columns exist
                            for old_col, new_col in column_mapping.items():
                                if old_col in df.columns:
                                    df = df.rename(columns={old_col: new_col})
                            
                            # Validate required columns
                            required_cols = ['district', 'current_cdst', 'lat', 'lon', 'tests_per_quarter']
                            if not all(col in df.columns for col in required_cols):
                                return jsonify({'status': 'error', 'message': f'District file missing columns: {[col for col in required_cols if col not in df.columns]}'})
                            
                            session['district_data'] = df.to_json()
                            
                        elif file_type == 'cdst_file':
                            # Handle different column naming conventions
                            column_mapping = {
                                'Name of CDST Lab linked': 'lab_name',
                                'Address': 'address',
                                'Latitude': 'lat',
                                'Longitude': 'lon',
                                'Capacity of lab': 'capacity'
                            }
                            
                            # Apply mapping if columns exist
                            for old_col, new_col in column_mapping.items():
                                if old_col in df.columns:
                                    df = df.rename(columns={old_col: new_col})
                            
                            # Validate required columns
                            required_cols = ['lab_name', 'address', 'lat', 'lon', 'capacity']
                            if not all(col in df.columns for col in required_cols):
                                return jsonify({'status': 'error', 'message': f'CDST file missing columns: {[col for col in required_cols if col not in df.columns]}'})
                            
                            session['cdst_data'] = df.to_json()
                    
                    except Exception as e:
                        return jsonify({'status': 'error', 'message': f'Error processing {file_type}: {str(e)}'})
        
        return jsonify({'status': 'success'})
    except Exception as e:
        return jsonify({'status': 'error', 'message': str(e)})

@app.route('/validate_data')
def validate_data():
    validation_status = {'districts': False, 'cdst': False, 'messages': []}
    
    if 'district_data' in session:
        try:
            df = pd.read_json(session['district_data'])
            required_cols = ['district', 'current_cdst', 'lat', 'lon', 'tests_per_quarter']
            if all(col in df.columns for col in required_cols):
                validation_status['districts'] = True
                validation_status['messages'].append(f"✅ District lab data: Valid ({len(df)} districts)")
            else:
                missing = [col for col in required_cols if col not in df.columns]
                validation_status['messages'].append(f"❌ District lab data: Missing columns {missing}")
        except Exception as e:
            validation_status['messages'].append(f"❌ District lab data: Invalid - {str(e)}")
    else:
        validation_status['messages'].append("⏳ District lab data: Not uploaded")
    
    if 'cdst_data' in session:
        try:
            df = pd.read_json(session['cdst_data'])
            required_cols = ['lab_name', 'address', 'lat', 'lon', 'capacity']
            if all(col in df.columns for col in required_cols):
                validation_status['cdst'] = True
                validation_status['messages'].append(f"✅ CDST lab data: Valid ({len(df)} labs)")
            else:
                missing = [col for col in required_cols if col not in df.columns]
                validation_status['messages'].append(f"❌ CDST lab data: Missing columns {missing}")
        except Exception as e:
            validation_status['messages'].append(f"❌ CDST lab data: Invalid - {str(e)}")
    else:
        validation_status['messages'].append("⏳ CDST lab data: Not uploaded")
    
    return jsonify(validation_status)

@app.route('/run_optimization', methods=['POST'])
def run_optimization():
    try:
        logger.clear()
        params = request.json
        
        if 'district_data' not in session or 'cdst_data' not in session:
            return jsonify({'status': 'error', 'message': 'Please upload data files first'})
        
        district_df = pd.read_json(session['district_data'])
        cdst_df = pd.read_json(session['cdst_data'])
        
        logger.log("=== OPTIMIZATION STARTED ===")
        logger.log(f"Districts: {len(district_df)}, CDST Labs: {len(cdst_df)}")
        
        # Calculate travel time matrix
        n_districts, n_labs = len(district_df), len(cdst_df)
        travel_times = np.zeros((n_districts, n_labs))
        
        distance_method = params.get('distance_method', 'euclidean')
        logger.log(f"Using distance method: {distance_method}")
        
        for i in range(n_districts):
            for j in range(n_labs):
                try:
                    start_coords = (float(district_df.iloc[i]['lat']), float(district_df.iloc[i]['lon']))
                    end_coords = (float(cdst_df.iloc[j]['lat']), float(cdst_df.iloc[j]['lon']))
                    
                    if distance_method == 'ors_api':
                        travel_time = DistanceCalculator.openroute_service_time(
                            start_coords, end_coords, params.get('ors_api_key', ''))
                    else:
                        travel_time = DistanceCalculator.euclidean_distance_time(start_coords, end_coords)
                    
                    travel_times[i, j] = travel_time
                except Exception as e:
                    logger.log(f"Error calculating distance for district {i}, lab {j}: {e}", "ERROR")
                    travel_times[i, j] = 999  # Large penalty for failed calculations
        
        logger.log("Travel time matrix calculated")
        
        # Run optimization
        capacities = cdst_df['capacity'].values.astype(float)
        demands = district_df['tests_per_quarter'].values.astype(float)
        
        optimization_method = params.get('optimization_method', 'linear_programming')
        logger.log(f"Using optimization method: {optimization_method}")
        
        if optimization_method == 'linear_programming':
            result = NetworkOptimizer.linear_programming_optimization(
                travel_times, capacities, demands, params.get('capacity_flexibility', 5) / 100)
        else:
            result = NetworkOptimizer.greedy_optimization(travel_times, capacities, demands)
        
        if result is None:
            return jsonify({'status': 'error', 'message': 'Optimization failed - check constraints', 'logs': logger.get_logs()})
        
        # Process results
        assignment_matrix = result['assignment']
        results_data = []
        
        for i in range(n_districts):
            assigned_lab_idx = np.argmax(assignment_matrix[i, :])
            optimized_assignment = cdst_df.iloc[assigned_lab_idx]['lab_name']
            optimized_travel_time = travel_times[i, assigned_lab_idx]
            
            current_cdst = district_df.iloc[i]['current_cdst']
            current_lab_matches = cdst_df[cdst_df['lab_name'] == current_cdst]
            
            if len(current_lab_matches) > 0:
                current_lab_idx = current_lab_matches.index[0]
                current_travel_time = travel_times[i, current_lab_idx]
            else:
                logger.log(f"Warning: Current assignment '{current_cdst}' not found in CDST data", "WARNING")
                current_travel_time = optimized_travel_time  # Default to optimized if current not found
            
            results_data.append({
                'district': district_df.iloc[i]['district'],
                'current_assignment': current_cdst,
                'optimized_assignment': optimized_assignment,
                'current_travel_time': float(current_travel_time),
                'optimized_travel_time': float(optimized_travel_time),
                'improvement_minutes': float(current_travel_time - optimized_travel_time),
                'tests_per_quarter': int(district_df.iloc[i]['tests_per_quarter']),
                'district_lat': float(district_df.iloc[i]['lat']),
                'district_lon': float(district_df.iloc[i]['lon'])
            })
        
        results_df = pd.DataFrame(results_data)
        
        # Lab utilization analysis
        utilization_data = []
        for j in range(n_labs):
            lab_name = cdst_df.iloc[j]['lab_name']
            capacity = float(cdst_df.iloc[j]['capacity'])
            optimized_load = float(result['lab_loads'][j])
            
            # Calculate current load
            current_assignments = district_df[district_df['current_cdst'] == lab_name]
            current_load = float(current_assignments['tests_per_quarter'].sum() if len(current_assignments) > 0 else 0)
            
            utilization_data.append({
                'lab_name': lab_name,
                'capacity': capacity,
                'current_load': current_load,
                'optimized_load': optimized_load,
                'current_utilization': (current_load / capacity) * 100 if capacity > 0 else 0,
                'optimized_utilization': (optimized_load / capacity) * 100 if capacity > 0 else 0
            })
        
        utilization_df = pd.DataFrame(utilization_data)
        
        # Store results in session
        session['optimization_results'] = results_df.to_json()
        session['lab_utilization'] = utilization_df.to_json()
        session['optimization_method'] = optimization_method
        session['distance_method'] = distance_method
        
        total_improvement = results_df['improvement_minutes'].sum()
        reassigned_count = len(results_df[results_df['current_assignment'] != results_df['optimized_assignment']])
        
        logger.log(f"Total time saved: {total_improvement:.2f} minutes")
        logger.log(f"Districts reassigned: {reassigned_count}/{len(results_df)}")
        logger.log("=== OPTIMIZATION COMPLETED ===")
        
        return jsonify({
            'status': 'success',
            'message': f'Optimization completed! Time saved: {total_improvement:.1f} min, {reassigned_count} reassignments',
            'logs': logger.get_logs()
        })
        
    except Exception as e:
        logger.log(f"Optimization failed: {str(e)}", "ERROR")
        return jsonify({'status': 'error', 'message': str(e), 'logs': logger.get_logs()})

@app.route('/get_optimization_data')
def get_optimization_data():
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
        return jsonify({'status': 'error', 'message': f'Error loading results: {str(e)}'})

@app.route('/generate_plots')
def generate_plots():
    if 'optimization_results' not in session:
        return jsonify({'status': 'error', 'message': 'No data available'})
    
    try:
        results_df = pd.read_json(session['optimization_results'])
        utilization_df = pd.read_json(session['lab_utilization'])
        
        # Utilization plot
        utilization_plot = go.Figure()
        utilization_plot.add_trace(go.Bar(
            name='Current', 
            x=utilization_df['lab_name'], 
            y=utilization_df['current_utilization'], 
            marker_color='lightcoral'
        ))
        utilization_plot.add_trace(go.Bar(
            name='Optimized', 
            x=utilization_df['lab_name'], 
            y=utilization_df['optimized_utilization'], 
            marker_color='lightblue'
        ))
        utilization_plot.update_layout(
            title='Lab Utilization Comparison (%)', 
            xaxis_title='Labs', 
            yaxis_title='Utilization (%)', 
            barmode='group'
        )
        
        # Improvement plot
        improvements = results_df[results_df['improvement_minutes'] > 0].nlargest(20, 'improvement_minutes')
        improvement_plot = go.Figure([go.Bar(
            x=improvements['improvement_minutes'], 
            y=improvements['district'], 
            orientation='h', 
            marker_color='steelblue'
        )])
        improvement_plot.update_layout(
            title='Top Travel Time Improvements', 
            xaxis_title='Minutes Saved', 
            yaxis_title='District',
            height=max(400, len(improvements) * 20)
        )
        
        return jsonify({
            'utilization_plot': json.dumps(utilization_plot, cls=plotly.utils.PlotlyJSONEncoder),
            'improvement_plot': json.dumps(improvement_plot, cls=plotly.utils.PlotlyJSONEncoder)
        })
    except Exception as e:
        return jsonify({'status': 'error', 'message': f'Error generating plots: {str(e)}'})

@app.route('/generate_map')
def generate_map():
    if 'optimization_results' not in session or 'cdst_data' not in session:
        return jsonify({'status': 'error', 'message': 'No data available'})
    
    try:
        results_df = pd.read_json(session['optimization_results'])
        cdst_df = pd.read_json(session['cdst_data'])
        
        # Create map centered on data
        center_lat = results_df['district_lat'].mean()
        center_lon = results_df['district_lon'].mean()
        m = folium.Map(location=[center_lat, center_lon], zoom_start=7)
        
        # Add CDST labs (red markers)
        for idx, lab in cdst_df.iterrows():
            folium.Marker(
                location=[lab['lat'], lab['lon']],
                popup=f"<b>{lab['lab_name']}</b><br>{lab['address']}<br>Capacity: {int(lab['capacity'])}",
                icon=folium.Icon(color='red', icon='info-sign')
            ).add_to(m)
        
        # Add districts (blue circles)
        for idx, district in results_df.iterrows():
            color = 'green' if district['current_assignment'] != district['optimized_assignment'] else 'blue'
            folium.CircleMarker(
                location=[district['district_lat'], district['district_lon']],
                radius=8,
                popup=f"""<b>{district['district']}</b><br>
                         Tests: {district['tests_per_quarter']}<br>
                         Current: {district['current_assignment']}<br>
                         Optimized: {district['optimized_assignment']}<br>
                         Time saved: {district['improvement_minutes']:.1f} min""",
                color=color,
                fillColor=color,
                fillOpacity=0.7
            ).add_to(m)
        
        # Add lines for reassigned districts (green lines)
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
                    popup=f"{district['district']} → {district['optimized_assignment']}"
                ).add_to(m)
        
        return m._repr_html_()
    except Exception as e:
        return f'<div class="alert alert-danger">Error generating map: {str(e)}</div>'

# Error handlers
@app.errorhandler(404)
def not_found(error):
    return redirect(url_for('index'))

@app.errorhandler(500)
def internal_error(error):
    return jsonify({'status': 'error', 'message': 'Internal server error'}), 500

# For Posit Connect deployment
if __name__ == '__main__':
    app.run(debug=True, host='0.0.0.0', port=5000)
else:
    application = app  # Posit Connect looks for 'application'