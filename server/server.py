
import os
from flask import Flask, flash, request, redirect, url_for, send_file, render_template, jsonify, abort
from flask_cors import CORS
from werkzeug.utils import secure_filename

import datetime
from time import sleep
import json
import df_utils
import random

UPLOAD_FOLDER = '/tmp'
ALLOWED_EXTENSIONS = {'xlsx'}
CARRIERE_PRESET = "../demo/carrieres.xlsx"
CARRIERE_LENGTH = 42
LIFE_LENGTH = 97

SMIC_NET = 14071.0
SMIC_BRUT = 17968.0

def allowed_file(filename):
    return '.' in filename and \
           filename.rsplit('.', 1)[1].lower() in ALLOWED_EXTENSIONS

app = Flask(__name__)
app.config['UPLOAD_FOLDER'] = UPLOAD_FOLDER
CORS(app)

@app.route('/example.xlsx', methods=['GET'])
def example():
    return send_file('example.xlsx', as_attachment=True)


@app.route('/carrieres.xlsx', methods=['GET'])
def carrieres():
    return send_file('../demo/carrieres.xlsx')


SANE_MODELS = ['ACTUEL', 'ACTUEL_MODIF', 'DELEVOYE', 'COMM_PM']
def common_parameters(form, sep=' '):
    params = []


    if ('modele' in form):
        if (form['modele'] in SANE_MODELS):
            params.append('--regime')
            params.append(form['modele'])

    if ('age' in form):
        val = int(request.form['age'])
        if val:
            params.append('--age-exo')
            params.append(str(val))

    return sep.join(params)


@app.route('/expert', methods=['GET', 'POST'])
def expert_mode():
    if request.method == 'POST':
        # check if the post request has the file part
        if 'file' not in request.files:
            return render_template('expert.html', message="Envoyez un fichier à traiter.")
        file = request.files['file']
        # if user does not select file, browser also
        # submit an empty part without filename
        if file.filename == '':
            return render_template('expert.html', message="Envoyez un fichier à traiter.")
        if file and allowed_file(file.filename):
            filename = secure_filename(file.filename)
            prefix = str(datetime.datetime.now()).replace(':', '-').replace(' ', '--')
            file_path = os.path.join(app.config['UPLOAD_FOLDER'], '%s-%s' % (prefix, filename))
            file.save(file_path)


            result_path = '%s.results.xlsx' % file_path[:-5]

            myCmd = 'Rscript ../demo/simulation.R --file %s %s' % (file_path, common_parameters(request.form))

            os.system(myCmd)
            return send_file(result_path, as_attachment=True)
        else:
            return render_template('expert.html', message="Le fichier envoyé doit être au format XLSX.")
    return render_template('expert.html')


# from collections import namedtuple
# C = namedtuple('Carriere', ['id', 'description'])
# carrieres = [C('SMPT', 'SMPT'), C('SMIC', 'SMIC')]

import pandas as pd
meta = pd.read_excel('../demo/carrieres.xlsx', sheet_name='meta')
carrieres = list(meta.itertuples())

def basic_mutualized(result_suffix=".results.xlsx"):
        filename = "config.json"
        prefix = str(datetime.datetime.now()).replace(':', '-').replace(' ', '--')
        file_path = os.path.join(app.config['UPLOAD_FOLDER'], '%s-%s' % (prefix, filename))
        data = {
            "naissance": int(request.form["naissance"]),
            "debut": int(request.form["debut"]),
            "carriere": request.form["carriere"],
            "carrieres_path": "../demo/carrieres.xlsx",
            "proportion": float(request.form["proportion"]),
        }
        with open(file_path, "w+") as fp:
            json.dump(data, fp)
            print(file_path)
        return file_path, '%s%s' % (file_path[:-5], result_suffix)


@app.route('/basic', methods=['GET', 'POST'])
def basic_mode():
    if request.method == 'POST':
        file_path, result_path = basic_mutualized()
        myCmd = 'Rscript ../demo/simulation.R --config %s %s' % (file_path, common_parameters(request.form))
        os.system(myCmd)
        return send_file(result_path, as_attachment=True)
        #return render_template('basic.html', form=request.form)
    return render_template('basic.html', carrieres=carrieres)

@app.route('/basic2', methods=['GET', 'POST'])
def basic2_mode():
    if request.method == 'POST':
        file_path, result_path = basic_mutualized()
        myCmdConfig = 'Rscript ../demo/generator.R %s' % (file_path)
        os.system(myCmdConfig)
        generated_path = '%s.xlsx' % file_path[:-5]
        myCmdResult = 'Rscript ../demo/simulate.R --file %s %s' % (generated_path, common_parameters(request.form))
        os.system(myCmdResult)
        return send_file(result_path, as_attachment=True)
        #return render_template('basic.html', form=request.form)
    return render_template('basic.html', carrieres=carrieres)

	
@app.route('/fetch_carrierPaths', methods=['GET'])
def fetch_carrierPaths():
	response = {}
	current_carrieres = pd.read_excel(CARRIERE_PRESET, sheet_name='serie')
	
	list_carrieres = list(current_carrieres.columns)
	list_carrieres.remove('age')
	
	response['carrieres'] = {}
	for carriere in list_carrieres:
		carrierList = current_carrieres[carriere].tolist()
		carrierListThreshold = df_utils.detect_threshold(carrierList)
		
		carrierList_res = carrierList[carrierListThreshold:]
		carrierList_res = carrierList_res[:CARRIERE_LENGTH]
		
		response['carrieres'][carriere] = carrierList_res
	
	
	response['list_carrieres'] = list_carrieres
	response['timestamps'] = current_carrieres['age'].tolist()
	
	response['smic_net'] = SMIC_NET
	response['smic_brut'] = SMIC_BRUT
	
	return jsonify(response)
	
	
@app.route('/custom_simulation', methods=['POST'])
def custom_simulation():
	# --- Check request format
	if not(request.json):return 'Payload needs to be a JSON', 400
	mydata = request.json 
	if not('timeSeries' in mydata): return 'timeSeries not found in json', 400
	if not('base' in mydata): return 'base not found in json', 400
	if not(mydata['base'] in ['SMPT','SMIC']): return 'base should be SMPT or SMIC', 400
	
	if not('naissance' in mydata): return 'naissance not found in json', 400
	if not('debut' in mydata): return 'debut not found in json', 400
	
	# --- Build params:
	params = []
	if ('noInfla' in mydata):
		if (mydata['noInfla'] == 'on'):
			params.append('--no-infla')

	if ('modele' in mydata):
		if (mydata['modele'] in SANE_MODELS):
			params.append('--regime')
			params.append(mydata['modele'])

	if ('age' in mydata):
		val = int(mydata['age'])
		if val:
			params.append('--age-exo')
			params.append(str(val))
	
	# --- Pre-processing
	normalizedTimeSeries = df_utils.normalize_size_timeSeries(mydata['timeSeries'], LIFE_LENGTH)
	
	# --- Create Custom Carriere
	custom_Carrier_preset_location_prefix = 'CARRIER_PRESET_'+str(datetime.datetime.now()).replace(':', '-').replace(' ', '--').replace('.','-') + str(random.randint(0,1000000)) + ".xlsx"
	custom_Carrier_preset_location = os.path.join(app.config['UPLOAD_FOLDER'], custom_Carrier_preset_location_prefix)
	custom_Carrier_preset_location = custom_Carrier_preset_location_prefix
	df_utils.generate_CustomScenario(LIFE_LENGTH, normalizedTimeSeries, custom_Carrier_preset_location, CARRIERE_PRESET, mydata['base'])
	
	# --- Basic route mecanics
	filename = "config.json"
	prefix = str(datetime.datetime.now()).replace(':', '-').replace(' ', '--')
	file_path = os.path.join(app.config['UPLOAD_FOLDER'], '%s-%s' % (prefix, filename))
	
	data = {
		"naissance": int(mydata['naissance']),
		"debut": int(mydata['debut']),
		"carriere": 'CUSTOM',
		"carrieres_path": custom_Carrier_preset_location,
		"proportion": 1,
	}
	with open(file_path, "w+") as fp:
		json.dump(data, fp)

	result_path = '%s.results.xlsx' % file_path[:-5]
	myCmd = 'Rscript ../demo/simulation.R --config %s %s' % (file_path, ' '.join(params))
	os.system(myCmd)
	
	# --- Clean local file
	os.remove(custom_Carrier_preset_location)
	
	# --- Parse results
	response = {}
	response['normalizedTimeSeries'] = normalizedTimeSeries
	response['output'] = df_utils.parse_results(result_path)
	
	return jsonify(response)

@app.route('/multi', methods=['GET', 'POST'])
def multi_mode():
    if request.method == 'POST':
        file_path, result_path = basic_mutualized(result_suffix=".aggregate.results.xlsx")
        myCmdConfig = 'Rscript ../demo/multi.R %s %s' % (file_path, result_path)
        os.system(myCmdConfig)
        return send_file(result_path, as_attachment=True)
    return render_template('multi.html', carrieres=carrieres)


@app.route('/debug', methods=['GET', 'POST'])
def debug():
    if request.method == 'POST':
        file_path, result_path = basic_mutualized(result_suffix=".aggregate.results.xlsx")

        myCmdConfig = 'Rscript ../demo/generator.R %s' % (file_path)
        os.system(myCmdConfig)
        xlsx_path = '%s.xlsx' % file_path[:-5]

        myCmdMultiply = 'Rscript ../demo/multiply.R %s' % (xlsx_path)
        os.system(myCmdMultiply)
        multiple_file = '%s.multiple.xlsx' % file_path[:-5]

        myCmdResultActuel = 'Rscript ../demo/simulate.R --file %s %s' % (multiple_file, common_parameters(request.form))
        os.system(myCmdResultActuel)
        multiple_results = '%s.results.xlsx' % multiple_file[:-5]

        return send_file(multiple_results, as_attachment=True)
    return render_template('debug.html', carrieres=carrieres)


@app.route('/', methods=['GET'])
def home():
    return render_template('home.html')