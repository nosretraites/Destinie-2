
import os
from flask import Flask, flash, request, redirect, url_for, send_file, render_template
from flask_cors import CORS
from werkzeug.utils import secure_filename

import datetime
from time import sleep
import json

UPLOAD_FOLDER = '/var/log/destinie/files'
ALLOWED_EXTENSIONS = {'xlsx'}

def allowed_file(filename):
    return '.' in filename and \
           filename.rsplit('.', 1)[1].lower() in ALLOWED_EXTENSIONS

app = Flask(__name__)
app.config['UPLOAD_FOLDER'] = UPLOAD_FOLDER
CORS(app)

@app.route('/example.xlsx', methods=['GET'])
def example():
    return send_file('example.xlsx', as_attachment=True)


SANE_MODELS = ['ACTUEL', 'ACTUEL_MODIF', 'DELEVOYE', 'COMM_PM']
def common_parameters(form, sep=' '):
    params = []

    if ('noInfla' in form):
        if (form['noInfla'] == 'on'):
            params.append('--no-infla')

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

@app.route('/multi', methods=['GET', 'POST'])
def multi_mode():
    if request.method == 'POST':
        file_path, result_path = basic_mutualized(result_suffix=".aggregate.results.xlsx")
        myCmdConfig = 'Rscript ../demo/generator.R %s' % (file_path)
        os.system(myCmdConfig)
        print("Done generator!")
        xlsx_path = '%s.xlsx' % file_path[:-5]
        myCmdShift = 'Rscript ../demo/shift.R %s' % (xlsx_path)
        os.system(myCmdShift)
        print("Done shift!")
        shifted_file = '%s.shifte.xlsx' % file_path[:-5]
        myCmdResultActuel = 'Rscript ../demo/simulate.R --file %s --regime %d %s' % (shifted_file, 1, common_parameters(request.form))
        os.system(myCmdResultActuel)
        print("Done SimulateActuel!")

        myCmdMultiply = 'Rscript ../demo/multiply.R %s' % (xlsx_path)
        os.system(myCmdMultiply)
        print("Done Multiply!")
        multiple_file = '%s.multiple.xlsx' % file_path[:-5]
        myCmdResultReform = 'Rscript ../demo/simulate.R --file %s --regime %s --age-exo %d' % (multiple_file, "COMM_PM", 0)
        os.system(myCmdResultReform)
        print("Done ResultReform!")

        shifted_result = '%s.results.xlsx' % shifted_file[:-5]
        multiple_results = '%s.results.xlsx' % multiple_file[:-5]
        myCmdAggreg = "Rscript ../demo/aggregate.R %s %s %s" % (shifted_result, multiple_results, result_path)
        os.system(myCmdAggreg)
        return send_file(result_path, as_attachment=True)
    return render_template('basic.html', carrieres=carrieres)


@app.route('/', methods=['GET'])
def home():
    return render_template('home.html')
