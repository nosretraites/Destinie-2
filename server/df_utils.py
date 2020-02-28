import os
import copy
import datetime
import xlsxwriter
import random
import shutil
import io
import xlrd

def detect_threshold(inputList):
	running = None
	for i in range(len(inputList)):
		el = inputList[i]
		if (running is None):
			running = el
		else:
			if (el != running): return i
	return None
	
def normalize_size_timeSeries(inputList, targetLength):
	if (len(inputList) < targetLength):
		resultList = copy.copy(inputList)
		lastValueInput = resultList[len(resultList) - 1]
		number_missing = targetLength - len(inputList)
		for p in range(number_missing):
			resultList.append(lastValueInput)
			
		return resultList
			
	if (len(inputList) == targetLength): return inputList
	
	if (len(inputList) > targetLength): return inputList[:targetLength]
	
	
def generate_CustomScenario(targetLength, normalizedTimeSeries, localPath, CARRIERE_PRESET, base):
	workbook = xlsxwriter.Workbook(localPath)
	worksheet = workbook.add_worksheet('meta')
	worksheet.write(0, 0, 'id')
	worksheet.write(0, 1, 'base')
	worksheet.write(0, 2, 'description')
	
	worksheet.write(1, 0, 'CUSTOM')
	worksheet.write(1, 1, base)
	worksheet.write(1, 2, 'Ceci est une description')
	
	worksheet_serie = workbook.add_worksheet('serie')
	worksheet_serie.write(0, 0, 'age')
	worksheet_serie.write(0, 1, 'CUSTOM')
	for p in range(targetLength):
		worksheet_serie.write(p+1, 0, p + 1)
		worksheet_serie.write(p+1, 1, normalizedTimeSeries[p])
	workbook.close()
	
def parse_results(input_file):
	workbook_open = xlrd.open_workbook(input_file)
	
	taux_remplacement_sheet = workbook_open.sheet_by_name('taux_remplacement')
	tr_net_neut = taux_remplacement_sheet.cell(1, 1).value
	
	taux_remplacement_brut_sheet = workbook_open.sheet_by_name('taux_remplacement_brut')
	taux_remplacement_brut_age = taux_remplacement_brut_sheet.cell(1, 1).value
	taux_remplacement_brut_pension = taux_remplacement_brut_sheet.cell(1, 2).value
	taux_remplacement_brut_salaire = taux_remplacement_brut_sheet.cell(1, 3).value
	taux_remplacement_brut_tr_brut = taux_remplacement_brut_sheet.cell(1, 4).value
	
	output = {}
	output['tr_net_neut'] = tr_net_neut
	output['taux_remplacement_brut_age'] = taux_remplacement_brut_age
	output['taux_remplacement_brut_pension'] = taux_remplacement_brut_pension
	output['taux_remplacement_brut_salaire'] = taux_remplacement_brut_salaire
	output['taux_remplacement_brut_tr_brut'] = taux_remplacement_brut_tr_brut
	
	return output