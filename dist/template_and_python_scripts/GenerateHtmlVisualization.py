#!/usr/bin/env python3

import sys
import os
import argparse
import logging
import json
import string
import lxml.html
import lzstring

"""
Script to combine the JSON data with the visualization template.
"""

# get the available templates
template_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)), "html_templates")
templates = list(filter(lambda x: os.path.isdir(os.path.join(template_dir, x)), os.listdir(template_dir)))
#templates = ["bivariatetests"]

# set up the logger
logger = logging.getLogger(os.path.basename(sys.argv[0]))

# set up the parser
parser = argparse.ArgumentParser("Script to combine the JSON data with the visualization templates.")
parser.add_argument("-i", "--input", action="store", dest="input", type=str, required=True, help="Input JSON filename.")
parser.add_argument("-o", "--output", action="store", dest="output", type=str, required=True, help="Output file basename.")
parser.add_argument("-t", "--template", action="store", dest="template", type=str, required=True, choices=templates, help="Template to use.")
opt = parser.parse_args()

# set the logging levels
logger.setLevel(logging.DEBUG)

# set up the logging handlers
if os.path.exists("%s.log" % opt.output):
	os.unlink("%s.log" % opt.output)
fh = logging.FileHandler("%s.log" % opt.output)
fh.setLevel(logging.DEBUG)
ch = logging.StreamHandler()
ch.setLevel(logging.DEBUG)
formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
fh.setFormatter(formatter)
ch.setFormatter(formatter)
logger.addHandler(fh)
logger.addHandler(ch)

logger.info("CMD: %s" % " ".join(sys.argv))

# read the JSON input file
if os.path.exists(opt.input):
	# locate the template html file
	tfilename = list(filter(lambda x: os.path.isfile(os.path.join(template_dir, opt.template, x)) and x.endswith(".html") and not x.startswith("."), os.listdir(os.path.join(template_dir, opt.template))))
	if len(tfilename) == 1:
		tfilename = tfilename[0]
		# read the template file
		logger.info("Reading HTML template file %s" % tfilename)
		infile = open(os.path.join(template_dir, opt.template, tfilename))
		html = infile.read()
		infile.close()
		# parse the html file
		html = lxml.html.fromstring(html)
		logger.info("HTML template file %s read and parsed" % tfilename)
		# read the input file
		logger.info("Reading input file %s" % opt.input)
		infile = open(opt.input)
		data = json.load(infile)
		infile.close()
		logger.info("Input file %s read" % opt.input)
		# replace the script with src in the local directory to inline
		logger.info("Combining files")
		for node in html.xpath("//script"):
			if node.get("src") != None:
				# special processing for data.js which will be replaced using actual data
				if node.get("src") == "data.js" or node.get("src") == "stats_test_data.js":
					logger.info("Inlining input data")
					x = lzstring.LZString()
					data = json.dumps(data)
					data = x.compressToBase64(data)
					node.text = "var data = " + "'" + data + "'" 
					del node.attrib["src"]
				# if the script file exists, replace with an inline version
				elif os.path.isfile(os.path.join(template_dir, opt.template, node.get("src"))):
					logger.info("Inlining script %s" % os.path.join(template_dir, opt.template, node.get("src")))
					infile = open(os.path.join(template_dir, opt.template, node.get("src")), encoding="utf8")
					code = infile.read()
					if (node.get("src") in ("plotly.js", "xlsx.full.min.js")):
						logger.info("Compressing using lzstring")
						x = lzstring.LZString()
						code = x.compressToBase64(code)
						code = "compressed_codes.push('" + code + "');"
					node.text = code
					infile.close()
					del node.attrib["src"]
		# replace the stylesheet links with inline versions
		for node in html.xpath("//link[@rel='stylesheet' and @type='text/css']"):
			if node.get("href") != None and os.path.isfile(os.path.join(template_dir, opt.template, node.get("href"))):
				logger.info("Inlining stylesheet %s" % os.path.join(template_dir, opt.template, node.get("href")))
				infile = open(os.path.join(template_dir, opt.template, node.get("href")), encoding="utf8")
				node.text = infile.read()
				infile.close()
				for key in list(node.attrib.keys()):
					del node.attrib[key]
				node.tag = "style"
		logger.info("Writing out combined HTML file.")
		outfile = open("%s_%s.html" % (opt.output, opt.template), "w")
		outfile.write(lxml.html.tostring(html, doctype="<!DOCTYPE HTML>", encoding="unicode"))
		outfile.close()
		logger.info("All done")
	else:
		logger.error("Unable to locate a single HTML file in the template directory")
		sys.exit(1)
else:
	logger.error("Unable to locate file %s" % opt.input)
	sys.exit(1)
