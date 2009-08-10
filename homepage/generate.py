#! /usr/bin/env python
"""
Generate HTML files using the same template

Usage: ./generate.py
"""

import sys
import string
import glob
import os.path

template = [string.Template(line) for line in open("template.html")]
tokens = { "ISA_VERSION": "0.1", "EISA_VERSION": "0.1", 
           "EXPRESSIONVIEW_VERSION": "0.1" }

files = glob.glob('*.html.in')

for file in files:
    f = open(file[:-3], "w")
    content = open(file).read()
    tokens["CONTENT"] = string.Template(content).safe_substitute(tokens)
    tokens["PAGENAME"] = file[:-8]
    for tmpl in template:
	print >>f, tmpl.safe_substitute(tokens),
	
    f.close()
