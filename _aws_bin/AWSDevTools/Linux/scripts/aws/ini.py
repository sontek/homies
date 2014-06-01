import re 
import os

#-*-python-*-

# Copyright 2014 Amazon.com, Inc. or its affiliates. All Rights
# Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License"). You
# may not use this file except in compliance with the License. A copy
# of the License is located at
#
#   http://aws.amazon.com/apache2.0/
#
# or in the "license" file accompanying this file. This file is
# distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF
# ANY KIND, either express or implied. See the License for the
# specific language governing permissions and limitations under the
# License.

class INI:
    def __init__(self, filename, default_header = True): 
	self.filename = filename
	self.default_header = default_header
	self.sections = []
	self.set_sections()

    class Section: 
	def __init__(self, name, lines): 
	    self.name = name
	    self.lines = lines

	def to_h(self):
	    h = {}
	    pattern = re.compile("^([^\=]+)\=")
	    for line in self.lines:
		match = pattern.match(line)
		if match:
		    (key, value) = line.split("=")
		    if key and value:
			h[key.strip()] = value.strip()
	    return h

	def write_settings(self, settings): 
	    pattern = re.compile("^([^\=]+)\=")
	    remaining_lines = []
	    for line in self.lines: 
		match = pattern.match(line)
		if not match or match.group(1) not in settings:
		    remaining_lines.append(line)

	    self.lines = remaining_lines
	    for key in sorted(settings.iterkeys()):
		self.lines.append("{0}={1}".format(key, settings[key]))

    def __getitem__(self, section_name):
	section = self.get_section(section_name)
	if section:
	    return section.to_h()
	return {}

    def contents(self): 
	if os.path.isfile(self.filename) and os.access(self.filename, os.R_OK):
	    with open(self.filename) as f: 
		alist = [line.rstrip() for line in f]
	    return alist
	return [] 

    def add_if_pending(self, pending_section, pending_lines): 
	if pending_lines: 
	    if not pending_section and self.default_header:
		pending_lines.insert(0, "[global]")
	    self.sections.append(self.Section(pending_section or "global", pending_lines))

    def get_section(self, section_name): 
	section = None
	if self.sections:
	    for s in self.sections:
		if s.name == section_name:
		    section = s

	if not self.sections and not section:
	    lines = []
	    if self.default_header or section_name != "global":
		lines.append("[{0}]".format(section_name))
	    section = self.Section(section_name, lines)
	    self.sections.append(section)

	return section 

    def set_sections(self):
	contents = self.contents()
	pattern = re.compile("^\[([^\]]*)\]\s*$")
	pending_lines = []
	pending_section = None

	if not contents:
	    self.sections = []
	    return

	for line in contents: 
	    match = pattern.match(line)
	    # New section beginning.Check if some old section ended here
	    # Old section would end if, pending section is not None 
	    if match: 
		self.add_if_pending(pending_section, pending_lines)
		pending_lines = [line.strip()] 
		pending_section = match.group(1).strip()
	    else: #some lines for the section
		pending_lines.append(line.strip())
	
	self.add_if_pending(pending_section, pending_lines)
	return self.sections

    def write_settings(self, section, settings):
	self.get_section(section).write_settings(settings)
	f = open(self.filename, 'w')
	for section in self.sections:
	    for line in section.lines:
		f.write(line.strip() + "\n")
