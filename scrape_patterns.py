#!/usr/bin/env python3
import csv
import sys
import re

from bs4 import BeautifulSoup

soup = BeautifulSoup(sys.stdin, 'html.parser')

output = csv.writer(sys.stdout, lineterminator='\n')
output.writerow(('name', 'dir', 'angles', 'inputs', 'outputs', 'great'))

title_regex = re.compile(r"(.+?)\s*(\((.*?)\s*→\s*(.*?)\))?")

for section in soup.main('section', recursive=False):
    section_title = section.h2.get_text().strip()
    section_first = True
    for subsection in section('div', recursive=False):
        if subsection.h3 is None:
            continue
        subsection_title = subsection.h3.get_text().strip()
        subsection_first = True

        # for pattern in subsection('h4'):
        for pattern in subsection('h4', class_='pattern-title'):
            pattern_title = pattern.get_text().strip()

            pattern_title_match = title_regex.fullmatch(pattern_title)
            if pattern_title_match is None:
                raise RuntimeError(pattern_title)

            pattern_name = pattern_title_match.group(1)
            pattern_name = pattern_name.replace('Rfln.', 'Reflection')
            pattern_name = pattern_name.replace('Refl.', 'Reflection')
            pattern_name = pattern_name.replace('Prfn.', 'Purification')
            pattern_name = pattern_name.replace('Purif.', 'Purification')
            pattern_name = pattern_name.replace('Dstl.', 'Distillation')
            # pattern_name = pattern_name.replace('Tan.', 'Tangent')
            # pattern_name = pattern_name.replace('Arch.', 'Archgenie')
            if '.' in pattern_name:
                raise RuntimeError(pattern_title)

            def parse_types(ts: str | None):
                return [] if ts is None else [s.strip() for s in ts.split(',')]

            pattern_inputs = parse_types(pattern_title_match.group(3))
            pattern_outputs = parse_types(pattern_title_match.group(4))
            pattern_inputs.reverse()
            pattern_outputs.reverse()

            pattern = pattern.parent

            data = pattern.canvas
            if data is None:
                continue
            output.writerow((
                ''.join(c for c in pattern_name if c.isalpha()),
                data['data-start'].upper(),
                data['data-string'],
                pattern_inputs,
                pattern_outputs,
                1 if data['data-per-world'] == 'True' else 0,
            ))
