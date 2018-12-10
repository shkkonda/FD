import csv, json

# Unique states
states = []
with open('states.csv', 'rbU') as csvfile:
    reader = csv.reader(csvfile, delimiter=',')
    for row in reader:
        states.append(row[0])

# Unique OCC_CODEs for 2004 and 2013
occ_codes = []
with open('occ_codes2013.csv', 'rbU') as csvfile:
    reader = csv.reader(csvfile, delimiter=',')
    for row in reader:
        occ_codes.append(row[0])

# Initialize dictionary to store all OCC_CODES for each state
occs_states = {}
for i in range(0, len(occ_codes)):
    occs_states[occ_codes[i]] = { }
    for j in range(0, len(states)):
        occs_states[occ_codes[i]][states[j]] = 0

# Store the data for 2013
with open('state2013.csv', 'rbU') as csvfile:
    reader = csv.reader(csvfile, delimiter=',')
    
    curr_state = None
    curr_occs = {}
    skipped_header = False
    for row in reader:
        
        if not skipped_header:
            skipped_header = True
            continue
        
        st = row[1]
        state = row[2]
        occ_code = row[3]
        occ_title = row[4]
        occ_group = row[5]
        a_median = row[20]
        
        # Convert employment count to integer
        # CSV contains astericks and missing values, hence try and except
        try:
            tot_emp = int(row[6])
        except:
            tot_emp = 0
        
        if a_median == "#":
            a_median = 187201
        else:
            try:
                a_median = int(a_median)
            except:
                a_median = -99
        
        
        # Store occupation for current state
        occs_states[occ_code]['name'] = occ_title
        occs_states[occ_code]['ocode'] = occ_code[0:2] + occ_code[3:7]
        occs_states[occ_code][st] = tot_emp
        occs_states[occ_code][st+"m"] = a_median
        


# Include national values
with open('nat2013.csv', 'rbU') as csvfile:
    reader = csv.reader(csvfile, delimiter=',')
    
    skipped_header = False
    for row in reader:
        
        if not skipped_header:
            skipped_header = True
            continue
        
        occ_code = row[0]
        occ_title = row[1]
        occ_group = row[2]
        a_median = row[15]
        
        if occ_group not in ['major', 'detailed']:
            continue
        
        if occ_code not in occs_states.keys():
            continue
        
        try:
            tot_emp = int(row[3])
        except:
            tot_emp = 0
        
        
        if a_median == "#":
            a_median = 187201
        else:
            try:
                a_median = int(a_median)
            except:
                a_median = -99
        
        
        occs_states[occ_code]['nat'] = tot_emp
        occs_states[occ_code]['natm'] = a_median
        
        

# Create JSON file
curr_title = None
curr_code = None
json_data = { 'name': 'occs', 'children': [] }
for code in occ_codes:
    if code == '00-0000':
        continue
    
    if curr_title == None:
        curr_title = occs_states[code]['name']
        curr_title = curr_title[0:len(curr_title)-12]   # Remove 'occupations' at the end
        
        curr_code = code[0:2] + code[3:7]
        curr_children = []
    elif code[3:] == '0000':
        json_data['children'].append({ 'name': curr_title, 'ocode':curr_code, 'children': curr_children })
        
        curr_title = occs_states[code]['name']
        curr_title = curr_title[0:len(curr_title)-12]   # Remove 'occupations' at the end
        
        curr_code = code[0:2] + code[3:7]
        curr_children = []
    else:
        curr_children.append(occs_states[code])

# Append the last occupation group
json_data['children'].append({ 'name': curr_title, 'ocode':curr_code, 'children': curr_children })

# Create the JSON file     
json_file = open('occs2013-plusmed.json', 'w')
json_file.write(json.dumps(json_data))
json_file.close()
    
