# encoding: latin1
import os
#import sys
import re
import pathlib
from copy import copy as copy

class NotLinearStatics(ValueError):
    pass

def _parse_displacement(displacement_out, f06):
    displacement_data = {}
    if not displacement_out:
        return displacement_data

    # get line where D I S P L A C E M E N T S starts
    start_index = 0
    for count, line in enumerate(f06):
        if re.match(r'^.*D I S P L A C E M E N T S.*$', line):
            start_index = count
    # get line where D I S P L A C E M E N T S ends
    end_index = start_index
    for count, line in enumerate(f06[start_index:]):
        if '------' in line:
            end_index = count + start_index
            break

    # consolidate data chunk
    data_chunk = f06[start_index+4:end_index]

    for line in data_chunk:
        data_line1 = re.sub('\s+', ',', line)
        data_line2 = re.sub('^,', '', data_line1)
        data_line3 = re.sub(',$', '', data_line2)
        if data_line3 == '':
            break
        data = data_line3.split(',')
        nid, cid, t1, t2, t3, r1, r2, r3 = _disp_line_to_values(data)
        # Coordinate ID, Displacements in X, Y, Z, Rotations about X, Y, Z
        displacement_data[nid] = (cid, t1, t2, t3, r1, r2, r3)
    return displacement_data

def _parse_applied_forces(applied_force_out, f06):
    applied_forces_data = {}
    if not applied_force_out:
        return applied_forces_data

    # get line where A P P L I E D    F O R C E S starts
    start_index = 0
    for count, line in enumerate(f06):
        if re.match(r'^.*A P P L I E D    F O R C E S.*$', line):
            start_index = count
    # get line where A P P L I E D    F O R C E S ends
    end_index = start_index
    for count, line in enumerate(f06[start_index:]):
        if '------' in line:
            end_index = count + start_index
            break

    # consolidate data chunk
    data_chunk = f06[start_index+4:end_index]

    for line in data_chunk:
        data_line1 = re.sub('\s+', ',', line)
        data_line2 = re.sub('^,', '', data_line1)
        data_line3 = re.sub(',$', '', data_line2)
        if data_line3 == '':
            break
        data = data_line3.split(',')
        nid, cid, t1, t2, t3, r1, r2, r3 = _disp_line_to_values(data)
        # Coordinate ID, Displacements in X, Y, Z, Rotations about X, Y, Z
        applied_forces_data[nid] = (cid, t1, t2, t3, r1, r2, r3)
    return applied_forces_data

def _disp_line_to_values(data):
    try:
        nid = int(data[0])
        cid = int(data[1])
        t1 = float(data[2])
        t2 = float(data[3])
        t3 = float(data[4])
        r1 = float(data[5])
        r2 = float(data[6])
        r3 = float(data[7])
    except:
        print(data)
        raise
    return nid, cid, t1, t2, t3, r1, r2, r3

def _parse_spc_forces(spc_forces_out, f06):
    spc_forces_data = {}
    if not spc_forces_out:
        return spc_forces_data

    # get line where S P C   F O R C E S starts
    start_index = 0
    for count, line in enumerate(f06):
        if re.match(r'^.*S P C   F O R C E S.*$', line):
            start_index = count

    # get line where S P C   F O R C E S ends
    end_index = start_index
    for count, line in enumerate(f06[start_index:]):
        if '------' in line:
            end_index = count + start_index
            break

    # consolidate data chunk
    data_chunk = f06[start_index+4:end_index]

    for line in data_chunk:
        data_line = re.sub('\s+', ',', line)
        data_line = re.sub('^,', '', data_line)
        data_line = re.sub(',$', '', data_line)
        if data_line == '':
            break
        data = data_line.split(',')
        nid, cid, t1, t2, t3, r1, r2, r3 = _disp_line_to_values(data)
        # Coordinate ID, SPC Forces in X, Y, Z, Rotations about X, Y, Z
        spc_forces_data[nid] = (cid, t1, t2, t3, r1, r2, r3)
    return spc_forces_data

def _parse_mpc_forces(mpc_forces_out, f06):
    mpc_forces_data = {}
    if not mpc_forces_out:
        return mpc_forces_data

    # get line where M P C   F O R C E S starts
    start_index = 0
    for count, line in enumerate(f06):
        if re.match(r'^.*M P C   F O R C E S.*$', line):
            start_index = count

    # get line where M P C   F O R C E S ends
    end_index = start_index
    for count, line in enumerate(f06[start_index:]):
        if '------' in line:
            end_index = count + start_index
            break

    # consolidate data chunk
    data_chunk = f06[start_index+4:end_index]

    for line in data_chunk:
        data_line = re.sub('\s+', ',', line)
        data_line = re.sub('^,', '', data_line)
        data_line = re.sub(',$', '', data_line)
        if data_line == '':
            break
        data = data_line.split(',')
        nid, cid, t1, t2, t3, r1, r2, r3 = _disp_line_to_values(data)
        # Coordinate ID, MPC Forces in X, Y, Z, Rotations about X, Y, Z
        mpc_forces_data[nid] = (cid, t1, t2, t3, r1, r2, r3)
    return mpc_forces_data

def _parse_stress(stress_out, f06):
    stress_data = []
    if not stress_out:
        return stress_data

    # this will be far harder.
    # results will be different for the different element/property types
    stress_indices = []
    for count, line in enumerate(f06):
        if re.match(r'^.*S T R E S S E S.*$', line):
            stress_indices.append(count)
    # stress_indices are now a list of line numbers at which point we
    # have S T R E S S E S appear. Need to loop through them
    for begin_index in stress_indices:
        # get line where S T R E S S E S ends
        end_index = copy(begin_index)
        for count, line in enumerate(f06[begin_index:]):
            if '------' in line:
                end_index = count + end_index
                break
        data_chunk = f06[begin_index:end_index]
        # need to get the element type
        mode = 'center'
        e_type = ''
        if re.match('^.*Q U A D 4.*$', data_chunk[1]) is not None:
            e_type = 'QUAD4'
        elif re.match('^.*T R I A 3.*$', data_chunk[1]) is not None:
            e_type = 'TRIA3'
        elif re.match('^.*T E T R A.*$', data_chunk[1]) is not None:
            e_type = 'TETRA'
        elif re.match('^.*H E X A.*$', data_chunk[1]) is not None:
            e_type = 'HEXA'
        elif re.match('^.*R O D.*$', data_chunk[1]) is not None:
            e_type = 'ROD'
        else:
            s = "Unknown element type produced stress results (%s)" % str(data_chunk)
            raise ValueError(s)
        # if the element type is CQUAD
        # edge case will exist if only one element has data out
        # hope that never happens.
        if e_type == 'QUAD4':
            e1_match_stat = re.match('^.*CENTER.*$', data_chunk[6])
            e2_match_stat = re.match('^.*CENTER.*$', data_chunk[9])
            if None == e1_match_stat and None == e2_match_stat:
                # in corner mode
                mode = 'corner'
        # write conditions to deal with both corner mode and center mode
        stress_data = []
        if mode == 'center':
            if e_type == 'QUAD4':
                for count, line in enumerate(data_chunk):
                    if re.match('^.*CENTER.*$', line) is not None:
                        data_line_1 = re.sub('\s+', ',', line)
                        data_line_1 = re.sub('^,', '', data_line_1)
                        data_line_1 = re.sub(',$', '', data_line_1)
                        data_line_1 = data_line_1.split(',')
                        data_line_2 = re.sub('\s+', ',',\
                                      data_chunk[count+1])
                        data_line_2 = re.sub('^,', '', data_line_2)
                        data_line_2 = re.sub(',$', '', data_line_2)
                        data_line_2 = data_line_2.split(',')
                        EID = int(data_line_1[0])
                        FD1 = float(data_line_1[2])
                        S_x_1 = float(data_line_1[3])
                        S_y_1 = float(data_line_1[4])
                        S_xy_1 = float(data_line_1[5])
                        FD2 = float(data_line_2[0])
                        S_x_2 = float(data_line_2[1])
                        S_y_2 = float(data_line_2[2])
                        S_xy_2 = float(data_line_2[3])
                        # EID, e_type, Fiber Distance, Sx, Sy, Sxy
                        L1 = [EID, e_type, FD1, S_x_1, S_y_1, S_xy_1]
                        L2 = [EID, e_type, FD2, S_x_2, S_y_2, S_xy_2]
                        stress_data.append(L1)
                        stress_data.append(L2)
            elif e_type == 'TRIA3':
                for count, line in enumerate(data_chunk):
                    if re.match('^.*CENTER.*$', line) is not None:
                        data_line_1 = re.sub('\s+', ',', line)
                        data_line_1 = re.sub('^,', '', data_line_1)
                        data_line_1 = re.sub(',$', '', data_line_1)
                        data_line_1 = data_line_1.split(',')
                        data_line_2 = re.sub('\s+', ',',\
                                      data_chunk[count+1])
                        data_line_2 = re.sub('^,', '', data_line_2)
                        data_line_2 = re.sub(',$', '', data_line_2)
                        data_line_2 = data_line_2.split(',')
                        EID = int(data_line_1[0])
                        FD1 = float(data_line_1[2])
                        S_x_1 = float(data_line_1[3])
                        S_y_1 = float(data_line_1[4])
                        S_xy_1 = float(data_line_1[5])
                        FD2 = float(data_line_2[0])
                        S_x_2 = float(data_line_2[1])
                        S_y_2 = float(data_line_2[2])
                        S_xy_2 = float(data_line_2[3])
                        # EID, e_type, Fiber Distance, Sx, Sy, Sxy
                        L1 = [EID, e_type, FD1, S_x_1, S_y_1, S_xy_1]
                        L2 = [EID, e_type, FD2, S_x_2, S_y_2, S_xy_2]
                        stress_data.append(L1)
                        stress_data.append(L2)
            elif e_type == 'TETRA':
                for count, line in enumerate(data_chunk[4:]):
                    data_line = re.sub('\s+', ',', line)
                    data_line = re.sub('^,', '', data_line)
                    data_line = re.sub(',$', '', data_line)
                    data_line = data_line.split(',')
                    EID = int(data_line[0])
                    Sxx = float(data_line[1])
                    Syy = float(data_line[2])
                    Szz = float(data_line[3])
                    Sxy = float(data_line[4])
                    #Syz = float(data_line[5])
                    #Szx = float(data_line[6])
                    # EID, e_type, Sxx, Syy, Szz, Sxy, Szx
                    L = [EID, e_type, Sxx, Syy, Szz, Sxy, Sxy]
                    stress_data.append(L)
            elif e_type == 'ROD':
                for count, line in enumerate(data_chunk[4:]):
                    # [eid, axial, axial_margin, torsion, torsion_margin], [eid, axial, axial_margin, torsion, torsion_margin]
                    sline = line.split()
                    if len(sline) == 4:
                        # not sure on torsion_margin
                        eid, axial, axial_margin, torsion, torsion_margin = line[0:9], line[9:23], line[23:33], line[33:48], line[48:58]
                        #eid, axial, axial_margin, torsion = sline
                        eid = int(eid)
                        axial = float(axial)
                        axial_margin = float(axial_margin)
                        torsion = float(torsion)
                        try:
                            torsion_margin = float(torsion_margin)
                        except ValueError:
                            torsion_margin = 0.
                        L = [eid, e_type, axial, axial_margin, torsion, torsion_margin]
                        stress_data.append(L)
                    else:
                        raise NotImplementedError(sline)
            else:
                raise NotImplementedError(e_type)
        else:
            s = "As of 2021.11.02, corner stress reading not written yet."
            raise ValueError(s)
    return stress_data

def _parse_gpforce(gpforce_out, f06):
    gpforce_data = []
    if not gpforce_out:
        return gpforce_data

    # get line with first G R I D   P O I N T   F O R C E   B A L A N C E
    gp_re_str = '^.*G R I D   P O I N T   F O R C E   B A L A N C E.*$'
    gpforce_start_index = 0
    for count, line in enumerate(f06):
        if re.match(gp_re_str, line):
            gpforce_start_index = count

    # get all of the strings
    for count, line in enumerate(f06[gpforce_start_index:]):
        # check if the line starts with "FORCE BALANCE FOR"
        s = r"^.*FORCE BALANCE FOR GRID POINT.*$"
        if re.match(s, line) is not None:
            data_line = re.sub('\s+', ',', line)
            data_line = re.sub('^,', '', data_line)
            data_line = re.sub(',$', '', data_line)
            data = data_line.split(",")
            NID = int(data[5])
            CoordID = int(data[len(data)-1])
            # gpforce_data will be of the form
            # NID, EID, T1, T2, T3, R1, R2, R3, CoordID
            gpforce_strings = []
            done_yet = False
            i = 3
            while not done_yet:
                i += 1
                newline = f06[gpforce_start_index+count+i]
                newline = re.sub('^\s+', '', newline)
                if "-----"in newline:
                    done_yet = True
                    continue
                gpforce_strings.append(newline)
            # have all the gpforce strings for this node ID
            for entry in gpforce_strings:
                if "ELEM" in entry:
                    elem_line = re.sub('\s+',',', entry)
                    elem_line = re.sub(',$','', elem_line)
                    elem_data = elem_line.split(',')
                    EID = int(elem_data[2])
                    T1 = float(elem_data[3])
                    T2 = float(elem_data[4])
                    T3 = float(elem_data[5])
                    R1 = float(elem_data[6])
                    R2 = float(elem_data[7])
                    R3 = float(elem_data[8])
                    dat_entry = [NID, EID, T1, T2, T3, R1, R2, R3, CoordID]
                    gpforce_data.append(dat_entry)
    return gpforce_data

def mystran_f06_reader(*args):
    """
    It is the goal of this subroutine to read in a f06 file, and return all
    pertinant results accordingly.
    """
    # look around, get list of the things here
    things_here = []
    basepath = '.'
    with os.scandir(basepath) as entries:
        for entry in entries:
            things_here.append(entry.name)

    # inspect user inputs
    if len(args) == 0:
        # need to look around for an f06 file in stuff here
        # get f06 files here
        f06_files_here = []
        for name in things_here:
            if re.match(r".*\.f06$", name) is not None:
                f06_files_here.append(name)
        if len(f06_files_here) == 0:
            s = "No f06 files found in" + os.getcwd()
            raise ValueError(s)
    elif len(args) == 1:
        argument = args[0]
        # check that it's a string
        if not isinstance(argument, str):
            raise ValueError("Argument passed in is not a string")
        # check that it ends in f06 or F06
        if not argument.lower().endswith("f06"):
            raise ValueError("argument passed in does not end in f06")
        # check that the file is here
        if not pathlib.Path(argument).is_file():
            raise ValueError("argument passed in isn't a file or doesn't exist")
        # if it passes all these, it's probably okay
        f06_filename = argument
    else:
        s = "Too many arguments passed into mystran_f06_reader"
        raise ValueError(s)

    results_to_return = read_f06(f06_filename)
    return results_to_return

def read_f06(f06_filename):
    # read in the f06 file
    with open(f06_filename, 'r', encoding='latin1') as f:
        f06 = f.readlines()

    # is it a linear static run?
    is_linear_static = False
    # SOL 101, SOL 1, SOL STATIC, SOL STATICS
    for count, line in enumerate(f06):
        if re.match(r'(?i)^SOL 1\s*$', line):
            is_linear_static = True
            break
        elif re.match(r'(?i)^SOL 101\s*$', line):
            is_linear_static = True
            break
        elif re.match(r'(?i)^SOL STATIC\s*$', line):
            is_linear_static = True
            break
        elif re.match(r'(?i)^SOL STATICS\s*$', line):
            is_linear_static = True
            break
    if not is_linear_static:
        raise NotLinearStatics("As of 2021.10.31, only know how to do linear statics")

    # what things are requested out?
    acceleration_out = False
    displacement_out = False
    elforce_out = False
    gpforce_out = False
    spc_force_out = False
    mpc_force_out = False
    applied_force_out = False
    stress_out = False
    strain_out = False
    for count, line in enumerate(f06):
        if re.match(r'(?i)^\s*accel(eration)?(\(.*\))?\s*=', line):
            acceleration_out = True
        elif re.match(r'(?i)^\s*displ(acement)?(\(.*\))?\s*=', line):
            displacement_out = True
        elif re.match(r'(?i)^\s*spcforce(\(.*\))?\s*=', line):
            spc_force_out = True
        elif re.match(r'(?i)^\s*mpcforce(\(.*\))?\s*=', line):
            mpc_force_out = True
        elif re.match(r'(?i)^\s*appliedforce(\(.*\))?\s*=', line):
            applied_force_out = True

        elif re.match(r'(?i)^\s*(el)?force(\(.*\))?\s*=', line):
            elforce_out = True
        elif re.match(r'(?i)^\s*gpforce(\(.*\))?\s*=', line):
            gpforce_out = True
        elif re.match(r'(?i)^\s*stress(\(.*\))?\s*=', line):
            stress_out = True
        elif re.match(r'(?i)^\s*strain(\(.*\))?\s*=', line):
            strain_out = True

    displacement_data = _parse_displacement(displacement_out, f06)
    applied_forces_data = _parse_applied_forces(applied_force_out, f06)
    spc_force_data = _parse_spc_forces(spc_force_out, f06)
    mpc_force_data = _parse_mpc_forces(mpc_force_out, f06)
    displacement_data = _parse_displacement(displacement_out, f06)
    stress_data = _parse_stress(stress_out, f06)
    gpforce_data = _parse_gpforce(gpforce_out, f06)

    # combine results to return into a dict to send back
    results_to_return = {}
    if displacement_out:
        results_to_return['displacement'] = displacement_data
    #if acceleration_out:
        #results_to_return['acceleration'] = acceleration_data
    if spc_force_out:
        results_to_return['spc_force'] = spc_force_data
    if mpc_force_out:
        results_to_return['mpc_force'] = mpc_force_data
    if applied_force_out:
        results_to_return['applied_force'] = applied_forces_data

    #if force_out:
        #results_to_return['force'] = force_data
    if stress_out:
        results_to_return['stress'] = stress_data
    #if strain_out:
        #results_to_return['strain'] = strain_data
    if gpforce_out:
        results_to_return['gpforce'] = gpforce_data

    return results_to_return


def main():
    # for development purposes, read anchor to testbed location from cli in
    #testbed_location = sys.argv[1]

    """ Development Chunks
    # get it able to read centers first
    tet_filename_center = testbed_location +\
    "/TETRA/center_stress/Example_output.F06"
    #mystran_f06_reader(tet_filename_center)
    tria_filename_center = testbed_location +\
    "/CTRIA3/centered_stresses/CTRIA3_centered_stress.F06"
    #mystran_f06_reader(tria_filename_center)
    quad_filename_center= testbed_location +\
    "/CQUAD4/centered_stresses/all_together_center_stresses.F06"
    mystran_f06_reader(quad_filename_center)
    return

    # get it able to read corners (only really does anything for QUAD4 elements
    # (thank GOD)
    tet_filename_center = testbed_location +\
    "/TETRA/corner_stress/Example_output.F06"
    #mystran_f06_reader(tet_filename_center)
    tria_filename_center = testbed_location +\
    "/CTRIA3/corner_stresses/CTRIA3_centered_stress.F06"
    #mystran_f06_reader(tria_filename_center)
    quad_filename_center= testbed_location +\
    "/CQUAD4/corner_stresses/all_together_corner_stress.F06"
    #mystran_f06_reader(quad_filename_center)
    """

if __name__ == "__main__":
    #read_f06(r'test_runs\Separated\Static\Banded\CurrentRuns\RODLOAD-MPC-FOR-SING-SB.f06')
    main()

