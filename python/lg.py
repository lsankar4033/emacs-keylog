"""Looking glass. Currently only implemented for emacs. Vim comes later.

Usage:
  lg.py [--num-days=<days>] [--mode=<mode>] [--max-output-length=<length>]

Options:
  -h --help                    Show this screen.
  --version                    Show version.
  --mode=<mode>                Limit results to the specified Emacs major-mode.
  --num-days=<days>            Number of days in the past to consider.
  --max-output-length=<length> Max number of rows to output per table  [default: 10].

"""
import os
import pytz
from datetime import datetime, timedelta
from dateutil import parser
from docopt import docopt
from itertools import chain, dropwhile, filterfalse, groupby
from tabulate import tabulate

# TODO should be configurable and synced with editor plugins
KEYLOG_BUFFER_DIR = os.path.expanduser("~/.emacs.d/looking_glass/")

def keylog_file_to_dt(filename):
    dt_str = filename.split(".log")[0]
    return parser.parse(dt_str)

# TODO The below methods are mutually redundant and due for a refactor.
def keylog_line_to_mode_key(l):
    elements = [e for e in l.strip().split(" ") if e != ''] # remove extra spaces
    mode = elements[1]
    key = " ".join(elements[2:]).strip()
    return (mode, key)

def keylog_line_to_dt(l):
    utc_timestamp = int(l.strip().split(" ")[0])
    return pytz.utc.localize(datetime.fromtimestamp(utc_timestamp))

def keylog_line_to_mode(l):
    return l.strip().split(" ")[1]

def build_key_count_map(num_days, mode):
    """Builds a map of key counts by reading all applicable keylog files found at KEYLOG_BUFFER_DIR.
    The format is {(mode,key): count}

    If mode isn't 'None', filters to keystrokes made in mode.

    If num_days isn't 'None', filters to only those keystrokes made in the last num_days days.
    """
    start_date = None
    if num_days is not None:
        start_date = pytz.utc.localize(datetime.utcnow() - timedelta(days=int(num_days)))

    keylog_filenames = os.listdir(KEYLOG_BUFFER_DIR)
    if start_date is not None:
        keylog_filenames = sorted(keylog_filenames, key=keylog_file_to_dt)

        # get the first index in the above list that might have logs we want
        later_files_with_indices = list(dropwhile(lambda x: keylog_file_to_dt(x[1]) <= start_date,
                                                  enumerate(keylog_filenames)))
        start_index = 0
        if len(later_files_with_indices) == 0:
            start_index = len(keylog_filenames) - 1
        elif later_files_with_indices[0][0] > 0:
            start_index = later_files_with_indices[0][0] - 1

        keylog_filenames = keylog_filenames[start_index:]

    keylog_files = (open(KEYLOG_BUFFER_DIR + f) for f in keylog_filenames)
    keylog_lines = chain(*(f.readlines() for f in keylog_files))
    keylog_lines = filterfalse(lambda l: l == "\n", keylog_lines) # first line in file is \n
    if start_date is not None:
        keylog_lines = filter(lambda l: keylog_line_to_dt(l) >= start_date, keylog_lines)

    if mode is not None:
        keylog_lines = filter(lambda l: keylog_line_to_mode(l) == mode, keylog_lines)

    keylog_lines = sorted(keylog_lines, key=keylog_line_to_mode_key) # need to do this because groupby only
                                                                     # creates new groups on group change
    ret = {}
    for k, g in groupby(keylog_lines, keylog_line_to_mode_key):
        ret[k] = len(list(g))
    return ret

# NOTE in the future, I may want more expressibility in what dimensions are retained.
def collapse_key_count_map(key_count_map, idx_to_retain):
    """Collapses the key_count_map (form is {(dimension_tuple): count}) to a map representing the aggregated
    counts for one of the dimensions in the tuple key.
    """
    ret = {}
    for k, v in key_count_map.items():
        new_k = k[idx_to_retain]

        if new_k in ret:
            ret[new_k] += v
        else:
            ret[new_k] = v

    return ret

def print_count_map(count_map, headers, max_output_length):
    """Print the provided 'count_map' as a formatted table with dimension headers 'headers'.
    """
    rows = []
    for k, v in count_map.items():
        dim_row = [k]
        if isinstance(k, tuple):
            dim_row = list(k)

        rows.append(dim_row + [v])
    rows.sort(key=lambda row: row[-1], reverse=True)
    rows = rows[0: max_output_length]

    print(tabulate(rows, headers=headers + ["count"]))

if __name__ == "__main__":
    arguments = docopt(__doc__, version="looking-glass 0.1")
    mode = arguments["--mode"]
    num_days = arguments["--num-days"]
    max_output_length = int(arguments["--max-output-length"])

    key_count_map = build_key_count_map(num_days, mode)

    if mode is None:
        print_count_map(collapse_key_count_map(key_count_map, 0), ["mode"], max_output_length)
        print("\n")
        print_count_map(collapse_key_count_map(key_count_map, 1), ["key"], max_output_length)

    else:
        print_count_map(collapse_key_count_map(key_count_map, 1), ["key"], max_output_length)
