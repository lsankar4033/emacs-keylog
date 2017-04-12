"""Looking glass. Currently only implemented for emacs. Vim comes later.

Usage:
  lg.py [--num-days=<days>] [--major-mode=<mode>]

Options:
  -h --help            Show this screen.
  --version            Show version.
  --major-mode=<mode>  Limit results to the specified Emacs major-mode.
  --num-days=<days>    Number of days in the past to consider.

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

# extract (major_mode, key) from keylog line
def keylog_line_to_mode_key(l):
    elements = [e for e in l.strip().split(" ") if e != ''] # remove extra spaces
    mode = elements[1]
    key = " ".join(elements[2:]).strip()
    return (mode, key)

def keylog_line_to_dt(l):
    utc_timestamp = int(l.strip().split(" ")[0])
    return pytz.utc.localize(datetime.fromtimestamp(utc_timestamp))

# TODO unit tests. Would involve mocking out fs or building test versions of keylog files
# TODO this method should allow caller to specify major modes to exclude (i.e. insert modes)
def build_key_count_map(num_days, major_mode):
    """Builds a map of key counts by reading all applicable keylog files found at KEYLOG_BUFFER_DIR.
    The format is {(mode,key): count}

    If major_mode isn't 'None', filters to keystrokes made in major_mode.

    If num_days isn't 'None', filters to only those keystrokes made in the last num_days days.
    """
    start_date = None
    if num_days is not None:
        start_date = pytz.utc.localize(datetime.utcnow() - timedelta(days=num_days))

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

    keylog_lines = sorted(keylog_lines, key=keylog_line_to_mode_key) # need to do this because groupby only
                                                                     # creates new groups on group change
    ret = {}
    for k, g in groupby(keylog_lines, keylog_line_to_mode_key):
        ret[k] = len(list(g))
    return ret

if __name__ == "__main__":
    arguments = docopt(__doc__, version="looking-glass 0.1")
    major_mode = arguments["--major-mode"]
    num_days = arguments["--num-days"]

    key_count_map = build_key_count_map(num_days, major_mode)

    if major_mode is None:
        # TODO print mode breakdown using k_c_map

    # TODO print keystroke breakdown using k_c_map
