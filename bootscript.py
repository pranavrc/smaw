#!/usr/bin/env python

import os
from subprocess import Popen

os.environ.update({'HOME': '/home/vanharp/'})
Popen(['sbcl', '--load', '/home/vanharp/workbase/smaw/bootscript.lisp']).wait()
