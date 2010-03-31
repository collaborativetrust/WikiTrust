#!/usr/bin/python

"""
Copyright (c) 2009 Luca de Alfaro
Copyright (c) 2010 Google Inc.
All rights reserved.

Authors: Luca de Alfaro

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation
and/or other materials provided with the distribution.

3. The names of the contributors may not be used to endorse or promote
products derived from this software without specific prior written
permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

"""

import sys, getopt, commands, os
from multiprocessing import Pool


class AccumulateFiles:
    def __init__(self, chunk_byte_size, chunk_file_size):
        self.chunk_size = 0
        self.file_list = []
        self.chunk_list = []
        self.target_chunk_size = chunk_byte_size
        self.target_chunk_length = chunk_file_size

    def add_file(self, file_name):
        file_size = os.path.getsize(file_name)
        self.file_list.append(file_name)
        self.chunk_size += file_size
        # Decides whether to move to the next chunk.
        if (self.chunk_size >= self.target_chunk_size or
            len(self.file_list) >= self.target_chunk_length):
            self.chunk_list.append(self.file_list)
            self.chunk_size = 0
            self.file_list = []

    def get_chunks(self):
        # Adds the last file list, if non-empty
        if self.file_list != []:
            self.chunk_list.append(self.file_list)
            self.chunk_size = 0
            self.file_list = []
        # Returns the list of all chunks.
        return self.chunk_list

# So we disregard errors if the directory exists.
def make_directory(d):
    commands.getoutput("mkdir " + d)

def dodo(x):
    print x
    print commands.getoutput (x)

def print_banner(s):
    print "****************************************************************"
    print s
    print "****************************************************************"

# Splits the input file into many chunks. 
def split_file(opt):
    cmd = (opt["nice"] + opt["decompression"] + " " + opt["input_file"] +
           " | " + opt["nice"] + opt["cmd_dir"] + "/splitwiki -n " +
           opt["n_pages_per_chunk"] + " -p " + opt["split_dir"])
    dodo(cmd)

# Computes the reduced statistics.  The input format is: 
# opt: contains the options
# in_file_list: list of input files
# out_dir: directory where to create the output files.
def compute_stats(descr):
    opt, dest_dir, in_file_list = descr
    in_file_str = " "
    for f in in_file_list:
        in_file_str += f + " "
    cmd = (opt["nice"] + opt["cmd_dir"] + 
           "/evalwiki -compute_stats -n_edit_judging 6 -n_text_judging 6 " + opt["dump_update_path"] +
           " -d " + dest_dir + in_file_str)
    dodo(cmd)

# Sorts the reduced statistics. 
def sort_stats(opt):
    commands.getoutput("rm -rf " + opt["bucket_dir"])
    cmd = (opt["nice"] + opt["cmd_dir"] + "/combinestats -bucket_dir " +
           opt["bucket_dir"] + " -input_dir " + opt["stats_dir"] +
           opt["hours_per_bucket"] + opt["max_n_stats_in_memory"] +
           " -use_subdirs -remove_unsorted")
    dodo(cmd)

# Computes the user reputations.
def compute_reps(opt):
    cmd = (opt["nice"] + opt["cmd_dir"] + "/generate_reputation -u " + 
           opt["rep_file"] + " -buckets " + opt["bucket_dir"] + 
           "/ -write_final_reps" + opt["robots"])
    # We don't want to print the extensive output of computerep.
    print cmd
    commands.getoutput(cmd)


# Computes the colored revisions.  The input format is:
# opt: contains the options.
# in_file_list: list of files to be processed.
def compute_trust(descr):
    opt, dest_dir, in_file_list = descr
    in_file_str = " "
    for f in in_file_list:
        in_file_str += f + " "
    cmd = (opt["nice"] + opt["cmd_dir"] + 
           "/evalwiki -trust_for_online -historyfile " + opt["rep_file"] + 
           opt["dump_update_path"] + " -blob_base_path " + 
           opt["blobs_dir"] + " -n_sigs 8 " + opt["robots"])
    cmd += " -d " + dest_dir + in_file_str
    dodo(cmd)

def usage():
    print "batch_process.py <options> <compressed xml file>"
    print "--cmd_dir <dir>: Directory where the executables are"
    print "--dir <dir>: Directory where all the processing will be done"
    print "--n_pages_per_chunk: How many pages in each chunk (default: 100)"
    print "--decomp <cmd>: command used to decompress (default: 7za e -so)"
    print "--robots <file>: file containing robots to exclude from evaluation"
    print "--n_cores <int>: number of processor cores to use (default: all)"
    print "--nice: nice the long-running processes"
    print "--dump_update_path <dir>: directory where a tree of revision"
    print "                          updates is rooted."
    print "--hours_per_bucket <int>: number of hours in each stat bucket"
    print "                            (default: 24)"
    print "--max_n_stats_in_memory <int>: max n. of statistics lines kept"
    print "                     in memory between flushes (default: 1000000)"
    print "--cleanup: remove results once they are used. This is used only"
    print "           if none of the do_... options below is specified."
    print "    If some of the following options are specified,"
    print "    it performs only part of the processing:"
    print "--do_split: splits the input file into chunks"
    print "--do_compute_stats: compute the stat files"
    print "--do_sort_stats: sorts the statistic files"
    print "--do_compute_rep: computes the user reputations"
    print "--do_compute_trust: computes text trust"

def main():
    try:
        opts, args = getopt.getopt(sys.argv[1:], "", 
                                   ["help",
                                    "cmd_dir=",
                                    "dir=",
                                    "n_pages_per_chunk=",
                                    "decomp=",
                                    "robots=",
                                    "n_cores=",
                                    "dump_update_path=",
                                    "hours_per_bucket=",
                                    "max_n_stats_in_memory=",
                                    "nice",
                                    "cleanup",
                                    "do_split",
                                    "do_compute_stats",
                                    "do_sort_stats",
                                    "do_compute_rep",
                                    "do_compute_trust",
                                    ])

    except getopt.GetoptError, err:
        # print help information and exit:
        print str(err) # will print something like "option -a not recognized"
        usage()
        sys.exit(2)

    # This dictionary is used to keep track of the options and other info.
    options = {}
    options["n_pages_per_chunk"] = "100"
    options["decompression"] = "7za e -so"
    options["cmd_dir"] = os.getcwd()
    # Some options are init to empty, and filled in only if present.
    options["robots"] = ""
    options["dump_update_path"] = ""
    options["hours_per_bucket"] = ""
    options["max_n_stats_in_memory"] = ""
    # Options on which parts to do.
    do_split = False
    do_compute_stats = False
    do_sort_stats = False
    do_compute_rep = False
    do_compute_trust = False
    do_cleanup = False

    for o, a in opts:
        if o == "--dir":
            options["base_dir"] = a
        elif o == "--n_pages_per_chunk":
            options["n_pages_per_chunk"] = a
        elif o == "--decomp":
            options["decompression"] = a
        elif o == "--cmd_dir":
            options["cmd_dir"] = a
        elif o == "--robots":
            options["robots"] = " -robots " + a
        elif o == "--do_split":
            do_split = True
        elif o == "--do_compute_stats":
            do_compute_stats = True
        elif o == "--do_sort_stats":
            do_sort_stats = True
        elif o == "--do_compute_rep":
            do_compute_rep = True
        elif o == "--do_compute_trust":
            do_compute_trust = True
        elif o == "--cleanup":
            do_cleanup = True
        elif o == "--n_cores":
            options["n_cores"] = int(a)
        elif o == "--nice":
            options["nice"] = "nice "
        elif o == "--dump_update_path":
            options["dump_update_path"] = " -dump_update_path " + a
        elif o == "--hours_per_bucket:":
            options["hours_per_bucket"] = " -hours_per_bucket " + a
        elif o == "--max_n_stats_in_memory":
            options["max_n_stats_in_memory"] = " -cache_lines " + a
        else:
            usage()
            sys.exit(2)
    if "base_dir" not in options:
        print "The --dir option cannot be omitted."
        sys.exit(2)
    if len(args) != 1:
        print "You must specify exacly one dump file to process."
        sys.exit(2)
    options["input_file"] = args[0]

    # If no particular option has been specified, then all is done.
    do_all = not (do_split or do_compute_stats or do_sort_stats or
                  do_compute_rep or do_compute_trust)
    if not do_all:
        cleanup = False

    # Fixes the nicing.
    if "nice" not in options:
        options["nice"] = ""

    # Generates the directory names.
    options["split_dir"] = options["base_dir"] + "/split_wiki"
    options["stats_dir"] = options["base_dir"] + "/stats"
    options["bucket_dir"] = options["base_dir"] + "/buckets"
    options["sql_dir"] = options["base_dir"] + "/sql"
    options["blobs_dir"] = options["base_dir"] + "/blobs"

    # Makes the directories.
    make_directory(options["base_dir"])
    make_directory(options["split_dir"])
    make_directory(options["stats_dir"])
    make_directory(options["sql_dir"])
    make_directory(options["blobs_dir"])

    # Makes some filenames.
    options["rep_file"] = options["base_dir"] + "/user_reputations.txt"

    # Makes the process pool.
    if "n_cores" in options:
        process_pool = Pool(options["n_cores"])
    else:
        process_pool = Pool()

    # Splits the input file.
    if do_split or do_all:
        print_banner("SPLITTING THE INPUT FILE")
        split_file(options)

    # Now, we have to get the names of the 3-digit subdirectories that
    # were generated.
    subdir_list = []
    for root_dir, subdirs, files in os.walk(options["split_dir"]):
        subdir_list = subdirs
        break
    subdir_list.sort()
    print "List of subdirs: ", subdir_list

    # Computes the statistics.
    if do_compute_stats or do_all:
        print_banner("COMPUTING THE STATISTICS")
        # Generates the list of tasks for computing the stats,
        # and creates the output directories.
        task_list = []
        for subdir in subdir_list:
            source_dir = options["split_dir"] + "/" + subdir
            dest_dir = options["stats_dir"] + "/" + subdir
            make_directory(dest_dir)
            for r, s, f in os.walk(source_dir):
                files = f
                break
            files.sort()
            # Accumulates the files in chunks
            accumulator = AccumulateFiles(1000000, 20)
            for source_file in files:
                full_source_file = source_dir + "/" + source_file
                accumulator.add_file(full_source_file)
            # Appends the result to the task list.
            chunks = accumulator.get_chunks()
            for c in chunks:
                task_list.append((options, dest_dir, c))
        # At this point the task list is ready.  Gives it to the pool.
        if len(task_list) > 0:
            process_pool.map(compute_stats, task_list, 1)
        
    # Sorts the statistics.
    if do_sort_stats or do_all:
        print_banner("SORTING THE STATISTICS")
        sort_stats(options)
        if do_cleanup:
            dodo("rm -rf " + options["stats_dir"]);

    # Computes the reputations.
    if do_compute_rep or do_all:
        print_banner("COMPUTING THE REPUTATIONS")
        compute_reps(options)
        if do_cleanup:
            dodo("rm -rf " + options["bucket_dir"])

    # Computes the text trust.
    if do_compute_trust or do_all:
        print_banner ("COMPUTING TEXT TRUST")
        # I can do this only if the reputation file already exists.
        if not os.path.exists(options["rep_file"]):
            print "You need to compute the user reputations first!"
        else:
            # Generates the list of tasks for computing the stats,
            # and creates the output directories.
            task_list = []
            history_size = os.path.getsize(options["rep_file"])
            for subdir in subdir_list:
                source_dir = options["split_dir"] + "/" + subdir
                dest_dir = options["sql_dir"] + "/" + subdir
                make_directory(dest_dir)
                for r, s, f in os.walk(source_dir):
                    files = f
                    break
                files.sort()
                # Accumulates the files in chunks
                accumulator = AccumulateFiles(5 * history_size, 50)
                for source_file in files:
                    full_source_file = source_dir + "/" + source_file
                    accumulator.add_file(full_source_file)
                # Appends the result to the task list.
                chunks = accumulator.get_chunks()
                for c in chunks:
                    task_list.append((options, dest_dir, c))
            # At this point the task list is ready.  Gives it to the pool.
            if len(task_list) > 0:
                process_pool.map(compute_trust, task_list, 1)
            # Cleans up if required
            if do_cleanup:
                dodo("rm -rf " + options["split_dir"])


if __name__ == '__main__':
    main()
