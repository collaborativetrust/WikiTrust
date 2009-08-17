#!/usr/bin/python

# Evaluates all the English Wiki

import commands, sys

source_prefix = "/notbackedup/wikitrust1/enwiki-20080103/"
dest_prefix   = "/notbackedup/wikitrust2/enwiki-20080103-sql"
sig_prefix    = "/notbackedup/wikitrust2/enwiki-20080103-sigs"
rev_prefix    = "/notbackedup/wikitrust1/enwiki-20080103-colrevs/"
cmd_dir = "/cse/faculty/luca/WikiTrust/batch/analysis/"

# Loops for each bunch of files

for subdir_idx in xrange(114):
    source_dir = source_prefix + "%03d" % subdir_idx
    dest_dir   = dest_prefix   + "%03d" % subdir_idx

    commands.getoutput ("mkdir " + dest_dir)
    files = commands.getoutput ("ls " + source_dir).split ()

    if subdir_idx < 2:
        step = 1
    elif subdir_idx < 10:
        step = 2
    elif subdir_idx < 20:
        step = 5
    elif subdir_idx < 40:
        step = 10
    elif subdir_idx < 70:
        step = 20
    else: 
        step = 25

    # Now processes each file, unless it already exists in the destination.
    for file_idx in xrange (0, len(files), step):
        source_file = files [file_idx]
        # gets the filename root and makes the input file names
        file_root = (source_file.split ("."))[0]
        dest_file = file_root + ".stats"
        full_source_file = source_dir + "/" + source_file
        full_dest_file   = dest_dir   + "/" + dest_file

        # Tries to process the file if it does not exist
        (missing, _) = commands.getstatusoutput ("test -f " + full_dest_file)
        if missing:
            # Creates or touches the file
            commands.getoutput ("touch " + full_dest_file)
            # Now tries to get a lock on the file
            (lock, _) = commands.getstatusoutput ("lockfile-create --retry 0 " + full_dest_file)
            if not lock:
                # Ok, it's not locked.
                # Constructs the sequence of files to be processed.
                file_list = source_file
                

                print "Processing", file_root
                sys.stdout.flush ()
                if True:
                    (err, s) = commands.getstatusoutput ("/bin/gunzip -c " + full_source_file + " | " + cmd_dir + "evalwiki -compute_stats -si " + full_dest_file)
                    if err > 0:
                        print "*** Error:", err, s
                        sys.stdout.flush ()
                # Removes the lock
                commands.getoutput ("lockfile-remove " + full_dest_file)
        # Ok, the file is processed.
