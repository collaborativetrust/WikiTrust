#!/usr/bin/python

# Evaluates all the English Wiki

# TODO: adapt to the new scheme, and compress the sql files.

import commands, sys

source_prefix = "/store/itwiki/split-wiki/"
sql_prefix    = "/store/itwiki/sql/"
blob_prefix   = "/store/itwiki/blobs/"
cmd_dir       = "/cse/faculty/luca/gaston/WikiTrust/analysis/"
history_file  = "/store/itwiki/rep_history.txt"
robots_file   = "/store/itwiki/wp_bots.txt"

# Creates, if needed, the main output directories.
commands.getoutput ("mkdir " + sql_prefix)
commands.getoutput ("mkdir " + blob_prefix)

# Loops for each bunch of files
for subdir_idx in xrange(19):
    bundle_name = "%03d/" % subdir_idx
    source_dir = source_prefix + bundle_name
    sql_dir    = sql_prefix    + bundle_name

    commands.getoutput ("mkdir " + sql_dir)
    files = commands.getoutput ("ls " + source_dir).split ()

    if subdir_idx < 2:
        step = 1
    elif subdir_idx < 4:
        step = 5
    else:
        step = 10

    # Now processes each file, unless it already exists in the destination.
    for file_idx in xrange (0, len (files), step):
        source_file = files [file_idx]
        # gets the filename root and makes the input file names
        file_root = (source_file.split ("."))[0]
        sql_file = file_root + ".sql"
        full_source_file = source_dir + source_file
        full_sql_file   = sql_dir   + sql_file
        # Tries to process the file if it has not been done yet.
        (missing, _) = commands.getstatusoutput ("test -f " + full_sql_file)
        if missing:
            # Creates or touches the file
            commands.getoutput ("touch " + full_sql_file)
            # Now tries to get a lock on the file
            (lock, _) = commands.getstatusoutput ("lockfile-create --retry 0 " + full_sql_file)
            if not lock:
                # Ok, it's not locked.
                # Constructs the sequence of files to be processed.
                arg_list = ""
                i = file_idx;
                while i < len(files) and i < file_idx + step:
                    arg_list += " " + source_dir + files [i]
                    i += 1
                # Builds the command.
                cmd = (cmd_dir + "evalwiki -trust_for_online -historyfile " + history_file
                       + " -blob_base_path " + blob_prefix
                       + " -n_sigs 8 -robots " + robots_file
                       + " -d " + sql_dir + " " + arg_list)
                # Debug
                print "Processing", arg_list
                print cmd
                sys.stdout.flush ()
                if True:
                    (err, s) = commands.getstatusoutput (cmd)
                    if err > 0:
                        print "*** Error:", err, s
                        sys.stdout.flush ()
                # Removes the lock
                commands.getoutput ("lockfile-remove " + full_sql_file)
        # Ok, the file is processed.
