# This script is not meant to be run!
# It is simply a collection of commands, to be used as a reminder.

# Computing the statistics (can be parallelized):
/bin/gunzip -c /home/luca/wiki-data/enwiki/wiki-00100000.xml.gz \
    | batch/analysis/evalwiki -compute_stats -si ~/wiki-data/enwork/stats/wiki-00100000.stats

/bin/gunzip -c /home/luca/wiki-data/enwiki/wiki-00100220.xml.gz \
    | batch/analysis/evalwiki -compute_stats -si ~/wiki-data/enwork/stats/wiki-00100220.stats

# Sorting the statistics (use -n_digits 5 for small wikis):
batch/analysis/combinestats \
    -bucket_dir ~/wiki-data/enwork/buckets/ \
    -input_dir ~/wiki-data/enwork/stats/ \
    -n_digits 4 -use_subdirs

# Computing the reputations (whole histories):
batch/analysis/generate_reputation -u ~/wiki-data/enwork/reps/rep_history.txt \
    -buckets -gen_exact_rep ~/wiki-data/enwork/buckets/ 

# Computing the reputations (only the final ones):
batch/analysis/generate_reputation -u ~/wiki-data/enwork/reps/rep_history.txt \
    -buckets ~/wiki-data/enwork/buckets/ -gen_exact_rep -write_final_reps

# Generating the colored pages and the sql file for batch-online:
batch/analysis/evalwiki -trust_for_online \
    -historyfile ~/wiki-data/enwork/reps/rep_history.txt \
    -rev_base_path ~/wiki-data/enwork/revtree \
    -sig_base_path ~/wiki-data/enwork/sigtree \
    -n_sigs 8 \
    -d ~/wiki-data/enwork/sql \
    /home/luca/wiki-data/enwiki/wiki-00100000.xml.gz

batch/analysis/evalwiki -trust_for_online \
    -historyfile ~/wiki-data/enwork/reps/rep_history.txt \
    -rev_base_path ~/wiki-data/enwork/revtree \
    -sig_base_path ~/wiki-data/enwork/sigtree \
    -n_sigs 8 \
    -d ~/wiki-data/enwork/sql \
    /home/luca/wiki-data/enwiki/wiki-00100220.xml.gz

# Load the xml files in the wiki db:
cd test-scripts 
python load_data.py --clear_db /home/luca/wiki-data/enwiki/wiki-00100000.xml /home/luca/wiki-data/enwiki/wiki-00100220.xml

# Loads the reputations in the wiki db:

# Loads the sql in the wiki db:
