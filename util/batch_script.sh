# Computing the statistics (can be parallelized):
/bin/gunzip -c /home/luca/wiki-data/enwiki/wiki-00100000.xml.gz \
    | batch/analysis/evalwiki -compute_stats -si ~/wiki-data/enwork/stats/wiki-00100000.stats

/bin/gunzip -c /home/luca/wiki-data/enwiki/wiki-00100220.xml.gz \
    | batch/analysis/evalwiki -compute_stats -si ~/wiki-data/enwork/stats/wiki-00100220.stats

# Sorting the statistics:
batch/analysis/combinestats -outfile ~/wiki-data/enwork/sorted_stats/all_sorted.stat \
    -outdir ~/wiki-data/enwork/sorted_stats_temp/ ~/wiki-data/enwork/stats/

# Computing the reputations (whole histories):
batch/analysis/generate_reputation -use_reputation_cap -use_nix -nix_interval 100000 \
    -local_global_algo -u ~/wiki-data/enwork/reps/rep_history.txt \
    ~/wiki-data/enwork/sorted_stats/all_sorted.stat

# Computing the reputations (only the final ones):
batch/analysis/generate_reputation -use_reputation_cap -use_nix -nix_interval 100000 \
    -local_global_algo -u ~/wiki-data/enwork/reps/rep_history.txt \
    -write_final_reps ~/wiki-data/enwork/sorted_stats/all_sorted.stat

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

