#############################################
# Neuroconductor Backend Pipeline
#############################################
run_prog_dir=${neuro}/programs/;
cd ${run_prog_dir};

Rnosave indiv_dep_table.R -N INDIV \
    -l mem_free=1G,h_vmem=1G,h_stack=512M

Rnosave all_dep_tables.R -N DEPS \
    -hold_jid INDIV \
    -l mem_free=1G,h_vmem=1G,h_stack=512M

Rnosave neuro_table.R -N TAB \
    -l mem_free=1G,h_vmem=1G,h_stack=512M
