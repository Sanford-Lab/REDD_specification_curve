# Synthetic Controls for all projects

source("code/functions.R")

# creating the Rdata functions

process_synth_data("adpml")
process_synth_data("agrocortex")
process_synth_data("florestal")
process_synth_data("jari")
process_synth_data("maisa")
process_synth_data("manoa")
process_synth_data("purus")
process_synth_data("riopreto")
process_synth_data("rmdlt")
process_synth_data("russas")
process_synth_data("surui")
process_synth_data("valparaiso")

source("code/synthetic_control_function.R")

# apply the synthetic control and plot procedures

synthetic_control_data("adpml", 9)
synthetic_control_data("agrocortex", 14)
synthetic_control_data("florestal", 9)
synthetic_control_data("jari", 11)
synthetic_control_data("maisa", 12)
synthetic_control_data("manoa", 13)
synthetic_control_data("purus", 11)
synthetic_control_data("riopreto", 12)
synthetic_control_data("rmdlt", 8)
synthetic_control_data("russas", 11)
synthetic_control_data("surui", 9)
synthetic_control_data("valparaiso", 11)


