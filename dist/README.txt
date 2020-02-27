## After running featureCounts and multiqc,

1. Run the script ~/combine_inputs_rscripts/DGEListEdgeRAnalysisAndCombinedInputJSON.R -r rna_seq_data.RData -p rna_seq_data_rpkm_pca_data.json -m multiqc_data.json -o json_output_name
2. Run the script ~/template_and_python_scripts/GenerateHTMLVisualization.py -i json_output_name.json -o html_output_name -t rnawebvis

## Browser compatibility
1. The tool was developed and tested using Google Chrome version 80+, Mozilla Firefox version 53+, and Safari version 11+.
