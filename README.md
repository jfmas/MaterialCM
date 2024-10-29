# MaterialCM
Código de R del manuscrito *Mapeo batimétrico en aguas costeras con imágenes de satélite: Un estudio de caso en el Caribe Mexicano*

1. **Preprocess_Landsat.R**: Untar Landsat files as downloaded, apply mask (clouds, shadows, water), cut to area of interest, read metadata, convert to Surface refelctance and save as a tiff file.
2. **Preprocess_Sentinel.R**: Read pp2 files, apply mask (clouds, shadows, water), cut to area of interest, read metadata, convert to Surface refelctance and save as a tiff file.
3. **Correct_Glint_Effect.R**: Apply Glint Correction according to hedley et al. (2005). It is a function which need to be sourced from other scripts
4. **Genera_tabla_ratio_profundidad.R**: makes iterations on sensors, glint correction option and filtering option. For each iteration (combination) save a csv table with ratio values ​​for each bathymetric point in situ
calcula_profundidadmax_compara3: Determines the maximum depth at which the signal "reaches" and allows depth assessment. based on iterations reducing the max depth, calculate the correlation R2 of the linear model
and creates a table with the R2 for different max depths for all combinations of sensor, glint option and filtering option.





## References
Hedley, J.D., A. R. Harborne & P. J. Mumby (2005), Technical note: Simple and robust removal of sun glint for mapping shallow‐water benthos, International Journal of Remote Sensing, 26:10, 2107-2112, DOI: 10.1080/01431160500034086
