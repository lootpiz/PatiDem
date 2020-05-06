visualizationBox <- function(object, cancer_type = NULL, patients = NULL,
    phenotype, metadata_col, metadata_levels)
{
    if (!is.null(cancer_type) && !is.null(patients)) {
        stop("Please provide either cancer_type or patients argument.")
    }

    phenotype_index = grep(phenotype, colnames(object))
    if (!is.numeric(object[,phenotype_index]) || length(phenotype_index) == 0) {
        stop("Invalid phenotype to compare. Please provide a column name with
            continuous variables.")
    }

    cancer_type_valid = is.element(cancer_type, unique(as.character(object$type)))
    if (!all(cancer_type_valid)) {
        stop(sprintf("Invalid cancer_type. Please provide among: %s",
            paste(unique(object$type), collapse = ", ")))
    } else {
        cancer_type_indices = which(as.character(object$type) %in% cancer_type)
    }

    patients_valid = is.element(patients, unique(as.character(object$bcr_patient_barcode)))
    if (!all(patients_valid)) {
        stop(sprintf("Invalid patients barcode. Please check your patients
            argument: %s", paste(patients, collapse = ", ")))
    } else {
        patients_indices = which(as.character(object$bcr_patient_barcode) %in% patients)
    }

    if (!(metadata_col %in% colnames(object)) || length(metadata_col) > 1) {
        stop("Invalid column name or too many column names.")
    } else {
        metadata_col_index = grep(metadata_col, colnames(object))
    }

    metadata_levels_valid <- is.element(metadata_levels, unique(as.character(object[,
        metadata_col_index])))
    if (!all(metadata_levels_valid) || length(metadata_levels) < 2) {
        stop(sprintf("Invalid levels %s in the column %s and/or too less levels.",
            paste(metadata_levels, collapse = ", "), metadata_col))
    } else {
        metadata_level_one_indices = which(as.character(object[,metadata_col_index]) %in%
            metadata_levels[1])
        metadata_level_two_indices = which(as.character(object[,metadata_col_index]) %in%
            metadata_levels[2])
        metadata_levels_indices = union(metadata_level_one_indices,
            metadata_level_two_indices)
    }

    if (!is.null(cancer_type)) {
        plot_title = paste0("TCGA, ", paste(cancer_type, collapse = "+"), ", N=",
            length(metadata_levels_indices))
    } else {
        plot_title = paste0("TCGA, Selected patients, N=", 
            length(metadata_levels_indices))
    }

    boxplot(object[metadata_level_one_indices, phenotype_index],
        object[metadata_level_two_indices, phenotype_index],
        names = c(paste0(metadata_levels[1], ", N=", length(metadata_level_one_indices)),
            paste0(metadata_levels[2], ", N=", length(metadata_level_two_indices))),
        xlab = toupper(metadata_col), ylab = toupper(phenotype),
        main = plot_title, pch = 20, col = c("#F2A104", "#1D65A6"))
}
