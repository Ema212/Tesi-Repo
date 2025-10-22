#
##>----------Function use-----------<


#check_bib_citations(bib_file = "files/references.bib", qmd_files = c("chapters/chapter1.qmd", "chapters/chapter2.qmd"))


##>----------Instructions------------<


# Input:
#   - bib_file: percorso (stringa) del file .bib
#   - qmd_files: vettore di percorsi file .qmd da analizzare (opzionale se qmd_dir)
#   - qmd_dir: percorso cartella contenente file .qmd (opzionale)
#              Se specificato, analizza TUTTI i .qmd nella cartella ignorando qmd_files
#   - strict: (opzionale, default = TRUE)determina quanto rigido deve essere il riconoscimento delle citazioni all’interno dei file .qmd.
#               - Se TRUE (rigido), la funzione cerca citazioni che seguono un formato stretto (es. senza caratteri strani).
#               - Se FALSE accetta anche citazioni con caratteri speciali come punti o slash, che possono essere usati in citazioni più complesse.
#   - verbose: (default= TRUE)controlla se la funzione deve stampare a video (console) i risultati del controllo
#               - TRUE, mostra i messaggi con le citazioni mancanti o non usate.
#               - FALSE, non mostra niente ma comunque ritorna i risultati in modo strutturato.


##>----------Requirements-----------<


#install.packages("stringr")

library(stringr)

#>------------Funzione -----------<

check_bib_citations <-function(bib_file, qmd_files = NULL, qmd_dir = NULL, strict = TRUE, verbose = TRUE, exclude_false = TRUE) {
  # Controllo file .bib
  if (!file.exists(bib_file)) stop("Il file .bib non esiste: ", bib_file)
  if (!grepl("\\.bib$", bib_file)) stop("Il file deve avere estensione .bib: ", bib_file)
  
  # Gestione directory dei .qmd
  if (!is.null(qmd_dir)) {
    if (!dir.exists(qmd_dir)) stop("La cartella qmd_dir non esiste: ", qmd_dir)
    qmd_files <- list.files(qmd_dir, pattern = "\\.qmd$", full.names = TRUE)
  }
  
  if (is.null(qmd_files) || length(qmd_files) == 0) {
    stop("Devi specificare almeno qmd_files o qmd_dir con almeno un file .qmd")
  }
  
  # Funzione di supporto per identificare fonti tipo "Autore2020"
  is_source_key <- function(key) {
    grepl("^[A-Za-z]+[0-9]{4}[a-zA-Z_\\-]*$", key)
  }
  
  # Funzione interna per lettura file con gestione errori
  safe_read <- function(file) {
    tryCatch(
      readLines(file, warn = FALSE, encoding = "UTF-8"),
      error = function(e) stop("Errore nella lettura di ", file, ": ", e$message)
    )
  }
  
  # Lettura file .bib ed estrazione chiavi
  bib_lines <- safe_read(bib_file)
  bib_keys_matches <- str_extract(bib_lines, "^@\\w+\\s*\\{\\s*([^,]+),")
  bib_keys <- na.omit(str_replace(bib_keys_matches, "^@\\w+\\s*\\{\\s*([^,]+),.*$", "\\1"))
  
  # Definizione pattern citazioni
  citation_pattern <- if (strict) "@[A-Za-z0-9_:-]+" else "@[A-Za-z0-9_:\\-./]+"
  
  extract_citations <- function(text) {
    keys_raw <- str_extract_all(text, citation_pattern)[[1]]
    unique(gsub("^@", "", keys_raw[nzchar(keys_raw)]))
  }
  
  # Funzione per rimuovere header YAML da un file .qmd
  remove_yaml_header <- function(text) {
    if (length(text) < 2 || text[1] != "---") return(text)
    end_yaml <- which(text == "---")[-1]
    if (length(end_yaml) == 0) return(text)
    text[-(1:end_yaml[1])]
  }
  
  all_used_keys <- list()
  missing_by_file <- list()
  possible_false_positives <- list()
  
  for (file in qmd_files) {
    lines <- safe_read(file)
    lines <- remove_yaml_header(lines)
    content <- paste(lines, collapse = "\n")
    
    used_keys <- extract_citations(content)
    all_used_keys[[file]] <- used_keys
    
    missing_in_bib <- setdiff(used_keys, bib_keys)
    
    if (length(missing_in_bib) > 0) {
      false_pos <- missing_in_bib[!is_source_key(missing_in_bib)]
      true_missing <- setdiff(missing_in_bib, false_pos)
      
      if (length(true_missing) > 0) {
        missing_by_file[[basename(file)]] <- true_missing
      }
      
      if (length(false_pos) > 0 && !exclude_false) {
        possible_false_positives[[basename(file)]] <- false_pos
      }
    }
  }
  
  combined_used_keys <- unique(unlist(all_used_keys))
  unused_bib_keys <- setdiff(bib_keys, combined_used_keys)
  
  # Output a video se richiesto
  if (verbose) {
    cat("=== Citazioni usate ma non presenti nel .bib ===\n")
    if (length(missing_by_file) == 0) {
      cat("Tutte le citazioni usate sono presenti nel file .bib.\n")
    } else {
      for (file in names(missing_by_file)) {
        cat(sprintf("- Nel file '%s' mancano in bibliografia i riferimenti per:\n", file))
        for (key in missing_by_file[[file]]) {
          cat(sprintf("  • %s\n", key))
        }
      }
    }
    
    if (!exclude_false && length(possible_false_positives) > 0) {
      cat("\n=== Possibili falsi positivi (non conformi a 'AutoreYYYY') ===\n")
      for (file in names(possible_false_positives)) {
        cat(sprintf("- Nel file '%s':\n", file))
        for (key in possible_false_positives[[file]]) {
          cat(sprintf("  • %s\n", key))
        }
      }
    }
    
    cat("\n=== Voci nella bibliografia che non hai mai usato ===\n")
    if (length(unused_bib_keys) == 0) {
      cat("Tutte le chiavi del file .bib sono state citate.\n")
    } else {
      cat("• ", paste(sort(unused_bib_keys), collapse = "\n• "), "\n")
    }
  }
  
  invisible(list(
    missing_citations = missing_by_file,
    unused_bib_keys = unused_bib_keys,
    false_positives = if (!exclude_false) possible_false_positives else NULL
  ))
}


check_bib_citations(bib_file= "files/references.bib", qmd_dir = "chapters", strict= TRUE, exclude_false = FALSE)





