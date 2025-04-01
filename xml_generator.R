# Main class for generating XML samples from XSD schemas

library(R6)
library(xml2)

# Load dependencies
source("logger.R")
source("schema_parser.R")
source("value_generator.R")
source("file_utils.R")

# Create data directories

ensure_directory("input")
ensure_directory("output")

XMLGenerator <- R6::R6Class(
  classname = "XMLGenerator",
  
  public = list(
    #' Initialize the generator with an XSD schema
    #' 
    #' @param xsd_path Path to the XSD schema file
    #' @param data_generators List of custom generator functions for specific element types
    #' @return A new XMLGenerator instance
    initialize = function(xsd_path, data_generators = list()) {
      private$validate_file_exists(xsd_path)
      private$xsd_path <- xsd_path
      private$data_generators <- data_generators
      
      tryCatch({
        private$xsd_doc <- read_xml(xsd_path)
        log_info(paste("Loaded XSD schema from", xsd_path))
      }, error = function(e) {
        log_error(paste("Failed to parse XSD schema:", e$message))
        stop(paste("Failed to parse XSD schema:", e$message))
      })
      
      # Extract namespace information
      private$namespaces <- xml_ns(private$xsd_doc)
      
      # Parse schema structure
      private$schema_parser <- SchemaParser$new(private$xsd_doc, private$namespaces)
      private$schema_parser$parse()
    },
    
    #' Generate a sample XML document
    #' 
    #' @param output_path Path where the generated XML will be saved
    #' @param count Number of sample instances to generate
    #' @return Invisibly returns the generated XML document
    generate_sample = function(output_path = NULL, count = 1) {
      log_info("Generating sample XML document")
      
      # Start with a new XML document
      xml_doc <- xml_new_document()
      
      # Find the root element
      root_element_name <- private$schema_parser$find_root_element_name()
      
      if (is.null(root_element_name)) {
        log_error("Could not determine root element from schema")
        stop("Could not determine root element from schema")
      }
      
      # Create root node
      root_node <- xml_add_child(xml_doc, root_element_name)
      
      # Add required namespaces
      for (ns_name in names(private$namespaces)) {
        if (ns_name != "") {
          xml_attr(root_node, paste0("xmlns:", ns_name)) <- private$namespaces[[ns_name]]
        } else {
          xml_attr(root_node, "xmlns") <- private$namespaces[[ns_name]]
        }
      }
      
      # Generate content for the root element
      for (i in 1:count) {
        private$generate_element_content(root_node, root_element_name)
      }
      
      # Save to file if output path is provided
      if (!is.null(output_path)) {
        tryCatch({
          write_xml(xml_doc, output_path)
          log_info(paste("Saved generated XML to", output_path))
        }, error = function(e) {
          log_error(paste("Failed to save XML document:", e$message))
          stop(paste("Failed to save XML document:", e$message))
        })
      }
      
      return(invisible(xml_doc))
    },
    
    #' Add a custom data generator for a specific element or attribute
    #' 
    #' @param element_name Name of the element or attribute
    #' @param generator_function Function that generates appropriate values
    #' @return Invisibly returns the updated XMLGenerator
    add_data_generator = function(element_name, generator_function) {
      if (!is.function(generator_function)) {
        stop("Generator must be a function")
      }
      
      private$data_generators[[element_name]] <- generator_function
      log_info(paste("Added custom data generator for", element_name))
      
      return(invisible(self))
    }
  ),
  
  private = list(
    xsd_path = NULL,
    xsd_doc = NULL,
    namespaces = NULL,
    schema_parser = NULL,
    data_generators = list(),
    
    #' Validate that a file exists
    #' 
    #' @param file_path Path to check
    #' @return TRUE if the file exists, stops execution otherwise
    validate_file_exists = function(file_path) {
      if (!file.exists(file_path)) {
        log_error(paste("File does not exist:", file_path))
        stop(paste("File does not exist:", file_path))
      }
      return(TRUE)
    },
    
    #' Generate content for an element
    #' 
    #' @param parent_node The parent XML node
    #' @param element_name The name of the element to generate
    #' @return Invisibly returns TRUE on success
    generate_element_content = function(parent_node, element_name) {
      # Check if we have a custom generator for this element
      if (!is.null(private$data_generators[[element_name]])) {
        tryCatch({
          value <- private$data_generators[[element_name]]()
          xml_text(parent_node) <- as.character(value)
          return(invisible(TRUE))
        }, error = function(e) {
          log_warn(paste("Custom generator for", element_name, "failed:", e$message))
          # Continue with default generation if custom generator fails
        })
      }
      
      # Get element definition
      element_def <- private$schema_parser$get_element(element_name)
      
      if (is.null(element_def)) {
        log_warn(paste("No definition found for element:", element_name))
        return(invisible(FALSE))
      }
      
      # Check if the element references a type
      type_name <- xml_attr(element_def, "type")
      
      if (!is.na(type_name)) {
        # Handle type reference
        if (private$schema_parser$is_complex_type(type_name)) {
          # Complex type
          private$generate_complex_type_content(parent_node, type_name)
        } else if (private$schema_parser$is_simple_type(type_name)) {
          # Simple type
          xml_text(parent_node) <- generate_simple_type_value(type_name, private$schema_parser)
        } else {
          # Built-in XSD type
          xml_text(parent_node) <- generate_builtin_type_value(type_name)
        }
      } else {
        # Check for inline type definition
        complex_type <- xml_find_first(element_def, "./xs:complexType", private$namespaces)
        if (!is.na(xml_name(complex_type))) {
          private$generate_inline_complex_type_content(parent_node, complex_type)
        } else {
          # Default to string if no type information is available
          xml_text(parent_node) <- generate_random_string()
        }
      }
      
      return(invisible(TRUE))
    },
    
    #' Generate content for a complex type
    #' 
    #' @param parent_node The parent XML node
    #' @param type_name The name of the complex type
    #' @return Invisibly returns TRUE on success
    generate_complex_type_content = function(parent_node, type_name) {
      type_def <- private$schema_parser$get_complex_type(type_name)
      
      if (is.null(type_def)) {
        log_warn(paste("No definition found for complex type:", type_name))
        return(invisible(FALSE))
      }
      
      private$generate_inline_complex_type_content(parent_node, type_def)
      
      return(invisible(TRUE))
    },
    
    #' Generate content for an inline complex type
    #' 
    #' @param parent_node The parent XML node
    #' @param type_def The complex type definition node
    #' @return Invisibly returns TRUE on success
    generate_inline_complex_type_content = function(parent_node, type_def) {
      # Handle sequence elements
      sequence <- xml_find_first(type_def, ".//xs:sequence", private$namespaces)
      if (!is.na(xml_name(sequence))) {
        seq_elements <- xml_find_all(sequence, "./xs:element", private$namespaces)
        
        for (elem in seq_elements) {
          elem_name <- xml_attr(elem, "name")
          min_occurs <- as.integer(xml_attr(elem, "minOccurs", default = "1"))
          max_occurs <- xml_attr(elem, "maxOccurs", default = "1")
          
          # Determine number of occurrences to generate
          if (max_occurs == "unbounded") {
            # Generate between min_occurs and a reasonable upper limit (e.g., 3)
            num_occurs <- min_occurs + sample(0:2, 1)
          } else {
            max_occurs <- as.integer(max_occurs)
            num_occurs <- sample(min_occurs:max_occurs, 1)
          }
          
          # Generate the specified number of occurrences
          for (i in 1:num_occurs) {
            child_node <- xml_add_child(parent_node, elem_name)
            
            # Check if element has a type attribute
            elem_type <- xml_attr(elem, "type")
            if (!is.na(elem_type)) {
              if (private$schema_parser$is_complex_type(elem_type)) {
                private$generate_complex_type_content(child_node, elem_type)
              } else if (private$schema_parser$is_simple_type(elem_type)) {
                xml_text(child_node) <- generate_simple_type_value(elem_type, private$schema_parser)
              } else {
                xml_text(child_node) <- generate_builtin_type_value(elem_type)
              }
            } else {
              # Check for inline type definition
              inline_type <- xml_find_first(elem, "./xs:complexType", private$namespaces)
              if (!is.na(xml_name(inline_type))) {
                private$generate_inline_complex_type_content(child_node, inline_type)
              } else {
                # Default to string
                xml_text(child_node) <- generate_random_string()
              }
            }
          }
        }
      }
      
      # Handle attributes
      attributes <- xml_find_all(type_def, "./xs:attribute", private$namespaces)
      for (attr in attributes) {
        attr_name <- xml_attr(attr, "name")
        attr_type <- xml_attr(attr, "type")
        attr_use <- xml_attr(attr, "use", default = "optional")
        
        # Only generate required attributes or with some probability for optional ones
        if (attr_use == "required" || runif(1) > 0.3) {
          # Check if we have a custom generator for this attribute
          if (!is.null(private$data_generators[[attr_name]])) {
            tryCatch({
              value <- private$data_generators[[attr_name]]()
              xml_attr(parent_node, attr_name) <- as.character(value)
            }, error = function(e) {
              log_warn(paste("Custom generator for attribute", attr_name, "failed:", e$message))
              # Continue with default generation if custom generator fails
            })
          } else if (!is.na(attr_type)) {
            if (private$schema_parser$is_simple_type(attr_type)) {
              xml_attr(parent_node, attr_name) <- generate_simple_type_value(attr_type, private$schema_parser)
            } else {
              xml_attr(parent_node, attr_name) <- generate_builtin_type_value(attr_type)
            }
          } else {
            # Default to string
            xml_attr(parent_node, attr_name) <- generate_random_string()
          }
        }
      }
      
      return(invisible(TRUE))
    }
  )
)