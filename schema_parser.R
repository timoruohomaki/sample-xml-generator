# Functions for parsing and working with XSD schemas

library(R6)
library(xml2)

SchemaParser <- R6::R6Class(
  classname = "SchemaParser",
  
  public = list(
    #' Initialize a new SchemaParser
    #' 
    #' @param xsd_doc The XML document containing the XSD schema
    #' @param namespaces Namespaces from the XSD document
    #' @return A new SchemaParser instance
    initialize = function(xsd_doc, namespaces) {
      private$xsd_doc <- xsd_doc
      private$namespaces <- namespaces
    },
    
    #' Parse the XSD schema to extract element and type definitions
    #' 
    #' @return Invisibly returns TRUE on success
    parse = function() {
      log_info("Parsing XSD schema structure")
      
      # Parse complex types
      complex_type_nodes <- xml_find_all(private$xsd_doc, "//xs:complexType", private$namespaces)
      for (node in complex_type_nodes) {
        name <- xml_attr(node, "name")
        if (!is.na(name)) {
          private$complex_types[[name]] <- node
        }
      }
      
      # Parse simple types
      simple_type_nodes <- xml_find_all(private$xsd_doc, "//xs:simpleType", private$namespaces)
      for (node in simple_type_nodes) {
        name <- xml_attr(node, "name")
        if (!is.na(name)) {
          private$simple_types[[name]] <- node
        }
      }
      
      # Parse elements
      element_nodes <- xml_find_all(private$xsd_doc, "//xs:element", private$namespaces)
      for (node in element_nodes) {
        name <- xml_attr(node, "name")
        if (!is.na(name)) {
          private$elements[[name]] <- node
        }
      }
      
      log_info(paste("Found", length(private$elements), "elements,", 
                     length(private$complex_types), "complex types, and",
                     length(private$simple_types), "simple types"))
      
      return(invisible(TRUE))
    },
    
    #' Find the root element name from the schema
    #' 
    #' @return The name of the root element or NULL if not found
    find_root_element_name = function() {
      # Look for global elements that are likely to be the root
      global_elements <- xml_find_all(private$xsd_doc, 
                                      "/xs:schema/xs:element", 
                                      private$namespaces)
      
      if (length(global_elements) > 0) {
        return(xml_attr(global_elements[1], "name"))
      }
      
      # If no global elements found, take the first element defined in the schema
      if (length(private$elements) > 0) {
        return(names(private$elements)[1])
      }
      
      return(NULL)
    },
    
    #' Get a specific element definition
    #' 
    #' @param element_name Name of the element to retrieve
    #' @return The element node or NULL if not found
    get_element = function(element_name) {
      return(private$elements[[element_name]])
    },
    
    #' Get a specific complex type definition
    #' 
    #' @param type_name Name of the complex type to retrieve
    #' @return The complex type node or NULL if not found
    get_complex_type = function(type_name) {
      return(private$complex_types[[type_name]])
    },
    
    #' Get a specific simple type definition
    #' 
    #' @param type_name Name of the simple type to retrieve
    #' @return The simple type node or NULL if not found
    get_simple_type = function(type_name) {
      return(private$simple_types[[type_name]])
    },
    
    #' Check if a type is a complex type
    #' 
    #' @param type_name Name of the type to check
    #' @return TRUE if the type is a complex type, FALSE otherwise
    is_complex_type = function(type_name) {
      return(!is.null(private$complex_types[[type_name]]))
    },
    
    #' Check if a type is a simple type
    #' 
    #' @param type_name Name of the type to check
    #' @return TRUE if the type is a simple type, FALSE otherwise
    is_simple_type = function(type_name) {
      return(!is.null(private$simple_types[[type_name]]))
    },
    
    #' Get all element names
    #' 
    #' @return A character vector of all element names
    get_element_names = function() {
      return(names(private$elements))
    },
    
    #' Get all complex type names
    #' 
    #' @return A character vector of all complex type names
    get_complex_type_names = function() {
      return(names(private$complex_types))
    },
    
    #' Get all simple type names
    #' 
    #' @return A character vector of all simple type names
    get_simple_type_names = function() {
      return(names(private$simple_types))
    }
  ),
  
  private = list(
    xsd_doc = NULL,
    namespaces = NULL,
    elements = list(),
    complex_types = list(),
    simple_types = list()
  )
)