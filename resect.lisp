(cl:defpackage :%resect
  (:use)
  (:export #:parse
           #:free

           #:collection-iterator
           #:iterator-next
           #:iterator-value
           #:iterator-free

           #:translation-unit-declarations
           #:translation-unit-language

           #:declaration-kind
           #:declaration-id
           #:declaration-owner
           #:declaration-name
           #:declaration-namespace
           #:declaration-mangled-name
           #:declaration-location
           #:declaration-type
           #:declaration-access-specifier
           #:declaration-template-p
           #:declaration-template-parameters
           #:declaration-template-arguments
           #:declaration-template
           #:declaration-partially-specialized-p
           #:declaration-forward-p
           #:declaration-source
           #:declaration-linkage
           #:declaration-inclusion-status


           #:location-name
           #:location-line
           #:location-column

           #:type-kind
           #:type-category
           #:type-name
           #:type-size
           #:type-alignment
           #:type-field-offset
           #:type-const-qualified-p
           #:type-plain-old-data-p
           #:type-declaration
           #:type-template-arguments
           #:type-undeclared-p
           #:type-fields

           #:template-parameter-kind

           #:template-argument-kind
           #:template-argument-type
           #:template-argument-value
           #:template-argument-position

           #:array-size
           #:array-element-type

           #:pointer-pointee-type

           #:reference-pointee-type
           #:reference-lvalue-p

           #:function-proto-result-type
           #:function-proto-parameters
           #:function-proto-variadic-p

           #:typedef-aliased-type

           #:enum-constants
           #:enum-type
           #:enum-constant-value

           #:function-parameters
           #:function-result-type
           #:function-variadic-p
           #:function-inlined-p
           #:function-storage-class
           #:function-calling-convention

           #:field-offset
           #:field-bitfield-p
           #:field-width
           #:record-fields
           #:record-methods
           #:record-parents
           #:record-abstract-p

           #:macro-function-like-p

           #:method-parameters
           #:method-result-type
           #:method-variadic-p
           #:method-pure-virtual-p
           #:method-const-p
           #:method-storage-class
           #:method-calling-convention

           #:variable-type
           #:variable-kind
           #:variable-storage-class
           #:variable-to-int
           #:variable-to-float
           #:variable-to-string

           #:make-options
           #:options-include-definition
           #:options-include-source
           #:options-exclude-definition
           #:options-exclude-source
           #:options-add-include-path
           #:options-add-framework-path
           #:options-add-language
           #:options-add-standard
           #:options-add-target
           #:options-add-define
           #:options-enable-intrinsic
           #:options-enable-single-header-mode
           #:options-enable-diagnostics
           #:destroy-options))
(cl:in-package :%resect)


(cffi:defcenum type-kind
  (:unknown 0)
  (:void 2)
  (:bool 3)
  (:char-u 4)
  (:unsigned-char 5)
  (:char16 6)
  (:char32 7)
  (:unsigned-short 8)
  (:unsigned-int 9)
  (:unsigned-long 10)
  (:unsigned-long-long 11)
  (:unsigned-int128 12)
  (:char-s 13)
  (:char 14)
  (:wchar 15)
  (:short 16)
  (:int 17)
  (:long 18)
  (:long-long 19)
  (:int128 20)
  (:float 21)
  (:double 22)
  (:long-double 23)
  (:nullptr 24)
  (:overload 25)
  (:dependent 26)
  (:float128 30)
  (:half 31)
  (:float16 32)
  (:complex 100)
  (:pointer 101)
  (:block-pointer 102)
  (:lvalue-reference 103)
  (:rvalue-reference 104)
  (:record 105)
  (:enum 106)
  (:typedef 107)
  (:function-no-prototype 110)
  (:function-prototype 111)
  (:constant-array 112)
  (:vector 113)
  (:incomplete-array 114)
  (:variable-array 115)
  (:dependent-sized-array 116)
  (:member-pointer 117)
  (:auto 118)
  (:atomic 177)
  (:extended-vector 178)

  (:template-parameter 10000))


(cffi:defcenum type-category
  (:unknown 0)
  (:arithmetic 1)
  (:pointer 2)
  (:reference 3)
  (:array 4)
  (:unique 5)
  (:aux 6))


(cffi:defcenum declaration-kind
  (:unknown 0)
  (:struct 1)
  (:union 2)
  (:class 3)
  (:enum 4)
  (:field 5)
  (:function 6)
  (:variable 7)
  (:parameter 8)
  (:typedef 9)
  (:method 10)
  (:enum-constant 11)
  (:macro 12)
  (:template-parameter 13))


(cffi:defcenum calling-convention
  (:unknown 0)
  (:default 1)
  (:c 2)
  (:x86-stdcall 3)
  (:x86-fastcall 4)
  (:x86-thiscall 5)
  (:x86-regcall 6)
  (:x86-vectorcall 7)
  (:x86-pascal 8)
  (:x86-64-win64 9)
  (:x86-64-sysv 10)
  (:aarch64-vectorcall 11)
  (:aapcs 12)
  (:aapcs-vfp 13)
  (:intel-ocl-bicc 14)
  (:swift 15)
  (:reserve-most 16)
  (:reserve-all 17))


(cffi:defcenum storage-class
  (:unknown 0)
  (:none 1)
  (:extern 2)
  (:static 3)
  (:private-extern 4)
  (:opencl-workgroup-local 5)
  (:auto 6)
  (:register 7))


(cffi:defcenum language
  (:unknown 0)
  (:c 1)
  (:c++ 2)
  (:obj-c 3))


(cffi:defcenum access-specifier
  (:unknown 0)
  (:public 1)
  (:protected 2)
  (:private 3))


(cffi:defcenum template-argument-kind
  (:unknown 0)
  (:null 1)
  (:type 2)
  (:declaration 3)
  (:null-ptr 4)
  (:integral 5)
  (:template 6)
  (:template-expansion 7)
  (:expression 8)
  (:pack 9))


(cffi:defcenum variable-kind
  (:unknown 0)
  (:int 1)
  (:float 2)
  (:string 3)
  (:other 4))


(cffi:defcenum linkage-kind
  (:unknown 0)
  (:no-linkage 1)
  (:internal 2)
  (:unique-external 3)
  (:external 4))


(cffi:defcenum template-parameter-kind
  (:unknown 0)
  (:template 1)
  (:type 2)
  (:non-type 3))


(cffi:defcenum resect-option-intrinsic
  (:unknown 0)
  (:sse 1)
  (:sse2 2)
  (:sse3 3)
  (:sse41 4)
  (:sse42 5)
  (:avx 6)
  (:avx2 7)
  (:neon 8))


(cffi:defcenum resect-inclusion-status
  (:excluded 0)
  (:weakly-excluded 1)
  (:weakly-included 2)
  (:weakly-enforced 3)
  (:included 4))


(cffi:defctype collection :pointer)
(cffi:defctype iterator :pointer)
(cffi:defctype type :pointer)
(cffi:defctype declaration :pointer)
(cffi:defctype location :pointer)
(cffi:defctype translation-unit :pointer)
(cffi:defctype options :pointer)
(cffi:defctype template-argument :pointer)
(cffi:defctype template-parameter :pointer)


;;;
;;; COLLECTION
;;;
(cffi:defcfun ("resect_collection_iterator" collection-iterator) iterator
  (collection collection))
(cffi:defcfun ("resect_iterator_next" iterator-next) :boolean
  (iterator iterator))
(cffi:defcfun ("resect_iterator_value" iterator-value) :pointer
  (iterator iterator))
(cffi:defcfun ("resect_iterator_free" iterator-free) :void
  (iterator iterator))

;;;
;;; LOCATION
;;;
(cffi:defcfun ("resect_location_line" location-line) :unsigned-int
  (location location))
(cffi:defcfun ("resect_location_column" location-column) :unsigned-int
  (location location))
(cffi:defcfun ("resect_location_name" location-name) :string
  (location location))

;;;
;;; TYPE
;;;
(cffi:defcfun ("resect_type_get_kind" type-kind) type-kind
  (type type))
(cffi:defcfun ("resect_type_get_category" type-category) type-category
  (type type))
(cffi:defcfun ("resect_type_get_name" type-name) :string
  (type type))
(cffi:defcfun ("resect_type_sizeof" type-size) :long-long
  (type type))
(cffi:defcfun ("resect_type_alignof" type-alignment) :long-long
  (type type))
(cffi:defcfun ("resect_type_offsetof" type-field-offset) :long-long
  (type type)
  (field :string))
(cffi:defcfun ("resect_type_is_const_qualified" type-const-qualified-p) :boolean
  (type type))
(cffi:defcfun ("resect_type_is_pod" type-plain-old-data-p) :boolean
  (type type))
(cffi:defcfun ("resect_type_get_declaration" type-declaration) declaration
  (type type))
(cffi:defcfun ("resect_type_template_arguments" type-template-arguments) collection
  (type type))
(cffi:defcfun ("resect_type_is_undeclared" type-undeclared-p) :boolean
  (type type))
(cffi:defcfun ("resect_type_fields" type-fields) collection
  (type type))


;;;
;;; TEMPLATE ARGUMENT
;;;
(cffi:defcfun ("resect_template_argument_get_kind" template-argument-kind) template-argument-kind
  (type template-argument))
(cffi:defcfun ("resect_template_argument_get_type" template-argument-type) type
  (type template-argument))
(cffi:defcfun ("resect_template_argument_get_value" template-argument-value) :long-long
  (type template-argument))
(cffi:defcfun ("resect_template_argument_get_position" template-argument-position) :int
  (type template-argument))

;;;
;;; ARRAY
;;;
(cffi:defcfun ("resect_array_get_size" array-size) :long-long
  (type type))
(cffi:defcfun ("resect_array_get_element_type" array-element-type) type
  (type type))

;;;
;;; POINTER
;;;
(cffi:defcfun ("resect_pointer_get_pointee_type" pointer-pointee-type) type
  (type type))


;;;
;;; REFERENCE
;;;
(cffi:defcfun ("resect_reference_get_pointee_type" reference-pointee-type) type
  (type type))
(cffi:defcfun ("resect_reference_is_lvalue" reference-lvalue-p) :boolean
  (type type))


;;;
;;; FUNCTION PROTO
;;;
(cffi:defcfun ("resect_function_proto_get_result_type" function-proto-result-type) type
  (type type))
(cffi:defcfun ("resect_function_proto_parameters" function-proto-parameters) collection
  (type type))
(cffi:defcfun ("resect_function_proto_is_variadic" function-proto-variadic-p) :boolean
  (type type))

;;;
;;; DECLARATION
;;;
(cffi:defcfun ("resect_decl_get_kind" declaration-kind) declaration-kind
  (declaration declaration))
(cffi:defcfun ("resect_decl_get_id" declaration-id) :string
  (declaration declaration))
(cffi:defcfun ("resect_decl_get_owner" declaration-owner) declaration
  (declaration declaration))
(cffi:defcfun ("resect_decl_get_location" declaration-location) location
  (declaration declaration))
(cffi:defcfun ("resect_decl_get_name" declaration-name) :string
  (declaration declaration))
(cffi:defcfun ("resect_decl_get_namespace" declaration-namespace) :string
  (declaration declaration))
(cffi:defcfun ("resect_decl_get_mangled_name" declaration-mangled-name) :string
  (declaration declaration))
(cffi:defcfun ("resect_decl_get_comment" declaration-comment) :string
  (declaration declaration))
(cffi:defcfun ("resect_decl_get_type" declaration-type) type
  (declaration declaration))
(cffi:defcfun ("resect_decl_get_access_specifier" declaration-access-specifier) access-specifier
  (declaration declaration))
(cffi:defcfun ("resect_decl_get_template" declaration-template) declaration
  (declaration declaration))
(cffi:defcfun ("resect_decl_is_template" declaration-template-p) :boolean
  (declaration declaration))
(cffi:defcfun ("resect_decl_is_partially_specialized" declaration-partially-specialized-p) :boolean
  (declaration declaration))
(cffi:defcfun ("resect_decl_is_forward" declaration-forward-p) :boolean
  (declaration declaration))
(cffi:defcfun ("resect_decl_template_parameters" declaration-template-parameters) collection
  (declaration declaration))
(cffi:defcfun ("resect_decl_template_arguments" declaration-template-arguments) collection
  (declaration declaration))
(cffi:defcfun ("resect_decl_get_source" declaration-source) :string
  (declaration declaration))
(cffi:defcfun ("resect_decl_get_linkage" declaration-linkage) linkage-kind
  (declaration declaration))
(cffi:defcfun ("resect_decl_get_inclusion_status" declaration-inclusion-status) resect-inclusion-status
  (declaration declaration))

;;;
;;; UNIT
;;;
(cffi:defcfun ("resect_unit_declarations" translation-unit-declarations) collection
  (unit translation-unit))

(cffi:defcfun ("resect_unit_get_language" translation-unit-language) language
  (unit translation-unit))


(cffi:defcfun ("resect_field_get_offset" field-offset) :long-long
  (record declaration))
(cffi:defcfun ("resect_field_is_bitfield" field-bitfield-p) :boolean
  (record declaration))
(cffi:defcfun ("resect_field_get_width" field-width) :long-long
  (record declaration))

;;;
;;; RECORD
;;;
(cffi:defcfun ("resect_record_fields" record-fields) collection
  (class declaration))
(cffi:defcfun ("resect_record_methods" record-methods) collection
  (class declaration))
(cffi:defcfun ("resect_record_parents" record-parents) collection
  (class declaration))
(cffi:defcfun ("resect_record_is_abstract" record-abstract-p) :boolean
  (class declaration))


;;;
;;; METHOD
;;;
(cffi:defcfun ("resect_method_parameters" method-parameters) collection
  (method declaration))
(cffi:defcfun ("resect_method_get_result_type" method-result-type) type
  (method declaration))
(cffi:defcfun ("resect_method_is_variadic" method-variadic-p) :boolean
  (method declaration))
(cffi:defcfun ("resect_method_is_pure_virtual" method-pure-virtual-p) :boolean
  (method declaration))
(cffi:defcfun ("resect_method_is_const" method-const-p) :boolean
  (method declaration))
(cffi:defcfun ("resect_method_get_storage_class" method-storage-class) storage-class
  (method declaration))
(cffi:defcfun ("resect_method_get_calling_convention" method-calling-convention) calling-convention
  (method declaration))


;;;
;;; MACRO
;;;
(cffi:defcfun ("resect_macro_is_function_like" macro-function-like-p) :boolean
  (declaration declaration))

;;;
;;; ENUM
;;;
(cffi:defcfun ("resect_enum_constant_value" enum-constant-value) :long-long
  (enum-constant declaration))
(cffi:defcfun ("resect_enum_constants" enum-constants) collection
  (enum declaration))
(cffi:defcfun ("resect_enum_get_type" enum-type) type
  (enum declaration))

;;;
;;; FUNCTION
;;;
(cffi:defcfun ("resect_function_parameters" function-parameters) collection
  (function declaration))
(cffi:defcfun ("resect_function_get_result_type" function-result-type) type
  (function declaration))
(cffi:defcfun ("resect_function_is_variadic" function-variadic-p) :boolean
  (function declaration))
(cffi:defcfun ("resect_function_is_inlined" function-inlined-p) :boolean
  (function declaration))
(cffi:defcfun ("resect_function_get_storage_class" function-storage-class) storage-class
  (function declaration))
(cffi:defcfun ("resect_function_get_calling_convention" function-calling-convention) calling-convention
  (function declaration))


;;;
;;; TYPEDEF
;;;
(cffi:defcfun ("resect_typedef_get_aliased_type" typedef-aliased-type) type
  (typedef declaration))

;;;
;;; VARIABLE
;;;
(cffi:defcfun ("resect_variable_get_type" variable-type) type
  (decl declaration))
(cffi:defcfun ("resect_variable_get_kind" variable-kind) variable-kind
  (decl declaration))
(cffi:defcfun ("resect_variable_get_value_as_int" variable-to-int) :long-long
  (decl declaration))
(cffi:defcfun ("resect_variable_get_value_as_float" variable-to-float) :double
  (decl declaration))
(cffi:defcfun ("resect_variable_get_value_as_string" variable-to-string) :string
  (decl declaration))
(cffi:defcfun ("resect_variable_get_storage_class" variable-storage-class) storage-class
  (decl declaration))

;;;
;;; TEMPLATE PARAMETER
;;;
(cffi:defcfun ("resect_template_parameter_get_kind" template-parameter-kind) template-parameter-kind
  (decl declaration))

;;;
;;; PARSING
;;;
(cffi:defcfun ("resect_options_create" make-options) options)
(cffi:defcfun ("resect_options_add_include_path" options-add-include-path) :void
  (opts options)
  (path :string))
(cffi:defcfun ("resect_options_add_framework_path" options-add-framework-path) :void
  (opts options)
  (path :string))
(cffi:defcfun ("resect_options_add_language" options-add-language) :void
  (opts options)
  (language :string))
(cffi:defcfun ("resect_options_add_standard" options-add-standard) :void
  (opts options)
  (standard :string))
(cffi:defcfun ("resect_options_add_target" options-add-target) :void
  "<arch><sub>-<vendor>-<sys>-<abi>"
  (opts options)
  (target :string))
(cffi:defcfun ("resect_options_add_define" options-add-define) :void
  (opts options)
  (name :string)
  (value :string))
(cffi:defcfun ("resect_options_include_definition" options-include-definition) :void
  (opts options)
  (pattern :string))
(cffi:defcfun ("resect_options_include_source" options-include-source) :void
  (opts options)
  (pattern :string))
(cffi:defcfun ("resect_options_exclude_definition" options-exclude-definition) :void
  (opts options)
  (pattern :string))
(cffi:defcfun ("resect_options_exclude_source" options-exclude-source) :void
  (opts options)
  (pattern :string))
(cffi:defcfun ("resect_options_intrinsic" options-enable-intrinsic) :void
  (opts options)
  (intrinsic resect-option-intrinsic))
(cffi:defcfun ("resect_options_single_header" options-enable-single-header-mode) :void
  (opts options))
(cffi:defcfun ("resect_options_print_diagnostics" options-enable-diagnostics) :void
  (opts options))
(cffi:defcfun ("resect_options_free" destroy-options) :void
  (opts options))


(cffi:defcfun ("resect_parse" parse) translation-unit
  (filename :string)
  (opts options))


(cffi:defcfun ("resect_free" free) :void
  (unit translation-unit))
